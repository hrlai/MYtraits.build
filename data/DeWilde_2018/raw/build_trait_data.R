# DeWilde_2018: extract traits from the Flora of Peninsular Malaysia
# Myristicaceae treatment (W.J.J.O. de Wilde & B.E.E. Duyfjes 2018, FPM
# Series II vol. 7: 127--256). Input is the PDF text layer (born-digital),
# segmented in raw/segment_source.R. Clause parser after Prance_2012.
#
# Writes (run from the repository root):
#   data/DeWilde_2018/raw/trait_data.csv      mapped and unmapped records
#   data/DeWilde_2018/raw/unmapped_terms.csv  summary of the unmapped records
#   data/DeWilde_2018/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with, else the organ of its
# piece or sentence. "Male flowers:" and "Female flowers:" sentences put
# their values on `entity_measured` (male / female flower, male / female
# flower bud). Scorers run per organ; every chunk that scores nothing stays
# as an unmapped row. Parenthetical asides that are not numbers
# ("(elsewhere ...)", "(or 5)") are taken out before scoring and kept as
# unmapped rows. Later sections (Distribution, Ecology, Conservation status,
# Uses, Taxonomy, Note, vernacular names) are kept one sentence per row;
# phenology, scent, dispersal and cited observations are in
# raw/manual_rows.csv. Keys and figures are not read. Names are as printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/DeWilde_2018/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "DeWilde_2018"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary.csv"
)
dict <- dict[names(dict) %in% names(traits)]

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

aside_re <- "\\s?\\((?![–\\d]|c\\.)[^)]*\\)"

prep <- function(x) {
  x |>
    # "Trees, monoecious (always?)", "aril yellow (always?)", "?without starch":
    # the author's query is a hedge
    str_replace_all("\\s?\\(always\\?\\)", " possibly") |>
    str_replace_all("\\?(?=[a-z])", "possibly ") |>
    # "(ob)ovoid" names two shapes; "(sub)globose" is subglobose
    str_replace_all("\\(ob\\)([a-z]+)", "\\1 or ob\\1") |>
    str_replace_all("\\(sub\\) ?([a-z]+)", "sub\\1") |>
    # counts: "lobes (2 or) 3 or 4", "anthers (6 or) 7--11", "lobes 3 or 4"
    str_replace_all("\\((\\d+) or\\) ?(?=\\d)", "(\\1–)") |>
    # "perianth (3- or) 4-lobed in male, (2- or) 3-lobed in female"
    str_replace_all("\\((\\d)- or\\) ?(?=\\d-lobed)", "(\\1–)") |>
    str_replace_all("\\b(lobes|anthers) (\\(\\d+–\\))?(\\d+) or (\\d+)\\b", "\\1 \\2\\3–\\4") |>
    # "Treelet 2 m tall to tree 12 m tall": one height range; "beginning to
    # flower at 2 m" is not the plant height (skill step 4)
    str_replace_all("\\b(beginning to flower|flowering) at [\\d.]+ m( tall)?\\s?", "") |>
    str_replace_all("(\\d+) m tall to tree (\\d+) m", "\\1–\\2 m") |>
    # "yellowish or (in New Guinea) bright red": the alternative after an
    # out-of-region aside is out of scope (Regions rule)
    str_remove_all(",? or \\((in New Guinea|elsewhere)\\) [^,;.]+") |>
    # other non-numeric asides are scored nowhere (see asides_of())
    str_remove_all(aside_re) |>
    # "Male flowers: ..." / "Leaves: petiole ..." / "Seed: aril red"
    str_replace_all("(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

# Non-numeric asides of a paragraph, for unmapped rows
asides_of <- function(x) {
  x <- str_replace_all(x, "\\((ob|sub)\\) ?(?=[a-z])", "")
  x <- str_remove_all(x, "\\s?\\(always\\?\\)|\\(\\d+ or\\)")
  a <- str_squish(str_extract_all(x, aside_re)[[1]])
  a[!str_detect(a, "^\\([a-z]{1,2}\\)$")]
}

# Organ of an aside: that of the last piece before it in its paragraph
aside_organ <- function(x, a) {
  before <- str_sub(x, 1, str_locate(x, fixed(a))[1, 1] - 1)
  cl <- last(split_clauses(prep(before)))
  if (is.null(cl) || !nrow(cl)) {
    return("habit")
  }
  unmapped_organ(coalesce(cl$organ, cl$piece_organ, cl$sent_organ, "habit"), cl$sent_organ)
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|(?:up )?to\\s)?", num,
  "(?:–", num, ")?(?:\\s?\\(–", num, "\\))?"
)
unit_re <- "(mm|cm|m)\\b"
cnt <- "(?:\\(\\d+–\\)\\s?)?(?:c\\.\\s?)?\\d+(?:–\\d+)?(?:\\s?\\(–\\d+\\))?"

grab <- function(x, keyword) {
  str_extract(x, paste0(rng, "\\s?", unit_re, "\\s?", keyword))
}
# a length printed without "long" ("peduncle 1--1.5 cm")
grab_len <- function(x) {
  str_extract(
    x,
    paste0(rng, "\\s?", unit_re, "(?!\\s?(wide|broad|thick|diam|across|high|tall))")
  )
}
grab_dims <- function(x) {
  m <- str_match(
    x,
    paste0("(", rng, ")\\s?×\\s?(", rng, ")\\s?", unit_re)
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(raw = m[1, 1], parts = m[1, 2:3], unit = m[1, 4])
}

hedged <- \(x) str_detect(x, flora_hedge_re)
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsut|pilose|",
  "villous|sericeous|velutinous|woolly|puberul|setose|strigose|lanate|",
  "indumentum|scurfy|felty|velvety"
)
# chunks that only qualify the hairs of the clause before ("powdery",
# "dense, rusty brown", "easily rubbed off")
hair_adj_re <- paste0(
  "^((and|or|to|more-or-less|very|dense|densely|sparse|sparsely|appressed|",
  "persistent|subpersistent|powdery|mealy|scurfy|stellate|dendroid|sessile|",
  "stalked|woolly|shaggy|felty|velvety|rough|soft|stiff|coarse|fine|minute|",
  "short|long|conspicuous|inconspicuous|weak|mixed|scale-like|easily|rubbed|",
  "off|rusty|brown|brownish|grey|greyish|pale|whitish|white|yellowish|",
  "reddish|golden|red|dark|light|buff|fulvous)[ -]?)+$"
)
age_re <- paste0(
  "glabrescent|when young|young|initially|at first|becoming|soon|",
  "when mature|with age|later|older|shed"
)
# colour markings and tinges are not the ground colour (standing rule)
marking_re <- "tinge|tinged|suffused|flushed|patch|spot|blotch|stripe|wart|variegat|marks|dots"
# comparisons with another taxon or flower sex are not scored
compare_re <- "\\b(than in|as in|like (that|those) of|similar to|differs|unlike)\\b"

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the FPM guide)
sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^((small|medium|medium-sized|big|large|slender|usually a|or|to|and) )*(trees?|treelets?|shrubs?)\\b|^(dioecious|monoecious)\\b",
  "bark", "^bark",
  "twigs", "^(young |older )?twigs?",
  "leaves", "^leaves|^leaf\\b(?! buds?)",
  "inflorescences", "^(male |female )?inflorescences?",
  "infructescences", "^infructescences?",
  "male flowers", "^male (flowers?|buds?)\\b",
  "female flowers", "^female (flowers?|buds?)\\b",
  "flowers", "^flowers?\\b",
  "fruits", "^fruits?\\b",
  "seeds", "^seeds?\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "bole", "^bole\\b",
  "buttresses", "^((often|sometimes|occasionally|occasional|mostly) )?((with|without|with or without) )?((a few|few) )?((small|short|low|weak|thick|plank|steep) )*buttress",
  "stilt roots", "^((often|sometimes|occasionally|mostly) )?((with|without|with or without) )?((a few|few) )?((small|short|low|weak) )*stilt[ -]roots",
  "crown", "^crown\\b",
  "sapwood", "^sapwood",
  "bark", "^(outer )?bark",
  "lenticels", "^lenticels?\\b",
  "petiole", "^petioles?\\b",
  "lamina", "^lamina\\b",
  "upper surface", "^upper (leaf )?surface\\b",
  "lower surface", "^lower (leaf )?surface\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^lateral (leaf )?veins?\\b",
  "interarching veins", "^interarching veins\\b",
  "intercostal veins", "^(intercostal|secondary|tertiary) (venation|veins)|^venation\\b|^reticulation\\b|^veins\\b",
  "dots", "^dots\\b",
  "leaf bud", "^leaf buds?\\b",
  "male inflorescences", "^(in )?male inflorescences?\\b|^in male\\b",
  "female inflorescences", "^(in )?female inflorescences?\\b|^in female\\b",
  "peduncle", "^peduncles?\\b",
  "short shoot", "^short shoots?\\b",
  "basal cataphylls", "^basal cataphylls?\\b",
  "bracts", "^bracts?\\b",
  "bracteole", "^bracteoles?\\b",
  "pedicel", "^pedicels?\\b",
  "bud", "^(male |female )?buds?\\b",
  "perianth", "^perianths?\\b",
  "lobes", "^lobes\\b",
  "tube", "^tube\\b",
  "androecium", "^androecium\\b",
  "staminal disc", "^staminal disc\\b",
  "synandrium", "^synandrium\\b",
  "androphore", "^androphore\\b",
  "anther locules", "^(anther )?locules\\b",
  "anthers", "^anthers?\\b",
  "column", "^(central )?column\\b",
  "connective", "^connectives?\\b",
  "disc", "^disc\\b",
  "ovary", "^ovar(y|ies)\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "ovule", "^ovules?\\b",
  "pericarp", "^pericarps?\\b",
  "aril", "^arils?\\b",
  "stalk", "^stalks?\\b",
  "testa", "^testa\\b",
  "endosperm", "^endosperm\\b",
  "embryo", "^embryo\\b",
  "cotyledons", "^cotyledons?\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text) {
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- match_organ(s, sent_organs)
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) p_org <- s_org
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      cur_text <- chunks[1]
      # chunks since the last organ label: "hairs rusty brown, powdery,
      # 0.1--0.3 mm long" gives the hair length, not the organ's
      ctx <- ""
      in_hairs <- FALSE
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        named <- !is.na(c_org) || ci == 1
        if (!is.na(c_org)) {
          cur <<- c_org
          cur_text <<- chunks[ci]
          ctx <<- ""
          in_hairs <<- FALSE
        }
        ctx_before <- ctx
        hairs_before <- in_hairs
        ctx <<- paste(ctx, chunks[ci])
        lc <- str_to_lower(chunks[ci])
        # a hair clause ("hairs rusty brown", "with dense hairs") runs on
        # through chunks of hair adjectives ("powdery") until another
        # character starts
        if (str_detect(lc, "^(with )?([a-z-]+ ){0,4}hairs\\b|^indumentum\\b") && !str_detect(lc, "\\d")) {
          in_hairs <<- TRUE
        } else if (!str_detect(lc, hair_adj_re)) {
          in_hairs <<- FALSE
        }
        tibble(
          ctx = ctx_before,
          hair_ctx = hairs_before,
          sentence = si,
          piece = pi,
          chunk = ci,
          sent_organ = s_org,
          sent_text = s,
          piece_organ = p_org,
          organ = cur,
          named = named,
          organ_text = cur_text,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# Organ named in an unmapped note ("leaf blade", "male flower bud")
unmapped_organ <- function(organ, sent_organ) {
  sex <- str_extract(coalesce(sent_organ, ""), "^(male|female)")
  case_when(
    organ %in% c("lamina", "base", "margin", "apex", "upper surface", "lower surface") &
      sent_organ %in% "leaves" ~ paste("leaf", str_replace(organ, "lamina", "blade")),
    organ %in% c("male flowers", "female flowers") ~ str_remove(organ, "s$"),
    !is.na(sex) & !organ %in% c("male flowers", "female flowers") ~ paste(sex, "flower", organ),
    TRUE ~ coalesce(organ, sent_organ, "habit")
  )
}

# ---- row helpers ------------------------------------------------------------------

row <- flora_row
num_rows <- flora_value_rows
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
# "margin not revolute", "crowded towards the apex or not": negations are not
# scored, except where the trait has a level for them
negated_ok <- c("flower_scent_production", "root_system_type")
cat_rows <- function(trait, x, em = NA_character_) {
  if (!trait %in% negated_ok && str_detect(x, "\\b(not|never|non)\\b")) {
    return(NULL)
  }
  flora_cat_rows(trait, x, dict, em)
}
# length, width and diameter of one organ ("bud ..., 1.5--1.6 mm long,
# 1.3--1.4 mm wide"; "Male buds 1--1.5 mm diameter")
size_rows <- function(len_trait, wid_trait, x, em = NA_character_, notes = NA_character_) {
  out <- list()
  if (!is.null(grab_dims(x))) {
    return(dim_rows(c(len_trait, wid_trait), x, em = em))
  }
  l <- grab(x, "long")
  if (!is.na(l) && !is.na(len_trait)) out[[1]] <- num_rows(len_trait, l, em = em, notes = notes, cq_text = x)
  w <- grab(x, "(wide|broad)")
  if (!is.na(w) && !is.na(wid_trait)) out[[2]] <- num_rows(wid_trait, w, em = em, notes = notes, cq_text = x)
  d <- grab(x, "(in )?diam(eter|\\.)?")
  if (!is.na(d) && !is.na(wid_trait)) {
    dn <- paste(c(notes[!is.na(notes)], "Diameter scored as width (skill step 4)."), collapse = " ")
    out[[3]] <- num_rows(wid_trait, str_remove(d, "\\s?(in )?diam.*"), em = em, notes = dn, cq_text = x)
  }
  bind_rows(out)
}

# ---- scorers --------------------------------------------------------------------

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  if (hedged(lx) || is.na(organ) || str_detect(lx, compare_re)) {
    return(NULL)
  }
  # hair lengths ("with minute hairs c. 0.1 mm long") are not organ sizes
  hairs_chunk <- str_detect(lx, hair_re)
  piece_l <- str_to_lower(cl$piece_text)
  side_before <- str_detect(
    str_sub(piece_l, 1, coalesce(str_locate(piece_l, fixed(lx))[1, 1], 1L) - 1L),
    "\\b(inside|outside)\\b"
  )
  sex <- str_extract(so, "^(male|female)")
  em_flower <- if (!is.na(sex)) paste(sex, "flower") else NA_character_
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  switch(
    organ,
    habit = if (so == "habit") {
      words <- str_remove_all(lx, paste0("(up )?to ", num, "\\s?m( tall| high)?"))
      add(cat_rows("plant_growth_form", words))
      add(cat_rows("sex_type", lx))
      h <- str_extract(x, paste0(rng, "\\s?m\\b(?!\\s?(diam|wide|high))"))
      if (!is.na(h) && !hairs_chunk) add(num_rows("plant_height", h, cq_text = x))
      add(cat_rows("root_system_type", lx))
      add(cat_rows("stem_base_form", lx))
    },
    bole = ,
    buttresses = ,
    `stilt roots` = if (so == "habit") {
      d <- grab(x, "diam(eter|\\.)?")
      if (!is.na(d)) {
        dr <- num_rows(
          "plant_diameter_breast_height",
          str_remove(d, "\\s?diam.*"),
          notes = "Bole diameter scored as diameter at breast height.",
          cq_text = x
        )
        add(dr)
      }
      add(cat_rows("stem_base_form", lx))
      add(cat_rows("root_system_type", lx))
    },
    crown = if (so == "habit") add(cat_rows("plant_canopy_form", lx)),
    bark = if (so == "bark" && !str_detect(lx, paste0(marking_re, "|inner|inside|cut|sap")) &&
      !str_detect(lx, age_re)) {
      add(cat_rows("bark_colour", lx))
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (so == "twigs" && !str_detect(str_to_lower(cl$piece_text), age_re)) {
      if (str_detect(lx, paste0("^(twigs? )?(densely |sparsely |thinly )?(with .*)?(", hair_re, ")|^(twigs? )?glabrous$"))) {
        add(cat_rows("stem_hairs", str_remove(lx, "\\d.*")))
      }
    },
    leaves = if (so == "leaves") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_compoundness", lx))
        add(cat_rows("leaf_margin", lx))
        add(cat_rows("leaf_phyllotaxis", lx))
        add(cat_rows("leaf_arrangement", lx))
      }
    },
    lamina = if (so == "leaves" && !str_detect(lx, "dry|drying")) {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d|broadest|widest|curved")) {
        add(cat_rows("leaf_shape", str_remove(lx, "^lamina ")))
      }
    },
    base = if (so == "leaves") add(cat_rows("leaf_base_shape", lx)),
    margin = if (so == "leaves" && !str_detect(lx, "dry|drying|or not")) {
      add(cat_rows("leaf_margin", lx))
      add(cat_rows("leaf_margin_posture", lx))
    },
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", lx)),
    `lateral veins` = if (so == "leaves") {
      a <- str_match(x, paste0("at (", rng, ")\\s?°"))[, 2]
      if (!is.na(a)) {
        add(count_rows(
          "leaf_secondary_vein_angle", x, a,
          notes = "Angle of the lateral veins to the midrib (skill step 4, Soepadmo_2000 question 10).",
          unit = "deg"
        ))
      }
    },
    petiole = if (!hairs_chunk) {
      l <- grab_len(x)
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
      w <- grab(x, "(wide|broad)")
      if (!is.na(w)) add(num_rows("petiole_width", w, cq_text = x))
    },
    `leaf bud` = if (!hairs_chunk) {
      add(size_rows("bud_length", "bud_width", x,
        em = "leaf bud",
        notes = "Leaf bud size scored as bud size (skill step 4, Ashton_2004 questions 1--5)."
      ))
    },
    inflorescences = ,
    `male inflorescences` = ,
    `female inflorescences` = if (so == "inflorescences") {
      em <- case_when(
        organ == "male inflorescences" | str_detect(lx, "^(in )?male\\b") ~ "male inflorescence",
        organ == "female inflorescences" | str_detect(lx, "^(in )?female\\b") ~ "female inflorescence",
        TRUE ~ NA_character_
      )
      # "flowers solitary or in clusters", "flowers in subumbels" place the
      # flowers on the inflorescence; they are not its type
      if (!str_detect(lx, "-like|like a|flowers|subumbel|supra-axillary") &&
        !str_detect(str_to_lower(cl$ctx), "flowers")) {
        add(cat_rows("inflorescence_type", lx, em = em))
      }
      l <- grab(x, "long")
      # "sessile or peduncle 1--5 mm long", "lateral branches to 5 mm long"
      if (!is.na(l) && !hairs_chunk && !str_detect(lx, "pedunc|branch")) {
        add(num_rows("inflorescence_length", l, em = em, cq_text = x))
      }
    },
    bracteole = if (!hairs_chunk) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_bract_length", l, em = "bracteole", cq_text = x))
    },
    flowers = if (so == "flowers") {
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("flower_scent_production", lx))
      m <- str_match(lx, paste0("(", cnt, ")-merous"))[, 2]
      if (!is.na(m)) add(count_rows("flower_perianth_merism", x, m))
      if (!hairs_chunk && !str_detect(lx, paste0(marking_re, "|inside|outside|\\d|dirty"))) {
        add(cat_rows("flower_colour", str_remove(lx, "^flowers? ")))
      }
    },
    `male flowers` = ,
    `female flowers` = if (so %in% c("male flowers", "female flowers") && str_detect(lx, "^(male|female) bud")) {
      add(size_rows("bud_length", "bud_width", x,
        em = paste(sex, "flower bud"),
        notes = "Flower bud size scored as bud size (skill step 4, Ashton_2004 questions 1--5)."
      ))
    },
    bud = if (!is.na(sex) && !hairs_chunk) {
      add(size_rows("bud_length", "bud_width", x,
        em = paste(sex, "flower bud"),
        notes = "Flower bud size scored as bud size (skill step 4, Ashton_2004 questions 1--5)."
      ))
    },
    perianth = if (so %in% c("flowers", "male flowers", "female flowers", "inflorescences")) {
      if (str_detect(lx, "uniseriate")) {
        add(row("flower_perianth_whorls_count", 1, vt = "mean", unit = "{count}", raw = x,
          em = em_flower, notes = "'uniseriate' scored as one perianth whorl."))
      }
      add(cat_rows("flower_shape", lx, em = em_flower))
      # "perianth 3-lobed", "perianth 2- or 3-lobed"
      m <- str_match(str_replace_all(lx, "(\\d)- or (\\d)-lobed", "\\1–\\2-lobed"), paste0("(", cnt, ")-lobed"))[, 2]
      if (!is.na(m)) {
        # "4-lobed in male, 3-lobed in female"
        in_sex <- str_match(lx, "\\bin (male|female)\\b")[, 2]
        em_l <- if (!is.na(in_sex)) paste(in_sex, "flower") else em_flower
        add(count_rows("flower_perianth_merism", x, m, em = em_l,
          notes = "Number of perianth lobes scored as perianth merism."))
      }
      # "inside glabrous, greenish creamy": a colour after an inside or outside
      # chunk may be that surface's, so it stays unmapped (ownership unclear)
      if (!hairs_chunk && !side_before && !str_detect(lx, paste0(marking_re, "|inside|outside|\\d|lobes"))) {
        add(cat_rows("flower_colour", str_remove(lx, "^perianth "), em = em_flower))
      }
    },
    lobes = if (!is.na(sex) || so == "flowers") {
      # "lobes 3--5", not "lobes 1--1.5 mm thick"
      m <- str_match(lx, paste0("^lobes (", cnt, ")(?![\\d.–(]|\\s?\\(|\\s?(mm|cm)|-)"))[, 2]
      if (!is.na(m)) {
        add(count_rows("flower_perianth_merism", x, m, em = em_flower,
          notes = "Number of perianth lobes scored as perianth merism."))
      }
    },
    anthers = if (!hairs_chunk) {
      m <- str_match(lx, paste0("^anthers? (", cnt, ")(?![\\d.–(]|\\s?\\(|\\s?(mm|cm)|-)"))[, 2]
      if (!is.na(m) && so != "female flowers" && !str_detect(lx, "\\d or \\(|to about")) {
        add(count_rows("flower_fertile_stamens_count", x, m,
          em = if (is.na(em_flower)) "male flower" else em_flower,
          notes = "Number of (fused) anthers scored as fertile stamen count (question 3)."))
      }
      add(cat_rows("flower_anther_orientation", lx))
      add(cat_rows("flower_anther_dehiscence", lx))
    },
    synandrium = ,
    androecium = ,
    `staminal disc` = if (!hairs_chunk) {
      # "synandrium ... with 2--6 completely fused ... anthers"
      m <- str_match(lx, paste0("\\bwith (?:at the margin )?(", cnt, ")(?!\\s?(mm|cm)|-)[a-z ,()-]* anthers\\b"))[, 2]
      if (!is.na(m) && so != "female flowers" && !str_detect(lx, "\\d or \\(|to about")) {
        add(count_rows("flower_fertile_stamens_count", x, m,
          em = if (is.na(em_flower)) "male flower" else em_flower,
          notes = "Number of (fused) anthers scored as fertile stamen count (question 3)."))
      }
      add(cat_rows("flower_anther_orientation", lx))
      add(cat_rows("flower_anther_dehiscence", lx))
    },
    ovary = {
      if (str_detect(lx, "monocarpellate")) {
        add(row("flower_structural_carpels_count", 1, vt = "mean", unit = "{count}", raw = x,
          notes = "'monocarpellate' scored as one carpel."))
      }
      add(cat_rows("flower_ovary_position", lx))
    },
    style = {
      add(cat_rows("flower_style_differentiation", lx))
      l <- grab(x, "long")
      # "style and stigma 2--2.5 mm long" measures both
      if (!is.na(l) && !str_detect(str_to_lower(cl$organ_text), "stigma")) {
        add(num_rows("flower_style_length", l, cq_text = x))
      }
    },
    fruits = if (so == "fruits") {
      core <- str_squish(str_remove(lx, "^fruits?\\b"))
      piece <- str_to_lower(cl$piece_text)
      add(cat_rows("fruit_dehiscence", lx))
      if (!hairs_chunk && !str_detect(lx, "per infructescence|together|panicle")) {
        add(size_rows("fruit_length", "fruit_width", x))
      }
      # "glabrous", "red or brownish tomentose"; not "tomentose or early
      # glabrescent" (glabrescence is a change with age)
      if (str_detect(lx, paste0("glabrous|", hair_re)) && !str_detect(piece, age_re) &&
        !str_detect(lx, "\\bin grooves|at the base|near the|persistent")) {
        add(cat_rows("fruit_surface_hairs", str_remove(lx, "\\d.*")))
      }
      # ground colour of the ripe fruit: "yellow-orange drying dark brown" ->
      # yellow orange
      col <- str_remove(core, "\\s?(drying|when dry|on drying)\\b.*$")
      # "yellowish green, ripening yellowish red" -> the ripe colour only
      ripe_only <- str_detect(piece, "ripening")
      if (!hairs_chunk && (!ripe_only || str_detect(col, "ripening")) &&
        !str_detect(col, paste0(marking_re, "|young|immature|unripe|inside|\\d|granul|lenticel|\\bwith\\b"))) {
        add(cat_rows("fruit_colour", col))
      }
    },
    pericarp = if (so == "fruits") {
      # "leathery or somewhat fleshy", "fleshy-leathery" are intermediate
      if (!str_detect(lx, "leathery|somewhat|woody|\\bor\\b")) add(cat_rows("fruit_fleshiness", lx))
      t <- grab(x, "thick")
      if (!is.na(t)) {
        add(num_rows(
          "fruit_wall_thickness", str_remove(t, "\\s?thick$"),
          notes = "Pericarp thickness scored as fruit_wall_thickness.",
          cq_text = x
        ))
      }
    },
    aril = if (so %in% c("fruits", "seeds") && cl$named) {
      add(row("dispersal_appendage", "aril", raw = x,
        notes = "Aril stated in the description; scored as an aril dispersal appendage."))
    },
    seeds = if (so == "seeds") {
      if (str_detect(lx, "^seeds? one\\b")) {
        add(row("seed_count", 1, vt = "mean", unit = "{count}", raw = x))
      }
      if (!str_detect(lx, "\\d")) add(cat_rows("seed_shape", str_remove(lx, "^seeds? ")))
      if (!hairs_chunk) add(size_rows("seed_length", "seed_width", x))
    }
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = x)
}

# Leaf indumentum is stated per surface ("upper surface ..., glabrous, lower
# surface early glabrescent"): hairy when a surface is hairy, glabrous when
# both are; nothing when a surface is glabrescent or only young leaves are
# described. Chunks of the lamina and surface pieces are read in order.
leaf_hairs <- function(cl_leaf) {
  ch <- cl_leaf |> filter(organ %in% c("lamina", "upper surface", "lower surface", "apex", "base"))
  if (!nrow(ch)) {
    return(NULL)
  }
  surf <- rep(NA_character_, nrow(ch))
  cur <- NA_character_
  for (i in seq_len(nrow(ch))) {
    lx <- str_to_lower(ch$text[i])
    if (str_detect(lx, "^upper surface|\\babove\\b")) cur <- "upper"
    if (str_detect(lx, "^lower surface|\\bbeneath\\b|\\bbelow\\b")) cur <- "lower"
    if (str_detect(lx, "both surfaces")) cur <- "both"
    surf[i] <- cur
  }
  lx <- str_to_lower(ch$text)
  h <- str_detect(lx, paste0("glabr|", hair_re)) & !is.na(surf)
  if (!any(h)) {
    return(NULL)
  }
  hx <- lx[h]
  hs <- surf[h]
  # hair scars, hair bases and glabrescence are not a state of the adult surface
  if (any(str_detect(hx, paste0(age_re, "|scar|bases|except|\\bor\\b|sometimes|hedged"))) ||
    any(str_detect(hx, flora_qual_re)) || any(hedged(hx))) {
    return(NULL)
  }
  hairy <- str_detect(hx, hair_re) & !str_detect(hx, "^(upper surface |lower surface )?glabrous\\b")
  raw <- paste(ch$text[h], collapse = ", ")
  if (any(hairy)) {
    return(list(row = row("leaf_hairs_adult_leaves", "hairy", raw = raw), ids = ch$id[h]))
  }
  if (any(hs == "both") || all(c("upper", "lower") %in% hs)) {
    return(list(row = row("leaf_hairs_adult_leaves", "glabrous", raw = raw), ids = ch$id[h]))
  }
  NULL
}

character_of <- function(x) {
  lx <- str_to_lower(flora_strip_specimens(x))
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "gland|\\bdots\\b") ~ "glands",
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membran|woody|leathery|fleshy|carnose|brittle") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|papill|prominul|prominent|raised|sunken") ~ "venation",
    str_detect(lx, "caducous|persistent") ~ "persistence",
    str_detect(lx, "\\d") ~ "count",
    str_detect(lx, "\\b(white|yellow|green|brown|red|black|grey|purple|orange|pink|cream|olivaceous|rusty)") ~ "colour",
    str_detect(lx, "ellipsoid|globose|ovoid|oblong|obovoid|elliptic|lanceolate|shaped|triangular") ~ "shape",
    TRUE ~ "description"
  )
}

rare_q <- c(
  "sometimes", "rarely", "occasionally", "very rarely", "more rarely",
  "exceptionally", "less frequently"
)

parse_description <- function(p) {
  asides <- asides_of(p$text)
  text <- prep(p$text)
  cl <- split_clauses(text)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5--25 cm long")
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?|at )?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    # ... nor right after a hair clause ("hairs rusty brown, powdery,
    # 0.1--0.3 mm long" is the hair length)
    if (!is.null(r) && !cl$named[i] && cl$hair_ctx[i]) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  # leaf indumentum per surface, once per leaves sentence
  cl$hair_done <- FALSE
  for (s in unique(cl$sentence[cl$sent_organ %in% "leaves"])) {
    h <- leaf_hairs(cl |> filter(sentence == s))
    if (!is.null(h)) {
      i <- min(h$ids)
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h$row)
      cl$hair_done[cl$id %in% h$ids] <- TRUE
    }
  }
  # locate each chunk after the previous one for its page
  pos <- rep(NA_integer_, nrow(cl))
  from <- 1L
  for (i in seq_len(nrow(cl))) {
    k <- str_locate(str_sub(text, from), fixed(str_sub(cl$text[i], 1, 25)))[1, 1]
    if (!is.na(k)) {
      pos[i] <- from + k - 1L
      from <- pos[i]
    }
  }
  pos <- round(coalesce(pos, 1L) * nchar(p$text) / max(nchar(text), 1))
  pages <- map(pos, \(k) flora_page_at(p, k))
  cl$pdf_page <- map_int(pages, \(x) as.integer(x[["pdf"]]))
  cl$printed_page <- map_int(pages, \(x) as.integer(x[["printed"]]))
  # a commonness word earlier in the piece bars the later chunks'
  # higher-rank rows from copy-down ("Bark generally greyish, smooth";
  # Kochummen_2000), but not the chunks before it ("Leaves simple, entire,
  # alternate, usually distichous": alternate is universal)
  cl <- cl |>
    group_by(sentence, piece) |>
    mutate(piece_qualified = cumany(str_detect(text, flora_qual_re))) |>
    ungroup()
  scored <- map_lgl(cl$rows, \(r) !is.null(r) && nrow(r) > 0)
  # an organ label alone ("Leaves", "Male flowers") is not a clause
  label_only <- str_detect(str_to_lower(cl$text), "^((male|female) )?(leaves|flowers?|seeds?|inflorescences)$")
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, piece_qualified, rows) |>
    unnest(rows)
  if (!nrow(mapped)) {
    mapped <- mapped |> mutate(trait = character(), entity_measured = character(), cq = character(), value_type = character())
  }
  mapped <- mapped |>
    # "base rounded, rarely ovate": the unqualified alternative is `usually`
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(cq = if_else(value_type == "mode" & is.na(cq) & any(cq %in% rare_q), "usually", cq)) |>
    ungroup() |>
    select(-sentence, -piece)
  unmapped <- cl |>
    filter(!scored, !hair_done, !label_only) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = flora_unmapped_note(
        unmapped_organ(organ, sent_organ),
        character_of(text),
        case_when(
          hedged(text) ~ "hedged",
          str_detect(str_to_lower(text), compare_re) ~ "comparison",
          TRUE ~ ""
        )
      ),
      pdf_page,
      printed_page
    )
  aside_rows <- tibble(
    trait = NA_character_,
    trait_value_raw = asides,
    notes = flora_unmapped_note(
      map_chr(trait_value_raw, \(a) aside_organ(p$text, a)),
      "description",
      case_when(
        str_detect(trait_value_raw, "elsewhere|New Guinea|Brazil") ~ "outside the flora region",
        str_detect(trait_value_raw, "^\\([A-Z][a-z]+\\)$") ~ "names a genus",
        TRUE ~ NA_character_
      )
    ),
    pdf_page = p$pdf_page,
    printed_page = p$printed_page
  )
  bind_rows(mapped |> select(-id), unmapped, aside_rows) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ------------------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description") |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# Two descriptions cite Sinclair for organs the authors did not see:
# "Female flowers (Sinclair, 1958)" in H. fulva (printed 158) and "Fruits
# (Sinclair, 1961)" in K. curtisii var. paludosa (printed 198).
cited <- tribble(
  ~block, ~cited_re, ~source_id,
  "Horsfieldia fulva", "^female flower", "Sinclair_1958",
  "Knema curtisii var. paludosa", "^fruit_", "Sinclair_1961"
)
own <- own |>
  left_join(cited |> select(block, cited_re), by = "block") |>
  mutate(
    cites = !is.na(trait) & !is.na(cited_re) &
      (str_detect(coalesce(entity_measured, ""), coalesce(cited_re, "(?!x)x")) |
        str_detect(coalesce(trait, ""), coalesce(cited_re, "(?!x)x"))),
    source_id = if_else(cites, cited$source_id[match(block, cited$block)], NA_character_),
    notes = if_else(
      cites,
      paste(coalesce(notes, ""), "Cited from", str_replace(source_id, "_", " "), "in the description.") |> str_squish(),
      notes
    )
  ) |>
  select(-cited_re, -cites)

# ---- later sections ----------------------------------------------------------------

later <- paras |>
  filter(!section %in% c("Description", "Nomenclature")) |>
  mutate(sentence = map(text, \(t) flora_split_sentences(t))) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "")

later_unmapped <- later |>
  transmute(
    block,
    rank,
    trait = NA_character_,
    trait_value_raw = sentence,
    notes = flora_unmapped_note("section", "text", str_to_lower(section)),
    source_section = section,
    pdf_page,
    printed_page
  )

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c", pdf_page = "i", printed_page = "i")
)

# ---- higher ranks, inheritance, assembly -------------------------------------------

taxa <- paras |>
  filter(rank %in% c("species", "subspecies", "variety")) |>
  distinct(block, rank, status) |>
  mutate(
    species = word(block, 1, 2),
    genus = word(block, 1),
    has_description = block %in% desc_paras$block
  )
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

# Organs a description says are not seen or unknown get no copied value
unseen <- desc_paras |>
  group_by(block) |>
  summarise(text = paste(text, collapse = " "), .groups = "drop") |>
  transmute(
    block,
    female = str_detect(text, "Female flowers (not seen|unknown)"),
    male = str_detect(text, "Male flowers (not seen|unknown)"),
    fruit = str_detect(text, "Fruits (not seen|unknown)")
  )
female_traits <- c("flower_ovary_position", "flower_structural_carpels_count", "flower_style_differentiation")
male_traits <- c("flower_anther_orientation", "flower_anther_dehiscence", "flower_fertile_stamens_count")

copied <- flora_copy_down(
  higher_rows = own |> filter(!piece_qualified),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
copied <- copied |>
  left_join(unseen, by = "block") |>
  filter(
    !(coalesce(female, FALSE) & (trait %in% female_traits | str_detect(coalesce(entity_measured, ""), "^female"))),
    !(coalesce(male, FALSE) & (trait %in% male_traits | str_detect(coalesce(entity_measured, ""), "^male"))),
    !(coalesce(fruit, FALSE) & str_detect(trait, "^(fruit|seed|dispersal)"))
  ) |>
  select(-female, -male, -fruit)
inherited <- flora_inherit_infraspecific(
  own_rows = own |> filter(!rank %in% c("genus", "family")),
  members = members
) |>
  select(-rank)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
  inherited,
  later_unmapped |> mutate(rank_scored_from = NA_character_)
) |>
  select(-piece_qualified) |>
  left_join(
    bind_rows(
      taxa |> select(block, taxon_rank = rank),
      paras |> filter(rank %in% c("family", "genus")) |> distinct(block, taxon_rank = rank)
    ),
    by = "block"
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank,
    rank_scored_from,
    commonness_qualifier = cq,
    # an FPM description describes the taxon in Peninsular Malaysia; family
    # and genus statements copied down and values cited from Sinclair carry
    # no region
    population_region = if_else(
      taxon_rank %in% c("species", "subspecies", "variety") & !is.na(trait) &
        coalesce(rank_scored_from, "species") == "species" & is.na(source_id),
      "Peninsular Malaysia",
      NA_character_
    ),
    form = NA_character_,
    entity_measured,
    trait,
    trait_value_raw,
    trait_value,
    trait_value_clean,
    value_type,
    unit,
    source_section,
    pdf_page,
    printed_page,
    notes,
    source_id
  ) |>
  bind_rows(manual |> mutate(across(c(pdf_page, printed_page), as.integer))) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(
    match(taxon_rank, c("family", "genus", "species", "subspecies", "variety")),
    taxon_name, pdf_page
  ) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")

single_infra <- taxa |> filter(rank != "species") |> count(species) |> filter(n == 1) |> pull(species)
flags <- taxa |>
  left_join(
    out |> filter(!is.na(trait), trait != "") |> count(taxon_name, name = "n_mapped_rows"),
    by = c("block" = "taxon_name")
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank = rank,
    status,
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      rank == "species" & block %in% single_infra ~
        "Only one infraspecific taxon in Peninsular Malaysia; no species description, so the infraspecific taxon carries the description.",
      rank != "species" & species %in% single_infra ~
        "Single infraspecific taxon in Peninsular Malaysia; its description is the local description of the species.",
      rank != "species" ~
        "Infraspecific description gives differences only; other traits inherited from the species description.",
      block == "Horsfieldia fulva" ~ "Female flowers described from Sinclair (1958); those rows cite Sinclair_1958.",
      TRUE ~ ""
    )
  ) |>
  mutate(flag = if_else(
    taxon_name == "Knema curtisii var. paludosa",
    paste(flag, "Fruits described from Sinclair (1961); those rows cite Sinclair_1961."),
    flag
  )) |>
  bind_rows(tribble(
    ~taxon_name, ~taxon_rank, ~status, ~n_mapped_rows, ~flag,
    "Myristica fragrans", "species", "cultivated", 0L,
    "Nutmeg; cultivated, introduced from Maluku. Only a key lead (printed 234) and genus remarks; no description, so not scored.",
    "Horsfieldia iryaghedhi", "species", "cultivated", 0L,
    "Introduced from Sri Lanka, grown as an ornamental (Horsfieldia Uses, printed 146); named in the genus description and keys only; not scored."
  ))
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
