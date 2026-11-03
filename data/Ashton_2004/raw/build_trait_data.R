# Ashton_2004: extract traits from the Tree Flora of Sabah and Sarawak
# Dipterocarpaceae treatment (P.S. Ashton 2004, TFSS vol. 5: 63--388). Input
# is the PDF text layer (born-digital 2008 electronic edition), segmented in
# raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Ashton_2004/raw/trait_data.csv      mapped and unmapped records
#   data/Ashton_2004/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Ashton_2004/raw/taxon_flags.csv     headings processed, rank, flags
#
# Clause parser after Soepadmo_2000: each description is split into sentences
# (one organ each), ";" pieces and "," chunks. A chunk takes the organ it
# starts with, else the organ of its piece or sentence. Scorers run per organ;
# every chunk that scores nothing stays as an unmapped row. Later sections
# (Distribution, Ecology, Uses, Notes, vernacular names, key annotations of
# subspecies) are kept one sentence per row. The family Ecology statements on
# pollination, breeding system, mycorrhiza and seed dormancy are in
# raw/manual_rows.csv. Keys and figure captions are not read. Names are as
# printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/Ashton_2004/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Ashton_2004"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# Dipterocarpaceae terms (nut, nodding, radially symmetrical, crown shapes)
# are in the shared dictionary.

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

prep <- function(x) {
  x |>
    str_replace_all("º", "°") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|to\\s)?", num,
  "(?:–", num, ")?(?:\\s?\\(–", num, "\\))?"
)
unit_re <- "(mm|cm|m)\\b"
cnt <- "(?:\\(\\d+–\\)\\s?)?\\d+(?:–\\d+)?(?:\\s?\\(–\\d+\\))?"

grab <- function(x, keyword) {
  str_extract(x, paste0(rng, "\\s?", unit_re, "\\s?", keyword))
}
# a size with no keyword ("to 2 × 2 mm" is a dimension pair, handled apart)
grab_any <- function(x) {
  if (str_detect(x, "×")) {
    return(NA_character_)
  }
  str_extract(x, paste0(rng, "\\s?", unit_re))
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
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no|non|none)\\b")
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsute|pilose|",
  "villous|sericeous|velutinous|woolly|puberul|setose|strigose|scabrid|",
  "lanate|floccose|stellate"
)

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide). The first sentence
# of a description is the habit.
sent_organs <- tribble(
  ~organ, ~re,
  "crown", "^(mature )?crowns?\\b",
  "buttresses", "^buttress",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^(sapwood|heartwood|wood)\\b",
  "resin", "^(dammar|resin)",
  # "Twig, inflorescence, petiole ... puberulent": hairs of several organs
  "indumentum", "^(young|exposed|all|indumentum|tomentum|twig,|parts|leaf bud,|whole)",
  "twigs", "^(young )?twigs?\\b",
  "leaf buds", "^(leaf |terminal |apical )?buds?\\b",
  "stipules", "^stipules?",
  "leaves", "^leaves|^leaf\\b|^blade\\b",
  "inflorescences", "^(inflorescences?|panicles?)\\b",
  "flowers", "^flowers?\\b",
  "fruits", "^fruits?\\b",
  "nuts", "^nuts?\\b",
  "seeds", "^(seeds?|cotyledons?|embryo|germination|seedlings?)\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "buttresses", "^(with )?((low|small|large|prominent|steep|thin|stout|tall|flying|short|thick|concave|plank-like|spreading) )*buttress",
  "stilt-roots", "^(with )?((low|small|large|short|stout) )*stilt[ -]?roots?",
  "crown", "^(mature )?crowns?\\b",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^(sapwood|heartwood)\\b",
  "resin", "^(dammar|resin)",
  "lamina", "^(leaf )?blade\\b",
  "domatia", "^domatia\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(lateral )?veins?\\b",
  "intercostal venation", "^(intercostal|tertiary) (venation|veins)",
  "petiole", "^petioles?\\b",
  "rachis", "^rach(is|ises)\\b",
  "branchlets", "^branch(es|lets?)\\b",
  "bracteoles", "^(bracts?|bracteoles?)\\b",
  "buds", "^buds?\\b",
  "pedicels", "^pedicels?\\b",
  "fruit calyx", "^((the )?(fruit )?calyx( lobes?| tube)?|(the )?\\d (longer|shorter|larger|smaller|outer|inner) (lobes?|ones?)|lobes?|(the )?(larger|longer|shorter|smaller) (lobes?|ones?)|wings?)\\b",
  "sepals", "^(sepals?|calyx)\\b",
  "petals", "^(petals?|corolla)\\b",
  "connective", "^connectiv(e|al)( appendages?)?\\b|^appendages?\\b",
  "filaments", "^(stamen )?filaments?\\b",
  "stamens", "^stamens?\\b",
  "anthers", "^anthers?\\b",
  "stylopodium", "^stylopodium\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "ovary", "^ovar(y|ies)\\b",
  "ovules", "^ovules?\\b",
  "nuts", "^nuts?\\b",
  "pericarp", "^pericarp\\b",
  "cotyledons", "^cotyledons?\\b",
  "germination", "^(seed )?germination\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text, first_par = TRUE) {
  # "Flowers: buds ..., sepals ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- if (si == 1 && first_par) "habit" else match_organ(s, sent_organs)
    # an unrecognised sentence keeps its first word as organ
    if (is.na(s_org)) s_org <- str_to_lower(word(s, 1)) |> str_remove("[^a-z]+$")
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) {
        p_org <- if (pi > 1 && identical(s_org, "leaves")) "lamina" else s_org
      }
      # "Fruits: calyx lobes ..." and "Fruits subsessile; calyx lobes ..."
      if (identical(s_org, "fruits") && identical(p_org, "sepals")) p_org <- "fruit calyx"
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      cur_text <- chunks[1]
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        if (identical(s_org, "fruits") && identical(c_org, "sepals")) c_org <- "fruit calyx"
        # "Leaves ...; blade ..., base ..." the leaf base is not a buttress
        named <- !is.na(c_org) || ci == 1
        if (!is.na(c_org)) {
          cur <<- c_org
          cur_text <<- chunks[ci]
        }
        tibble(
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

# ---- row helpers ------------------------------------------------------------------

row <- flora_row
num_rows <- flora_value_rows
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
cat_rows <- function(trait, x, em = NA_character_) {
  flora_cat_rows(trait, x, dict, em)
}

# ---- scorers --------------------------------------------------------------------

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|shortly|thinly|very|rather|evenly|",
  "loosely|softly|faintly|slightly|appressed|adpressed|simple|persistently|",
  "golden|silvery|brownish|brown|greyish|grey|whitish|yellowish|rusty|reddish|",
  "pale|buff|tawny|rufous|fulvous|cream|ochreous|fawn|",
  "or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z]*$"
)
hair_core <- \(x) str_squish(str_remove(x, "\\s+with [a-z ,-]*\\bhairs?$"))
change_re <- "glabrescent|caducous|when young|initially|becoming|at first|early|young"

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  # a chunk naming a subspecies ("4.5–10 × 2.5–4 cm (subsp. diminuta)") or
  # another taxon is not a species value
  if (hedged(lx) || str_detect(lx, "\\bsubsp\\.|\\b[a-z]\\. [a-z]{4,}\\b") ||
    str_detect(x, "\\b[A-Z]\\. [a-z]{4,}\\b")) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  if (is.na(organ)) {
    return(NULL)
  }
  switch(
    organ,
    habit = {
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(up to|to|c\\.)\\s*$")
      # "Emergent, sometimes main canopy trees": the qualifier is the canopy
      # position's, not the growth form's; "shrub-like" is not a shrub
      pre <- pre |>
        str_remove_all(paste0(flora_qual_re, " (low |main |sub|small )*(canopy|emergent|understor(e)?y)")) |>
        str_remove_all("shrub-like")
      add(cat_rows("plant_growth_form", pre))
      add(cat_rows("sex_type", lx))
      add(cat_rows("leaf_phenology", lx))
      h <- grab(x, "(tall|high)\\b")
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
      d <- grab(x, "(in )?diam(eter|\\.)?")
      if (!is.na(d)) {
        dr <- num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*"), cq_text = x)
        # "to 70 m tall, to 2 m diameter": the diameter keeps its own "to"
        add(dr)
      }
    },
    buttresses = ,
    `stilt-roots` = if (!negative(lx) && str_detect(lx, "buttress|stilt") &&
      so %in% c("habit", "buttresses")) {
      q <- qualifier(coalesce(str_extract(cl$piece_text, "[^,]*\\bpresent\\b"), x))
      # "flying buttresses" and "buttresses absent" are not the stem base form
      if (str_detect(lx, "(?<!flying )buttress")) add(row("stem_base_form", "buttressed", cq = q, raw = x))
      if (str_detect(lx, "stilt")) add(row("root_system_type", "stilt-roots", "stilt_roots", cq = q, raw = x))
    },
    crown = if (!str_detect(lx, "\\d")) add(cat_rows("plant_canopy_form", lx)),
    bark = if (so == "bark") {
      # lenticels, patches and blazes are not the bark colour
      if (!str_detect(lx, "lenticel|patch|spot|blaze|slash|inside|when cut|mottled|scroll|resin|dammar")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (so == "twigs") {
      core <- hair_core(str_remove(lx, "^(young )?twigs? "))
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    `leaf buds` = if (so == "leaf buds" && !is.null(grab_dims(x))) {
      add(dim_rows(c("bud_length", "bud_width"), x, em = "leaf bud"))
    } else if (so == "leaf buds") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("bud_length", l, em = "leaf bud", cq_text = x))
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_arrangement", lx))
      add(cat_rows("leaf_phyllotaxis", lx))
      if (str_detect(lx, "\\bsimple\\b")) add(row("leaf_compoundness", "simple", raw = x))
    },
    lamina = if (so == "leaves") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_shape", lx))
      }
    },
    base = if (so == "leaves") add(cat_rows("leaf_base_shape", lx)),
    margin = if (so == "leaves") {
      add(cat_rows("leaf_margin", lx))
      add(cat_rows("leaf_margin_posture", lx))
    },
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", lx)),
    petiole = if (so == "leaves") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    inflorescences = {
      add(cat_rows("inflorescence_type", lx))
      l <- grab(x, "long")
      if (!is.na(l) && so == "inflorescences") add(num_rows("inflorescence_length", l, cq_text = x))
    },
    # the inflorescence axis ("rachis to 8 cm long, singly branched")
    rachis = if (so == "inflorescences") {
      l <- grab(x, "long")
      if (!is.na(l)) {
        add(num_rows("inflorescence_length", l, em = "inflorescence rachis", notes = "Rachis length scored as inflorescence_length.", cq_text = x))
      }
    },
    flowers = {
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("flower_perianth_symmetry", lx))
      add(cat_rows("flower_scent_production", lx))
      add(cat_rows("flower_orientation", lx))
    },
    buds = if (so == "flowers") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("bud_length", "bud_width"), x, em = "flower bud"))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("bud_length", l, em = "flower bud", cq_text = x))
      }
    },
    sepals = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_sepal_length", l, cq_text = x))
    },
    petals = if (so == "flowers") {
      # colour of the petals, not of their hairs or a part ("pink at base");
      # "cream generally with a pink median stripe, or pink": a chunk
      # continuing an alternative is not scored alone
      if (!str_detect(lx, "^(or|to|and) ") && !str_detect(lx, paste0(hair_re, "|\\bat (the )?(base|apex)\\b|tinge|stripe|spot|blotch|veins?|outside|inside|exposed|margin"))) {
        add(cat_rows("flower_colour", lx))
      }
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_petal_length", NA), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_petal_length", l, cq_text = x))
      }
    },
    stamens = if (so == "flowers") {
      m <- str_match(lx, paste0("^stamens? (c\\. ?)?(", cnt, ")(?!\\s?(mm|cm|×))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_fertile_stamens_count", x, paste0(coalesce(m[1, 2], ""), m[1, 3])))
      }
    },
    connective = if (so == "flowers" && str_detect(lx, "^connectiv|^appendage")) {
      add(row(
        "flower_anther_connective_extension",
        if (str_detect(lx, "absent|\\bnone\\b")) "absent" else "connectival appendage",
        if (str_detect(lx, "absent|\\bnone\\b")) "absent" else "present_general",
        raw = x,
        notes = "A connectival appendage scored as a connective extension (present_general); its length relative to the anther is not mapped to a length level."
      ))
    },
    anthers = add(cat_rows("flower_anther_attachment", lx)),
    ovary = if (so %in% c("flowers", "habit") || cl$rank_ %in% c("family", "genus")) {
      add(cat_rows("flower_ovary_position", lx))
      m <- str_match(lx, paste0("^(ovary [a-z ,-]*?)?(", cnt, ")[–-]locul(ed|ar)"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_structural_carpels_count", x, m[1, 3], notes = "Ovary locules scored as carpels (one locule per carpel; placentation axile, family description)."))
      }
    },
    ovules = {
      m <- str_match(lx, paste0("^ovules (", cnt, ") per locule"))
      if (!is.na(m[1, 1])) add(count_rows("flower_ovules_per_functional_carpel_count", x, m[1, 2]))
    },
    style = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, cq_text = x))
    },
    fruits = if (so == "fruits") {
      # "around the base of the nut": the organ, not the fruit type
      if (str_detect(lx, "^fruits?\\b")) add(cat_rows("fruit_type", lx))
      add(cat_rows("fruit_dehiscence", lx))
    },
    nuts = if (so %in% c("nuts", "fruits")) {
      core <- str_squish(str_remove(lx, "^nuts?\\b"))
      tn <- "Nut size: the fruit without its persistent calyx (wings)."
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x, em = "nut") |> mutate(notes = paste(notes, tn)))
      } else {
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?(in )?diam.*"), em = "nut", notes = paste("Nut diameter scored as fruit_width.", tn), cq_text = x))
        } else {
          l <- grab(x, "long")
          if (!is.na(l)) add(num_rows("fruit_length", l, em = "nut", notes = tn, cq_text = x))
        }
      }
      if (str_detect(hair_core(core), hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("fruit_surface_hairs", hair_core(core)))
      }
    },
    germination = add(cat_rows("seedling_germination_location", lx)),
    seeds = add(cat_rows("seedling_germination_location", lx)),
    cotyledons = add(cat_rows("seedling_germination_location", lx))
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = x)
}

# Leaf indumentum is stated per surface in the first leaf piece ("glabrous
# above, sparsely tomentose below"): hairy when a surface is hairy, glabrous
# when both are; nothing when a surface is glabrescent (a change with age).
# Scales ("lepidote") are not hairs.
leaf_hairs <- function(piece) {
  lp <- str_to_lower(piece)
  # the leaf surfaces only, not "Leaf venation below, petiole ... pubescent"
  if (!str_detect(lp, "^(leaves|leaf (under|lower|upper) ?surface|blade)\\b")) {
    return(NULL)
  }
  # "epilose or tomentose below", "sparsely pubescent to glabrous below":
  # alternatives, not scored
  if (str_detect(lp, paste0(
    "(glabr|epilose)[a-z]* (or|to) [a-z -]*(", hair_re, ")|",
    "(", hair_re, ")[a-z]* (or|to) [a-z -]*(glabr|epilose)"
  ))) {
    return(NULL)
  }
  ch <- str_to_lower(flora_split_pieces(piece, ","))
  ch <- str_remove(ch, "^leaves ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  if (!length(h) || any(str_detect(h, paste0(change_re, "|lepidot|hedge"))) || hedged(piece)) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b")
  if (any(hairy)) {
    return(row("leaf_hairs_adult_leaves", "hairy", raw = piece))
  }
  both <- any(str_detect(h, "both (sides|surfaces)|^glabrous$")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) row("leaf_hairs_adult_leaves", "glabrous", raw = piece) else NULL
}

character_of <- function(x) {
  lx <- str_to_lower(flora_strip_specimens(x))
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, paste0("glabr|lepidot|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|bony|horny|corky|rigid|fibrous|laminated") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|scalariform") ~ "venation",
    str_detect(lx, "caducous|persistent|fugaceous|deciduous") ~ "persistence",
    str_detect(lx, "\\d") ~ "count",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink|crimson|cream|mauve|tawny|ochre") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "very rarely", "less often")

parse_description <- function(p) {
  text <- prep(p$text)
  # a description continued after a page turn has no habit sentence
  cl <- split_clauses(text, p$first_par)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
  cl$rank_ <- p$rank
  cl <- cl |>
    group_by(sentence, piece) |>
    mutate(prev = lag(text)) |>
    ungroup()
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5–25 cm long")
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  cl$hair_done <- FALSE
  lp <- cl |> filter(sent_organ %in% "leaves", piece == 1, chunk == 1)
  for (i in lp$id) {
    h <- leaf_hairs(cl$piece_text[i])
    if (!is.null(h)) {
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h)
      hair_ids <- cl$id[cl$sentence == cl$sentence[i] & cl$piece == 1 &
        str_detect(str_to_lower(cl$text), paste0("glabr|", hair_re))]
      cl$hair_done[hair_ids] <- TRUE
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
  scored <- map_lgl(cl$rows, \(r) !is.null(r) && nrow(r) > 0)
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(cq = if_else(value_type == "mode" & is.na(cq) & any(cq %in% rare_q), "usually", cq)) |>
    ungroup() |>
    select(-sentence, -piece)
  # "10–18 × 3–7 cm, or narrowly oblong, 12–17 × 2–4.5 cm" (two forms, Shorea
  # ovalis p. 302): only the first size of a trait in a piece is scored; the
  # chunks of later sizes stay unmapped
  dup_size <- mapped |>
    filter(unit %in% c("mm", "cm", "m")) |>
    left_join(cl |> select(id, sentence, piece), by = "id") |>
    arrange(id) |>
    group_by(sentence, piece, trait, entity_measured) |>
    filter(id != first(id)) |>
    pull(id)
  mapped <- mapped |> filter(!(id %in% dup_size & unit %in% c("mm", "cm", "m")))
  scored[cl$id %in% dup_size & !cl$id %in% mapped$id] <- FALSE
  unmapped <- cl |>
    filter(!scored, !hair_done) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = flora_unmapped_note(
        case_when(
          organ %in% c("lamina", "base", "margin", "apex", "acumen", "midrib", "lateral veins", "intercostal venation", "domatia") &
            sent_organ %in% "leaves" ~ paste("leaf", str_replace(organ, "lamina", "blade")),
          organ %in% c("base", "apex", "margin") ~ paste(sent_organ, organ),
          TRUE ~ organ
        ),
        character_of(text),
        case_when(
          hedged(text) ~ "hedged",
          str_detect(text, "\\bsubsp\\.") ~ "value of a named subspecies",
          str_detect(text, "\\b[A-Z]\\. [a-z]{4,}\\b") ~ "names another taxon",
          organ == "fruit calyx" & character_of(text) == "size" ~ "fruit wing (calyx lobe) size; candidate trait",
          TRUE ~ ""
        )
      ),
      pdf_page,
      printed_page
    )
  bind_rows(mapped |> select(-id), unmapped) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ------------------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description", rank %in% c("family", "genus", "species", "subspecies")) |>
  group_by(block) |>
  mutate(first_par = row_number() == 1) |>
  ungroup() |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections ----------------------------------------------------------------

later <- paras |>
  filter(
    !section %in% c("Description", "Nomenclature"),
    rank %in% c("family", "genus", "species", "subspecies")
  ) |>
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

# ---- higher ranks, assembly -------------------------------------------------------

taxa <- paras |>
  filter(rank %in% c("species", "subspecies")) |>
  distinct(block, rank, status) |>
  mutate(
    genus = word(block, 1),
    species = word(block, 1, 2),
    has_description = block %in% desc_paras$block
  )
# copy-down members: taxa with their own description (the seven single
# subspecies stand for their species)
# subspecies annotated in a key have no description: they inherit the species
# rows and receive the copied family and genus values
members <- taxa |>
  filter(has_description | rank == "subspecies") |>
  transmute(block, genus, group = NA_character_, species, rank)

# Family and genus statements tied to named genera ("(Dipterocarpus)",
# "in Cotylelobium, Dryobalanops and some Hopea") hold for those genera only.
named_genus <- paste0("\\b(", gen_re, ")\\b|\\bspecies\\b|\\bsect\\.|\\bsome\\b|\\bfew\\b")
copied <- flora_copy_down(
  higher_rows = own |> filter(!str_detect(coalesce(trait_value_raw, ""), named_genus)),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")

inherited <- flora_inherit_infraspecific(
  own |> filter(!is.na(trait)),
  members |> filter(!block %in% desc_paras$block)
)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
  inherited,
  later_unmapped |> mutate(rank_scored_from = NA_character_)
) |>
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
    # a TFSS species description describes the species in Sabah and Sarawak
    population_region = if_else(
      taxon_rank %in% c("species", "subspecies") & is.na(rank_scored_from) & !is.na(trait),
      "Sabah and Sarawak",
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
    source_id = NA_character_
  )

out <- out |>
  bind_rows(manual |> mutate(across(c(pdf_page, printed_page), as.integer))) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(match(taxon_rank, c("family", "genus", "species", "subspecies")), taxon_name, pdf_page) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")
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
      rank == "species" & !has_description ~
        "No species description: the one subspecies in Sabah and Sarawak carries the description.",
      rank == "subspecies" & has_description ~
        "Only subspecies in Sabah and Sarawak; its description is the local description of the species.",
      rank == "subspecies" ~
        "Subspecies treated in a key annotation (distribution and ecology only); species rows inherited.",
      block == "Upuna borneensis" ~ "Only species of a monotypic genus; heading unnumbered.",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
