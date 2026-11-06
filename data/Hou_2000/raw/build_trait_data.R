# Hou_2000: extract traits from the Tree Flora of Sabah and Sarawak
# Leguminosae-Caesalpinioideae treatment (Ding Hou 2000, TFSS vol. 3:
# 119--180). Input is the PDF text layer (born-digital 2007 electronic
# edition), segmented in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Hou_2000/raw/trait_data.csv      mapped and unmapped records
#   data/Hou_2000/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Hou_2000/raw/taxon_flags.csv     headings processed, rank, flags
#
# Clause parser after Soepadmo_2000 and Ashton_2004: each description is split
# into sentences (one organ each), ";" pieces and "," chunks. A chunk takes the
# organ it starts with, else the organ of its piece or sentence. Scorers run per
# organ; every chunk that scores nothing stays as an unmapped row. Later
# sections (Distribution, Ecology, Uses, vernacular names, key annotations of
# varieties) are kept one sentence per row; months in Ecology are scored. Keys
# (and the three genera described only in the key to genera) and figure
# captions are not read. Names are as printed; the subfamily is the
# family-rank block.
#
# Compound leaves: the leaf sentence gives the whole leaf (length, leaflet or
# pinna number, petiole); the "Leaflets ..." sentence gives the leaflet
# (shape, size, base, apex, hairs) with entity_measured = leaflet. Pairs of
# leaflets (paripinnate leaves) are leaflet_count in unit `pairs`.
suppressPackageStartupMessages(library(tidyverse))
source("data/Hou_2000/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Hou_2000"
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

# "a or b" counts before "pairs" / "leaflets" are ranges ("3 or 4 pairs");
# elsewhere ("stamens 2 or 6", "calyx lobes 5 or 3") they are alternatives.
or_counts <- function(x) {
  x |>
    str_replace_all(
      "\\b(\\d+) (?:and/)?or (\\d+)(?= (pairs?|leaflets)\\b)",
      "\\1–\\2"
    ) |>
    str_replace_all("(?<=^|\\. )(Seeds?) (\\d+) or (\\d+)\\b", "\\1 \\2–\\3")
}
# "10(or 9)", "(9 or)10", "1(or 2)": a rarer count, written as an extreme
or_extremes <- function(x) {
  x <- str_replace_all(x, "\\((\\d+) or\\)\\s?(\\d+)", "(\\1–)\\2")
  str_replace_all(x, "(\\d+)\\s?\\(or (\\d+)\\)", \(m) {
    n <- as.numeric(str_extract_all(m, "\\d+")[[1]])
    if (n[2] < n[1]) paste0("(", n[2], "–)", n[1]) else paste0(n[1], "(–", n[2], ")")
  })
}

prep <- function(x) {
  x |>
    str_replace_all("º", "°") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    # an en dash split from its number at a line end ("5– 7(–10)")
    str_replace_all("(?<=\\d)–\\s+(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    # "1–2(–3)- seeded" (line break after the hyphen)
    str_replace_all("(?<=[\\d)])- (?=(seeded|ovuled)\\b)", "-") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    # "(sub)coriaceous", "(ob)ovate-oblong", "(shortly)acuminate": both states
    str_replace_all("\\(([a-z]+)\\)([a-z][a-z-]+)", \(m) {
      g <- str_match(m, "\\(([a-z]+)\\)(.+)")
      paste0(g[, 3], " or ", g[, 2], if_else(str_ends(g[, 2], "ly"), " ", ""), g[, 3])
    }) |>
    # "basi- or dorsifixed" -> "basifixed or dorsifixed"
    str_replace_all("\\b([a-z]+)- or ([a-z]+)(fixed)\\b", "\\1\\3 or \\2\\3") |>
    or_extremes() |>
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
# "a × b unit" or "a × b × c unit" (length, width, thickness)
grab_dims <- function(x) {
  m <- str_match(
    x,
    paste0("(", rng, ")\\s?×\\s?(", rng, ")(?:\\s?×\\s?(", rng, "))?\\s?", unit_re)
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  parts <- m[1, 2:4]
  list(raw = m[1, 1], parts = parts[!is.na(parts)], unit = m[1, 5])
}

# "more or less" and "almost" qualify the state they precede: not scored
hedged <- \(x) str_detect(x, flora_hedge_re) | str_detect(str_to_lower(x), "\\bmore or less\\b|\\balmost\\b|\\(\\?\\)")
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no|non|none|never)\\b")
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsute|pilose|",
  "villous|villose|sericeous|velutinous|velvety|woolly|puberul|setose|",
  "strigose|scabrid|lanate|floccose|stellate|ciliate"
)

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide). The first sentence
# of a description is the habit.
sent_organs <- tribble(
  ~organ, ~re,
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^(sapwood|heartwood|wood)\\b",
  "twigs", "^(young )?(twigs?|branchlets?|shoots?)\\b",
  "buds", "^buds?\\b",
  "new leaves", "^new leaves",
  "stipules", "^stipules?",
  "stipels", "^stipels?",
  "leaflets", "^leaflets?\\b",
  "leaves", "^leaves|^leaf\\b",
  "inflorescences", "^(inflorescences?|infructescences?)\\b",
  "flowers", "^flowers?\\b",
  "fruits", "^fruits?\\b",
  "seeds", "^seeds?\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "buttresses", "^(with )?((low|small|large|prominent|steep|thin|stout|tall|flat|short|thick|plank-like|spreading|very) )*buttress",
  "bole", "^(with )?((fluted|straight|cylindrical) )*bole\\b",
  "trunk", "^trunks?\\b",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^(sapwood|heartwood)\\b",
  "petiole and rachis", "^petiole and (rachis|petiolule)\\b",
  "petiole", "^petioles?\\b",
  "rachis", "^(main )?rach(is|ises)\\b",
  "pinnae", "^(stalk of |rachis of )?pinnae\\b",
  "petiolules", "^petiolules?\\b",
  "stipels", "^stipels?\\b",
  "leaflets", "^leaflets?\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(lateral )?veins?\\b",
  "intercostal venation", "^(intercostal|tertiary) (venation|veins)",
  "glands", "^(often )?(with )?([0-9–]+ |a pair of )?([a-z, ]+ )?glands?\\b",
  "upper surface", "^upper surface\\b",
  "lower surface", "^(lower|under) ?surface\\b",
  "peduncle", "^peduncles?\\b",
  "pedicel", "^pedicels?\\b",
  "bracts", "^(bracts and bracteoles|bracts?|bracteoles?)\\b",
  "hypanthium", "^hypanthium\\b",
  "calyx lobes", "^(calyx lobes?|sepals?)\\b",
  "calyx", "^calyx\\b",
  "petals", "^(developed )?(petals?|corolla)\\b",
  "disk", "^disk\\b",
  "stamens", "^(fertile )?stamens?\\b",
  "filaments", "^(stamen |free )?filaments?\\b",
  "anthers", "^anthers?\\b",
  "connective", "^connective\\b",
  "staminodes", "^staminodes?\\b",
  "ovary", "^ovar(y|ies)\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "valves", "^valves?\\b",
  "exocarp", "^(exocarp|mesocarp|pericarp)\\b",
  "beak", "^beak\\b",
  "aril", "^aril\\b",
  "testa", "^testa\\b",
  "cotyledons", "^cotyledons?\\b",
  "endosperm", "^endosperm\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text, first_par = TRUE) {
  # "Flowers: calyx lobes ...", "Leaves: petiole ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- if (si == 1 && first_par) "habit" else match_organ(s, sent_organs)
    # an unrecognised sentence keeps its first word as organ
    if (is.na(s_org)) s_org <- str_to_lower(word(s, 1)) |> str_remove("[^a-z]+$")
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      # "petals: lamina ...", "stamens: free filaments ...": the label before
      # the colon names the organ
      pc <- str_replace(pieces[pi], "^([a-z ]+): ", "\\1 ")
      p_org <- match_organ(pc, part_organs)
      if (is.na(p_org)) p_org <- s_org
      chunks <- flora_split_pieces(pc, ",")
      cur <- p_org
      cur_text <- chunks[1]
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
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
          piece_text = pc,
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- row helpers ------------------------------------------------------------------

row <- flora_row
num_rows <- flora_value_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
cat_rows <- function(trait, x, em = NA_character_) {
  flora_cat_rows(trait, x, dict, em)
}
# a count with the commonness word of its chunk ("each usually with 7–9")
count_rows <- function(trait, x, value_txt, em = NA_character_, notes = NA_character_, unit = "{count}") {
  r <- flora_count_rows(trait, x, value_txt, em, notes, unit)
  q <- qualifier(x)
  if (!is.null(r) && !is.na(q)) r <- r |> mutate(cq = coalesce(cq, q))
  r
}

pairs_note <- "Pairs of leaflets (unit pairs, doubled by config/unit_conversions.csv; provisional, Hou_2000 question 1)."

# ---- scorers --------------------------------------------------------------------

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|shortly|short|thinly|very|rather|evenly|",
  "loosely|softly|faintly|slightly|appressed|appressedly|adpressed|simple|",
  "golden|silvery|brownish|brown|greyish|grey|whitish|white|yellowish|rusty|",
  "reddish|pale|buff|tawny|rufous|fulvous|cream|ochreous|fawn|curly|patent|",
  "or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z]*$"
)
hair_core <- \(x) str_squish(str_remove(x, "\\s+with [a-z ,-]*\\bhairs?$"))
change_re <- "glabrescent|caducous|when young|initially|becoming|at first|early|young|later|easily fallen"
immature_re <- "\\((young|very young|immature|unripe)\\)|^(young|immature|unripe) "

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  # another taxon named ("similar to that of Sindora", "C. reticulata auct.")
  if (hedged(lx) || str_detect(x, "\\b[A-Z]\\. [a-z]{4,}\\b") || is.na(organ)) {
    return(NULL)
  }
  # immature fruits and seeds are not the mature organ (Prance_2012)
  if (so %in% c("fruits", "seeds") && str_detect(str_to_lower(cl$sent_text), immature_re)) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  switch(
    organ,
    habit = {
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(up to|to|c\\.)\\s*$")
      # "shrub-like tree" is a tree
      pre <- str_remove_all(pre, "shrub-like")
      # "occasionally as shrub-like tree or treelet", "sometimes treelet": a
      # smaller tree, not a second growth form
      if (cl$chunk == 1 || !str_detect(lx, "treelet|shrub-like")) add(cat_rows("plant_growth_form", pre))
      add(cat_rows("leaf_phenology", lx))
      if (str_detect(lx, "\\b(un)?armed\\b") && !str_detect(lx, "fruit")) {
        add(cat_rows("plant_physical_defence_structures", lx))
      }
      h <- grab(x, "(tall|high)\\b")
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
      d <- grab(x, "(in )?diam(eter|\\.)?")
      if (!is.na(d)) {
        add(num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*"), cq_text = x))
      }
    },
    trunk = if (so == "habit") add(cat_rows("plant_physical_defence_structures", lx)),
    bole = if (so == "habit") {
      add(cat_rows("stem_base_form", lx))
      add(cat_rows("leaf_phenology", lx))
    },
    buttresses = if (!negative(lx) && str_detect(lx, "buttress") && so == "habit") {
      q <- qualifier(coalesce(str_extract(cl$piece_text, "[^,]*\\bpresent\\b"), x))
      add(row("stem_base_form", "buttressed", cq = q, raw = x))
    },
    bark = if (so == "bark") {
      # lenticels, patches and mottles are not the bark colour
      if (!str_detect(lx, "lenticel|patch|spot|mottled|tinge|inside|ridges|rings")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (so == "twigs") {
      core <- hair_core(str_remove(lx, "^(young )?(twigs?|branchlets?|shoots?) "))
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_arrangement", lx))
      add(cat_rows("leaf_phenology", lx))
      add(cat_rows("leaf_lamina_division", str_remove_all(lx, "\\([^)]*\\)")))
      if (str_detect(lx, "\\b(compound|pinnate|paripinnate|imparipinnate|bipinnate|unifoliolate)\\b|leaflets") &&
        !str_detect(lx, "^(often )?mixed with")) {
        add(row("leaf_compoundness", if (str_detect(lx, "unifoliolate")) "unifoliolate" else "compound", "compound", raw = x))
      }
      # the whole compound leaf: "Leaves 10–14(–19) cm long"
      l <- grab(x, "long")
      if (!is.na(l) && str_detect(lx, "^leaves (up to |to )?[\\d(]")) {
        add(num_rows("leaf_length", l, em = "compound leaf", cq_text = x))
      }
      add(leaflet_counts(x))
    },
    pinnae = if (so == "leaves") {
      m <- str_match(lx, paste0("^pinnae (", cnt, ") pairs\\b"))
      if (!is.na(m[1, 1])) add(count_rows("leaflet_count", x, m[1, 2], em = "pinnae", unit = "pairs", notes = "Pairs of pinnae of a bipinnate leaf (unit pairs; provisional, Hou_2000 question 1)."))
    },
    `petiole and rachis` = NULL,
    # the petiole is printed in the leaf or in the leaflet sentence
    petiole = if (so %in% c("leaves", "leaflets")) {
      # "petiole 1.5–3(–5) cm and rachis 4–14 cm long": the petiole's own size
      l <- str_match(x, paste0("^(?i:petioles?) ((?:up )?", rng, "\\s?", unit_re, ")"))[1, 2]
      if (!is.na(l)) add(num_rows("petiole_length", str_remove(l, "^up "), cq_text = x))
    },
    leaflets = if (so == "leaflets") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaflet_length", "leaflet_width", NA), x, em = "leaflet"))
      } else if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_shape", lx, em = "leaflet"))
      }
      add(leaflet_counts(x))
    } else if (so == "leaves") {
      add(leaflet_counts(x))
    },
    base = if (so == "leaflets") add(cat_rows("leaf_base_shape", lx, em = "leaflet")),
    apex = if (so == "leaflets") add(cat_rows("leaf_apex_shape", lx, em = "leaflet")),
    inflorescences = if (so == "inflorescences") {
      add(cat_rows("inflorescence_type", lx))
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(lx, "rachis|branch|peduncle|infructescence|pedicel|flowers")) {
        add(num_rows("inflorescence_length", l, cq_text = x))
      }
    },
    # the inflorescence axis ("rachis 9–15 cm long")
    rachis = if (so == "inflorescences") {
      l <- grab(x, "long")
      if (!is.na(l)) {
        add(num_rows("inflorescence_length", l, em = "inflorescence rachis", notes = "Rachis length scored as inflorescence_length (provisional, Ashton_2004 question 3).", cq_text = x))
      }
    },
    # flower pedicels (Turner_2014 rule); "articulated at the apex" is not a size
    pedicel = if (so %in% c("inflorescences", "flowers")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("pedicel_length", l, cq_text = x))
    },
    bracts = if (so == "inflorescences") {
      b <- str_extract(str_to_lower(cl$organ_text), "^(bracts and bracteoles|bracts?|bracteoles?)\\b")
      em <- case_when(
        str_detect(b, "and") ~ "bract and bracteole",
        str_detect(b, "bracteole") ~ "bracteole",
        TRUE ~ "bract"
      )
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_bract_length", NA), x, em = em))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_bract_length", l, em = em, cq_text = x))
      }
    },
    flowers = if (so == "flowers") {
      add(cat_rows("flower_structural_sex_type", str_remove(lx, "functionally [a-z]+")))
      add(cat_rows("flower_perianth_symmetry", lx))
      # colour of the flower, not of its hairs ("Flowers white, later pink")
      if (!str_detect(lx, paste0(hair_re, "|^later |^(or|to) "))) add(cat_rows("flower_colour", lx))
    },
    hypanthium = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) {
        add(num_rows("flower_tube_length", l, em = "hypanthium", notes = "Hypanthium length scored as flower_tube_length (receptacle tube; provisional, Prance_2012 question 2).", cq_text = x))
      }
    },
    `calyx lobes` = if (so == "flowers" && !str_detect(lx, "staminod")) {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_sepal_length", NA), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_sepal_length", l, cq_text = x))
      }
    },
    petals = if (so == "flowers" && !str_detect(lx, "lamina|claw")) {
      if (!str_detect(lx, paste0("^(or|to|and) |", hair_re, "|\\bat (the )?(base|apex)\\b|inside|outside|margin"))) {
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
      pt <- str_to_lower(cl$piece_text)
      m1 <- str_match(lx, paste0("(?:^|\\b)(?:fertile stamens |stamens )?(?:usually |often )?(", cnt, ") fertile\\b|^fertile stamens (", cnt, ")(?![\\d.]*\\s?(mm|cm))"))
      m2 <- str_match(lx, paste0("^stamens? (c\\. ?)?(", cnt, ")(?!\\s?(mm|cm|×|or fewer|or more|long|short)|[\\d.])"))
      if (!is.na(m1[1, 1])) {
        add(count_rows("flower_fertile_stamens_count", x, coalesce(m1[1, 2], m1[1, 3])))
      } else if (!is.na(m2[1, 1]) && (!str_detect(pt, "fertile|staminod|reduced|\\+|or fewer") || str_detect(pt, "all fertile"))) {
        add(count_rows("flower_fertile_stamens_count", x, paste0(coalesce(m2[1, 2], ""), m2[1, 3])))
      }
      # "stamens with filaments 10–15 mm long", "fertile stamens: filaments c. 30 mm long"
      f <- str_match(lx, paste0("filaments? ((?:up )?", rng, "\\s?", unit_re, ") long"))[1, 2]
      if (!is.na(f) && !str_detect(lx, "staminod")) add(num_rows("flower_filament_length", str_remove(f, "^up "), cq_text = x))
    },
    filaments = if (so == "flowers" && !str_detect(lx, "staminod")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, cq_text = x))
    },
    # anther dehiscence only when longitudinal: "dehiscing by slits" (Cassia)
    # and "both slits drawn towards the abaxial side" (Dialium) give no slit
    # length; "appearing as longitudinally dehiscent" (Koompassia) is not
    anthers = {
      add(cat_rows("flower_anther_attachment", lx))
      l <- grab(x, "long")
      if (!is.na(l) && so == "flowers") add(num_rows("flower_anther_length", l, cq_text = x))
      if (str_detect(lx, "longitudinal") && !str_detect(lx, "appearing|rim")) {
        add(cat_rows("flower_anther_dehiscence", lx))
      }
    },
    ovary = if (so %in% c("flowers", "habit") || cl$rank_ %in% c("family", "genus")) {
      m <- str_match(lx, paste0("(?:^|\\s)(?:usually |often )?(", cnt, ")-ovuled\\b"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_ovules_per_functional_carpel_count", x, m[1, 2], notes = "Ovules per ovary scored per carpel: the legume ovary is one carpel."))
      }
    },
    style = if (so == "flowers" && !str_detect(lx, "kinds")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, cq_text = x))
    },
    fruits = if (so == "fruits") {
      core <- str_squish(str_remove(lx, "^fruits?\\b"))
      add(cat_rows("fruit_type", lx))
      add(cat_rows("fruit_dehiscence", lx))
      # "(including the wing)" in a size is not the wing statement
      if (!negative(lx) && !str_detect(lx, "including")) add(cat_rows("dispersal_appendage", str_remove(lx, "^fruits?\\b")))
      wing <- if (str_detect(lx, "including")) " Size includes the wing." else ""
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width", "fruit_height"), x) |> mutate(notes = paste0(notes, wing)))
      } else {
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?(in )?diam.*"), notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        } else {
          l <- grab(x, "long")
          if (!is.na(l) && !str_detect(lx, "spine|wing|beak|stalk")) add(num_rows("fruit_length", l, cq_text = x))
        }
      }
      s <- str_match(lx, paste0("(?:^|\\s)(?:usually |often )?(", cnt, ")-seeded\\b"))
      if (!is.na(s[1, 1])) add(count_rows("seeds_per_fruit", x, s[1, 2], unit = "{count}/{count}"))
      if (str_detect(hair_core(core), hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("fruit_surface_hairs", hair_core(core)))
      }
      # colour of the ripe fruit, not of its hairs, spines or warts
      if (!str_detect(lx, paste0(hair_re, "|scurf|spine|wart|young|immature|unripe|dried|dry"))) {
        add(cat_rows("fruit_colour", core))
      }
    },
    exocarp = if (so == "fruits" && str_detect(str_to_lower(cl$organ_text), "^pericarp")) {
      t <- str_extract(x, paste0(rng, "\\s?", unit_re))
      if (!is.na(t) && str_detect(lx, "thick")) add(num_rows("fruit_wall_thickness", t, notes = "Pericarp thickness scored as fruit_wall_thickness (provisional, DeWilde_2018 question 6).", cq_text = x))
    },
    seeds = if (so == "seeds") {
      core <- str_squish(str_remove(lx, "^seeds?\\b"))
      add(cat_rows("seed_shape", lx))
      if (!negative(lx)) add(cat_rows("dispersal_appendage", lx))
      add(cat_rows("seed_surface_texture", lx))
      add(cat_rows("seed_surface_reflectivity", lx))
      # seed colour, not the aril's ("black with a rich orange aril")
      if (!str_detect(lx, paste0(hair_re, "|scurf|young"))) {
        add(cat_rows("seed_colour", str_remove(lx, "with [a-z ]*aril.*$")))
      }
      dd <- grab_dims(x)
      if (!is.null(dd)) {
        add(dim_rows(c("seed_length", "seed_width", "seed_height"), x))
      } else if (str_detect(lx, "long and wide")) {
        l <- grab(x, "long and wide")
        v <- str_remove(l, "\\s?long and wide")
        add(num_rows("seed_length", v, notes = "Printed 'long and wide': the size is both seed length and width.", cq_text = x))
        add(num_rows("seed_width", v, notes = "Printed 'long and wide': the size is both seed length and width.", cq_text = x))
      } else {
        d <- grab(x, "(in )?(diam(eter|\\.)?|across)")
        if (!is.na(d)) {
          add(num_rows("seed_width", str_remove(d, "\\s?(in )?(diam|across).*"), notes = "Seed diameter scored as seed_width.", cq_text = x))
        } else {
          t <- grab(x, "thick")
          if (!is.na(t)) {
            add(num_rows("seed_height", str_remove(t, "\\s?thick"), notes = "Seed thickness scored as seed_height.", cq_text = x))
          }
        }
      }
      # "Seeds 4–8", "Seeds 1(–2) per fruit", "Seeds 1–5 per fruit"
      s <- str_match(lx, paste0("^(?:seeds? )?(?:usually )?(", cnt, ")(?: per fruit)?$"))
      if (!is.na(s[1, 1]) && (str_detect(lx, "^seeds? ") || str_detect(lx, "per fruit"))) {
        add(count_rows("seeds_per_fruit", x, s[1, 2], unit = "{count}/{count}"))
      }
    },
    aril = if (so == "seeds") add(row("dispersal_appendage", "aril", raw = x))
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = x)
}

# Leaflet and pinna counts of a compound leaf: "each with 3–5 pairs of
# leaflets", "each with (1–)2(–3) leaflets", "Leaflets (2–)4(–6) for each leaf",
# "Leaflets 5–12(–17) per leaf", "Leaflets 6–16 pairs per pinna".
leaflet_counts <- function(x) {
  lx <- str_to_lower(x)
  # "(usually one leaflet early shed ...)" is not the count
  lx0 <- str_remove_all(lx, "\\((?![–\\d])[^)]*\\)")
  pp <- str_match(lx0, paste0("each pinna with (", cnt, ") pairs of leaflets"))
  if (!is.na(pp[1, 1])) {
    return(count_rows("leaflet_count", x, pp[1, 2], em = "leaflets per pinna", unit = "pairs", notes = pairs_note))
  }
  pp <- str_match(lx0, paste0("^leaflets (", cnt, ") pairs per pinna"))
  if (!is.na(pp[1, 1])) {
    return(count_rows("leaflet_count", x, pp[1, 2], em = "leaflets per pinna", unit = "pairs", notes = pairs_note))
  }
  m <- str_match(lx0, paste0("(?:each |with |^leaflets )(?:usually |often )?(?:with )?(", cnt, ") pairs? of [a-z ]*leaflets"))
  if (!is.na(m[1, 1])) {
    return(count_rows("leaflet_count", x, m[1, 2], unit = "pairs", notes = pairs_note))
  }
  m <- str_match(lx0, paste0("(?:each (?:usually |often )?with |^leaflets )(", cnt, ")(?: [a-z]+)? (?:leaflets|for each leaf|per leaf)\\b"))
  if (!is.na(m[1, 1])) {
    return(count_rows("leaflet_count", x, m[1, 2]))
  }
  NULL
}

# Leaf indumentum is stated per surface in the first piece of the leaflet (or
# leaf) sentence ("glabrous above, rusty pubescent beneath"): hairy when a
# surface is hairy, glabrous when both are; nothing when a surface is
# glabrescent (a change with age) or hairs are confined to the midrib.
leaf_hairs <- function(piece, em) {
  lp <- str_to_lower(piece)
  if (!str_detect(lp, "^(leaves|leaflets?)\\b")) {
    return(NULL)
  }
  # "glabrous or rarely sparsely minutely hairy below": alternatives
  if (str_detect(lp, paste0(
    "glabr[a-z]* (or|to) [a-z -]*(", hair_re, ")|",
    "(", hair_re, ")[a-z]* (or|to) [a-z -]*glabr"
  ))) {
    return(NULL)
  }
  ch <- str_to_lower(flora_split_pieces(piece, ","))
  ch <- str_remove(ch, "^(leaves|leaflets?) ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  if (!length(h) || any(str_detect(h, paste0(change_re, "|except|midrib|almost|more or less"))) || hedged(piece)) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b")
  if (any(hairy)) {
    return(row("leaf_hairs_adult_leaves", "hairy", em = em, raw = piece))
  }
  both <- any(str_detect(h, "both (sides|surfaces)|^glabrous$")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) row("leaf_hairs_adult_leaves", "glabrous", em = em, raw = piece) else NULL
}

character_of <- function(x) {
  lx <- str_to_lower(flora_strip_specimens(x))
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "glands?\\b|punctate|pellucid") ~ "glands",
    str_detect(lx, paste0("glabr|scurf|", hair_re)) ~ "indumentum",
    str_detect(lx, "spin|prickl|thorn|armed") ~ "armature",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|crustace|fleshy|hard|fibrous|laminated|granular") ~ "texture",
    str_detect(lx, "vein|venation|reticulat") ~ "venation",
    str_detect(lx, "caducous|persistent|deciduous|shed") ~ "persistence",
    str_detect(lx, "connate|united|free|adnate|joined") ~ "fusion",
    str_detect(lx, "\\d") ~ "count",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink|cream") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "very rarely", "less often")

parse_description <- function(p) {
  text <- prep(p$text)
  cl <- split_clauses(text, p$first_par)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
  cl$rank_ <- p$rank
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5–25 cm long")
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  # leaf and leaflet indumentum per surface, on the first chunk of the first
  # piece of the sentence
  cl$hair_done <- FALSE
  lp <- cl |> filter(sent_organ %in% c("leaves", "leaflets"), piece == 1, chunk == 1)
  for (i in lp$id) {
    em <- if (cl$sent_organ[i] == "leaflets") "leaflet" else NA_character_
    h <- leaf_hairs(cl$piece_text[i], em)
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
  # only the first size of a trait in a piece is scored; later sizes of the
  # same trait ("style of two kinds") stay unmapped
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
          organ %in% c("base", "apex", "margin", "acumen", "midrib", "lateral veins", "intercostal venation", "glands", "upper surface", "lower surface") &
            sent_organ %in% c("leaflets", "leaves") ~ paste("leaflet", organ),
          organ %in% c("base", "apex", "margin") ~ paste(sent_organ, organ),
          organ == "habit" & str_detect(str_to_lower(text), "diam") ~ "habit",
          TRUE ~ organ
        ),
        character_of(text),
        case_when(
          hedged(text) ~ "hedged",
          str_detect(text, "\\b[A-Z]\\. [a-z]{4,}\\b") ~ "names another taxon",
          sent_organ %in% c("fruits", "seeds") & str_detect(str_to_lower(sent_text), immature_re) ~ "immature organ; not the mature size or shape",
          organ == "petiole and rachis" & character_of(text) == "size" ~ "petiole and rachis together; not petiole_length",
          organ == "pinnae" & character_of(text) == "size" ~ "pinna size; candidate trait",
          organ %in% c("calyx lobes", "petals", "bracts") & str_detect(text, "×") ~ "width part of a length x width size",
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
  filter(section == "Description", rank %in% c("family", "genus", "species", "subspecies", "variety")) |>
  group_by(block) |>
  mutate(first_par = row_number() == 1) |>
  ungroup() |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections ----------------------------------------------------------------

later <- paras |>
  filter(
    !section %in% c("Description", "Nomenclature"),
    rank %in% c("family", "genus", "species", "subspecies", "variety")
  ) |>
  mutate(sentence = map(text, \(t) flora_split_sentences(t))) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "") |>
  # citations, types and etymology inside a key annotation are nomenclature
  # (types are in raw/Hou_2000_vouchers.csv)
  filter(!(section == "Infraspecific annotation" &
    str_detect(sentence, "l\\.c\\.|Basionym|Synonyms?:|Type:|^\\(Latin|\\(\\d{4}\\)|^[A-Z][a-z]+ \\d|^\\d|^[A-Z]{2,}|^SING")))

months <- c(
  "January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"
)
month_re <- paste(months, collapse = "|")
calendar <- function(x) {
  if (str_detect(x, "throughout the year|the whole year round|all (the )?year")) {
    return(strrep("y", 12))
  }
  tok <- str_extract_all(x, paste0(month_re, "|–|-|\\bto\\b|,|\\band\\b"))[[1]]
  on <- rep(FALSE, 12)
  last <- NA
  joining <- FALSE
  for (t in tok) {
    if (t %in% months) {
      m <- match(t, months)
      if (joining && !is.na(last)) {
        span <- if (m >= last) last:m else c(last:12, 1:m)
        on[span] <- TRUE
      }
      on[m] <- TRUE
      last <- m
      joining <- FALSE
    } else {
      joining <- t %in% c("–", "-", "to")
    }
  }
  if (!any(on)) NA_character_ else paste(if_else(on, "y", "n"), collapse = "")
}
pheno_part <- function(s, what) {
  re <- if (what == "flower") {
    "(?i)\\bflower(ing|s|ed)?( and fruiting)?\\b(?:(?!fruit)[^;.])*"
  } else {
    "(?i)\\bfruit(ing|s|ed)?\\b[^;.]*"
  }
  str_extract(s, re)
}
pheno <- later |>
  filter(section == "Ecology", rank %in% c("species", "subspecies", "variety")) |>
  mutate(
    sentence = flora_norm(sentence),
    flower = map_chr(sentence, pheno_part, "flower"),
    fruit = map_chr(sentence, pheno_part, "fruit")
  ) |>
  pivot_longer(c(flower, fruit), names_to = "what", values_to = "part") |>
  filter(
    !is.na(part),
    str_detect(part, paste0(month_re, "|throughout the year|year round")),
    # "almost all year round" is a hedge
    !str_detect(part, "\\balmost\\b")
  ) |>
  mutate(
    # "mainly in June–August", "heaviest in July–October"
    cq = str_match(part, "\\b(mainly|heaviest)\\b")[, 2],
    value = map_chr(part, calendar)
  )
pheno_rows <- bind_rows(
  pheno |>
    transmute(block, rank, trait = if_else(what == "flower", "flowering_time", "fruiting_time"), part, cq, value, pdf_page, printed_page, sentence),
  # "Flowering and fruiting all the year round" names both
  pheno |>
    filter(what == "flower", str_detect(part, "(?i)^flowering and fruiting")) |>
    transmute(block, rank, trait = "fruiting_time", part, cq, value, pdf_page, printed_page, sentence)
) |>
  distinct() |>
  transmute(
    block,
    rank,
    trait,
    cq,
    trait_value_raw = sentence,
    trait_value = str_remove(part, "(?i)^(flowering|fruiting)( and (flowering|fruiting))?( [a-z ,]*?)?( in| between)?\\s*(?=[A-Z]|all|throughout)"),
    trait_value_clean = value,
    value_type = "mode",
    notes = if_else(
      value == strrep("y", 12),
      "Months as printed: 'all the year round' or 'throughout the year' scored as every month.",
      "Months as printed; from herbarium collections."
    ),
    source_section = "Ecology",
    pdf_page,
    printed_page
  )

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

# ---- higher ranks, assembly -------------------------------------------------------

taxa <- paras |>
  filter(rank %in% c("species", "subspecies", "variety")) |>
  distinct(block, rank, status) |>
  mutate(
    genus = word(block, 1),
    species = word(block, 1, 2),
    has_description = block %in% desc_paras$block
  )
# copy-down members: taxa with their own description (the two single
# infraspecific taxa stand for their species) and the varieties annotated in a
# key, which inherit the species rows (Ashton_2004 treatment of key
# annotations)
members <- taxa |>
  filter(has_description | rank != "species") |>
  transmute(block, genus, group = NA_character_, species, rank)

# Subfamily and genus statements tied to named genera or to some members
# ("(in taxa outside Sabah and Sarawak)", "Members of some genera") are not
# copied.
named_genus <- paste0("\\b(", gen_re, ")\\b|\\bspecies\\b|\\bsome\\b|\\bfew\\b|outside")
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
  pheno_rows |> mutate(rank_scored_from = NA_character_),
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
    # the family-rank block is the subfamily Caesalpinioideae
    taxon_rank = if_else(taxon_rank == "family", "subfamily", taxon_rank),
    rank_scored_from,
    commonness_qualifier = cq,
    # a TFSS species description describes the taxon in Sabah and Sarawak
    population_region = if_else(
      taxon_rank %in% c("species", "subspecies", "variety") & is.na(rank_scored_from) & !is.na(trait),
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
  ) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(match(taxon_rank, c("subfamily", "genus", "species", "subspecies", "variety")), taxon_name, pdf_page) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")

key_only <- tibble(
  taxon_name = c("Bauhinia", "Caesalpinia", "Pterolobium"),
  taxon_rank = "genus",
  status = "key only",
  n_mapped_rows = 0L,
  flag = "Genus described only inside the key to genera (pp. 120--121); keys are not trait evidence, not scored."
)
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
        "No species description: the one infraspecific taxon in Sabah and Sarawak carries the description.",
      rank != "species" & has_description ~
        "Only infraspecific taxon in Sabah and Sarawak; its description is the local description of the species.",
      rank != "species" ~
        "Variety treated in a key annotation (distribution and ecology only); species rows inherited.",
      block %in% unnumbered ~ "Only species of its genus in the region; heading unnumbered.",
      TRUE ~ ""
    )
  ) |>
  bind_rows(key_only)
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
