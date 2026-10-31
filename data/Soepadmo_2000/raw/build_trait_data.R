# Soepadmo_2000: extract traits from the Tree Flora of Sabah and Sarawak
# Fagaceae treatment (E. Soepadmo, S. Julia & Rusea Go 2000, TFSS vol. 3:
# 1--118). Input is the PDF text layer (born-digital 2007 electronic
# edition), segmented in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Soepadmo_2000/raw/trait_data.csv      mapped and unmapped records
#   data/Soepadmo_2000/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Soepadmo_2000/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with, else the organ of its
# piece or sentence. Scorers run per organ; every chunk that scores nothing
# stays as an unmapped row. Later sections (Distribution, Ecology, Uses,
# vernacular names) are kept one sentence per row; months in Ecology are
# scored; the family Ecology statements on pollination and dispersal are in
# raw/manual_rows.csv. Keys and figures are not read. Names are as printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/Soepadmo_2000/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Soepadmo_2000"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# Skill rule: a hyphenated shape scores both levels ("elliptic-oblong" ->
# elliptical oblong). The seed blocks these compounds (Coode_2018, Adema_1996
# rows); drop the blocking rows here.
dict$leaf_shape <- dict$leaf_shape[!str_detect(names(dict$leaf_shape), "elliptic-oblong|ovate-lanceolate")]
# Fagaceae terms missing from the shared dictionary; first, so they win over
# shorter shared terms
local_terms <- list(
  # "plane" is the margin's posture, not a shape
  leaf_margin_posture = c("\\bplane\\b" = "flat"),
  leaf_margin = c(
    # "toothed above the middle", "remotely serrulate in the apical half"
    "\\b(toothed|dentate|denticulate)\\b" = "toothed",
    "\\bserrulate\\b" = "toothed_serrate"
  ),
  leaf_phenology = c("\\bevergreen\\b" = "evergreen"),
  # "crowded near the end of twigs or rarely pseudowhorled": not whorled
  leaf_phyllotaxis = c("pseudo-?whorled" = ""),
  fruit_type = c("\\bnuts?\\b|\\bacorns?\\b" = "nut")
)
for (t in names(local_terms)) {
  dict[[t]] <- c(local_terms[[t]], dict[[t]])
}

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

# Words split at a line end ("mar- gin") are rejoined by flora_segment() when
# the unhyphenated word occurs elsewhere in the treatment.
prep <- function(x) {
  x |>
    # "dorsi- or basifixed" -> "dorsifixed or basifixed"
    str_replace_all("\\b([a-z]+)- or ([a-z]+)(fixed|petiolar)\\b", "\\1\\3 or \\2\\3") |>
    # "margin revolute sometimes undulate": the qualifier is the second state's
    str_replace_all("\\b(revolute|plane|entire) (sometimes|rarely|occasionally) ", "\\1 or \\2 ") |>
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
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no|non)\\b")
hair_re <- "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsute|pilose|villous|sericeous|velutinous|woolly|puberul|setose|strigose"

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide)
sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^(monoecious|stunted |small |evergreen )*(trees?|treelets?|shrubs?)\\b|^monoecious",
  "growth", "^growth mode",
  "indumentum", "^indumentum",
  "wood", "^wood\\b",
  "bark", "^bark",
  "sapwood", "^sapwood",
  "twigs", "^(young )?twigs?",
  "buds", "^terminal buds?",
  "stipules", "^stipules?",
  "leaves", "^leaves|^leaf\\b",
  "male inflorescences", "^(male|staminate) inflorescences?",
  "female inflorescences", "^(female|pistillate|androgynous|mixed)\\b[a-z ,]*inflorescences?",
  "inflorescences", "^inflorescences?",
  "male flowers", "^male flowers?",
  "female flowers", "^(female|pistillate) flowers?",
  "cupules", "^(young |mature )?cupules?",
  "fruits", "^(fruits?|acorns?|nuts?)\\b",
  "seeds", "^(seeds?|embryo|germination)\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "buttresses", "^((low|small|large|prominent|steep|thin) )*buttress",
  "stilt-roots", "^((low|small|large) )*stilt-?roots?",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^sapwood",
  "bracts", "^(bracts?|bracteoles?)\\b",
  "perianth", "^(perianth|tepals?)\\b",
  "lobes", "^lobes\\b|^the lobes\\b",
  "filaments", "^(stamen )?filaments?\\b",
  "stamens", "^stamens?\\b",
  "anthers", "^anthers?\\b",
  "pistillode", "^pistillodes?\\b",
  "staminodes", "^staminodes?\\b",
  "styles", "^styles?\\b",
  "stigmas", "^stigmas?\\b",
  "ovary", "^ovar(y|ies)\\b",
  "ovules", "^ovules?\\b",
  "stalk", "^stalks?\\b",
  "rim", "^rim\\b",
  "wall", "^(the )?(external |outer |inner )?walls?\\b",
  "lamellae", "^(lamellae|lamellas|lamella)\\b",
  "edge", "^edges?\\b",
  "appendages", "^(spines?|scale-like appendages|scales|appendages|tubercles|bristles)\\b",
  "scar", "^(basal )?scars?\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^lateral veins?\\b",
  "intercostal venation", "^(intercostal|tertiary) (venation|veins)",
  "petiole", "^petioles?\\b",
  "cotyledons", "^cotyledons?\\b",
  "embryo", "^embryo\\b",
  "germination", "^(seed )?germination\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text) {
  # "Male flowers: perianth 5-lobed, ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- match_organ(s, sent_organs)
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) {
        p_org <- if (pi > 1 && identical(s_org, "leaves")) "lamina" else s_org
      }
      chunks <- flora_split_pieces(pieces[pi], ",")
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
          # the chunk that named the current organ ("bracteoles linear")
          organ_text = cur_text,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- row helpers ------------------------------------------------------------------

# Generic versions are flora_*() in flora_parse.R; these bind the dataset's
# dictionary and dimension grabber.
row <- flora_row
num_rows <- flora_value_rows
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
term_level <- function(trait, term) flora_term_level(trait, term, dict)
cat_rows <- function(trait, x, em = NA_character_) {
  flora_cat_rows(trait, x, dict, em)
}

# ---- scorers --------------------------------------------------------------------

# entity_measured for inflorescences, their bracts and flowers, from the sentence
infl_kind <- function(s) {
  k <- str_to_lower(str_extract(s, "^[A-Za-z, ]*?(?= inflorescences?\\b)"))
  k <- str_replace_all(coalesce(k, ""), "pistillate", "female")
  k <- str_replace_all(k, "staminate", "male")
  str_squish(paste(k, "inflorescence"))
}
flower_kind <- \(o) if (identical(o, "female flowers")) "female flower" else "male flower"

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|shortly|thinly|very|rather|evenly|",
  "loosely|softly|faintly|slightly|appressed|adpressed|stellate|simple|",
  "golden|silvery|brownish|brown|greyish|grey|whitish|yellowish|rusty|reddish|",
  "or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z]*$"
)

# "densely tomentose with simple and stellate hairs" -> "densely tomentose"
hair_core <- \(x) str_squish(str_remove(x, "\\s+with [a-z ,-]*\\bhairs?$"))

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  if (hedged(lx)) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  # no sentence organ: family and genus "Seeds without endosperm", etc.
  if (is.na(organ)) {
    return(NULL)
  }
  switch(
    organ,
    habit = {
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(up to|to|c\\.)\\s*$")
      add(cat_rows("plant_growth_form", pre))
      add(cat_rows("sex_type", lx))
      add(cat_rows("leaf_phenology", lx))
      h <- grab(x, "(tall|high)\\b")
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
      d <- grab(x, "(in )?diam(eter|\\.)?")
      if (!is.na(d)) {
        dr <- num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*"), cq_text = x)
        # "Tree up to 30 m tall, 50 cm diameter": the height's "to" also bounds
        # the diameter in the next chunk
        if (!is.null(dr) && !str_detect(lx, "^c\\.") &&
          str_detect(str_to_lower(cl$prev %||% ""), "\\bto (c\\.\\s?)?\\d[^,;]*\\bm (tall|high)\\b")) {
          dr <- dr |> mutate(value_type = if_else(value_type == "mean", "maximum", value_type))
        }
        add(dr)
      }
    },
    # scored once, on the chunk naming them; the qualifier is the one of the
    # "... present" clause ("stilt-roots occasionally present, up to 1.8 m tall")
    buttresses = ,
    `stilt-roots` = if (!negative(lx) && str_detect(lx, "buttress|stilt")) {
      q <- qualifier(coalesce(str_extract(cl$piece_text, "[^,]*\\bpresent\\b"), x))
      if (str_detect(lx, "buttress")) add(row("stem_base_form", "buttressed", cq = q, raw = x))
      if (str_detect(lx, "stilt")) add(row("root_system_type", "stilt-roots", "stilt_roots", cq = q, raw = x))
    },
    bark = if (so == "bark") {
      # lenticels, patches and blazes are not the bark colour
      if (!str_detect(lx, "lenticel|patch|spot|blaze|slash|inside|when cut")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = {
      core <- hair_core(str_remove(lx, "^(young )?twigs? "))
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), "glabrescent|glabrous when|when young|initially|becoming")) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_arrangement", lx))
      add(cat_rows("leaf_phyllotaxis", lx))
      # "simple" elsewhere in the piece names hairs
      if (str_detect(lx, "^leaves simple$")) add(row("leaf_compoundness", "simple", raw = x))
    },
    lamina = if (so == "leaves") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (str_detect(lx, "length:width ratio")) {
        r <- str_match(lx, "ratio (\\d+(?:\\.\\d+)?)(?:–(\\d+(?:\\.\\d+)?))?:1")
        if (!is.na(r[1, 1])) {
          add(count_rows("leaf_length_to_width_ratio", x, paste0(r[1, 2], if (!is.na(r[1, 3])) paste0("–", r[1, 3])), unit = "{dimensionless}"))
        }
      } else if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_shape", lx))
      }
    },
    base = if (so == "leaves") add(cat_rows("leaf_base_shape", lx)),
    margin = if (so == "leaves") {
      add(cat_rows("leaf_margin", lx))
      add(cat_rows("leaf_margin_posture", lx))
    },
    apex = if (so == "leaves") {
      add(cat_rows("leaf_apex_shape", lx))
    },
    `lateral veins` = {
      a <- str_match(lx, paste0("angle of (c\\. ?)?(", num, ")(?:–(", num, "))?\\s?°"))
      if (!is.na(a[1, 1])) {
        add(count_rows("leaf_secondary_vein_angle", x, paste0(if (!is.na(a[1, 2])) "c. ", a[1, 3], if (!is.na(a[1, 4])) paste0("–", a[1, 4])), unit = "deg"))
      }
    },
    petiole = {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    # Only "spikes": the "paniculate clusters" are clusters of spikes, and
    # "solitary in the axils" is one spike per axil, not a single flower
    inflorescences = if (str_detect(lx, "\\bspikes?\\b")) {
      add(row("inflorescence_type", "spikes", "spike", raw = x))
    },
    `male inflorescences` = ,
    `female inflorescences` = {
      em <- infl_kind(cl$sent_text)
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("inflorescence_length", l, em = em, cq_text = x))
    },
    bracts = if (str_detect(so, "inflorescences")) {
      b <- str_extract(str_to_lower(cl$organ_text), "^(bracts and bracteoles|bracts|bracteoles|bract)")
      em <- paste(infl_kind(cl$sent_text), coalesce(str_replace(b, "s\\b", ""), "bract") |> str_replace("bracts and bracteole", "bract and bracteole"))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_bract_length", NA), x, em = em))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_bract_length", l, em = em, cq_text = x))
      }
    },
    perianth = ,
    lobes = if (str_detect(so, "flowers")) {
      em <- flower_kind(so)
      m <- str_match(lx, paste0("^perianth (", cnt, ")[–-]lobed"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_perianth_parts_count", x, m[1, 2], em = em, notes = "Perianth lobes counted as perianth parts."))
      }
      tn <- "Perianth lobe (tepal) length scored as flower_sepal_length (provisional, Cheek_2012 question 4)."
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_sepal_length", NA), x, em = em) |> mutate(notes = paste(notes, tn)))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_sepal_length", l, em = em, notes = tn, cq_text = x))
      }
    },
    stamens = if (str_detect(so, "flowers")) {
      m <- str_match(lx, paste0("^stamens? (", cnt, ")(?!\\s?(mm|cm))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_fertile_stamens_count", x, m[1, 2], em = "male flower"))
      }
    },
    filaments = if (str_detect(so, "flowers")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, em = "male flower", cq_text = x))
    },
    anthers = add(cat_rows("flower_anther_attachment", lx)),
    ovary = {
      add(cat_rows("flower_ovary_position", lx))
      m <- str_match(lx, paste0("(", cnt, ")[–-]locul(ed|ar)"))
      if (!is.na(m[1, 1]) && so != "fruits") {
        add(count_rows("flower_structural_carpels_count", x, m[1, 2], notes = "Ovary locules scored as carpels (one locule per carpel; placentation axile)."))
      }
    },
    ovules = {
      m <- str_match(lx, "(\\d+) per locule")
      if (!is.na(m[1, 1])) add(count_rows("flower_ovules_per_functional_carpel_count", x, m[1, 2]))
    },
    styles = if (identical(so, "female flowers")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, em = "female flower", cq_text = x))
    },
    fruits = {
      core <- str_squish(str_remove(lx, "^(fruits?|acorns?|nuts?)( \\((acorns|nuts)\\))?\\b"))
      # the type from family and genus "Fruits ..." statements; species
      # sentences only name the organ ("Acorns ...", "Nuts ...")
      if (str_detect(lx, "^fruits")) add(cat_rows("fruit_type", lx))
      add(cat_rows("fruit_dehiscence", lx))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x))
      } else {
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?(in )?diam.*"), notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        }
      }
      if (str_detect(hair_core(core), hairs_only) && !str_detect(str_to_lower(cl$piece_text), "glabrescent|when young|initially|becoming")) {
        add(cat_rows("fruit_surface_hairs", hair_core(core)))
      }
      # colour of the ripe fruit, not of its hairs or of young fruit
      if (!str_detect(lx, hair_re) && !str_detect(lx, "young|immature|unripe|scar|part|dried|dry")) {
        add(cat_rows("fruit_colour", core))
      }
    },
    wall = if (so == "fruits") {
      t <- grab(x, "thick")
      if (!is.na(t)) add(num_rows("fruit_wall_thickness", t, cq_text = x))
    },
    germination = add(cat_rows("seedling_germination_location", lx)),
    seeds = add(cat_rows("seedling_germination_location", lx))
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
leaf_hairs <- function(piece) {
  ch <- str_to_lower(flora_split_pieces(piece, ","))
  ch <- str_remove(ch, "^leaves ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  if (!length(h) || any(str_detect(h, "glabrescent|young|hedge")) || hedged(piece)) {
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
  lx <- str_to_lower(x)
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|bony|horny|corky|rigid") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|scalariform") ~ "venation",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "very rarely", "less often")

parse_description <- function(p) {
  text <- prep(p$text)
  cl <- split_clauses(text)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
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
  # leaf indumentum per surface, on the first chunk of the first leaf piece
  cl$hair_done <- FALSE
  lp <- cl |> filter(sent_organ %in% "leaves", piece == 1, chunk == 1)
  for (i in lp$id) {
    h <- leaf_hairs(cl$piece_text[i])
    if (!is.null(h)) {
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h)
      # the hair chunks of this piece are scored by it
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
  # text was rewritten by prep(); scale positions back to the segmented text
  pos <- round(coalesce(pos, 1L) * nchar(p$text) / max(nchar(text), 1))
  pages <- map(pos, \(k) flora_page_at(p, k))
  cl$pdf_page <- map_int(pages, \(x) as.integer(x[["pdf"]]))
  cl$printed_page <- map_int(pages, \(x) as.integer(x[["printed"]]))
  scored <- map_lgl(cl$rows, \(r) !is.null(r) && nrow(r) > 0)
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    # "base rounded, rarely acute": the unqualified alternative is `usually`
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(cq = if_else(value_type == "mode" & is.na(cq) & any(cq %in% rare_q), "usually", cq)) |>
    ungroup() |>
    select(-sentence, -piece)
  unmapped <- cl |>
    filter(!scored, !hair_done) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = paste0(
        "UNMAPPED: ",
        if_else(organ %in% c("lamina", "base", "margin", "apex", "acumen") & sent_organ %in% "leaves",
          paste("leaf", str_replace(organ, "lamina", "blade")),
          if_else(organ %in% c("base", "apex", "wall", "scar", "stalk") & !is.na(sent_organ),
            paste(sent_organ, organ), coalesce(organ, "unknown organ")
          )
        ),
        " | ",
        character_of(text),
        if_else(hedged(text), " | hedged", "")
      ),
      pdf_page,
      printed_page
    )
  bind_rows(mapped |> select(-id), unmapped) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ------------------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description", rank %in% c("family", "genus", "species")) |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections ----------------------------------------------------------------

later <- paras |>
  filter(
    section %in% c("Distribution", "Ecology", "Uses", "Use", "Vernacular name", "Vernacular names", "Taxonomy and Classification"),
    rank %in% c("family", "genus", "species")
  ) |>
  mutate(sentence = map(text, \(t) flora_split_sentences(t))) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "")

months <- c(
  "January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"
)
month_re <- paste(months, collapse = "|")
calendar <- function(x) {
  if (str_detect(x, "throughout the year|the whole year round|all year")) {
    return(strrep("y", 12))
  }
  # "between September and February" is a range
  x <- str_replace_all(x, paste0("between (", month_re, ") and (", month_re, ")"), "\\1–\\2")
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
  filter(section == "Ecology", rank == "species") |>
  mutate(
    flower = map_chr(sentence, pheno_part, "flower"),
    fruit = map_chr(sentence, pheno_part, "fruit")
  ) |>
  pivot_longer(c(flower, fruit), names_to = "what", values_to = "part") |>
  filter(!is.na(part), str_detect(part, paste0(month_re, "|throughout the year|whole year round"))) |>
  mutate(value = map_chr(part, calendar))
pheno_rows <- bind_rows(
  pheno |>
    transmute(block, rank, trait = if_else(what == "flower", "flowering_time", "fruiting_time"), part, value, pdf_page, printed_page, sentence),
  # "Flowering and fruiting the whole year round" names both
  pheno |>
    filter(what == "flower", str_detect(part, "(?i)^flowering and fruiting")) |>
    transmute(block, rank, trait = "fruiting_time", part, value, pdf_page, printed_page, sentence)
) |>
  distinct() |>
  transmute(
    block,
    rank,
    trait,
    trait_value_raw = sentence,
    trait_value = str_remove(part, "(?i)^(flowering|fruiting)( and (flowering|fruiting))?( in| between)?\\s*"),
    trait_value_clean = value,
    value_type = "mode",
    notes = if_else(
      value == strrep("y", 12),
      "Months as printed: 'the whole year round' scored as every month.",
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
    notes = paste0("UNMAPPED: ", str_to_lower(section), " | text"),
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
  filter(rank == "species") |>
  distinct(block, rank, status) |>
  mutate(genus = word(block, 1), has_description = block %in% desc_paras$block)
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species = block, rank)

# Family statements tied to named genera ("in whorls of 3 (Trigonobalanus)",
# "(Lithocarpus and Quercus)") hold for those genera only.
named_genus <- paste0("\\b(", gen_re, ")\\b|\\bspecies of\\b|\\ba few\\b")
copied <- flora_copy_down(
  higher_rows = own |> filter(!str_detect(coalesce(trait_value_raw, ""), named_genus)),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
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
    taxon_rank,
    rank_scored_from,
    commonness_qualifier = cq,
    # a TFSS species description describes the species in Sabah and Sarawak
    population_region = if_else(
      taxon_rank == "species" & is.na(rank_scored_from) & !is.na(trait),
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
  bind_rows(manual |> mutate(across(c(pdf_page, printed_page), as.integer))) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(match(taxon_rank, c("family", "genus", "species")), taxon_name, pdf_page) |>
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
      block == "Lithocarpus tawaiensis" ~ "Female flowers unknown; cupule described from young material.",
      block == "Trigonobalanus verticillata" ~ "Only species of the genus in the region; heading unnumbered.",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
