# Kochummen_2000: extract traits from the Tree Flora of Sabah and Sarawak
# Moraceae treatment (K.M. Kochummen & Rusea Go 2000, TFSS vol. 3: 181--334).
# Input is the PDF text layer (born-digital 2007 electronic edition),
# segmented in raw/segment_source.R. Clause parser after data/Soepadmo_2000
# (same volume).
#
# Writes (run from the repository root):
#   data/Kochummen_2000/raw/trait_data.csv      mapped and unmapped records
#   data/Kochummen_2000/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Kochummen_2000/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with, else the organ of its
# piece or sentence. Scorers run per organ; every chunk that scores nothing
# stays as an unmapped row. Later sections (Distribution, Ecology, Uses,
# vernacular names, notes, infraspecific annotations) are kept one sentence
# per row; months in Ecology are scored; family and genus Ecology statements
# on pollination and dispersal are in raw/manual_rows.csv. Keys and figures
# are not read. Names are as printed.
#
# Figs (syconia) and Artocarpus syncarps are compound fruiting structures,
# not fruits (SKILL.md step 4, "Adjacent organs"): their ripe colour is
# fruit_colour with entity_measured syconium / syncarp (provisional,
# Beentje_2023 questions 1--2); their size stays unmapped. Achenes and
# drupes are the fruits.
suppressPackageStartupMessages(library(tidyverse))
source("data/Kochummen_2000/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Kochummen_2000"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# "or more frequently strangler" (F. xylophylla); not yet in the shared list
flora_qual_words <- c(flora_qual_words, "more frequently")
flora_qual_re <- paste0("\\b(", paste(flora_qual_words, collapse = "|"), ")\\b")
# Moraceae terms missing from the shared dictionary; first, so they win over
# shorter shared terms
local_terms <- list(
  leaf_compoundness = c("compound|imparipinnate" = "compound"),
  leaf_margin = c(
    "\\b(toothed|dentate|denticulate)\\b" = "toothed",
    "\\bserrulate\\b" = "toothed_serrate"
  ),
  leaf_margin_posture = c("\\bplane\\b" = "flat"),
  # "distichous" as a seed row changed Soepadmo_2000 and Ashton_2004 copy-down
  leaf_arrangement = c("\\bdistichous\\b" = "distichous"),
  inflorescence_type = c("capitate|\\bheads?\\b" = "head")
)
for (t in names(local_terms)) {
  dict[[t]] <- c(local_terms[[t]], dict[[t]])
}

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

prep <- function(x) {
  x |>
    # "(or strangler)", "(or epiphyte)": an alternative growth form, not an aside
    str_replace_all("\\((or [a-z]+)\\)", "\\1") |>
    str_replace_all("º", "°") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|(?:up )?to\\s)?", num,
  "(?:–", num, ")?(?:\\s?\\(–", num, "\\))?"
)
unit_re <- "(mm|cm|m)\\b"
cnt <- "(?:\\(\\d+–\\)\\s?)?\\d+(?:(?:–| or )\\d+)?(?:\\s?\\(–\\d+\\))?"

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
diam <- \(x) str_remove(grab(x, "(in )?(diam(eter|\\.)?|across)"), "\\s?(in )?(diam.*|across)$")

hedged <- \(x) str_detect(x, flora_hedge_re)
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsute|hispid|",
  "pilose|villous|sericeous|velutinous|velvety|woolly|puberul|setose|strigose"
)

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide)
sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^(dioecious|monoecious|deciduous|small|medium-sized|tree|trees|treelets?|shrubs?)\\b",
  "indumentum", "^(young (shoots|parts|twigs)|all parts)\\b",
  "bark", "^bark",
  "sapwood", "^sapwood",
  "twigs", "^(young )?twigs?",
  "buds", "^(terminal )?buds?",
  "stipules", "^stipules?",
  "juvenile leaves", "^juvenile leaves",
  "leaflets", "^leaflets",
  "leaves", "^leaves|^leaf\\b",
  "male inflorescences", "^male (\\(staminate\\) )?(inflorescences?|heads?)",
  "female inflorescences", "^female (\\(pistillate\\) )?(inflorescences?|heads?)",
  "inflorescences", "^(inflorescences?|heads)\\b",
  "male flowers", "^male (\\(staminate\\) )?flowers?",
  "female flowers", "^female (\\(pistillate\\) )?(or bisexual )?flowers?",
  "gall flowers", "^gall flowers?",
  "flowers", "^flowers?\\b",
  "syconia", "^syconia|^syconium",
  "tepals", "^(tepals?|perianth)\\b",
  "achenes", "^achenes?",
  "syncarps", "^syncarps?",
  "fruits", "^fruits?\\b",
  "seeds", "^(seeds?|embryo|germination)\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "buttresses", "^(with |without |often |shortly |occasionally )?((low|short|small|large|prominent|spreading|rounded) )*buttress",
  "stilt-roots", "^(sometimes )?(with )?stilt-?roots?",
  "aerial roots", "^(with|without)\\b[^,;]*aerial roots",
  "latex", "^(latex|sap)\\b|^(with|all parts with) [a-z ]*latex",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "bracts", "^((basal|lateral|interfloral|involucral|intrafloral) )?(bracts?|bracteoles?)\\b",
  "bristles", "^(internal )?bristles\\b",
  "peduncle", "^peduncles?\\b",
  "pedicel", "^pedicels?\\b",
  "tepals", "^(perianth|tepals?)\\b",
  "filaments", "^filaments?\\b",
  "stamens", "^stamens?\\b",
  "anthers", "^anthers?\\b",
  "pistillode", "^pistillodes?\\b",
  "styles", "^styles?\\b",
  "stigmas", "^(stigmas?|stigmatic arms?)\\b",
  "ovary", "^ovar(y|ies)\\b",
  "ovules", "^ovules?\\b",
  "processes", "^processes\\b",
  "pulp", "^(pulp|flesh)\\b",
  "males", "^males\\b",
  "females", "^females\\b",
  "rachis", "^rachis\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(basal )?lateral veins?\\b|^basal (pair|veins)",
  "intercostal venation", "^(intercostal|tertiary) (venation|veins)|^venation",
  "petiolule", "^petiolules?\\b",
  "petiole", "^petioles?\\b",
  "surface", "^surface\\b",
  "wall", "^(pulpy )?wall\\b",
  "cotyledons", "^cotyledons?\\b",
  "embryo", "^embryo\\b",
  "germination", "^germination\\b"
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
with_note <- \(r, n) if (is.null(r)) NULL else mutate(r, notes = n)

# ---- scorers --------------------------------------------------------------------

infl_em <- function(so, organ) {
  case_when(
    so == "male inflorescences" | organ == "males" ~ "male inflorescence",
    so == "female inflorescences" | organ == "females" ~ "female inflorescence",
    TRUE ~ "inflorescence"
  )
}
flower_em <- function(so) {
  case_when(
    str_detect(so, "^male") ~ "male flower",
    str_detect(so, "^female") ~ "female flower",
    TRUE ~ NA_character_
  )
}

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|shortly|thinly|very|rather|evenly|",
  "loosely|softly|faintly|slightly|appressed|adpressed|stiff|rough|soft|",
  "golden|silvery|brownish|brown|greyish|grey|whitish|white|yellowish|rusty|",
  "reddish|tawny|yellow|or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z]*$"
)
# "densely pubescent with reddish brown hairs" -> "densely pubescent"
hair_core <- \(x) str_squish(str_remove(x, "\\s+with [a-z ,0-9.–-]*\\bhairs?\\b.*$"))
age_re <- "glabrescent|glabrous when|when young|initially|becoming|older|young"

# Syconia and syncarps: colour only from "ripening <colour>"
ripe_colour <- function(x, em) {
  lx <- str_to_lower(x)
  if (!str_detect(lx, "^ripening\\b")) {
    return(NULL)
  }
  with_note(
    cat_rows("fruit_colour", lx, em = em),
    paste0(
      "Ripe colour of the ", em, " scored as fruit_colour (provisional, ",
      "Beentje_2023 questions 1--2; Kochummen_2000 question 1)."
    )
  )
}

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  if (hedged(lx) || is.na(organ)) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  switch(
    organ,
    habit = {
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(rarely )?(up to|to|c\\.)\\s*$")
      add(cat_rows("plant_growth_form", pre))
      add(cat_rows("plant_growth_substrate", pre))
      add(cat_rows("sex_type", lx))
      add(cat_rows("leaf_phenology", lx))
      add(cat_rows("plant_physical_defence_structures", lx))
      # the first chunk may omit "tall" ("Tree up to 10 m, 15 cm diameter")
      kw <- if (cl$chunk == 1) "(tall|high)?" else "(tall|high)\\b"
      h <- str_extract(x, paste0(rng, "\\s?m\\b\\s?", kw))
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
      d <- diam(x)
      if (!is.na(d)) {
        dr <- num_rows("plant_diameter_breast_height", d, cq_text = x)
        # "Tree up to 30 m tall, 50 cm diameter": the height's "to" also bounds
        # the diameter in the next chunk
        if (!is.null(dr) && !str_detect(lx, "^c\\.") &&
          str_detect(str_to_lower(cl$prev %||% ""), "\\bto (c\\.\\s?)?\\d[^,;]*\\bm( (tall|high))?\\b")) {
          dr <- dr |> mutate(value_type = if_else(value_type == "mean", "maximum", value_type))
        }
        add(dr)
      }
    },
    `aerial roots` = if (!str_detect(lx, "without or with|with or without")) {
      add(cat_rows("root_system_type", lx))
    },
    # scored once, on the chunk naming them; "with or without buttresses" and
    # "without buttresses" are not scored (no absent level)
    buttresses = ,
    `stilt-roots` = if (!str_detect(lx, "\\bwithout\\b|\\babsent\\b") &&
      !str_detect(str_to_lower(cl$piece_text), "or without|or absent")) {
      q <- qualifier(coalesce(str_extract(cl$piece_text, "[^,]*\\b(present|buttress|stilt)[a-z-]*"), x))
      if (str_detect(lx, "buttress")) add(row("stem_base_form", "buttressed", cq = q, raw = x))
      if (str_detect(lx, "stilt")) add(row("root_system_type", "stilt-roots", "stilt_roots", cq = q, raw = x))
    },
    bark = if (so == "bark") {
      # lenticels, patches and blazes are not the bark colour
      if (!str_detect(lx, "lenticel|patch|spot|blaze|slash|inside|when cut|exposure")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = {
      core <- hair_core(str_remove(lx, "^(young )?twigs? "))
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), age_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_arrangement", lx))
      add(cat_rows("leaf_phyllotaxis", lx))
      if (str_detect(lx, "^leaves simple$|^simple$")) add(row("leaf_compoundness", "simple", raw = x))
      if (str_detect(lx, "compound")) add(cat_rows("leaf_compoundness", lx))
      # "often glaucous below"
      if (str_detect(lx, "glaucous")) add(cat_rows("leaf_glaucousness", lx))
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
      add(cat_rows("leaf_lobation", lx))
    },
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", lx)),
    petiole = if (so %in% c("leaves", "leaflets")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    leaflets = if (so == "leaflets" && !is.null(grab_dims(x))) {
      add(dim_rows(c("leaflet_length", "leaflet_width"), x))
    },
    inflorescences = ,
    `male inflorescences` = ,
    `female inflorescences` = ,
    males = ,
    females = if (str_detect(so, "inflorescences")) {
      em <- infl_em(so, organ)
      # positions printed with an unscored alternative ("axillary or on twigs
      # behind the leaves") are not scored
      # positions in a comma list of alternatives ("solitary, in pairs or
      # clustered") are not scored either
      pl <- str_to_lower(cl$piece_text)
      if (!str_detect(lx, "behind|stem|trunk|branches|stolon|runner|older|shoots") &&
        !(str_detect(lx, "^(solitary|axillary|paired)$") && str_detect(pl, "in pairs|paired|cluster| or "))) {
        add(cat_rows("inflorescence_type", lx, em = em))
      }
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("inflorescence_length", "inflorescence_diameter"), x, em = em) |>
          mutate(notes = if_else(str_detect(notes, "Dimension 2"),
            paste(notes, "Head width scored as inflorescence_diameter."), notes)))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("inflorescence_length", l, em = em, cq_text = x))
        d <- diam(x)
        if (!is.na(d)) add(num_rows("inflorescence_diameter", d, em = em, cq_text = x))
      }
    },
    tepals = if (so %in% c("tepals", "flowers") | str_detect(so, "^(male|female)")) {
      em <- flower_em(so)
      m <- str_match(lx, paste0("^(?:tepals|perianth(?: with)?) (", cnt, ")(?![\\d.–]*\\s?(mm|cm))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_perianth_parts_count", x, str_replace(m[1, 2], " or ", "–"), em = em,
          notes = "Tepals counted as perianth parts."))
      }
      tn <- "Tepal length scored as flower_sepal_length (provisional, Cheek_2012 question 4)."
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_sepal_length", l, em = em, notes = tn, cq_text = x))
      if (!str_detect(lx, hair_re)) {
        add(with_note(
          cat_rows("flower_colour", lx, em = em),
          "Tepal colour scored as flower_colour (provisional, Cheek_2012 question 4)."
        ))
      }
    },
    `male flowers` = ,
    `female flowers` = if (organ == so) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_length", l, em = flower_em(so), cq_text = x))
    },
    stamens = if (so %in% c("male flowers", "male inflorescences")) {
      m <- str_match(lx, paste0("^stamens? (", cnt, "|one or two)(?![\\d.–]*\\s?(mm|cm))"))
      if (!is.na(m[1, 1])) {
        v <- str_replace(str_replace(m[1, 2], "one or two", "1–2"), " or ", "–")
        add(count_rows("flower_fertile_stamens_count", x, v, em = "male flower"))
      }
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_stamen_length", l, em = "male flower", cq_text = x))
    },
    filaments = if (str_detect(so, "^male")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, em = "male flower", cq_text = x))
    },
    anthers = if (str_detect(so, "^male")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_anther_length", l, em = "male flower", cq_text = x))
    },
    styles = if (so == "female flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, em = "female flower", cq_text = x))
    },
    syconia = add(ripe_colour(x, "syconium")),
    syncarps = add(ripe_colour(x, "syncarp")),
    achenes = {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x, em = "achene"))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("fruit_length", l, em = "achene", cq_text = x))
      }
    },
    fruits = {
      core <- str_squish(str_remove(lx, "^fruits?\\b( \\(syncarps\\))?"))
      # the type from family and genus "Fruits ..." statements
      if (cl$chunk == 1 && cl$piece == 1) add(cat_rows("fruit_type", lx))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("fruit_length", l, cq_text = x))
        d <- diam(x)
        if (!is.na(d)) {
          add(num_rows("fruit_width", d, notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        }
      }
      if (str_detect(hair_core(core), hairs_only) && !str_detect(str_to_lower(cl$piece_text), age_re)) {
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
    seeds = {
      if (!is.null(grab_dims(x))) add(dim_rows(c("seed_length", "seed_width"), x))
      if (cl$chunk == 1 && !str_detect(lx, "\\d")) add(cat_rows("seed_shape", lx))
      add(cat_rows("seedling_germination_location", lx))
    }
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
  if (!length(h) || any(str_detect(h, "glabrescent|young|when")) || hedged(piece)) {
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
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|bony|horny|corky|rigid|fleshy") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|scalariform") ~ "venation",
    str_detect(lx, "\\d") ~ "count",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink") ~ "colour",
    TRUE ~ "description"
  )
}
# candidate traits for frequent unmapped organs
candidate <- c(
  syconia = "candidate fruit_length / fruit_width (syconium); Kochummen_2000 question 1",
  syncarps = "candidate fruit_length / fruit_width (syncarp); Kochummen_2000 question 1",
  latex = "candidate latex trait",
  stipules = "no stipule trait"
)

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
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?|rarely up to )?[\\d(]")) {
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
  # a paragraph with nothing scored still needs the row columns
  mapped <- cl |>
    filter(scored) |>
    # a commonness word anywhere in the piece ("Bark generally greyish,
    # smooth") bars its values from copy-down
    mutate(piece_qualified = str_detect(str_to_lower(piece_text), flora_qual_re)) |>
    select(id, sentence, piece, pdf_page, printed_page, piece_qualified, rows) |>
    unnest(rows) |>
    bind_rows(row(NA_character_, NA, raw = NA_character_)[0, ]) |>
    # "base rounded, rarely acute": the unqualified alternative is `usually`
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(cq = if_else(value_type == "mode" & is.na(cq) & any(cq %in% rare_q), "usually", cq)) |>
    ungroup() |>
    select(-sentence, -piece)
  unmapped <- cl |>
    filter(!scored, !hair_done) |>
    mutate(
      org = case_when(
        organ %in% c("lamina", "base", "margin", "apex", "acumen") & sent_organ %in% "leaves" ~
          paste("leaf", str_replace(organ, "lamina", "blade")),
        organ %in% c("base", "apex", "wall", "surface", "bracts", "peduncle", "processes", "pulp") &
          !is.na(sent_organ) & sent_organ != organ ~ paste(sent_organ, organ),
        !is.na(organ) ~ organ,
        # "Inflorescences and flowers unknown", "Perianth absent"
        TRUE ~ coalesce(sent_organ, "habit")
      )
    ) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = flora_unmapped_note(
        org,
        character_of(text),
        str_squish(paste(
          if_else(hedged(text), "hedged", ""),
          coalesce(unname(candidate[organ]), "")
        ))
      ),
      pdf_page,
      printed_page
    )
  bind_rows(mapped |> select(-id), unmapped) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ------------------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description") |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# F. cumingii "Dioecious shrub, up to 4 cm tall" (p. 268) is not a plant
# height (Kochummen_2000 question 6); kept unmapped
cumingii_h <- own$block == "Ficus cumingii" &
  own$trait %in% "plant_height"
own <- bind_rows(
  own[!cumingii_h, ],
  own[cumingii_h, ] |>
    distinct(block, rank, trait_value_raw, pdf_page, printed_page, source_section) |>
    mutate(trait = NA_character_, notes = flora_unmapped_note(
      "habit", "size",
      "printed 'up to 4 cm tall' for a shrub, probably a misprint; Kochummen_2000 question 6"
    ))
)

# ---- later sections ----------------------------------------------------------------

later_sections <- c(
  "Distribution", "Ecology", "Uses", "Use", "Vernacular name", "Vernacular names",
  "Taxonomy", "Notes", "Note", "Unlabelled note", "Infraspecific annotation"
)
later <- paras |>
  filter(section %in% later_sections) |>
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
  if (str_detect(x, "throughout the year|whole year round|all (the )?year( round)?")) {
    return(strrep("y", 12))
  }
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
  filter(section == "Ecology", rank %in% c("species", "subspecies", "variety"), !hedged(sentence)) |>
  mutate(
    flower = map_chr(sentence, pheno_part, "flower"),
    fruit = map_chr(sentence, pheno_part, "fruit")
  ) |>
  pivot_longer(c(flower, fruit), names_to = "what", values_to = "part") |>
  filter(!is.na(part), str_detect(part, paste0(month_re, "|throughout the year|year round"))) |>
  mutate(value = map_chr(part, calendar)) |>
  filter(!is.na(value))
pheno_rows <- bind_rows(
  pheno |>
    transmute(block, rank, trait = if_else(what == "flower", "flowering_time", "fruiting_time"), part, value, pdf_page, printed_page, sentence),
  # "Flowering and fruiting all year round" names both
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
    trait_value = str_remove(part, "(?i)^(flowering|fruiting)( and (flowering|fruiting))?( in| between| from)?\\s*"),
    trait_value_clean = value,
    value_type = "mode",
    notes = if_else(
      value == strrep("y", 12),
      "Months as printed: 'all year round' scored as every month.",
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

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c", pdf_page = "i", printed_page = "i")
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
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

# Statements naming a genus or species group hold for those taxa only; out-of-
# region alternatives ("climbers (not in Sabah and Sarawak)") are dropped.
named_genus <- paste0("\\b(", gen_re, ")\\b|\\bspecies of\\b|\\ba few\\b|\\bsubgen\\b|not in Sabah")
higher_manual <- manual |>
  filter(taxon_rank %in% c("family", "genus"), !is.na(trait)) |>
  transmute(block = taxon_name, rank = taxon_rank, trait, trait_value, trait_value_clean,
    value_type, unit, cq = commonness_qualifier, entity_measured, notes, trait_value_raw,
    pdf_page, printed_page, source_section)
copied <- flora_copy_down(
  higher_rows = bind_rows(own, higher_manual) |>
    filter(!str_detect(coalesce(trait_value_raw, ""), named_genus), !coalesce(piece_qualified, FALSE)),
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
  bind_rows(manual |> mutate(across(c(pdf_page, printed_page), as.integer))) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(
    match(taxon_rank, c("family", "genus", "species", "subspecies", "variety")),
    taxon_name,
    pdf_page
  ) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")

# Taxa printed only inside keys (not segmented, not scored)
key_only <- read_csv(file.path(raw_dir, "key_only_taxa.csv"), col_types = cols(.default = "c"))
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
      block %in% single_infra ~
        "Only infraspecific taxon in the region; its description is the local description of the species (TFSS guide).",
      rank == "species" & !has_description ~
        "Species heading only; the description belongs to its one infraspecific taxon in the region.",
      rank %in% c("subspecies", "variety") ~
        "Annotation in a key to varieties or subspecies; kept as unmapped rows (as in Kochummen_1995); species values not inherited. Kochummen_2000 question 3.",
      block == "Ficus cumingii" ~
        "One variety in the region (var. terminalifolia), printed as a short note after the species description; the note is kept unmapped.",
      TRUE ~ ""
    )
  ) |>
  bind_rows(key_only |> mutate(n_mapped_rows = 0L))
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
