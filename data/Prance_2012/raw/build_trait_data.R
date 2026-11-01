# Prance_2012: extract traits from the Flora of Peninsular Malaysia
# Chrysobalanaceae treatment (G.T. Prance 2012, FPM Series II vol. 3: 13--46).
# Input is the PDF text layer (born-digital), segmented in
# raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Prance_2012/raw/trait_data.csv      mapped and unmapped records
#   data/Prance_2012/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Prance_2012/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with, else the organ of its
# piece or sentence. Scorers run per organ; every chunk that scores nothing
# stays as an unmapped row. Parenthetical asides that are not numbers
# ("(elsewhere ...)", "(except in P. congolana ...)") are taken out before
# scoring and kept as unmapped rows. Later sections (Distribution, Ecology,
# Conservation status, Uses, Taxonomy, vernacular names) are kept one
# sentence per row; flower visitors, scent and dispersers cited in species
# Ecology paragraphs are in raw/manual_rows.csv. Keys and figures are not
# read. Names are as printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/Prance_2012/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Prance_2012"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# Chrysobalanaceae terms missing from the shared dictionary; first, so they
# win over shorter shared terms
local_terms <- list(
  # "raceme-like reduced ... panicles" (P. elmeri) is not a raceme
  inflorescence_type = c("raceme-like" = ""),
  # "base round to subcuneate" (P. costata)
  leaf_base_shape = c("\\bround\\b" = "rounded")
)
for (t in names(local_terms)) {
  dict[[t]] <- c(local_terms[[t]], dict[[t]])
}

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

aside_re <- "\\s?\\((?![–\\d]|c\\.)[^)]*\\)"

prep <- function(x) {
  x |>
    # "Fruit a fleshy drupe (rarely dry)": the aside is an alternative state
    str_replace_all("\\((rarely|sometimes|occasionally) ([a-z]+)\\)", ", \\1 \\2") |>
    # "Fruits (immature) ellipsoid": not the mature fruit
    str_replace_all("\\bFruits \\(immature\\)", "Immature fruits") |>
    # "lamina elliptic, ... (index 1.7--2.7)": length:width index
    str_replace_all("\\s?\\(index ([\\d.–]+)\\)", ", index \\1") |>
    # other non-numeric asides are scored nowhere (see asides_of())
    str_remove_all(aside_re) |>
    # "Flowers with receptacle tube ..." / "Leaves: petioles ..."
    str_replace_all("\\bFlowers with receptacle", "Flowers; receptacle") |>
    str_replace_all("(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ") |>
    # "Big tree ... but sometimes a shrub": the shrub is the rarer form
    str_replace_all(" but (sometimes|rarely|occasionally) ", ", \\1 ") |>
    # "trees or, more rarely, shrubs"
    str_replace_all("\\bor, (more rarely|rarely|sometimes), ", "or \\1 ") |>
    # "axillary and rarely terminal racemes": both positions name the racemes
    str_replace_all(
      "\\b(axillary|terminal) and (rarely|sometimes|occasionally) (terminal|axillary) ([a-z]+s)\\b",
      "\\1 \\4 and \\2 \\3 \\4"
    ) |>
    # "bluntly acuminate at apex, cuneate to rounded at base"
    str_replace_all(
      "(?<=[,;] )((?:bluntly |shortly |broadly )?(?!except)[a-z]+(?: to [a-z]+)?) at (apex|base)(?=[,;.]|$)",
      "\\2 \\1"
    ) |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

# Non-numeric asides of a paragraph, for unmapped rows
# ("carpel(s)" is a plural, not an aside)
asides_of <- function(x) {
  a <- str_squish(str_extract_all(x, aside_re)[[1]])
  a[!str_detect(a, "^\\([a-z]{1,2}\\)$")]
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
# a length printed without "long" ("petioles 2--5 mm", "mesocarp 2--2.5 mm")
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
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsut|pilose|",
  "villous|sericeous|velutinous|woolly|puberul|setose|strigose|lanate|arachnoid"
)

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the FPM guide)
sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^((small|medium|medium-sized|big|large|or|to|and) )*(trees?|shrubs?|subshrubs?)\\b",
  "bark", "^bark",
  "twigs", "^(young )?twigs?",
  "stipules", "^stipules?",
  "leaves", "^leaves|^leaf\\b",
  "inflorescences", "^inflorescences?",
  "flowers", "^flowers?\\b",
  "immature fruits", "^immature fruits?",
  "fruits", "^fruits?\\b",
  "seeds", "^seeds?\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "bole", "^bole\\b",
  "buttresses", "^((often|sometimes|occasionally|older trees) )?(with )?((low|small|short|steep|thick|plank|fluted|thin) )*buttress",
  "crown", "^crown\\b",
  "inner bark", "^inner bark",
  "bark", "^(outer )?bark",
  "sapwood", "^sapwood",
  "shoots", "^ultimate shoots",
  "petiole", "^petioles?\\b",
  "lamina", "^(lamina|lower surface of lamina)\\b",
  "lower surface", "^(the )?lower surface\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^lateral (leaf )?veins?\\b",
  "intercostal veins", "^(intercostal|secondary|tertiary) (venation|veins)|^venation\\b",
  "rachis", "^(the )?rachis\\b",
  "bracts", "^(bracts?|bracteoles?)\\b",
  "pedicels", "^(upper )?pedicels?\\b",
  "receptacle", "^receptacle\\b",
  "disc", "^disc\\b",
  "throat", "^throat\\b",
  "sepals", "^sepals?\\b",
  "petals", "^petals?\\b",
  "fertile stamens", "^fertile stamens\\b",
  "stamens", "^stamens?\\b",
  "filaments", "^filaments?\\b",
  "anthers", "^anthers?\\b",
  "staminodes", "^staminodes?\\b",
  "gynoecium", "^gynoecium\\b",
  "ovary", "^ovar(y|ies)\\b",
  "carpel", "^carpels?\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "ovules", "^ovules?\\b",
  "epicarp", "^epicarp\\b",
  "mesocarp", "^mesocarp\\b",
  "endocarp", "^endocarp\\b",
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
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        # "base cuneate, shiny above, glabrous beneath": surfaces are the lamina's
        if (is.na(c_org) && identical(s_org, "leaves") && cur %in% c("base", "apex") &&
          str_detect(chunks[ci], "\\b(above|beneath|below|both surfaces)\\b")) {
          c_org <- "lamina"
        }
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
  "loosely|softly|faintly|slightly|lightly|appressed|adpressed|",
  "golden|silvery|brownish|brown|greyish|grey|whitish|yellowish|rusty|reddish|",
  "or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z-]*$"
)
age_re <- "glabrescent|when young|young|initially|becoming|soon|when mature|with age"
# colour markings and tinges are not the ground colour (standing rule)
marking_re <- "tinge|tinged|suffused|flushed|patch|spot|blotch|stripe|wart"
flower_colour_of <- function(x) {
  x |>
    str_remove("^(petals?|flowers?) ") |>
    str_remove_all("\\b(pale|deep|dark|light|bright)\\b")
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
  in_flower <- so %in% c("flowers", "inflorescences")
  switch(
    organ,
    habit = if (so == "habit") {
      # growth form from the words only ("Trees to 35 m tall or shrubs")
      words <- str_remove_all(lx, paste0("(up )?to ", num, "\\s?m( tall| high)?"))
      add(cat_rows("plant_growth_form", words))
      h <- str_extract(x, paste0(rng, "\\s?m\\b(?!\\s?(diam|wide))"))
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
    },
    bole = ,
    buttresses = if (so == "habit") {
      d <- grab(x, "diam(eter|\\.)?")
      if (!is.na(d)) {
        dr <- num_rows(
          "plant_diameter_breast_height",
          str_remove(d, "\\s?diam.*"),
          notes = "Bole diameter scored as diameter at breast height.",
          cq_text = x
        )
        # "bole to 30 m tall and 1 m diameter": the "to" bounds both
        if (!is.null(dr) && str_detect(lx, "^bole (up )?to ")) {
          dr <- dr |> mutate(value_type = if_else(value_type == "mean", "maximum", value_type))
        }
        add(dr)
      }
      # "unbuttressed", "fluted or not", "unbuttressed or with short
      # buttresses" are not scored; a size statement after a scored one
      # ("often buttressed, buttresses to 1.3 m high") repeats it
      repeat_size <- str_detect(lx, "^buttresses") &&
        str_detect(str_to_lower(coalesce(cl$prev, "")), "buttress|fluted")
      if (str_detect(lx, "buttress|fluted") &&
        !str_detect(str_to_lower(cl$piece_text), "unbuttressed|or not") &&
        !repeat_size) {
        add(cat_rows("stem_base_form", lx))
      }
    },
    bark = if (so == "bark" && !str_detect(lx, marking_re) && !str_detect(lx, age_re)) {
      add(cat_rows("bark_colour", lx))
      add(cat_rows("bark_texture", lx))
    },
    twigs = {
      core <- str_remove(lx, "^(young )?twigs? ")
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), age_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_compoundness", lx))
      add(cat_rows("leaf_margin", lx))
      add(cat_rows("leaf_phyllotaxis", lx))
    },
    lamina = if (so == "leaves") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (str_detect(lx, "^index ")) {
        add(count_rows(
          "leaf_length_to_width_ratio", x, str_remove(lx, "^index "),
          notes = "Printed leaf index (length divided by width).",
          unit = "{dimensionless}"
        ))
      } else if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_shape", str_remove(lx, "^lamina ")))
        if (!str_detect(lx, "beneath|below")) add(cat_rows("leaf_surface_reflectivity", lx))
      }
    },
    base = if (so == "leaves") add(cat_rows("leaf_base_shape", lx)),
    margin = if (so == "leaves") add(cat_rows("leaf_margin", lx)),
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", lx)),
    petiole = {
      l <- grab_len(x)
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    inflorescences = if (so == "inflorescences") {
      add(cat_rows("inflorescence_type", lx))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("inflorescence_length", l, cq_text = x))
    },
    bracts = if (in_flower) {
      b <- str_extract(str_to_lower(cl$organ_text), "^(bracts and bracteoles|bracts|bracteoles|bract)")
      em <- case_when(
        b == "bracts and bracteoles" ~ "bract and bracteole",
        b == "bracteoles" ~ "bracteole",
        TRUE ~ "bract"
      )
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_bract_length", l, em = em, cq_text = x))
    },
    flowers = if (so == "flowers") {
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("sex_type", lx))
      add(cat_rows("flower_perianth_symmetry", lx))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_length", l, cq_text = x))
    },
    receptacle = if (in_flower) {
      tn <- "Receptacle tube (hypanthium) length scored as flower_tube_length (question 2)."
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_tube_length", NA), x, em = "receptacle tube") |>
          mutate(notes = paste(notes, tn)))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_tube_length", l, em = "receptacle tube", notes = tn, cq_text = x))
      }
    },
    sepals = if (in_flower) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_sepal_length", l, cq_text = x))
    },
    petals = if (in_flower) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_petal_length", l, cq_text = x))
      # "petals white tinged pink or deep red": tinge and alternative, not scored
      if (!str_detect(lx, marking_re) && !str_detect(lx, "\\d")) {
        add(cat_rows("flower_colour", flower_colour_of(lx)))
      }
    },
    `fertile stamens` = ,
    stamens = if (in_flower) {
      m <- str_match(lx, paste0("^(?:fertile )?stamens? (", cnt, ")(?![\\d.]|\\s?(mm|cm)| fertile)|^(", cnt, ")$"))
      m2 <- str_match(lx, "^stamens? (\\d+) fertile\\b")
      v <- coalesce(m[1, 2], m[1, 4], m2[1, 2])
      if (!is.na(v)) {
        add(count_rows("flower_fertile_stamens_count", x, v))
      }
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_stamen_length", l, cq_text = x))
    },
    filaments = if (in_flower) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, cq_text = x))
    },
    anthers = if (in_flower) {
      add(cat_rows("flower_anther_attachment", lx))
      add(cat_rows("flower_anther_dehiscence", lx))
      if (str_detect(lx, "^anthers (yellow|white|red|purple|pink)$")) {
        add(row("flower_anther_colour", str_remove(lx, "^anthers "), raw = x))
      }
    },
    gynoecium = ,
    carpel = ,
    ovary = if (in_flower) {
      m <- str_match(lx, paste0("(?:^|\\s)(", cnt, ")-carpell(?:ary|ate)\\b|\\bof (", cnt, ") carpels\\b"))
      v <- coalesce(m[1, 2], m[1, 3])
      if (!is.na(v)) add(count_rows("flower_structural_carpels_count", x, v))
    },
    style = if (in_flower) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, cq_text = x))
    },
    fruits = if (so == "fruits") {
      core <- str_squish(str_remove(lx, "^fruits?\\b( a)?"))
      add(cat_rows("fruit_type", lx))
      # "deeply ridged when dry" is the herbarium state, not a dry fruit
      if (!str_detect(lx, "when dry")) add(cat_rows("fruit_fleshiness", lx))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("fruit_length", l, cq_text = x))
        w <- grab(x, "(wide|broad)")
        if (!is.na(w)) add(num_rows("fruit_width", w, cq_text = x))
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?(in )?diam.*"), notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        }
      }
      # ripe colour: "pale green or cream-white, ripening pink" -> pink only
      ripe_only <- str_detect(str_to_lower(cl$piece_text), "ripening")
      if (!str_detect(lx, hair_re) && !str_detect(lx, marking_re) &&
        !str_detect(lx, "young|immature|unripe|when dry|\\d") &&
        (!ripe_only || str_detect(lx, "ripening"))) {
        add(cat_rows("fruit_colour", str_remove(core, "with a waxy bloom")))
      }
    },
    epicarp = if (so == "fruits") {
      core <- str_remove(lx, "^epicarp ")
      if (str_detect(core, hairs_only)) add(cat_rows("fruit_surface_hairs", core))
      add(cat_rows("fruit_fleshiness", lx))
      if (!str_detect(lx, paste0(hair_re, "|glabr|", marking_re))) add(cat_rows("fruit_colour", core))
    },
    mesocarp = if (so == "fruits") {
      add(cat_rows("fruit_fleshiness", lx))
      t <- grab_len(x)
      if (is.na(t)) t <- str_remove(grab(x, "thick"), "\\s?thick$")
      if (!is.na(t)) {
        add(num_rows(
          "fruit_wall_thickness", t,
          em = "mesocarp",
          notes = "Mesocarp thickness scored as fruit_wall_thickness (question 3).",
          cq_text = x
        ))
      }
    }
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = x)
}

# Leaf indumentum is stated per surface in the lamina piece ("glabrous above,
# densely lanate pubescent beneath"): hairy when a surface is hairy, glabrous
# when both are; nothing when a surface is glabrescent, young or only partly
# hairy ("glabrous beneath except for sparsely pilose venation").
leaf_hairs <- function(piece) {
  ch <- str_to_lower(flora_split_pieces(piece, ","))
  ch <- str_remove(ch, "^lamina ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  # alternatives and qualified statements ("glabrous, lanate or strigose",
  # "usually with ... hairs") stay unmapped
  if (!length(h) || any(str_detect(h, "glabrescent|young|except|\\bor\\b")) ||
    any(str_detect(h, flora_qual_re)) || hedged(piece)) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b")
  if (any(hairy)) {
    return(row("leaf_hairs_adult_leaves", "hairy", raw = piece))
  }
  both <- any(str_detect(h, "both (sides|surfaces)")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) row("leaf_hairs_adult_leaves", "glabrous", raw = piece) else NULL
}

character_of <- function(x) {
  lx <- str_to_lower(x)
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "gland") ~ "glands",
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membran|woody|bony|fibrous|fleshy|hard|leathery") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|papill|prominul|prominent|impressed") ~ "venation",
    str_detect(lx, "caducous|persistent") ~ "persistence",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "\\b(white|yellow|green|brown|red|black|grey|purple|orange|pink|blue|violet|lilac|magenta)") ~ "colour",
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
  cl <- cl |>
    group_by(sentence, piece) |>
    mutate(prev = lag(text)) |>
    ungroup()
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5--25 cm long")
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  # leaf indumentum per surface, on the first chunk of each lamina piece
  cl$hair_done <- FALSE
  lp <- cl |> filter(sent_organ %in% "leaves", piece_organ %in% "lamina", chunk == 1)
  for (i in lp$id) {
    h <- leaf_hairs(cl$piece_text[i])
    if (!is.null(h)) {
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h)
      hair_ids <- cl$id[cl$sentence == cl$sentence[i] & cl$piece == cl$piece[i] &
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
  # an organ label alone ("Leaves", "Flowers") is not a clause
  label_only <- str_detect(str_to_lower(cl$text), "^(leaves|flowers)$")
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
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
      notes = paste0(
        "UNMAPPED: ",
        if_else(
          organ %in% c("lamina", "base", "margin", "apex", "lower surface") & sent_organ %in% "leaves",
          paste("leaf", str_replace(organ, "lamina", "blade")),
          coalesce(organ, "unknown organ")
        ),
        " | ",
        character_of(text),
        if_else(hedged(text), " | hedged", "")
      ),
      pdf_page,
      printed_page
    )
  aside_rows <- tibble(
    trait = NA_character_,
    trait_value_raw = asides[!str_detect(asides, "^\\((rarely|sometimes|occasionally) [a-z]+\\)$|^\\(immature\\)$|^\\(index ")],
    notes = paste0(
      "UNMAPPED: aside | ",
      if_else(str_detect(trait_value_raw, "elsewhere"), "outside the flora region", "description")
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
    notes = paste0("UNMAPPED: ", str_to_lower(section), " | text"),
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
  filter(rank %in% c("species", "subspecies")) |>
  distinct(block, rank, status) |>
  mutate(
    species = word(block, 1, 2),
    genus = word(block, 1),
    has_description = block %in% desc_paras$block
  )
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

# Family statements that name genera ("Apart from Parastemon ...") are not in
# descriptions; copy-down uses descriptions only.
copied <- flora_copy_down(
  higher_rows = own,
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
inherited <- flora_inherit_infraspecific(
  own_rows = own |> filter(!is.na(trait), !rank %in% c("genus", "family")),
  members = members
) |>
  select(-rank)

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
    # an FPM species description describes the taxon in Peninsular Malaysia;
    # family and genus statements copied down carry no region
    population_region = if_else(
      taxon_rank %in% c("species", "subspecies") & !is.na(trait) &
        coalesce(rank_scored_from, "species") == "species",
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
    source_id = NA_character_
  ) |>
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
      block %in% c("Atuna latifrons", "Kostermanthus malayanus") ~
        "Known only from the type collection; fruits unknown.",
      block == "Atuna penangiana" ~ "Fruits described from immature material; fruit size not scored.",
      rank == "subspecies" & species == "Parinari costata" ~
        "Subspecies description gives differences only; other traits inherited from the species description.",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
