# Prance_1995: extract traits from the Tree Flora of Sabah and Sarawak
# Chrysobalanaceae treatment (Ghillean T. Prance 1995, TFSS vol. 1: 155--179).
# Input is the PDF text layer (born-digital 2007 electronic edition),
# segmented in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Prance_1995/raw/trait_data.csv      mapped and unmapped records
#   data/Prance_1995/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Prance_1995/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with (or ends with: "cordate
# at base"), else the organ of its piece or sentence. Scorers run per organ;
# every chunk that scores nothing stays as an unmapped row. Later sections
# (Distribution, Ecology, Uses, Taxonomy, vernacular names) and the subspecies
# annotations are kept one paragraph per row, unmapped. Keys and figures are
# not read. Names are as printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/Prance_1995/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Prance_1995"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# Chrysobalanaceae terms missing from the shared dictionary; first, so they
# win over shorter shared terms
local_terms <- list(
  # "with stomatal crypts" (Parinari); provisional, question 2
  leaf_stomatal_position = c("stomatal crypts" = "encrypted"),
  # not in the shared dictionary: "black" there would claim resin and
  # blackish bark in other datasets
  bark_colour = c("(?<![a-z])(black|blackish)\\b" = "black")
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
    # "Flowers (seen in fruiting specimens): calyx-lobes 5, ..."
    str_replace_all("\\):\\s", "); ") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|to\\s|up to\\s)?", num,
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

# "almost glabrous" is a near-state, not glabrous
hedged <- \(x) str_detect(x, paste0(flora_hedge_re, "|\\b(almost|nearly)\\b"))
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentell|hairy|hairs|hirsut|pilose|villous|",
  "sericeous|velutinous|woolly|lanate|puberul|setose|strigose|arachnoid"
)
# a hair statement that describes a change with age or young parts only
young_re <- "glabrescent|young|becoming|soon|caducous|when dry"

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide)
sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^(small|medium-sized|large|small to large|small or large|medium-sized to large)?\\s?(trees?|shrubs?)\\b",
  "bark", "^bark",
  "inner bark", "^inner bark",
  "sapwood", "^(sapwood|wood)\\b",
  "twigs", "^(young )?branches",
  "stipules", "^stipules?",
  "leaves", "^leaves|^leaf\\b",
  "inflorescences", "^inflorescences?",
  "flowers", "^flowers?\\b",
  "gynoecium", "^gynoecium",
  "fruits", "^fruits?\\b",
  "seeds", "^(seeds?|cotyledons?)\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "buttresses", "^(the )?trunk\\b",
  "inner bark", "^inner bark",
  "heartwood", "^heartwood",
  "bracts", "^(bracts?|bracteoles?)\\b",
  "receptacle", "^(receptacles?|throat|disc)\\b|^flowers with (a )?([a-z]+ )?receptacles?\\b",
  "pedicels", "^pedicels?\\b",
  "calyx", "^calyx",
  "petals", "^petals?\\b",
  "filaments", "^(the )?filaments?\\b",
  "stamens", "^(fertile )?stamens?\\b",
  "anthers", "^anthers?\\b",
  "staminodes", "^staminodes?\\b",
  "style", "^style\\b",
  "stigma", "^(the )?stigma\\b",
  "ovary", "^ovary\\b",
  "carpel", "^carpel\\b",
  "ovules", "^ovules?\\b",
  "epicarp", "^(epicarp|exocarp)\\b",
  "mesocarp", "^mesocarp\\b",
  "endocarp", "^endocarp\\b",
  "cotyledons", "^cotyledons?\\b",
  "germination", "^germination\\b",
  "eophylls", "^(first 2 )?eophylls|^cataphylls",
  "rachis", "^(the )?rachis\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^lateral veins?\\b",
  "intercostal venation", "^(intercostal|tertiary) (venation|veins)|^(the )?venation",
  "petiole", "^petioles?\\b",
  "acumen", "^(the )?(acumen|tip)\\b",
  "base", "^(at )?base\\b|\\bat base$",
  "apex", "\\bat apex$|^apex\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but) "))
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
      if (is.na(p_org)) {
        p_org <- if (pi > 1 && identical(s_org, "leaves")) "lamina" else s_org
      }
      chunks <- flora_split_pieces(pieces[pi], ",")
      # "rounded, subcordate or subcuneate at base": unlabelled alternatives
      # before an "at base" / "at apex" chunk belong to that label
      tail_org <- rep(NA_character_, length(chunks))
      pending <- NA_character_
      for (ci in rev(seq_along(chunks))) {
        lc <- str_to_lower(chunks[ci])
        if (str_detect(lc, "at (base|apex)$")) {
          pending <- str_extract(lc, "(base|apex)$")
        } else if (!is.na(match_organ(chunks[ci], part_organs)) || str_detect(lc, "\\d")) {
          pending <- NA_character_
        } else {
          tail_org[ci] <- pending
        }
      }
      cur <- p_org
      cur_text <- chunks[1]
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        # "3 mm broad at base" of a stipule is not the leaf base
        if (c_org %in% c("base", "apex") && !identical(s_org, "leaves")) c_org <- NA_character_
        named <- !is.na(c_org) || ci == 1
        # "cordate at base" names only its own chunk
        own_only <- !is.na(c_org) && c_org %in% c("base", "apex") &&
          str_detect(str_to_lower(chunks[ci]), "at (base|apex)$")
        if (!is.na(c_org) && !own_only) {
          cur <<- c_org
          cur_text <<- chunks[ci]
        }
        if (!is.na(tail_org[ci]) && identical(s_org, "leaves")) {
          own_only <- TRUE
          c_org <- tail_org[ci]
        }
        tibble(
          sentence = si,
          piece = pi,
          chunk = ci,
          sent_organ = s_org,
          sent_text = s,
          piece_organ = p_org,
          organ = if (own_only) c_org else cur,
          named = named,
          organ_text = if (own_only) chunks[ci] else cur_text,
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
  "^((sparsely|densely|minutely|finely|shortly|thinly|very|rather|lightly|",
  "appressed|grey|brown|ferrugineous|yellow-brown|or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z-]*$"
)

# colour words of a petal clause; markings and tinges are not the colour
colour_core <- \(x) str_remove_all(x, "-tinged [a-z]+|\\btinged [a-z]+")

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
      if (cl$chunk == 1) {
        pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(up to|to|c\\.)\\s*$")
        add(cat_rows("plant_growth_form", pre))
        # "Tree to 20 m, usually smaller": no "tall", but the habit's height
        h <- grab(x, "(tall|high)?\\b")
        # the qualifier of "often with short buttresses" is not the height's
        if (!is.na(h)) add(num_rows("plant_height", h, cq_text = str_remove(x, "\\s(often |usually )?with .*$")))
      }
      d <- grab(x, "(in )?diam(eter|\\.)?")
      if (!is.na(d)) {
        dr <- num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*"), cq_text = x)
        # "Tree to 35 m tall, 2 m diameter": the height's "to" also bounds it
        if (!is.null(dr) && !str_detect(lx, "^c\\.") &&
          str_detect(str_to_lower(cl$prev %||% ""), "\\bto \\d[^,;]*\\bm (tall|high)\\b")) {
          dr <- dr |> mutate(value_type = if_else(value_type == "mean", "maximum", value_type))
        }
        add(dr)
      }
      if (str_detect(lx, "\\bbuttress")) {
        add(row("stem_base_form", "buttressed", cq = qualifier(lx), raw = x))
      }
    },
    # "the trunk often with thick buttresses", "trunk buttressed up to 2 m"
    buttresses = if (str_detect(lx, "\\bbuttress")) {
      add(row("stem_base_form", "buttressed", cq = qualifier(lx), raw = x))
    },
    bark = if (so == "bark") {
      # mottling, lichen patches and lenticels are not the bark colour
      if (!str_detect(lx, "mottled|patch|spot|lenticel|lichen|excrescence")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = {
      core <- str_squish(str_remove(lx, "^(young )?branches "))
      if (str_detect(core, hairs_only) &&
        !str_detect(str_to_lower(cl$piece_text), young_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_phyllotaxis", lx))
      add(cat_rows("leaf_margin", lx))
      if (str_detect(lx, "^leaves simple$")) add(row("leaf_compoundness", "simple", raw = x))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d|glabr|crypt|gland|surface|above|beneath|coriaceous|chartaceous")) {
        add(cat_rows("leaf_shape", lx))
      }
      if (str_detect(lx, "crypt") && !str_detect(lx, "\\b(without|no)\\b")) {
        add(cat_rows("leaf_stomatal_position", lx) |>
          mutate(notes = "Stomatal crypts scored as leaf_stomatal_position 'encrypted' (provisional, question 2)."))
      }
      if (str_detect(lx, "\\b(shiny|shining|glossy)\\b") && !str_detect(lx, "when dry")) {
        add(cat_rows("leaf_surface_reflectivity", lx))
      }
    },
    lamina = if (so == "leaves") {
      if (str_detect(lx, "crypt") && !str_detect(lx, "\\b(without|no)\\b")) {
        add(cat_rows("leaf_stomatal_position", lx) |>
          mutate(notes = "Stomatal crypts scored as leaf_stomatal_position 'encrypted' (provisional, question 2)."))
      }
      if (str_detect(lx, "\\b(shiny|shining|glossy)\\b") && !str_detect(lx, "when dry")) {
        add(cat_rows("leaf_surface_reflectivity", lx))
      }
    },
    base = if (so == "leaves") add(cat_rows("leaf_base_shape", str_remove(lx, "\\s?at base$"))),
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", str_remove(lx, "\\s?at apex$"))),
    petiole = {
      l <- grab(x, "(long)?\\b")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    inflorescences = {
      if (cl$chunk == 1 && cl$piece == 1) {
        # the type statement runs over commas ("racemose, paniculate or
        # cymose") up to the first size, indumentum or other organ
        ch <- flora_split_pieces(str_to_lower(cl$piece_text), ",")
        stop_at <- which(str_detect(ch, paste0("\\d|rachis|borne|", hair_re)))
        it <- paste(head(ch, if (length(stop_at)) stop_at[1] - 1 else length(ch)), collapse = ", ")
        if (it == "") it <- lx
        # position and type are scored apart, so "axillary or rarely terminal
        # racemes" qualifies the position only
        type_words <- "\\b(racem[a-z]*|panic[a-z]*|cym[a-z]*|corymb[a-z]*|spikes?|umbels?)\\b"
        add(cat_rows("inflorescence_type", str_squish(str_remove_all(it, paste0("\\b(sub)?terminal\\b|\\baxillary\\b|", flora_qual_re)))))
        add(cat_rows("inflorescence_type", str_squish(str_remove_all(it, type_words))))
      }
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("inflorescence_length", l, cq_text = x))
    },
    bracts = if (str_detect(so, "inflorescences")) {
      l <- grab(x, "long")
      if (!is.na(l)) {
        add(num_rows("flower_bract_length", l, em = "bract and bracteole", cq_text = x))
      }
    },
    flowers = {
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("flower_perianth_symmetry", lx))
      if (str_detect(lx, "polygamo-?dioecious") && !str_detect(x, gen_re)) {
        add(row("sex_type", "polygamo-dioecious", "polygamodioecious", raw = x))
      }
      if (cl$chunk == 1 || !cl$named) {
        l <- grab(x, "long")
        if (!is.na(l) && !str_detect(str_to_lower(cl$piece_text), "receptacle")) {
          add(num_rows("flower_length", l, cq_text = x))
        }
      }
    },
    calyx = if (so == "flowers") {
      l <- grab(x, "(long\\b|$)")
      if (!is.na(l) && !str_detect(lx, "\\bmm\\s(thick|wide)")) {
        add(num_rows("flower_sepal_length", l, notes = "Calyx-lobe length.", cq_text = x))
      }
    },
    petals = if (so == "flowers") {
      l <- grab(x, "(long)?\\b")
      if (!is.na(l) && !str_detect(lx, "up to|largest|others")) {
        add(num_rows("flower_petal_length", l, cq_text = x))
      }
      if (!str_detect(lx, "\\d")) {
        cc <- colour_core(lx)
        r <- cat_rows("flower_colour", cc)
        if (!is.null(r) && cc != lx) {
          r <- r |> mutate(notes = "Petal colour; the tinge is not scored.")
        }
        add(r |> (\(d) if (is.null(d)) NULL else mutate(d, entity_measured = "petal"))())
      }
    },
    stamens = if (so == "flowers") {
      m <- str_match(lx, paste0("^(?:fertile )?stamens? (?:c\\. )?(", cnt, ")(?![\\d.])(?!\\s?(mm|cm))"))
      if (!is.na(m[1, 1]) && !str_detect(lx, "\\bfertile and\\b")) {
        add(count_rows("flower_fertile_stamens_count", x, m[1, 2]))
      }
      m2 <- str_match(lx, "^stamens? (\\d+) fertile")
      if (!is.na(m2[1, 1])) add(count_rows("flower_fertile_stamens_count", x, m2[1, 2]))
      # "stamens 13–20, pale blue, 8–15 mm long": the length chunk
      if (!cl$named) {
        l <- grab(x, "long")
        if (!is.na(l) && str_detect(lx, "^\\d")) add(num_rows("flower_stamen_length", l, cq_text = x))
      }
    },
    filaments = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, cq_text = x))
    },
    style = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_style_length", l, cq_text = x))
    },
    carpel = ,
    ovary = {
      m <- str_match(lx, "\\bwith (\\d) ovules\\b")
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_ovules_per_functional_carpel_count", x, m[1, 2]))
      }
    },
    fruits = {
      if (str_detect(lx, "^fruits? an? ")) {
        add(cat_rows("fruit_type", lx))
        add(cat_rows("fruit_fleshiness", lx))
      }
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l) && (cl$chunk == 1 || !cl$named)) add(num_rows("fruit_length", l, cq_text = x))
      }
    },
    epicarp = if (so == "fruits") {
      core <- str_squish(str_remove(lx, "^(epicarp|exocarp)\\s?"))
      if (str_detect(core, hairs_only) &&
        !str_detect(str_to_lower(cl$piece_text), "glabrescent|becoming|soon")) {
        add(cat_rows("fruit_surface_hairs", str_remove(core, "\\s?on exterior.*")))
      }
    }
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = x)
}

# Leaf indumentum per surface over the leaf-blade chunks of the sentence
# ("glabrous above, densely pubescent beneath"): hairy when a surface is
# hairy, glabrous when both are; statements about young leaves are left out;
# nothing when a surface is glabrescent (a change with age) or hedged.
leaf_hairs <- function(ch) {
  ch <- str_to_lower(str_remove(ch, "^(?i)leaves "))
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  h <- h[!str_detect(h, "when (very )?young")]
  if (!length(h) || any(str_detect(h, paste0("glabrescent|", flora_qual_re))) || any(hedged(h))) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b(?!.*(beneath|below))")
  if (any(hairy)) {
    return("hairy")
  }
  both <- any(str_detect(h, "both (sides|surfaces)|^glabrous( when mature)?$")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) "glabrous" else NULL
}

character_of <- function(x) {
  lx <- str_to_lower(x)
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "crypt") ~ "stomatal crypts",
    str_detect(lx, "gland") ~ "glands",
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|bony|fleshy|fibrous|hard|stiff|foliaceous") ~ "texture",
    str_detect(lx, "caducous|persistent|deciduous") ~ "persistence",
    str_detect(lx, "vein|venation|reticulat|midrib") ~ "venation",
    str_detect(lx, "lenticel") ~ "lenticels",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "\\b(white|yellow|green|brown|red|black|grey|purple|orange|pink|blue|ochre|cream)") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "more rarely", "less often")

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
    if (!is.null(r) && !cl$named[i] &&
      !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  # leaf indumentum per surface, over the blade chunks of the leaf sentence
  cl$hair_done <- FALSE
  for (s in unique(cl$sentence[cl$sent_organ %in% "leaves"])) {
    blade <- cl$id[cl$sentence == s & cl$organ %in% c("leaves", "lamina")]
    hl <- leaf_hairs(cl$text[blade])
    if (!is.null(hl)) {
      hair_ids <- blade[str_detect(str_to_lower(cl$text[blade]), paste0("glabr|", hair_re)) &
        !str_detect(str_to_lower(cl$text[blade]), "young")]
      i <- hair_ids[1]
      cl$rows[[i]] <- bind_rows(
        cl$rows[[i]],
        row("leaf_hairs_adult_leaves", hl, raw = paste(cl$text[hair_ids], collapse = ", "))
      )
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
    # "rounded, subcordate or subcuneate": the unqualified alternative is `usually`
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
        case_when(
          organ %in% c("lamina", "base", "apex", "acumen") & sent_organ %in% "leaves" ~
            paste("leaf", str_replace(organ, "lamina", "blade")),
          organ == "buttresses" ~ "trunk",
          TRUE ~ coalesce(organ, "unknown organ")
        ),
        " | ",
        character_of(text),
        if_else(hedged(str_to_lower(text)), " | hedged", "")
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

later_secs <- c(
  "Distribution", "Ecology", "Uses", "Vernacular names", "Taxonomy",
  "Infraspecific annotation"
)
# kept whole: place names ("Mt. Tawai", "Bt. Raya", "4th Div.") defeat the
# sentence splitter
later <- paras |>
  filter(section %in% later_secs) |>
  transmute(block, rank, section, pdf_page, printed_page, sentence = str_squish(text)) |>
  filter(sentence != "")

later_unmapped <- later |>
  transmute(
    block,
    rank,
    trait = NA_character_,
    trait_value_raw = sentence,
    notes = paste0(
      "UNMAPPED: ",
      case_when(
        section == "Infraspecific annotation" ~
          "infraspecific annotation | synonyms, range, habitat",
        TRUE ~ paste(str_to_lower(section), "| text")
      ),
      if_else(hedged(str_to_lower(sentence)), " | hedged", "")
    ),
    source_section = section,
    pdf_page,
    printed_page
  )

# ---- higher ranks, assembly -------------------------------------------------------

taxa <- paras |>
  filter(rank %in% c("species", "subspecies")) |>
  distinct(block, rank, status) |>
  mutate(genus = word(block, 1), has_description = block %in% desc_paras$block)
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species = block, rank)

# Family statements tied to a named genus ("(in Parastemon, plant sometimes
# polygamo-dioecious)") hold for that genus only.
named_genus <- paste0("\\b(", gen_re, ")\\b")
copied <- flora_copy_down(
  higher_rows = own |> filter(!str_detect(coalesce(trait_value_raw, ""), named_genus)),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
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
      rank == "subspecies" ~
        "Subspecies annotation under a species description (key lead, nomenclature, range, habitat); no description; species values not inherited.",
      block %in% c("Kostermanthus heteropetalus", "Licania splendens", "Maranthes corymbosa") ~
        "Only species of the genus in the region; heading unnumbered.",
      block == "Parastemon grandifructus" ~
        "Flowers described from fruiting specimens only.",
      block == "Parinari metallica" ~ "Fruits not seen.",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
