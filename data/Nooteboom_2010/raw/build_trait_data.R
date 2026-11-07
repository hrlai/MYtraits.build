# Nooteboom_2010: extract traits from the Flora of Peninsular Malaysia
# Symplocaceae treatment (H.P. Nooteboom 2010, FPM Series II vol. 1:
# 219--264). Input is the image-checked transcription of the scanned PDF,
# segmented in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Nooteboom_2010/raw/trait_data.csv      mapped and unmapped records
#   data/Nooteboom_2010/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Nooteboom_2010/raw/taxon_flags.csv     headings processed, rank, flags
#
# Clause parser after Prance_2012. Each description is split into sentences
# (one organ each), ";" pieces and "," chunks. A chunk takes the organ it
# starts with, else the organ of its piece or sentence. Every chunk that
# scores nothing stays as an unmapped row; non-numeric asides are taken out
# before scoring and kept as unmapped rows. Later sections are kept one
# sentence per row; values cited there are in raw/manual_rows.csv. Keys and
# figures are not read. Names are as printed.
suppressPackageStartupMessages(library(tidyverse))
source("data/Nooteboom_2010/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Nooteboom_2010"
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

# ---- text preparation -----------------------------------------------------

aside_re <- "\\s?\\((?![–\\d]|c\\.)[^)]*\\)"

prep <- function(x) {
  x |>
    # genus habit: "evergreen (exceptionally deciduous) shrubs to (rarely)
    # huge trees"; "exceptionally" governs "deciduous" only
    str_replace_all(
      "Small, evergreen \\(exceptionally deciduous\\) shrubs to \\(rarely\\) huge trees",
      "Small, evergreen, exceptionally deciduous; shrubs to huge trees"
    ) |>
    # "glabrous (rarely hairy), often ...": an aside closing a clause is an
    # alternative state; "cordate (sometimes obliquely) or rounded" is not
    str_replace_all(
      "\\((rarely|sometimes|occasionally|exceptionally) ([a-z]+(?<!ly))\\)(?=[,;.]|$)",
      ", \\1 \\2"
    ) |>
    str_remove_all(aside_re) |>
    # "Leaves: petiole ..." / "Flowers: hypanthium ..."
    str_replace_all("(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ") |>
    # "Flowers pedicel articulate" (S. theifolia) lacks the colon
    str_replace_all("\\bFlowers pedicel\\b", "Flowers; pedicel") |>
    # "Inflorescence axis 3--15 cm long" (S. cochinchinensis varieties)
    str_replace_all("\\bInflorescence axis\\b", "Inflorescence; axis") |>
    # "a short, often clustered spike": "often" governs "clustered" only
    str_replace_all("a short, often clustered spike", "a short spike, often clustered") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

asides_of <- function(x) {
  a <- str_squish(str_extract_all(x, aside_re)[[1]])
  a[!str_detect(a, "^\\([a-z]{1,2}\\)$")]
}

aside_organ <- function(x, a) {
  before <- str_sub(x, 1, str_locate(x, fixed(a))[1, 1] - 1)
  if (!nzchar(str_squish(before))) {
    return("habit")
  }
  cl <- last(split_clauses(prep(before)))
  leaf_part(coalesce(cl$organ, cl$piece_organ, cl$sent_organ, "habit"), cl$sent_organ)
}

leaf_part <- function(organ, sent_organ) {
  if_else(
    organ %in% c("lamina", "base", "margin", "apex", "acumen") &
      sent_organ %in% "leaves",
    paste("leaf", str_replace(organ, "lamina", "blade")),
    organ
  )
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
# a length printed without "long" ("corolla c. 5 mm", "lobes 0.5–2 mm")
grab_len <- function(x) {
  str_extract(
    x,
    paste0(rng, "\\s?", unit_re, "(?!\\s?(wide|broad|thick|diam|across|high|tall))")
  )
}
grab_dims <- function(x) {
  m <- str_match(x, paste0("(", rng, ")\\s?×\\s?(", rng, ")\\s?", unit_re))
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(raw = m[1, 1], parts = m[1, 2:3], unit = m[1, 4])
}

hedged <- \(x) str_detect(x, flora_hedge_re)
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsut|pilose|",
  "villous|sericeous|velutinous|velvety|woolly|puberul|pulverulent|setose|",
  "strigose|lanate|silky|ciliate|ciliolate|indument"
)

# ---- organs ---------------------------------------------------------------

sent_organs <- tribble(
  ~organ, ~re,
  "habit", "^((small|medium|medium-sized|big|large|huge|bushy|or|to|and|evergreen) )*(trees?|shrubs?|treelets?|shrubs/trees)\\b",
  "twigs", "^(young )?twigs?",
  "leaves", "^leaves|^leaf\\b",
  "inflorescences", "^inflorescences?",
  "flowers", "^flowers?\\b",
  "fruits", "^fruits?\\b",
  "seeds", "^seeds?\\b"
)
part_organs <- tribble(
  ~organ, ~re,
  "bole", "^bole\\b",
  "buttresses", "^((often|sometimes|occasionally) )?(with )?((low|small|short) )*buttress",
  "terminal buds", "^(and )?terminal buds?\\b",
  "growth", "^growth\\b",
  "petiole", "^petioles?\\b",
  "lamina", "^laminas?\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  "tip", "^tip\\b",
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^lateral (leaf )?veins?\\b",
  "intramarginal veins", "^intra-?marginal veins?\\b",
  "intercostal veins", "^intercostal veins?\\b|^reticulum\\b",
  "axis", "^axis\\b",
  "bracts", "^(bracts?|bracteoles?)\\b",
  "pedicels", "^pedicels?\\b",
  "hypanthium", "^hypanthium\\b",
  "calyx", "^calyx\\b",
  "lobes", "^lobes\\b",
  "tube", "^tube\\b",
  "limb", "^limb\\b",
  "corolla", "^corolla\\b",
  "petals", "^petals?\\b",
  "disc", "^disc\\b",
  "stamens", "^stamens?\\b",
  "anthers", "^anthers?\\b",
  "ovary", "^ovar(y|ies)\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "mesocarp", "^mesocarp\\b",
  "endocarp", "^endocarp\\b",
  "stones", "^stones?\\b",
  "locules", "^locules?\\b",
  "embryo", "^embryo\\b",
  "endosperm", "^endosperm\\b",
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
    # the first sentence is the habit ("Small, evergreen ... shrubs")
    if (si == 1 && is.na(s_org)) s_org <- "habit"
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) p_org <- s_org
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      cur_text <- chunks[1]
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        # "base cuneate, glabrous beneath": surfaces are the lamina's
        if (is.na(c_org) && identical(s_org, "leaves") &&
          cur %in% c("base", "apex", "margin") &&
          str_detect(chunks[ci], "\\b(above|beneath|below|both surfaces)\\b")) {
          c_org <- "lamina"
        }
        named <- !is.na(c_org) || ci == 1
        if (!is.na(c_org)) {
          cur <<- c_org
          cur_text <<- chunks[ci]
        }
        tibble(
          sentence = si, piece = pi, chunk = ci,
          sent_organ = s_org, sent_text = s, piece_organ = p_org,
          organ = cur, named = named, organ_text = cur_text,
          piece_text = pieces[pi], text = chunks[ci]
        )
      })
    })
  })
}

# ---- row helpers ----------------------------------------------------------

row <- flora_row
num_rows <- flora_value_rows
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
cat_rows <- function(trait, x, em = NA_character_) {
  flora_cat_rows(trait, x, dict, em)
}

# ---- scorers --------------------------------------------------------------

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|fine|shortly|short|thinly|very|rather|",
  "evenly|loosely|softly|faintly|slightly|appressed|adpressed|patently|",
  "spreading|long|thin|obliquely|golden|silvery|brownish|brown|dark|red|",
  "rarely|sometimes|often|usually|",
  "greyish|grey|whitish|yellowish|rusty|reddish|or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z-]*$"
)
age_re <- "glabrescent|when young|young|initially|becoming|soon|when mature|with age"
marking_re <- "tinge|tinged|suffused|flushed|patch|spot|blotch|stripe"
calyx_note <- "Calyx lobe length scored as flower_sepal_length (question 2)."
corolla_note <- paste(
  "Corolla length scored as flower_length with entity_measured corolla;",
  "the flower itself is not measured (user rule, 2026-10-30)."
)

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
  dbh <- function(d, cq_text) {
    dr <- num_rows(
      "plant_diameter_breast_height",
      str_remove(d, "\\s?diam.*"),
      notes = "Bole or stem diameter scored as diameter at breast height.",
      cq_text = cq_text
    )
    # "to 10 m tall and c. 12 cm diameter": the "to" bounds both
    if (!is.null(dr) && str_detect(str_to_lower(cq_text), "(^| )to [^,]*diam")) {
      dr <- dr |> mutate(value_type = if_else(value_type == "mean", "maximum", value_type))
    }
    dr
  }
  switch(
    organ,
    habit = if (so == "habit") {
      d <- grab(x, "diam(eter|\\.)?")
      if (!is.na(d)) add(dbh(d, x))
      words <- str_remove_all(lx, paste0(rng, "\\s?c?m\\b( tall| high| diam\\w*)?"))
      add(cat_rows("plant_growth_form", words))
      add(cat_rows("leaf_phenology", words))
      h <- str_extract(x, paste0(rng, "\\s?m\\b(?!\\s?(diam|wide))"))
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
    },
    bole = if (so == "habit") {
      d <- grab(x, "diam(eter|\\.)?")
      if (!is.na(d)) add(dbh(d, x))
    },
    buttresses = if (so == "habit") add(cat_rows("stem_base_form", lx)),
    twigs = {
      core <- str_remove(lx, "^(young )?twigs? ")
      # "Twigs glabrous, pubescent or tomentose": a comma list of
      # alternatives with no commonness word is not scored
      pc <- str_to_lower(flora_split_pieces(cl$piece_text, ","))
      n_hair <- sum(str_detect(pc, paste0("glabr|", hair_re)))
      hc <- pc[str_detect(pc, paste0("glabr|", hair_re))]
      listed <- n_hair > 1 && any(!str_detect(hc[-1], "^(or )?(rarely|sometimes|often|usually|occasionally)\\b"))
      if (str_detect(core, hairs_only) && !listed &&
        !str_detect(str_to_lower(cl$piece_text), age_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves") {
      add(cat_rows("leaf_compoundness", lx))
      add(cat_rows("leaf_arrangement", lx))
    },
    lamina = if (so == "leaves") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d|when dry|dry |drying|beneath|above|surface")) {
        add(cat_rows("leaf_shape", str_remove(lx, "^laminas? ")))
      }
    },
    base = if (so == "leaves" && !str_detect(lx, "narrowed")) {
      add(cat_rows("leaf_base_shape", lx))
    },
    margin = if (so == "leaves" && !str_detect(lx, "gland(s)? per|recurved|reflexed|revolute|almost|nearly")) {
      add(cat_rows("leaf_margin", lx))
    },
    apex = if (so == "leaves") add(cat_rows("leaf_apex_shape", lx)),
    petiole = {
      l <- grab_len(x)
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    inflorescences = if (so == "inflorescences") {
      # "an often branched spike": the commonness word governs "branched"
      add(cat_rows(
        "inflorescence_type",
        str_remove_all(lx, "\\b(often|sometimes|rarely) (basally )?branched\\b")
      ))
      # a length on the inflorescence itself ("spike to 1(--2) cm long")
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(str_to_lower(cl$piece_text), "\\baxis\\b")) {
        add(num_rows("inflorescence_length", l, cq_text = x))
      }
    },
    axis = if (so == "inflorescences") {
      l <- grab_len(x)
      # "axis ... with long ... hairs, 1--4 mm long" is the hairs' length;
      # "axis to 3 cm long in fruit" is the infructescence
      pt <- str_to_lower(cl$piece_text)
      hair_at <- str_locate(pt, "\\bhairs\\b")[1, 1]
      hair_size <- !is.na(hair_at) && !is.na(l) &&
        hair_at < str_locate(pt, fixed(str_to_lower(l)))[1, 1]
      # "axis 0--1--2 cm long" (var. curtisii) is a printed slip: not scored
      if (!is.na(l) && !hair_size && !str_detect(lx, "in fruit|\\d–\\d+–\\d")) {
        add(num_rows(
          "inflorescence_length", l,
          em = "inflorescence axis",
          notes = "Inflorescence axis length scored as inflorescence_length (question 3).",
          cq_text = x
        ))
      }
    },
    bracts = if (in_flower) {
      b <- str_extract(
        str_to_lower(cl$organ_text),
        "^(bracts and bracteoles|bracts|bracteoles|bract)"
      )
      em <- case_when(
        b == "bracts and bracteoles" ~ "bract and bracteole",
        b == "bracteoles" ~ "bracteole",
        TRUE ~ "bract"
      )
      # "bracts 1--1.5 mm long, with bracteoles hairy outside": the size is
      # the bracts'
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_bract_length", l, em = em, cq_text = x))
    },
    flowers = if (so == "flowers") {
      add(cat_rows("flower_perianth_symmetry", lx))
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("flower_scent_production", lx))
      if (!str_detect(lx, paste0(hair_re, "|", marking_re, "|\\d"))) {
        add(cat_rows("flower_colour", str_remove(lx, "^flowers? ")))
      }
    },
    pedicels = if (in_flower) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("pedicel_length", l, cq_text = x))
    },
    # calyx lobes; the calyx can follow the hypanthium after a comma
    lobes = if (in_flower && str_detect(str_to_lower(cl$piece_text), "calyx[^;]*\\blobes\\b")) {
      l <- grab_len(x)
      if (!is.na(l)) add(num_rows("flower_sepal_length", l, em = "calyx lobe", notes = calyx_note, cq_text = x))
    },
    corolla = if (in_flower) {
      l <- grab_len(x)
      if (!is.na(l)) add(num_rows("flower_length", l, em = "corolla", notes = corolla_note, cq_text = x))
    },
    stamens = if (in_flower) {
      m <- str_match(lx, paste0("^stamens? (", cnt, ")(?![\\d.]|\\s?(mm|cm))|^(", cnt, ")$"))
      v <- coalesce(m[1, 2], m[1, 4])
      if (!is.na(v)) add(count_rows("flower_fertile_stamens_count", x, v))
    },
    anthers = if (in_flower) {
      add(cat_rows("flower_anther_dehiscence", lx))
      if (str_detect(lx, "^anthers (yellow|white|red|purple|pink|orange)$")) {
        add(row("flower_anther_colour", str_remove(lx, "^anthers "), raw = x))
      }
    },
    fruits = if (so == "fruits") {
      core <- str_squish(str_remove(lx, "^fruits?\\b( a)?"))
      add(cat_rows("fruit_type", lx))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("fruit_length", "fruit_width"), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("fruit_length", l, cq_text = x))
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows(
            "fruit_width", str_remove(d, "\\s?(in )?diam.*"),
            notes = "Fruit diameter scored as fruit_width.", cq_text = x
          ))
        }
      }
      if (str_detect(core, hairs_only) && !str_detect(lx, age_re)) {
        add(cat_rows("fruit_surface_hairs", core))
      }
      # ripe colour: "green ripening dark purple or blue" -> purple blue;
      # dried colours ("drying pale yellow") are never scored
      ripe <- str_match(core, "\\bripen(?:ing|s)? (.+)$")[, 2]
      if (!is.na(ripe)) {
        col <- ripe
      } else if (!str_detect(str_to_lower(cl$piece_text), "ripen")) {
        col <- core
      } else {
        col <- NA_character_
      }
      if (!is.na(col) && !str_detect(col, paste0(hair_re, "|", marking_re, "|dry|drying|\\d|young"))) {
        add(cat_rows("fruit_colour", col))
      }
    },
    mesocarp = if (so == "fruits") {
      # "shrunken when dry" is the herbarium state, not a dry fruit
      if (!str_detect(lx, "when dry")) add(cat_rows("fruit_fleshiness", lx))
      t <- str_remove(grab(x, "thick"), "\\s?thick$")
      if (!is.na(t)) {
        add(num_rows(
          "fruit_wall_thickness", t,
          em = "mesocarp",
          notes = "Mesocarp thickness scored as fruit_wall_thickness, as in Prance_2012.",
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

# Leaf indumentum per surface (standing rule): hairy when a surface is hairy,
# glabrous only when both are; nothing when a statement is about young leaves,
# glabrescent, an alternative or only the midrib and veins.
leaf_hairs <- function(sent) {
  ch <- str_to_lower(unlist(map(flora_split_pieces(sent, ";"), \(p) flora_split_pieces(p, ","))))
  ch <- str_remove(ch, "^laminas? ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re)) &
    str_detect(ch, "beneath|below|above|both surfaces|on both")]
  h <- h[!str_detect(h, "^(midrib|margin|lateral|intercostal)")]
  if (!length(h) || any(str_detect(h, "glabrescent|young|except|\\bor\\b|midrib|veins|nearly|seemingly|sometimes|rarely|often")) ||
    hedged(sent)) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b")
  if (any(hairy)) {
    return(row("leaf_hairs_adult_leaves", "hairy", raw = paste(h, collapse = ", ")))
  }
  both <- any(str_detect(h, "both (sides|surfaces)|on both")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) row("leaf_hairs_adult_leaves", "glabrous", raw = paste(h, collapse = ", ")) else NULL
}

character_of <- function(x, organ) {
  lx <- str_to_lower(flora_strip_specimens(x))
  case_when(
    organ %in% c("lateral veins", "stamens", "locules") & str_detect(lx, "\\d") ~ "count",
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "°|angle") ~ "shape",
    str_detect(lx, "gland") ~ "glands",
    str_detect(lx, paste0("glabr|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|crustaceous|membran|leathery|fleshy|hard|thick") ~ "texture",
    str_detect(lx, "vein|venation|reticul|prominent|impressed|sunken|flat") ~ "venation",
    str_detect(lx, "caducous|persistent") ~ "persistence",
    str_detect(lx, "connate|joined|adnate|fused") ~ "fusion",
    str_detect(lx, "branched") ~ "branching",
    str_detect(lx, "when dry|drying|dry ") ~ "colour",
    str_detect(lx, "\\b(white|yellow|green|brown|red|black|grey|purple|orange|pink|blue|olive)") ~ "colour",
    str_detect(lx, "\\d") ~ "count",
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
    # a chunk that inherits its organ gives a number only when the number
    # comes first ("5--25 cm long")
    if (!is.null(r) && !cl$named[i] &&
      !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  # leaf indumentum per surface, once per leaf sentence
  cl$hair_done <- FALSE
  for (s in unique(cl$sentence[cl$sent_organ %in% "leaves"])) {
    i <- min(cl$id[cl$sentence == s & cl$organ %in% "lamina"], Inf)
    if (!is.finite(i)) next
    h <- leaf_hairs(cl$sent_text[i])
    if (!is.null(h)) {
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h)
      hair_ids <- cl$id[cl$sentence == s & cl$organ %in% "lamina" &
        str_detect(str_to_lower(cl$text), paste0("glabr|", hair_re)) &
        str_detect(str_to_lower(cl$text), "beneath|below|above|both")]
      cl$hair_done[hair_ids] <- TRUE
    }
  }
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
  label_only <- str_detect(str_to_lower(cl$text), "^(leaves|flowers|fruits)$")
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
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
        leaf_part(coalesce(organ, "habit"), sent_organ),
        map2_chr(text, coalesce(organ, ""), character_of),
        if_else(hedged(text), "hedged", NA_character_)
      ),
      pdf_page,
      printed_page
    )
  keep <- !str_detect(asides, "^\\((rarely|sometimes|occasionally|exceptionally) [a-z]+\\)$")
  aside_rows <- tibble(
    trait = NA_character_,
    trait_value_raw = asides[keep],
    notes = flora_unmapped_note(
      map_chr(trait_value_raw, \(a) aside_organ(p$text, a)),
      "description",
      if_else(str_detect(trait_value_raw, "elsewhere|Malesia|Thailand"), "outside the flora region", NA_character_)
    ),
    pdf_page = p$pdf_page,
    printed_page = p$printed_page
  )
  bind_rows(mapped |> select(-id), unmapped, aside_rows) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ---------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description") |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections -------------------------------------------------------

later <- paras |>
  filter(!section %in% c("Description", "Nomenclature")) |>
  mutate(sentence = map(text, \(t) flora_split_sentences(t))) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "")

later_unmapped <- later |>
  transmute(
    block, rank,
    trait = NA_character_,
    trait_value_raw = sentence,
    notes = flora_unmapped_note("section", "text", str_to_lower(section)),
    source_section = section,
    pdf_page, printed_page
  )

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c", pdf_page = "i", printed_page = "i")
)

# ---- higher ranks, inheritance, assembly ----------------------------------

taxa <- paras |>
  filter(rank %in% c("species", "subspecies", "variety")) |>
  distinct(block, rank, status) |>
  mutate(
    species = word(block, 1, 2),
    genus = word(block, 1),
    has_description = block %in% desc_paras$block
  )
# copy-down members: every described taxon, plus varieties without a
# description of their own (S. pendula var. hirtistylis and var. pendula),
# which inherit their species description
members <- taxa |>
  filter(has_description | rank != "species") |>
  transmute(block, genus, group = NA_character_, species, rank)

# A monogeneric family with no description of its own: the genus
# description holds everything (Campbell-Gasis_1996).
copied <- flora_copy_down(
  higher_rows = own |> filter(rank == "genus"),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
# "Fruit unknown" (S. nivea) blocks fruit copy-down (skill step 5)
copied <- copied |>
  filter(!(block == "Symplocos nivea" & str_detect(trait, "^fruit|^seed")))
# Species values (own and copied) go to infraspecific taxa silent on a trait;
# the taxon's own and copied rows block inheritance.
inherited <- flora_inherit_infraspecific(
  own_rows = bind_rows(
    own,
    copied |> left_join(taxa |> select(block, rank), by = "block")
  ) |>
    filter(rank != "genus"),
  members = members
) |>
  select(-rank)

singapore <- "Symplocos theifolia"
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
    # S. theifolia is known in the region only from Singapore (question 1)
    population_region = if_else(
      !taxon_rank %in% c("family", "genus") & !is.na(trait) &
        coalesce(rank_scored_from, "species") == "species" &
        word(block, 1, 2) != singapore,
      "Peninsular Malaysia",
      NA_character_
    ),
    form = NA_character_,
    entity_measured, trait, trait_value_raw, trait_value, trait_value_clean,
    value_type, unit, source_section, pdf_page, printed_page,
    notes = if_else(
      word(block, 1, 2) == singapore & !is.na(trait),
      str_squish(paste(
        coalesce(notes, ""),
        "Recorded only from Singapore (Kranji); not yet collected from Peninsular Malaysia, so no population region (question 1)."
      )),
      notes
    ),
    source_id = NA_character_
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
      block == singapore ~
        "Recorded only from Singapore (Kranji); not yet collected from Peninsular Malaysia. Scored with no population region (question 1).",
      block == "Symplocos nivea" ~ "Fruit unknown; fruit traits not copied down.",
      block %in% c("Symplocos pendula var. hirtistylis", "Symplocos pendula var. pendula") ~
        "No description of its own (varieties differ only in the key); traits inherited from the species description.",
      rank == "species" & !has_description ~
        "Single variety in the region carries the description; species not scored separately.",
      block == "Symplocos crassipes var. crassipes" ~
        "Inflorescences and flowers 'as in var. brandiana'; not copied (question 4).",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
