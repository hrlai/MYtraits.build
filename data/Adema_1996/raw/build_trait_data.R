# Adema_1996 (TFSS vol. 2, Sapindaceae): description clauses -> raw/trait_data.csv.
# Input is the image-checked transcription, segmented in raw/segment_source.R.
# Each description is split into sentences (one organ each), ";" pieces and ","
# chunks; a chunk takes the organ it starts with, else the organ of its piece or
# sentence. Scorers run per organ; every chunk that scores nothing stays as an
# unmapped row.
suppressPackageStartupMessages(library(tidyverse))
source("data/Adema_1996/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- read_csv(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv",
  col_types = cols(.default = "c"),
  na = character()
) |>
  filter(trait %in% names(traits)) |>
  group_by(trait) |>
  # Adema_1996 rows first: they are the more specific Sapindaceae terms
  arrange(!str_detect(source, "Adema_1996"), .by_group = TRUE) |>
  ungroup()
dict <- split(dict, factor(dict$trait, levels = unique(dict$trait))) |>
  map(\(x) set_names(x$level, x$term_regex))

# ---- text preparation ---------------------------------------------------------

# This edition mixes hyphens and en dashes in ranges, and letter x for the
# multiplication sign. Normalise only between numbers.
prep <- function(x) {
  x |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(-(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)-\\)", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(",
  num,
  "–\\)\\s?)?(?:c\\.\\s?|to\\s)?",
  num,
  "(?:–",
  num,
  ")?(?:\\s?\\(–",
  num,
  "\\))?"
)
unit_re <- "(mm|cm|m)\\b"
# counts: "(4–)5–8(–9)", "8", "4–5"
cnt <- "(?:\\(\\d+–\\)\\s?)?\\d+(?:–\\d+)?(?:\\s?\\(–\\d+\\))?"
cnt_end <- "(?=,|$| or\\b|\\s\\(|;)"

# one value or range followed by a unit and a keyword
grab <- function(x, keyword) {
  str_extract(x, paste0(rng, "\\s?", unit_re, "\\s?", keyword))
}
# "a x b (x c) unit"
grab_dims <- function(x) {
  m <- str_match(
    x,
    paste0(
      "(",
      rng,
      ")\\s?×\\s?(",
      rng,
      ")(?:\\s?×\\s?(",
      rng,
      "))?\\s?",
      unit_re
    )
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(raw = m[1, 1], parts = discard(m[1, 2:4], is.na), unit = m[1, 5])
}

lead_num_re <- "^((to|up to|c\\.|usually|mostly|often|sometimes|rarely|about)\\s?)*[\\d(]"

hedged <- \(x) str_detect(x, flora_hedge_re)
weak_re <- "\\bor not\\b|\\bnot or\\b|\\bnever\\b|\\bhardly\\b|\\bseemingly\\b|\\bnot in sabah|\\boutside borneo"
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no)\\b")
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]

# ---- organs -------------------------------------------------------------------

organs <- tribble(
  ~organ, ~re,
  "habit", "^(a )?(small |medium-sized |large |slender )?(trees?|treelets?|shrubs?|lianas?|climbers?|herbaceous climbers?|scandent shrubs?)\\b",
  "bole", "^(clear )?bole\\b",
  "buttresses", "^buttress",
  "stem", "^stems?\\b",
  "inner bark", "^inner bark",
  "cambium", "^cambium",
  "bark", "^(outer )?bark",
  "sapwood", "^sapwood",
  "heartwood", "^heartwood",
  "exudate", "^(exudate|sap|latex)\\b",
  "indumentum", "^(indumentum|hairs)\\b",
  "twigs", "^(young )?(twigs?|branchlets?)\\b",
  "buds", "^(terminal )?buds?\\b",
  "petiole and rachis", "^petiole,? and rachis",
  "petiole", "^petioles?\\b",
  "rachis", "^rach(is|ides)\\b",
  "pseudostipules", "^pseudostipules?",
  "stipules", "^(true )?stipules?",
  "leaves", "^(the )?leaves|^leaf\\b",
  "leaflets", "^(the )?((lowermost|lower|upper|uppermost|basal|apical|terminal|lateral|distal) (pair of )?)?leaflets?\\b",
  "petiolules", "^petiolules?",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  # the acumen's own tip shape is not the leaf apex shape
  "acumen", "^acumen\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(lateral veins|nerves|secondary veins)\\b",
  "intercostal veins", "^(intercostal veins|veinlets|tertiary veins|reticulation)",
  "domatia", "^domatia",
  "glands", "^(exposed |small |round |flat )?glands",
  "inflorescences", "^((male|female|the) )?(inflorescences?|infructescences?)\\b",
  "cymules", "^(cymules|cymes|cincinni)\\b",
  "branches", "^(branches|axes|axis)\\b",
  "bracts", "^(bracts|bracteoles|bract)\\b",
  "pedicels", "^pedicels?\\b",
  "peduncles", "^peduncles?\\b",
  "flowers", "^((male|female|bisexual) )?flowers?\\b",
  "calyx", "^calyx",
  "sepals", "^((outer|inner) (one|two|three|1|2|3) )?sepals?\\b|^(outer|inner) (one|two|three|1|2|3)\\b",
  "petals", "^petals?\\b",
  "petal scales", "^((petal )?scales?|crests?|auricles)\\b",
  "disc", "^disc\\b",
  "stamens", "^stamens?\\b(?! filaments)",
  "filaments", "^(stamen )?filaments?\\b",
  "anthers", "^anthers?\\b",
  "staminodes", "^staminodes?",
  "pistillode", "^pistillodes?",
  "gynoecium", "^(gynoecium|pistil)\\b",
  "ovary", "^ovar(y|ies)\\b",
  "style", "^styles?\\b",
  "stigma", "^stigmas?\\b",
  "ovules", "^ovules?\\b",
  "fruit lobes", "^fruit lobes?\\b",
  "lobes", "^lobes?\\b",
  "fruit wall", "^(fruit wall|pericarp|exocarp|mesocarp|endocarp|valves?)\\b",
  "appendages", "^((fruit )?appendages|spines|warts|tubercles|protuberances|hairs)\\b",
  "fruit stalk", "^(fruit )?stalks?\\b",
  "fruits", "^(fruits?|capsules?|drupes?)\\b",
  "seeds", "^seeds?\\b",
  "sarcotesta", "^sarcotesta",
  "arillode", "^arill?odes?|^arils?\\b",
  "testa", "^testa",
  "hilum", "^hilum",
  "embryo", "^(embryo|cotyledons?)\\b"
)
leaf_labels <- c("base", "margin", "apex")
organ_of <- function(x) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the|in|on) "))
  hit <- which(map_lgl(organs$re, \(r) str_detect(x, r)))
  if (length(hit)) organs$organ[hit[1]] else NA_character_
}

# ---- clause splitting ----------------------------------------------------------

split_clauses <- function(text) {
  # "Flowers: sepals ..." and "Leaflets: petiolules ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- sents[si]
    s_org <- organ_of(s)
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- organ_of(pieces[pi])
      p_org <- coalesce(p_org, s_org)
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      # the structure a base / margin / apex label belongs to; a piece that
      # opens with a label ("; base acute, apex ...") belongs to the sentence organ
      parent <- if (p_org %in% leaf_labels) s_org else p_org
      side <- NA_character_
      sex <- NA_character_
      pos <- NA_character_
      map_dfr(seq_along(chunks), \(ci) {
        lc <- str_to_lower(chunks[ci])
        c_org <- organ_of(chunks[ci])
        if (!is.na(c_org)) {
          cur <<- c_org
          if (!c_org %in% leaf_labels) {
            parent <<- c_org
            side <<- NA_character_
            pos <<- NA_character_
          }
        }
        # Labels carried to the following chunks of the piece: "outer two, 1.5–3.5
        # x 1.2–2 mm"; "male ones ..., to 20 cm long"; "lowermost leaflets ovate,
        # c. 2.5 x 1 cm". A comparison ("outer two smaller") is not a label.
        s <- str_extract(lc, "\\b(outer|inner)\\b")
        if (!is.na(s) && !str_detect(lc, "\\d|smaller|larger|shorter|longer")) {
          side <<- s
        }
        x <- str_extract(lc, "\\b(fe)?male\\b")
        if (!is.na(x)) {
          sex <<- x
        }
        p <- str_extract(lc, "^(the )?(lowermost|lower|upper|uppermost|basal|apical|terminal) (pair of )?leaflets?")
        if (!is.na(p)) {
          pos <<- str_remove(p, "^the ")
        }
        tibble(
          sentence = si,
          piece = pi,
          chunk = ci,
          sent_organ = s_org,
          organ = cur,
          parent = parent,
          # TRUE when the chunk names its own organ rather than inheriting it
          named = !is.na(c_org),
          side = side,
          sex = sex,
          pos = pos,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- scorers --------------------------------------------------------------------

# Each scorer gets one chunk (and its clause context) and returns rows with
# trait, trait_value, trait_value_clean, value_type, unit, cq, entity_measured,
# notes, raw. NULL when nothing is scored.
row <- function(
  trait,
  value,
  clean = value,
  vt = "mode",
  unit = NA_character_,
  cq = NA_character_,
  em = NA_character_,
  notes = NA_character_,
  raw
) {
  tibble(
    trait,
    trait_value = as.character(value),
    trait_value_clean = as.character(clean),
    value_type = vt,
    unit,
    cq,
    entity_measured = em,
    notes,
    trait_value_raw = raw
  )
}

num_rows <- function(trait, raw, em = NA_character_, notes = NA_character_, cq_text = raw) {
  # "c. 2–2.5 mm" is a range, not a mean of the lower bound
  r <- flora_parse_range(str_replace(raw, "c\\.\\s?(?=[\\d.]+–)", ""))
  if (is.null(r) || is.na(r$unit)) {
    return(NULL)
  }
  q <- qualifier(cq_text)
  lc <- str_to_lower(cq_text)
  n <- flora_num_rows(r)
  # a single value bounded by the wording: "to c. 35 m", "less than 10 m",
  # "8 or fewer" -> maximum; "2 cm long or longer" -> minimum
  if (nrow(n) == 1 && n$value_type == "mean") {
    v <- format(r$lo, drop0trailing = TRUE)
    if (str_detect(lc, paste0("(to c\\.\\s?|less than |up to )", v, "\\b|\\b", v, "[^,;]*or (fewer|less)\\b"))) {
      n$value_type <- "maximum"
    } else if (str_detect(lc, paste0("\\b", v, "[^,;]*or (longer|more|wider)\\b"))) {
      n$value_type <- "minimum"
    }
  }
  n |>
    # "0–1.7 mm" means the part can be absent; zero is not a size
    filter(value > 0) |>
    mutate(cq = if_else(is.na(cq) & !is.na(q), q, cq)) |>
    pmap_dfr(\(value, value_type, cq, unit) {
      row(trait, value, value, value_type, unit, cq, em, notes, raw)
    })
}

count_rows <- function(trait, raw, value_txt, em = NA_character_, notes = NA_character_) {
  r <- flora_parse_range(paste(value_txt, "mm"))
  if (is.null(r)) {
    return(NULL)
  }
  flora_num_rows(r) |>
    pmap_dfr(\(value, value_type, cq, unit) {
      row(trait, value, value, value_type, "{count}", cq, em, notes, raw)
    })
}

# A count after `prefix` with its alternatives: "4 or 5" -> 4–5; "5 (or 6)",
# "5 (6)", "4, 5 (or 6)" -> extreme 6; "2-, rarely 3-locular" -> 2 usually and
# 3 rarely. `suffix` follows each number ("-locular", "-merous", "").
count_alt_rows <- function(trait, raw, lx, prefix, suffix = "", em = NA_character_, notes = NA_character_) {
  # "2- rarely 3-locular": the first number may carry the suffix or a bare hyphen
  sfx <- if (nzchar(suffix)) paste0("(?:", suffix, "|-)?") else ""
  m <- str_match(
    lx,
    paste0(
      prefix, "(", cnt, ")", sfx,
      "(?:,? (or|rarely|sometimes|occasionally|exceptionally) (\\d+)", sfx, ")?",
      "(?:,? (\\d+))?",
      "(?:\\s?\\((?:or )?(\\d+)\\??\\))?",
      if (nzchar(suffix)) suffix else cnt_end
    )
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  main <- m[1, 2]
  word <- m[1, 3]
  alt <- m[1, 4]
  list_alt <- m[1, 5]
  ext <- m[1, 6]
  simple <- str_detect(main, "^\\d+$")
  if (!is.na(list_alt) && simple) {
    main <- paste0(main, "–", list_alt)
    simple <- FALSE
  }
  if (!is.na(alt) && word == "or" && simple) {
    main <- paste0(main, "–", alt)
    alt <- NA
  }
  if (!is.na(ext)) {
    main <- paste0(main, "(–", ext, ")")
  }
  out <- count_rows(trait, raw, main, em, notes)
  if (!is.na(alt) && word != "or") {
    out <- bind_rows(
      out |> mutate(cq = coalesce(cq, "usually")),
      row(trait, alt, alt, "mean", "{count}", word, em, notes, raw)
    )
  }
  out
}

dim_rows <- function(traits3, raw_text, em = NA_character_, cq_text = raw_text) {
  d <- grab_dims(raw_text)
  if (is.null(d)) {
    return(NULL)
  }
  labels <- c("length", "width", "height")
  # "to 2.5 x 1.5 cm": the leading "to" or "c." applies to every dimension
  lead <- str_extract(d$parts[1], "^(to |c\\.\\s?)")
  if (!is.na(lead)) {
    d$parts[-1] <- if_else(
      str_detect(d$parts[-1], "^(to |c\\.)"),
      d$parts[-1],
      paste0(lead, d$parts[-1])
    )
  }
  map_dfr(seq_along(d$parts), \(k) {
    if (is.na(traits3[k])) {
      return(NULL)
    }
    num_rows(
      traits3[k],
      paste(str_squish(d$parts[k]), d$unit),
      em,
      paste0(
        "Dimension ",
        k,
        " of '",
        d$raw,
        "' interpreted as ",
        labels[k],
        "."
      ),
      cq_text
    ) |>
      mutate(trait_value_raw = raw_text)
  })
}

cat_rows <- function(trait, x, em = NA_character_) {
  # hedged, negated or out-of-region alternatives are not scored
  if (hedged(x) || str_detect(str_to_lower(x), weak_re)) {
    return(NULL)
  }
  s <- flora_score_cat(x, trait, dict)
  if (is.null(s)) {
    return(NULL)
  }
  # trait_value keeps one source term per level, and only terms that map to a
  # level, so metadata substitutions (word by word) give exactly the levels
  s |>
    filter(trait_value_clean != "") |>
    pmap_dfr(\(cq, trait_value, trait_value_clean, trait) {
      terms <- str_split_1(trait_value, " ")
      lv <- map_chr(terms, \(t) term_level(trait, t))
      keep <- lv != "" & !duplicated(lv)
      row(
        trait,
        paste(terms[keep], collapse = " "),
        paste(lv[keep], collapse = " "),
        cq = cq,
        em = em,
        raw = x
      )
    })
}

# level of one matched source term (as written by flora_score_cat())
term_level <- function(trait, term) {
  d <- dict[[trait]]
  t <- str_replace_all(term, "_", " ")
  hit <- which(map_lgl(names(d), \(re) str_detect(t, regex(re, ignore_case = TRUE))))
  if (length(hit)) d[[hit[1]]] else ""
}

hairs_simple <- "^(glabrous|glabrescent|(sparsely |densely |minutely |finely )?(hairy|pubescent|puberulous|tomentose|tomentulose|velutinous|sericeous))( on both (sides|surfaces)| above and beneath| all over)?$"

sex_em <- function(x) {
  case_when(
    str_detect(x, "\\bfemale\\b") ~ "female flower",
    str_detect(x, "\\bmale\\b") ~ "male flower",
    TRUE ~ NA_character_
  )
}

score_chunk <- function(
  organ,
  x,
  sent_organ,
  leaf_kind,
  parent = NA,
  side = NA,
  sex = NA,
  pos = NA
) {
  lx <- str_to_lower(x)
  if (hedged(lx)) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  em_leaf <- coalesce(pos, if (identical(leaf_kind, "leaflet")) "leaflet" else NA_character_)
  # male / female named in the chunk, else carried from earlier in the clause
  em_sex <- coalesce(sex_em(lx), if (!is.na(sex)) paste(sex, "flower") else NA_character_)
  leafy <- parent %in% c("leaves", "leaflets")
  switch(
    organ,
    habit = {
      pre <- str_extract(lx, "^[^0-9]*")
      add(cat_rows("plant_growth_form", pre))
      add(cat_rows("sex_type", lx))
      h <- grab(x, "(tall|high)\\b")
      if (is.na(h)) {
        # "Shrub or tree to 30 m": a height without "tall"
        h <- str_extract(x, paste0(rng, "\\s?m$"))
      }
      if (is.na(h)) {
        # "Tree mostly less than 10 m"
        h <- str_extract(x, paste0("less than ", rng, "\\s?m\\b"))
      }
      if (!is.na(h) && !str_detect(lx, "bole|buttress|stilt")) {
        add(num_rows("plant_height", str_remove(h, "^less than "), cq_text = x))
      }
      d <- grab(x, "(in )?diam(eter|\\.)?|dbh")
      if (!is.na(d)) {
        add(num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*|\\s?dbh"), cq_text = x))
      }
      if (str_detect(lx, "buttress") && !negative(lx)) {
        add(row("stem_base_form", "buttressed", cq = qualifier(lx), raw = x))
      }
      if (str_detect(lx, "\\bfluted\\b") && !negative(lx)) {
        add(row("stem_base_form", "fluted", cq = qualifier(lx), raw = x))
      }
    },
    buttresses = if (!negative(lx)) {
      add(row("stem_base_form", "buttressed", cq = qualifier(lx), raw = x))
    },
    stem = if (str_detect(lx, "\\bfluted\\b") && !negative(lx)) {
      add(row("stem_base_form", "fluted", cq = qualifier(lx), raw = x))
    },
    bark = {
      # spots, patches and lenticels are not the bark colour
      if (!str_detect(lx, "spot|patch|mottl|lenticel|blaze|slash")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (str_detect(str_remove(lx, "^(young )?(twigs?|branchlets?) "), hairs_simple)) {
      add(cat_rows("stem_hairs", str_remove(lx, "^(young )?(twigs?|branchlets?) ")))
    },
    leaves = {
      add(cat_rows("leaf_compoundness", lx))
      add(cat_rows("leaf_lamina_division", lx))
      add(cat_rows("leaf_arrangement", lx))
      if (!str_detect(lx, "leaflet")) add(cat_rows("leaf_phyllotaxis", lx))
      if (!identical(leaf_kind, "leaflet")) {
        add(dim_rows(c("leaf_length", "leaf_width", NA), x, cq_text = x))
      }
    },
    petiole = {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    leaflets = {
      core <- str_remove(lx, organs$re[organs$organ == "leaflets"])
      em <- coalesce(pos, "leaflet")
      add(dim_rows(c("leaflet_length", "leaflet_width", NA), x, em = em, cq_text = x))
      # shapes of glands, domatia, hairs or scales are not the leaflet shape
      if (is.null(grab_dims(x)) && !str_detect(lx, "gland|domati|hair|scale|pseudostipule")) {
        add(cat_rows("leaf_shape", lx, em = em))
      }
      if (str_detect(str_squish(core), hairs_simple)) {
        add(cat_rows("leaf_hairs_adult_leaves", str_squish(core), em = em))
      }
      add(cat_rows("leaf_surface_reflectivity", lx, em = em))
    },
    # base, margin and apex labels count only for a leaf or leaflet, not for a
    # pseudostipule, petal or sepal in the same sentence
    base = if (leafy) {
      add(cat_rows("leaf_base_shape", lx, em = coalesce(em_leaf, "leaf")))
    },
    margin = if (leafy) {
      add(cat_rows("leaf_margin", lx, em = coalesce(em_leaf, "leaf")))
    },
    apex = if (leafy && !str_detect(lx, "^(the )?acumen")) {
      # "apex tapering into an acute acumen": the tip shape is the acumen's
      a <- str_replace(lx, "\\b(abruptly |gradually )?(tapering )?into (an? |the )?[a-z ,-]*acumen.*", "acuminate")
      add(cat_rows("leaf_apex_shape", a, em = coalesce(em_leaf, "leaf")))
    },
    inflorescences = {
      em <- em_sex
      # "to 20 cm long if solitary" and "flowers solitary" are not an inflorescence type
      no_branch <- lx |>
        str_remove_all("\\bif (solitary|tufted)\\b|\\bflowers? solitary\\b") |>
        str_remove("\\b(cymes|cymules|branches|cyme-like)\\b.*")
      add(cat_rows("inflorescence_type", no_branch, em = em))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("inflorescence_length", l, em = em, cq_text = x))
    },
    pedicels = if (!str_detect(lx, "fruit")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("pedicel_length", l, em = em_sex, cq_text = x))
    },
    peduncles = {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("peduncle_length", l, cq_text = x))
    },
    bracts = {
      em <- if (str_detect(lx, "^bracteoles")) "bracteole" else "bract"
      l <- grab(x, "long")
      if (!is.na(l)) {
        add(num_rows("flower_bract_length", l, em = em, cq_text = x))
      } else {
        add(dim_rows(c("flower_bract_length", NA, NA), x, em = em, cq_text = x))
      }
    },
    flowers = {
      em <- em_sex
      # "in male flowers ..." qualifies another organ; it does not state sexuality
      add(cat_rows("flower_structural_sex_type", str_remove_all(lx, "\\b(fe)?male\\b")))
      sym <- str_detect(lx, "symmetr")
      if ((sym && str_detect(lx, "radially")) || str_detect(lx, "actinomorphic")) {
        add(row("flower_perianth_symmetry", "radially_symmetrical", "actinomorphic_general", cq = qualifier(lx), raw = x))
      }
      if ((sym && str_detect(lx, "bilaterally")) || str_detect(lx, "zygomorphic")) {
        add(row("flower_perianth_symmetry", "bilaterally_symmetrical", "zygomorphic", cq = qualifier(lx), raw = x))
      }
      add(count_alt_rows("flower_perianth_merism", x, lx, "\\b", "-merous", em = em))
      add(cat_rows("flower_colour", str_remove(lx, "^(male|female) flowers?"), em = em))
      d <- grab(x, "(in )?(diam(eter|\\.)?|across)")
      if (!is.na(d)) add(num_rows("flower_diameter", str_remove(d, "\\s?(in )?(diam.*|across)"), em = em, cq_text = x))
    },
    sepals = {
      if (!str_detect(lx, unit_re)) {
        add(count_alt_rows("flower_perianth_merism", x, lx, "^sepals? ", em = coalesce(em_sex, "calyx"), notes = "Sepal number scored as perianth merism."))
      }
      em <- case_when(
        !is.na(em_sex) ~ em_sex,
        str_detect(lx, "\\bouter\\b") & !str_detect(lx, "smaller|larger") ~ "outer sepals",
        str_detect(lx, "\\binner\\b") & !str_detect(lx, "smaller|larger") ~ "inner sepals",
        !is.na(side) ~ paste(side, "sepals"),
        TRUE ~ NA_character_
      )
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_sepal_length", NA, NA), x, em = em, cq_text = x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_sepal_length", l, em = em, cq_text = x))
      }
      if (str_detect(lx, "^sepals? free$")) {
        add(row("flower_perianth_fusion_categorical", "free", "perianth_parts_unfused", raw = x))
      }
    },
    petals = {
      # petal number alone is not flower_perianth_parts_count (all parts): unmapped
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_petal_length", NA, NA), x, em = em_sex, cq_text = x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_petal_length", l, em = em_sex, cq_text = x))
      }
      add(cat_rows("flower_colour", lx, em = "petals"))
    },
    stamens = {
      add(count_alt_rows("flower_fertile_stamens_count", x, lx, "^stamens? ", em = em_sex))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_stamen_length", l, em = em_sex, cq_text = x))
    },
    filaments = {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, em = em_sex, cq_text = x))
    },
    anthers = {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_anther_length", l, em = em_sex, cq_text = x))
      add(cat_rows("flower_anther_attachment", lx))
      add(cat_rows("flower_anther_orientation", lx))
    },
    ovary = {
      # "3-locular at base, higher up 1-locular" is not a carpel number
      if (!str_detect(lx, "fruit|at base|higher up")) {
        add(count_alt_rows("flower_structural_carpels_count", x, lx, "(?<![\\d–(])", "-locular", notes = "Ovary locules scored as carpels (one locule per carpel assumed)."))
      }
      add(cat_rows("flower_ovary_position", lx))
    },
    style = {
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(lx, "stigma") && !identical(sent_organ, "fruits")) {
        add(num_rows("flower_style_length", l, cq_text = x))
      }
    },
    fruits = {
      # "when capsular usually loculicidal": the qualifier is the dehiscence's
      add(cat_rows("fruit_type", str_remove(lx, "^when [a-z]+ ")))
      add(cat_rows("fruit_dehiscence", str_remove(lx, "\\bmostly unequal\\b")))
      add(dim_rows(c("fruit_length", "fruit_width", "fruit_height"), x, cq_text = x))
      if (is.null(grab_dims(x))) {
        d <- grab(x, "(in )?diam(eter|\\.)?")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?(in )?diam.*"), notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        } else {
          l <- grab(x, "long")
          if (!is.na(l)) add(num_rows("fruit_length", l, cq_text = x))
        }
      }
      # colour of the fruit itself, not of its pulp, septa, seeds or hairs
      if (!str_detect(lx, "pulp|sept|seed|aril|hair|inside")) {
        add(cat_rows("fruit_colour", lx))
      }
      core <- str_squish(str_remove(lx, organs$re[organs$organ == "fruits"]))
      if (str_detect(core, hairs_simple)) add(cat_rows("fruit_surface_hairs", core))
    },
    `fruit wall` = if (!str_detect(lx, "^endocarp|^mesocarp|^exocarp")) {
      t <- grab(x, "thick")
      if (!is.na(t)) add(num_rows("fruit_wall_thickness", t, cq_text = x))
    },
    seeds = {
      add(dim_rows(c("seed_length", "seed_width", "seed_height"), x, cq_text = x))
      if (is.null(grab_dims(x))) {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("seed_length", l, cq_text = x))
      }
      add(cat_rows("seed_shape", lx))
      # colour of the seed itself, not of its sarcotesta or arillode
      own <- str_remove(lx, "\\b(with|covered|enclosed|partly|completely)\\b.*")
      add(cat_rows("seed_colour", own))
      if (!negative(lx)) add(cat_rows("dispersal_appendage", lx))
    }
  )
  # the verbatim clause, not the matched fragment
  bind_rows(out) |> mutate(trait_value_raw = x)
}

# rough label for an unmapped chunk
character_of <- function(x) {
  lx <- str_to_lower(x)
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "hair|glabr|pubesc|tomentos|tomentul|velutin|sericeous|ciliate|woolly|pilose|puberul") ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|herbaceous|woody|fleshy|papyraceous|crustaceous|parchment") ~ "texture",
    str_detect(lx, "vein|nerve|reticulat|scalariform") ~ "venation",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink") ~ "colour",
    TRUE ~ "description"
  )
}

parse_description <- function(p) {
  text <- prep(p$text)
  cl <- split_clauses(text)
  if (!nrow(cl)) {
    return(NULL)
  }
  leaf_kind <- if (str_detect(str_to_lower(text), "leaflet")) "leaflet" else "leaf"
  cl$id <- seq_len(nrow(cl))
  cl$rows <- pmap(cl, \(sentence, piece, chunk, sent_organ, organ, parent, named, side, sex, pos, piece_text, text, id) {
    if (is.na(organ)) {
      return(NULL)
    }
    r <- score_chunk(organ, text, sent_organ, leaf_kind, parent, side, sex, pos)
    # A chunk that inherits its organ may only give a number when the number
    # comes first ("5–25 cm long"); "branches to 4 cm long" belongs to an
    # unlisted part.
    if (!is.null(r) && nrow(r) && !named && !str_detect(str_to_lower(text), lead_num_re)) {
      r <- r |> filter(!value_type %in% c("minimum", "maximum", "mean"))
    }
    r
  })
  pos <- str_locate(text, fixed(str_sub(cl$text, 1, 25)))[, 1]
  pages <- map(coalesce(pos, 1L), \(k) flora_page_at(p, k))
  cl$pdf_page <- map_int(pages, \(x) as.integer(x[["pdf"]]))
  cl$printed_page <- map_int(pages, \(x) as.integer(x[["printed"]]))
  mapped <- cl |>
    filter(map_lgl(rows, \(r) !is.null(r) && nrow(r) > 0)) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    # "Tree, rarely shrub" is split into two chunks: the unqualified
    # alternative becomes `usually`, as within one chunk
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(
      cq = if_else(
        value_type == "mode" & is.na(cq) & any(!is.na(cq) & cq != "extreme_range"),
        "usually",
        cq
      )
    ) |>
    ungroup() |>
    select(-sentence, -piece)
  # A second number for the same trait and context ("pedicels 6–13 mm long"
  # after "pedicels 2–7 mm long", "to 5 cm long if tufted") needs a context
  # the parser cannot name: keep the first, leave the others unmapped.
  later_dup <- mapped |>
    filter(value_type != "mode") |>
    distinct(id, trait, entity_measured, cq) |>
    group_by(trait, entity_measured, cq) |>
    filter(id != min(id)) |>
    ungroup() |>
    distinct(id, trait)
  mapped <- mapped |> anti_join(later_dup, by = c("id", "trait"))
  unmapped <- cl |>
    mutate(
      dup_of = map_chr(id, \(i) paste(later_dup$trait[later_dup$id == i], collapse = ", "))
    ) |>
    filter(map_lgl(rows, \(r) is.null(r) || !nrow(r)) | (dup_of != "" & !id %in% mapped$id)) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = paste0(
        "UNMAPPED: ",
        coalesce(organ, "unknown organ"),
        " | ",
        character_of(text),
        if_else(hedged(text), " | hedged", ""),
        if_else(dup_of != "", paste0(" | second statement for ", dup_of, "; context not resolved"), "")
      ),
      pdf_page,
      printed_page
    )
  # chunks with a duplicate numeric trait and other mapped traits keep those
  dup_partial <- cl |>
    filter(id %in% later_dup$id, id %in% mapped$id) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = paste0("UNMAPPED: ", organ, " | size | second statement for ", map_chr(id, \(i) paste(later_dup$trait[later_dup$id == i], collapse = ", ")), "; context not resolved"),
      pdf_page,
      printed_page
    )
  bind_rows(mapped |> select(-id), unmapped, dup_partial) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- run ---------------------------------------------------------------------------

fixes <- read_csv(
  "data/Adema_1996/raw/source_fixes.csv",
  col_types = cols(.default = "c", page = "i")
)

desc_paras <- paras |>
  filter(
    section == "Description",
    status == "accepted",
    rank %in% c("family", "genus", "species", "subspecies", "variety")
  )
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections ----------------------------------------------------------------

later_sections <- c(
  "Distribution", "Ecology", "Uses", "Use", "Notes", "Note",
  "Unlabelled note", "Field characters"
)
later <- paras |>
  filter(
    status == "accepted",
    section %in% later_sections,
    rank %in% c("family", "genus", "species", "subspecies", "variety", "subvariety", "form")
  ) |>
  mutate(sentence = map(text, flora_split_sentences)) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "")

# Phenology: months only where the text names them. "A to B" and "A–B" are
# inclusive ranges and may wrap past December.
months <- c(
  "January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"
)
month_re <- paste(months, collapse = "|")
calendar <- function(x) {
  if (str_detect(x, "throughout the year|all year")) {
    return(strrep("y", 12))
  }
  # this edition also prints month ranges with a hyphen ("January-August")
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
    # stop before a following "fruiting" clause ("..., fruiting May–September")
    "(?i)\\bflower(ing|s|ed)?( and fruiting)?\\b(?:(?!fruit)[^;.])*"
  } else {
    "(?i)\\bfruit(ing|s|ed)?\\b[^;.]*"
  }
  str_extract(s, re)
}
pheno <- later |>
  filter(section == "Ecology" | section == "Unlabelled note") |>
  mutate(
    flower = map_chr(sentence, pheno_part, "flower"),
    fruit = map_chr(sentence, pheno_part, "fruit")
  ) |>
  pivot_longer(c(flower, fruit), names_to = "what", values_to = "part") |>
  filter(!is.na(part), str_detect(part, paste0(month_re, "|throughout the year"))) |>
  mutate(value = map_chr(part, calendar))
pheno_rows <- bind_rows(
  pheno |>
    transmute(block, rank, trait = if_else(what == "flower", "flowering_time", "fruiting_time"), part, value, pdf_page, printed_page, sentence),
  # "Flowering and fruiting in ..." names both
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
    trait_value = str_remove(part, "(?i)^(flowering|fruiting)( and (flowering|fruiting))?( noted)?( in| from)?\\s*"),
    trait_value_clean = value,
    value_type = "mode",
    notes = "Months as printed; from herbarium collections.",
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

# ---- higher ranks, inheritance, assembly -------------------------------------------

described <- paras |>
  filter(section == "Description") |>
  distinct(block) |>
  pull(block)
taxa <- paras |>
  filter(rank %in% c("species", "subspecies", "variety", "subvariety", "form")) |>
  distinct(block, rank, status) |>
  mutate(
    species = str_extract(block, "^[A-Z][a-z]+ (sp\\. [A-Z]|[a-z-]+)"),
    genus = word(block, 1),
    has_description = block %in% described
  )
# A species whose only local infraspecific taxon carries the description is
# represented by that taxon; formae have no description beyond a key lead.
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

copied <- flora_copy_down(higher_rows = own, own_rows = own, members = members)
rejected <- attr(copied, "rejected")
# a subvariety inherits from its species, whose description covers it
inherited <- flora_inherit_infraspecific(
  own_rows = own |> filter(!is.na(trait), rank != "genus", rank != "family"),
  members = members
) |>
  select(-rank)

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
    taxon_rank,
    rank_scored_from,
    commonness_qualifier = cq,
    population_region = NA_character_,
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
  arrange(match(taxon_rank, c("family", "genus", "species", "subspecies", "variety", "subvariety", "form")), taxon_name, pdf_page) |>
  flora_finalise(traits)

write_csv(out, "data/Adema_1996/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/Adema_1996/raw/unmapped_terms.csv",
  na = ""
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
      str_detect(block, " sp\\. [A-Z]$") ~ "Unnamed species, kept under its printed label.",
      !has_description & rank == "species" ~ "No species description: the single local infraspecific taxon carries the description.",
      !has_description ~ "No description: key lead and nomenclature only; not scored.",
      rank %in% c("variety", "subvariety", "subspecies") & str_detect(block, paste(in_key_species, collapse = "|")) ~ "Infraspecific treatment printed inside a key; its description is scored at its own rank.",
      TRUE ~ ""
    )
  )
write_csv(flags, "data/Adema_1996/raw/taxon_flags.csv", na = "")

# Source value -> levels pairs for metadata.yml `substitutions:` (pasted by
# hand). The build matches whole values, so each distinct value gets an entry.
subs <- out |>
  filter(!is.na(trait), !trait %in% c("flowering_time", "fruiting_time")) |>
  filter(map_lgl(trait, \(t) identical(traits[[t]]$type, "categorical"))) |>
  distinct(trait, find = trait_value, replace = trait_value_clean) |>
  filter(find != replace) |>
  arrange(trait, find)
