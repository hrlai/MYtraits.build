# Jones_1995 (TFSS vol. 1, Rutaceae): description clauses -> raw/trait_data.csv.
# Input is the transcription segmented in raw/segment_source.R. The clause
# parser follows data/Adema_1996 (TFSS vol. 2): each description is split into
# sentences (one organ each), ";" pieces and "," chunks; a chunk takes the
# organ it starts with, else the organ of its piece or sentence. Scorers run
# per organ; every chunk that scores nothing stays as an unmapped row. The
# dispersal statements of the family and Merope Ecology paragraphs are written
# by hand in raw/manual_rows.csv.
suppressPackageStartupMessages(library(tidyverse))
source("data/Jones_1995/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Jones_1995"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- read_csv(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv",
  col_types = cols(.default = "c"),
  na = character()
) |>
  filter(trait %in% names(traits)) |>
  group_by(trait) |>
  # Jones_1995, then Adema_1996 rows first: the more specific terms
  arrange(
    !str_detect(source, "Jones_1995"),
    !str_detect(source, "Adema_1996"),
    .by_group = TRUE
  ) |>
  ungroup()
dict <- split(dict, factor(dict$trait, levels = unique(dict$trait))) |>
  map(\(x) set_names(x$level, x$term_regex))

# ---- text preparation ---------------------------------------------------------

# Pages 403--419 print ranges with the minus sign; a range broken over a page
# turn ("(6–)9–10- carpellate") is rejoined; "(occasionally unifoliolate)"
# becomes an alternative with its qualifier.
prep <- function(x) {
  x |>
    str_replace_all("−", "–") |>
    str_replace_all("(?<=\\d)- (?=[a-z])", "-") |>
    str_replace_all(
      "\\s?\\((rarely|sometimes|usually|often|occasionally|mostly) ([a-z][^()]*)\\)",
      ", \\1 \\2"
    ) |>
    # "axillary or (rarely) terminal panicles"
    str_replace_all("\\((rarely|sometimes|occasionally)\\) ", "\\1 ") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(",
  num,
  "–\\)\\s?)?(?:c\\.\\s?|to\\s)?",
  num,
  "(?:\\s?–\\s?",
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
weak_re <- "\\bin unisexual plants\\b|\\bor without\\b|\\bwith or without\\b|\\bor not\\b|\\bnot or\\b|\\bnever\\b|\\bhardly\\b|\\bseemingly\\b|\\boutside borneo|\\bin some\\b|\\bif any\\b"
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no)\\b")
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]

# ---- organs -------------------------------------------------------------------

organs <- tribble(
  ~organ, ~re,
  "habit", "^((a|aromatic|erect|scandent|climbing|small|medium-sized|large|tall|slender|or|to|and) )*(trees?|treelets?|shrubs?|lianas?|climbers?|woody climbers?)\\b",
  "crown", "^crown\\b",
  "bole", "^(clear )?bole\\b",
  "stem", "^(stems?|trunk)\\b",
  "bark", "^(outer )?bark",
  "armature", "^((stout|paired|thick) )?(spines?|prickles?|thorns?)\\b",
  "trichomes", "^(trichomes|all vegetative)",
  "twigs", "^(young |younger |smaller )?(twigs?|branchlets?|shoot tips|young shoots|leafy stems)\\b",
  "branches", "^(branches|axes|axis|main branches)\\b",
  "buds", "^(terminal |floral )?buds?\\b",
  "paraphylls", "^(axillary )?paraphylls",
  "petiolules", "^petiolules?",
  "petiole", "^petioles?\\b|^(leaf )?stalks?\\b",
  "rachis", "^(leaf[- ])?rach(is|ides)\\b",
  "stipules", "^(true )?stipules?",
  "blade", "^(leaf[- ])?blades?\\b",
  "leaves", "^(the )?leaves|^leaf\\b",
  "leaflets", "^(the )?((lowest|lowermost|lower|upper|uppermost|basal|apical|terminal|lateral|distal|larger) (pair of )?)?leaflets?\\b|^(the )?(lowest|lateral and terminal) ones\\b|^larger laterals\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  "apex", "^(apex|apices)\\b",
  # the acumen's own tip shape is not the leaf apex shape
  "acumen", "^(acumen|tip)\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(lateral veins|nerves|secondary veins)\\b",
  "intercostal veins", "^(intercostal veins|veinlets|tertiary veins|reticulations?)",
  "inflorescences", "^((male|female|the) )?(inflorescences?|infructescences?)\\b",
  "bracts", "^(bracts|bracteoles|bract)\\b",
  "pedicels", "^pedicels?\\b",
  "peduncles", "^peduncles?( axis)?\\b",
  # "Flowers with sepals linear, 9–12 x 2–3 mm": the sepals
  "flowers", "^((male|female|bisexual) )?flowers?\\b(?! with (sepals|calyx))",
  "calyx", "^(flowers with )?calyx",
  "sepals", "^(flowers with )?sepals?\\b",
  "petals", "^petals?\\b",
  "disc", "^disc\\b",
  "stamens", "^stamens?\\b",
  "filaments", "^(staminal |stamen )?filaments?\\b",
  "anthers", "^(anthers?|anther connectives?|connectives?)\\b",
  "staminodes", "^staminodes?",
  "gynophore", "^gynophore",
  "carpels", "^carpels?\\b",
  "ovary", "^ovar(y|ies)\\b",
  "style", "^(styles?|stylar)\\b",
  "stigma", "^stigmas?\\b",
  "ovules", "^ovules?\\b|^locules\\b",
  "fruit wall", "^(fruit wall|pericarp|exocarp|epicarp|mesocarp|endocarp|peel|rind|outer wall)\\b",
  "pulp", "^(pulp-vesicles|pulp|juice|flesh|segments)\\b",
  "fruit stalk", "^(fruit )?stalks?\\b",
  "fruits", "^(fruits?|follicles?|drupes?|berr(y|ies))\\b",
  "seeds", "^seeds?\\b",
  "testa", "^(outer |inner )?testa",
  "funiculus", "^(funiculus|raphe)\\b",
  "endosperm", "^endosperm",
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
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- sents[si]
    s_org <- organ_of(s)
    pieces <- flora_split_pieces(s, ";")
    # "Male inflorescences ...; sepals ...; petals ...": the sex and a leaflet
    # position hold for the rest of the sentence
    sex <- NA_character_
    pos <- NA_character_
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- organ_of(pieces[pi])
      p_org <- coalesce(p_org, s_org)
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      # the structure a base / margin / apex label belongs to; a piece that
      # opens with a label ("; base acute, apex ...") belongs to the sentence organ
      parent <- if (p_org %in% leaf_labels) s_org else p_org
      map_dfr(seq_along(chunks), \(ci) {
        lc <- str_to_lower(chunks[ci])
        c_org <- organ_of(chunks[ci])
        if (!is.na(c_org)) {
          cur <<- c_org
          if (!c_org %in% leaf_labels) {
            parent <<- c_org
            pos <<- NA_character_
          }
        }
        x <- str_extract(lc, "^(fe)?male\\b")
        if (!is.na(x)) {
          sex <<- x
        }
        # "terminal leaflets ...", "leaflets of unifoliolate leaves ..."
        p <- str_extract(lc, "^(the )?(lowest|lowermost|lower|upper|uppermost|basal|apical|terminal|lateral) (pair of )?leaflets?|^leaflets of (trifoliolate|unifoliolate) leaves|^the (lowest|lateral and terminal) ones|^larger laterals")
        if (!is.na(p)) {
          pos <<- p |>
            str_remove("^the ") |>
            str_replace("^leaflets of (\\w+) leaves", "leaflet of \\1 leaf") |>
            str_replace(" ones$", " leaflet") |>
            str_replace("^larger laterals$", "larger lateral leaflet") |>
            str_replace("leaflets$", "leaflet")
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
          sex = sex,
          pos = pos,
          sent_text = s,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- scorers --------------------------------------------------------------------

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
  # "c. 2–2.5 mm" and "to 5–8 mm" are ranges, not a mean or maximum of the
  # lower bound
  r <- flora_parse_range(str_replace(raw, "(c\\.\\s?|\\bto )(?=[\\d.]+\\s?–)", ""))
  if (is.null(r) || is.na(r$unit)) {
    return(NULL)
  }
  # a reversed range ("10–8 x 8–11 cm") is a printing slip: not scored
  if (!is.na(r$hi) && r$hi < r$lo) {
    return(NULL)
  }
  q <- qualifier(cq_text)
  lc <- str_to_lower(cq_text)
  n <- flora_num_rows(r)
  # a single value bounded by the wording: "to c. 35 m", "less than 10 m",
  # "8 or fewer" -> maximum; "20 cm long or more" -> minimum
  if (nrow(n) == 1 && n$value_type == "mean") {
    v <- format(r$lo, drop0trailing = TRUE)
    if (str_detect(lc, paste0("(to c\\.\\s?|less than |up to |under )", v, "\\b|\\b", v, "[^,;]*or (fewer|less)\\b"))) {
      n$value_type <- "maximum"
    } else if (str_detect(lc, paste0("(at least |over )", v, "\\b|\\b", v, "[^,;]*or (longer|more|wider|larger)\\b"))) {
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

# A count after `prefix` with its alternatives: "4 or 5" -> 4–5; "5 (or 6)" ->
# extreme 6; "4 or 8, rarely 4–8" keeps the first statement only. `suffix`
# follows each number ("-merous", "-carpellate", "").
count_alt_rows <- function(trait, raw, lx, prefix, suffix = "", em = NA_character_, notes = NA_character_) {
  sfx <- if (nzchar(suffix)) paste0("(?:", suffix, "|-)?") else ""
  m <- str_match(
    lx,
    paste0(
      prefix, "(", cnt, ")", sfx,
      "(?:,? (or|rarely|sometimes|occasionally|commonly) (\\d+)", sfx, ")?",
      "(?:\\s?\\((?:or )?(\\d+)\\??\\))?",
      if (nzchar(suffix)) suffix else cnt_end
    )
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  # "stamens 4–5 or 8–10 or more (to 60)": a second range is not an extreme
  rest <- str_sub(lx, str_locate(lx, fixed(m[1, 1]))[1, 2] + 1)
  if (str_detect(rest, "^,? or (\\d+–|more\\b)")) {
    return(NULL)
  }
  main <- m[1, 2]
  word <- m[1, 3]
  alt <- m[1, 4]
  ext <- m[1, 5]
  simple <- str_detect(main, "^\\d+$")
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

dim_rows <- function(traits3, raw_text, em = NA_character_, cq_text = raw_text, notes = NA_character_) {
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
      str_squish(paste(
        coalesce(notes, ""),
        paste0("Dimension ", k, " of '", d$raw, "' interpreted as ", labels[k], ".")
      )),
      cq_text
    )
  }) |>
    mutate(trait_value_raw = raw_text)
}

cat_rows <- function(trait, x, em = NA_character_, notes = NA_character_) {
  # hedged, negated or partial alternatives are not scored
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
        notes = notes,
        raw = x
      ) |>
        # an alternative term without a level ("pinnate or unifoliolate")
        mutate(partial = any(lv == ""))
    })
}

# level of one matched source term (as written by flora_score_cat())
term_level <- function(trait, term) {
  d <- dict[[trait]]
  t <- str_replace_all(term, "_", " ")
  hit <- which(map_lgl(names(d), \(re) str_detect(t, regex(re, ignore_case = TRUE))))
  if (length(hit)) d[[hit[1]]] else ""
}

# A chunk that is only an indumentum statement, with alternatives and an
# optional surface: "glabrous to finely hairy", "sparsely hairy below".
hair_word <- "(?:(?:[a-z]+ly|rusty|short|softly|silky|appressed|whitish)[ -])*(?:glabrous|glabrescent|glabrate|hairy|pubescent|puberulous|tomentose|velvety|velutinous|sericeous|hirsute)"
hairs_simple <- paste0(
  "^(?:(?:usually|often|sometimes|mostly) )?",
  hair_word,
  "(?:,? (?:to|or) ", hair_word, ")*",
  "(?:,? or nearly so)?",
  "(?: (above|below|beneath))?$"
)
hair_rows <- function(trait, core, em) {
  m <- str_match(str_squish(core), hairs_simple)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  side <- m[1, 2]
  if (!is.na(side)) {
    em <- paste(em, if (side == "above") "adaxial surface" else "abaxial surface")
  }
  cat_rows(trait, str_remove(str_squish(core), "(,? or nearly so)?( (above|below|beneath))?$"), em = em)
}

# Fruit colour when ripe: "green ripening glossy deep yellow" -> yellow;
# "green turning pale yellow" -> yellow; "green when ripe" -> green.
ripe_colour <- function(lx) {
  lx |>
    str_replace("^.*\\b(ripening|becoming|turning)( to)?\\s", "") |>
    str_remove("\\bwhen (ripe|fresh)\\b")
}

score_chunk <- function(organ, x, sent_organ, leaf_kind, parent = NA, sex = NA, pos = NA, sent_text = "") {
  lx <- str_to_lower(x)
  if (hedged(lx)) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  # unifoliolate and simple blades are leaves; leaflets of a compound leaf
  # carry their position ("terminal leaflet") where stated
  em_leaf <- coalesce(pos, if (identical(leaf_kind, "leaflet")) "leaflet" else NA_character_)
  em_sex <- if (!is.na(sex)) paste(sex, "flower") else NA_character_
  leafy <- parent %in% c("leaves", "leaflets", "blade")
  core_of <- \(o) str_squish(str_remove(lx, organs$re[organs$organ == o]))
  switch(
    organ,
    habit = {
      pre <- str_extract(lx, "^[^0-9]*")
      add(cat_rows("plant_growth_form", str_remove(pre, "\\bto$")))
      add(cat_rows("sex_type", lx))
      add(cat_rows("leaf_phenology", lx))
      h <- grab(x, "(tall|high)\\b")
      if (!is.na(h) && !str_detect(lx, "bole|buttress|stilt")) {
        add(num_rows("plant_height", h, cq_text = x))
      }
      d <- grab(x, "(in )?diam(eter|\\.)?|dbh")
      if (!is.na(d)) {
        # "to 25 m tall and 60 cm in diameter": the "to" bounds both values
        add(num_rows(
          "plant_diameter_breast_height",
          str_remove(d, "\\s?(in )?diam.*|\\s?dbh"),
          notes = "Stem diameter as printed; height of measurement not stated.",
          cq_text = x
        ) |> (\(r) if (is.null(r)) r else mutate(r, value_type = if_else(value_type == "mean", "maximum", value_type)))())
      }
      if (str_detect(lx, "\\bfluted\\b") && !negative(lx)) {
        add(row("stem_base_form", "fluted", cq = qualifier(lx), raw = x))
      }
    },
    bole = ,
    stem = if (str_detect(lx, "\\bfluted\\b") && !negative(lx)) {
      add(row("stem_base_form", "fluted", cq = qualifier(lx), raw = x))
    },
    bark = {
      if (!str_detect(lx, "spot|patch|mottl|lenticel|blaze|slash|inner")) {
        add(cat_rows("bark_colour", lx))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = add(hair_rows("stem_hairs", str_remove(lx, organs$re[organs$organ == "twigs"]), NA_character_)),
    leaves = {
      add(cat_rows("leaf_compoundness", lx))
      add(cat_rows("leaf_lamina_division", lx))
      add(cat_rows("leaf_arrangement", lx))
      if (!str_detect(lx, "leaflet")) add(cat_rows("leaf_phyllotaxis", lx))
      if (!identical(leaf_kind, "leaflet")) {
        # a simple or unifoliolate leaf: the clause describes its blade
        add(dim_rows(
          c("leaf_length", "leaf_width", NA),
          x,
          em = "lamina",
          notes = "Lamina size (source excludes the petiole).",
          cq_text = x
        ))
        if (is.null(grab_dims(x)) && !str_detect(lx, "\\d|gland|hair|paraphyll")) {
          add(cat_rows("leaf_shape", lx))
        }
        add(cat_rows("leaf_surface_reflectivity", lx))
      }
      # "Leaves pinnate, ..., glabrous": the whole leaf
      add(hair_rows("leaf_hairs_adult_leaves", core_of("leaves"), "leaf"))
      # whole-leaf length, including the petiole ("Leaves to 60 cm long")
      if (is.null(grab_dims(x))) {
        l <- grab(x, "long")
        if (!is.na(l) && !str_detect(lx, "petiol|rachis|acumen|tip|wing")) {
          add(num_rows(
            "leaf_length",
            l,
            em = if (identical(leaf_kind, "leaflet")) "compound leaf" else "leaf",
            cq_text = x
          ))
        }
      }
    },
    blade = {
      add(dim_rows(
        c("leaf_length", "leaf_width", NA),
        x,
        em = "lamina",
        notes = "Lamina size (source excludes the petiole).",
        cq_text = x
      ))
      if (is.null(grab_dims(x)) && !str_detect(lx, "\\d|gland|hair")) {
        add(cat_rows("leaf_shape", lx))
      }
      add(hair_rows("leaf_hairs_adult_leaves", core_of("blade"), "leaf"))
    },
    petiole = if (!str_detect(lx, "wing")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    leaflets = {
      em <- coalesce(pos, "leaflet")
      add(dim_rows(c("leaflet_length", "leaflet_width", NA), x, em = em, cq_text = x))
      if (is.null(grab_dims(x)) && !str_detect(lx, "\\d|gland|hair|scal|pairs|alternate|opposite")) {
        add(cat_rows("leaf_shape", lx, em = em))
      }
      add(hair_rows("leaf_hairs_adult_leaves", core_of("leaflets"), em))
      add(cat_rows("leaf_surface_reflectivity", lx, em = em))
    },
    # base, margin and apex labels count only for a leaf or leaflet, not for a
    # petiolar wing, petal or sepal in the same sentence
    base = if (leafy) {
      add(cat_rows("leaf_base_shape", lx, em = coalesce(em_leaf, "leaf")))
    },
    margin = if (leafy) {
      add(cat_rows("leaf_margin", lx, em = coalesce(em_leaf, "leaf")))
    },
    apex = if (leafy) {
      add(cat_rows("leaf_apex_shape", lx, em = coalesce(em_leaf, "leaf")))
    },
    inflorescences = {
      em <- if (!is.na(sex)) paste(sex, "inflorescence") else NA_character_
      no_branch <- lx |>
        str_remove_all("\\bflowers? solitary\\b") |>
        str_remove("\\b(branches|axes|side branches)\\b.*")
      add(cat_rows("inflorescence_type", no_branch, em = em))
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(lx, "branches|in fruit")) {
        add(num_rows("inflorescence_length", l, em = em, cq_text = x))
      }
    },
    pedicels = if (!str_detect(lx, "fruit")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("pedicel_length", l, em = em_sex, cq_text = x))
    },
    flowers = {
      em <- em_sex
      # "in male flowers ..." qualifies another organ; "male only" is a sex
      add(cat_rows("flower_structural_sex_type", str_remove_all(lx, "\\bin (fe)?male flowers?\\b")))
      add(count_alt_rows("flower_perianth_merism", x, lx, "\\b", "-merous", em = em))
      add(cat_rows("flower_colour", lx, em = em))
      add(cat_rows("flower_scent_production", lx, em = em))
      if (!str_detect(lx, "in bud|wide")) {
        d <- grab(x, "(in )?(diam(eter|\\.)?|across)")
        if (!is.na(d)) add(num_rows("flower_diameter", str_remove(d, "\\s?(in )?(diam.*|across)"), em = em, cq_text = x))
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_length", l, em = em, cq_text = x))
      }
    },
    sepals = {
      if (!str_detect(lx, unit_re)) {
        add(count_alt_rows("flower_perianth_merism", x, lx, "^sepals? ", em = coalesce(em_sex, "calyx"), notes = "Sepal number scored as perianth merism."))
      }
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_sepal_length", NA, NA), x, em = em_sex, cq_text = x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_sepal_length", l, em = em_sex, cq_text = x))
      }
      if (str_detect(lx, "^sepals? (\\d )?free$")) {
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
      if (!str_detect(lx, "margin|dotted|glandular")) {
        add(cat_rows("flower_colour", lx, em = "petals"))
      }
    },
    stamens = {
      # "stamens 3 ..., rudimentary in female flowers": fertile in male flowers
      add(count_alt_rows("flower_fertile_stamens_count", x, lx, "^stamens? ", em = em_sex))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_stamen_length", l, em = em_sex, cq_text = x))
    },
    filaments = {
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(lx, "longer")) add(num_rows("flower_filament_length", l, em = em_sex, cq_text = x))
    },
    anthers = if (str_detect(lx, "^anthers?\\b")) {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_anther_length", l, em = em_sex, cq_text = x))
      add(cat_rows("flower_anther_attachment", lx))
    },
    ovary = if (!str_detect(lx, "rudimentary")) {
      add(count_alt_rows("flower_structural_carpels_count", x, lx, "(?<![\\d–(])", "-carpellate"))
      add(cat_rows("flower_ovary_position", lx))
    },
    ovules = {
      # "ovules (1–)2 per locule", "locules 2-ovulate": one locule per carpel
      n <- coalesce(
        str_match(lx, paste0("^ovules (", cnt, ") per locule"))[, 2],
        str_match(lx, paste0("^locules (", cnt, ")-ovulate"))[, 2]
      )
      if (!is.na(n)) {
        add(count_rows(
          "flower_ovules_per_functional_carpel_count",
          x,
          n,
          notes = "Ovules per locule scored per carpel (one locule per carpel assumed)."
        ))
      }
    },
    style = {
      l <- grab(x, "long")
      if (!is.na(l) && !str_detect(lx, "stigma") && !identical(sent_organ, "fruits")) {
        add(num_rows("flower_style_length", l, cq_text = x))
      }
    },
    fruits = {
      add(cat_rows("fruit_type", str_remove(lx, "\\b(grading|or) to a .*$")))
      add(cat_rows("fruit_fleshiness", lx))
      add(cat_rows("fruit_dehiscence", lx))
      # "Fruits of round to ellipsoid follicles, 3–5 mm long": a follicle size
      em <- if (str_detect(str_to_lower(sent_text), "follicle")) "follicle" else NA_character_
      add(dim_rows(c("fruit_length", "fruit_width", "fruit_height"), x, em = em, cq_text = x))
      if (is.null(grab_dims(x))) {
        d <- grab(x, "(in )?diam(eter|\\.)?|wide|across")
        if (!is.na(d)) {
          add(num_rows("fruit_width", str_remove(d, "\\s?((in )?diam.*|wide|across)"), em = em, notes = "Fruit diameter scored as fruit_width.", cq_text = x))
        } else {
          l <- grab(x, "long")
          if (!is.na(l) && !str_detect(lx, "beak|stalk|pedicel")) add(num_rows("fruit_length", l, em = em, cq_text = x))
        }
      }
      # colour of the fruit itself when ripe, not of its pulp, seeds or hairs
      if (!str_detect(lx, "pulp|seed|hair|inside|dry")) {
        add(cat_rows("fruit_colour", ripe_colour(lx)))
      }
      add(hair_rows("fruit_surface_hairs", core_of("fruits"), NA_character_))
    },
    `fruit wall` = if (!str_detect(lx, "^(endocarp|mesocarp)")) {
      t <- grab(x, "thick")
      if (!is.na(t) && !str_detect(lx, "when dry|^(exocarp|epicarp)")) {
        add(num_rows("fruit_wall_thickness", t, cq_text = x))
      }
      # the outer wall (peel, pericarp, exocarp) gives the fruit colour
      if (!str_detect(lx, "inside|gland|dot|layer|mesocarp|endocarp|albedo")) {
        add(cat_rows("fruit_colour", ripe_colour(lx), notes = "Colour of the pericarp (peel) scored as fruit colour."))
      }
      add(hair_rows("fruit_surface_hairs", core_of("fruit wall"), NA_character_))
    },
    seeds = {
      add(dim_rows(c("seed_length", "seed_width", "seed_height"), x, cq_text = x))
      if (is.null(grab_dims(x))) {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("seed_length", l, cq_text = x))
      }
      add(cat_rows("seed_shape", lx))
      own <- str_remove(lx, "\\b(with|covered|enclosed|embedded|partly|completely)\\b.*")
      add(cat_rows("seed_colour", own))
      add(cat_rows("seed_surface_reflectivity", own))
      if (!negative(lx)) add(cat_rows("dispersal_appendage", lx))
    },
    testa = if (!str_detect(lx, "^inner|spotted")) {
      add(cat_rows("seed_colour", lx, notes = "Testa colour scored as seed colour."))
      add(cat_rows("seed_surface_reflectivity", lx))
      add(cat_rows("seed_surface_texture", lx))
    },
    embryo = if (!str_detect(lx, "\\bor\\b.*\\b(green|white)\\b.*\\bor\\b")) {
      add(cat_rows("embryo_colour", lx))
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
    str_detect(lx, "hair|glabr|pubesc|tomentos|velvet|velutin|sericeous|ciliate|ciliolate|scurfy|trichome") ~ "indumentum",
    str_detect(lx, "spine|prickl|thorn|armed") ~ "armature",
    str_detect(lx, "gland|dotted|oil") ~ "glands",
    str_detect(lx, "leathery|coriaceous|chartaceous|membranous|woody|fleshy|papery|parchment|spongy|cartilaginous") ~ "texture",
    str_detect(lx, "vein|reticulat") ~ "venation",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink|cream") ~ "colour",
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
  cl$rows <- pmap(cl, \(sentence, piece, chunk, sent_organ, organ, parent, named, sex, pos, sent_text, piece_text, text, id) {
    if (is.na(organ)) {
      return(NULL)
    }
    r <- score_chunk(organ, text, sent_organ, leaf_kind, parent, sex, pos, sent_text)
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
    select(id, sentence, piece, piece_text, pdf_page, printed_page, rows) |>
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
    # Not copied down (family and genus rows): a value that is one of several
    # alternatives, in its own clause ("pinnate or unifoliolate", where the
    # second term has no level) or across the
    # clauses of one piece ("axillary, paniculate, subcorymbose, or 1- to
    # few-flowered")
    group_by(sentence, piece, trait) |>
    mutate(
      no_copy = coalesce(partial, FALSE) |
        (n_distinct(id) > 1 & str_detect(piece_text, "\\bor\\b"))
    ) |>
    ungroup() |>
    select(-sentence, -piece, -piece_text)
  # A second number for the same trait and context ("petioles 1–4 cm long"
  # of the lateral leaflets after the leaf petiole) needs a context the parser
  # cannot name: keep the first, leave the others unmapped.
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

desc_paras <- paras |>
  filter(
    section == "Description",
    rank %in% c("family", "genus", "species", "variety")
  )
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ])) |>
  mutate(source_section = case_when(
    rank == "family" ~ "Family description",
    rank == "genus" ~ "Genus description",
    TRUE ~ source_section
  ))

# ---- later sections ----------------------------------------------------------------

# Distribution, Ecology, Uses and the unlabelled distribution text of the
# in-key varieties: no configured trait is scored from them (the dispersal
# statements are in raw/manual_rows.csv), so each sentence is kept as an
# unmapped row. Taxonomy, vernacular names and nomenclature are not kept.
later_sections <- c("Distribution", "Ecology", "Uses", "Unlabelled note")
later <- paras |>
  filter(
    section %in% later_sections,
    rank %in% c("family", "genus", "species", "variety"),
    !str_detect(text, "^Vernacular name")
  ) |>
  mutate(sentence = map(text, flora_split_sentences)) |>
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
    notes = paste0(
      "UNMAPPED: ",
      str_to_lower(if_else(section == "Unlabelled note", "distribution", section)),
      " | text"
    ),
    source_section = if_else(section == "Unlabelled note", "Distribution (unlabelled)", section),
    pdf_page,
    printed_page
  )

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c"),
  na = ""
) |>
  transmute(
    block = taxon_name,
    rank = taxon_rank,
    trait,
    cq = commonness_qualifier,
    trait_value,
    trait_value_clean,
    value_type,
    unit,
    trait_value_raw,
    entity_measured,
    notes,
    pdf_page = as.integer(pdf_page),
    printed_page = as.integer(printed_page),
    rank_scored_from,
    source_section,
    source_id
  )

# ---- higher ranks, inheritance, assembly -------------------------------------------

described <- desc_paras |> distinct(block) |> pull(block)
taxa <- paras |>
  filter(rank %in% c("species", "variety")) |>
  distinct(block, rank) |>
  mutate(
    species = word(block, 1, 2),
    genus = word(block, 1),
    has_description = block %in% described
  )
# A species whose only local variety carries the description is represented
# by that variety (TFSS guide).
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

higher <- bind_rows(
  own |> filter(rank %in% c("family", "genus"), !coalesce(no_copy, FALSE)),
  manual |> filter(rank %in% c("family", "genus"))
)
species_rows <- bind_rows(
  own |> filter(!rank %in% c("family", "genus")),
  manual |> filter(!rank %in% c("family", "genus"))
)
# The family dispersal sentence names its genera, so its "most other
# Aurantioideae" does not scope the named genera: mask the raw text for the
# agreement test and restore it below.
copied <- flora_copy_down(
  higher_rows = higher |> mutate(trait_value_raw = if_else(trait == "dispersal_syndrome", "", trait_value_raw)),
  own_rows = species_rows,
  members = members
)
rejected <- attr(copied, "rejected")
# Manual rows are Ecology statements, not descriptions: keep their section.
copied <- copied |>
  left_join(
    manual |> transmute(from_block = block, trait, man_section = source_section, man_raw = trait_value_raw),
    by = "trait",
    relationship = "many-to-many"
  ) |>
  filter(is.na(from_block) | str_detect(notes, fixed(paste0("the ", from_block, " ")))) |>
  mutate(
    source_section = coalesce(man_section, source_section),
    trait_value_raw = coalesce(man_raw, trait_value_raw),
    notes = if_else(
      !is.na(man_section),
      paste0("Copied from the ", from_block, " Ecology statement."),
      notes
    )
  ) |>
  select(-from_block, -man_section, -man_raw)
# The three Glycosmis chlorosperma varieties have their own descriptions in
# the key; silent traits are inherited from the species description.
inherited <- flora_inherit_infraspecific(
  own_rows = species_rows |> filter(!is.na(trait)),
  members = members
) |>
  select(-rank)

out <- bind_rows(
  own |> mutate(rank_scored_from = if_else(rank %in% c("family", "genus"), rank, NA_character_)),
  copied,
  inherited,
  manual |> filter(!rank %in% c("family", "genus")),
  manual |> filter(rank %in% c("family", "genus")) |> mutate(rank_scored_from = rank),
  later_unmapped |> mutate(rank_scored_from = NA_character_)
) |>
  select(-any_of(c("rank", "no_copy", "partial"))) |>
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
    source_id
  ) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(
    match(taxon_rank, c("family", "genus", "species", "variety")),
    taxon_name,
    is.na(trait),
    trait,
    pdf_page
  ) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")

# ---- taxon flags -------------------------------------------------------------------

annotations <- tribble(
  ~taxon_name, ~taxon_rank, ~flag,
  "Toddalia", "genus", "Scandent genus annotated only in the key to native genera; not scored.",
  "Luvunga", "genus", "Scandent genus annotated only in the key to native genera; not scored.",
  "Paramignya", "genus", "Scandent genus annotated only in the key to native genera; not scored.",
  "Citrus medica", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus limon", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus reticulata", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus microcarpa", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus grandis", "species", "Cultivated; annotated only in the key; possibly indigenous at Ulu Segama (Stone 1985) but excluded by the author; not scored.",
  "Citrus sinensis", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus aurantifolia", "species", "Cultivated; annotated only in the key; not scored.",
  "Citrus hystrix", "species", "Cultivated; annotated only in the key; not scored.",
  "Clausena lansium", "species", "Cultivated; annotated only in the key (authority printed 'Lous'); not scored.",
  "Glycosmis parviflora", "species", "Cultivated; annotated only in the key; not scored.",
  "Murraya koenigii", "species", "Cultivated; annotated only in the key; not scored.",
  "Murraya exotica", "species", "Cultivated; annotated only in the key; not scored.",
  "Zanthoxylum scandens", "species", "Scandent shrub annotated only in the key; not scored.",
  "Zanthoxylum nitidum", "species", "Scandent shrub annotated only in the key; not scored."
)
flags <- bind_rows(
  taxa |>
    left_join(
      out |> filter(!is.na(trait), trait != "") |> count(taxon_name, name = "n_mapped_rows"),
      by = c("block" = "taxon_name")
    ) |>
    transmute(
      taxon_name = block,
      taxon_rank = rank,
      status = "accepted",
      n_mapped_rows = coalesce(n_mapped_rows, 0L),
      flag = case_when(
        !has_description & rank == "species" ~ "No species description: the single local variety carries the description.",
        rank == "variety" & str_detect(block, "^Glycosmis chlorosperma") ~ "Variety treatment printed inside the key to varieties; its description is scored at its own rank and silent traits are inherited from the species.",
        rank == "variety" ~ "Only variety in Sabah and Sarawak; its description is the local description of the species (TFSS guide).",
        block == "Melicope glabra" ~ "Reported once from Sabah (Keith 1952); occurrence needs confirmation according to the author. Scored as treated.",
        TRUE ~ ""
      )
    ),
  annotations |> mutate(status = "annotation only", n_mapped_rows = 0L)
)
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")

if (nrow(rejected)) {
  message("Higher-rank values not copied down (agreement test):")
  print(rejected, n = Inf)
}

# Source value -> levels pairs for metadata.yml `substitutions:` (pasted by
# hand). The build matches whole values, so each distinct value gets an entry.
subs <- out |>
  filter(!is.na(trait), trait != "") |>
  filter(map_lgl(trait, \(t) identical(traits[[t]]$type, "categorical"))) |>
  distinct(trait, find = trait_value, replace = trait_value_clean) |>
  filter(find != replace) |>
  arrange(trait, find)
