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
# Kochummen_1995 leaves "subcordate" unscored; here it is the cordate end of a
# base-shape range ("obtuse to subcordate"), so let "cordate" match inside it.
dict$leaf_base_shape <- dict$leaf_base_shape[names(dict$leaf_base_shape) != "subcordate"]
# Sapindaceae terms missing from the shared dictionary (row-by-row review,
# 2026-10); first, so they win over shorter shared terms
local_terms <- list(
  plant_growth_form = c("woody climbers?" = "climber_woody"),
  leaf_phyllotaxis = c("\\bwhorled\\b" = "whorled"),
  inflorescence_type = c("dichasi[a-z]*" = "cyme", "\\baxils\\b" = "axillary"),
  seed_shape = c("\\borbicular\\b" = "orbicular"),
  seed_colour = c("(?<![a-z])(purple|purplish)\\b" = "blue_purple"),
  fruit_colour = c("\\bcrimson\\b" = "red", "(?<![a-z])(grey|greyish)\\b" = "grey"),
  bark_colour = c("(?<![a-z])greenish\\b" = "green"),
  leaf_compoundness = c("pairs? of leaflets" = "compound")
)
for (t in names(local_terms)) dict[[t]] <- c(local_terms[[t]], dict[[t]])

# ---- text preparation ---------------------------------------------------------

# This edition mixes hyphens and en dashes in ranges, and letter x for the
# multiplication sign. Normalise only between numbers.
prep <- function(x) {
  x |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(-(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)-\\)", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    # "sepals 4, 5 (or 6)": a count list, not two clauses
    str_replace_all("(?<=\\b(sepals?|petals?|stamens?) \\d), (?=\\d \\()", " or ") |>
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
  "branches", "^((main|lateral|primary|secondary) )?(branches|axes|axis)\\b",
  "bracts", "^(bracts|bracteoles|bract)\\b",
  "pedicels", "^pedicels?\\b",
  "peduncles", "^peduncles?\\b",
  "flowers", "^((male|female|bisexual) )?flowers?\\b",
  "calyx", "^calyx",
  "sepals", "^((outer|inner) (one|two|three|1|2|3) )?sepals?\\b|^(outer|inner) (one|two|three|1|2|3)\\b",
  # a blade or claw size is not the whole petal length
  "petal blade", "^(petal )?(blades?|claws?)\\b",
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
  "appendages", "^((fruit )?appendages|spines|warts|warty|tubercles|protuberances|hairs)\\b",
  "fruit stalk", "^(fruit )?stalks?\\b",
  # parts of a fruit, and its pulp and septum, are not the fruit
  "fruit parts", "^fruit parts?\\b",
  "pulp", "^pulp\\b",
  "septum", "^sept(um|a)\\b",
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
    # "Male inflorescences ...; pedicels ...": the sentence's sex covers its
    # pieces; "male ones somewhat smaller than the female ones" is a comparison
    sex_label <- \(t) if (str_detect(t, "\\bthan\\b")) NA_character_ else str_match(t, "^(fe)?male (inflorescences?|flowers?)\\b")[, 1]
    s_sex <- str_extract(sex_label(str_to_lower(s)), "^(fe)?male")
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- organ_of(pieces[pi])
      p_org <- coalesce(p_org, s_org)
      chunks <- flora_split_pieces(pieces[pi], ",")
      # "...; male inflorescences ...; pedicels 2–7 mm long; female
      # inflorescences ...": a piece that opens with a sex sets it for the
      # following pieces of the sentence
      p_sex <- str_extract(sex_label(str_to_lower(pieces[pi])), "^(fe)?male")
      if (!is.na(p_sex)) {
        s_sex <<- p_sex
      }
      cur <- p_org
      # the structure a base / margin / apex label belongs to; a piece that
      # opens with a label ("; base acute, apex ...") belongs to the sentence organ
      parent <- if (p_org %in% leaf_labels) s_org else p_org
      side <- NA_character_
      sex <- s_sex
      pos <- NA_character_
      map_dfr(seq_along(chunks), \(ci) {
        lc <- str_to_lower(chunks[ci])
        c_org <- organ_of(chunks[ci])
        if (!is.na(c_org)) {
          cur <<- c_org
          if (!c_org %in% leaf_labels) {
            parent <<- c_org
            side <<- NA_character_
            # "filaments ..., in female flowers c. 1.5 mm; anthers": a new organ
            # drops the sex of the previous one
            sex <<- s_sex
            pos <<- NA_character_
          }
        }
        # Labels carried to the following chunks of the piece: "outer two, 1.5–3.5
        # x 1.2–2 mm"; "male ones ..., to 20 cm long"; "lowermost leaflets ovate,
        # c. 2.5 x 1 cm". A comparison ("outer two smaller") is not a label unless
        # the chunk opens with it ("2 outer smaller ones 1–2.8 x 1–2.3 mm",
        # "outer 2 ovate", "inner 3 sepals ...").
        s <- str_extract(lc, "\\b(outer|inner)\\b")
        lead_side <- str_match(lc, "^(?:sepals? )?(?:\\d+ )?(outer|inner)\\b")[, 2]
        # "outer 2 sometimes smaller, ..., 2–4 x 1.5–3 mm": a comparison without
        # its own size leaves the later size to all sepals
        if (!is.na(lead_side) && (!str_detect(lc, "smaller|larger|shorter|longer") || str_detect(lc, paste0(num, "\\s?", unit_re)))) {
          side <<- lead_side
        } else if (!is.na(s) && !str_detect(lc, "\\d|smaller|larger|shorter|longer")) {
          side <<- s
        }
        x <- str_extract(lc, "\\b(fe)?male\\b")
        if (!is.na(x)) {
          sex <<- x
        }
        # "lateral ones often ovate", "the upper ones ..." name leaflets too
        p <- str_extract(lc, "^(the )?(lowermost|lower|upper|uppermost|basal|apical|terminal|lateral) (pair of )?(leaflets?|ones)")
        if (!is.na(p)) {
          pos <<- str_replace(str_remove(p, "^the "), "ones$", "leaflets")
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
  # "stamens 8 or fewer" is a maximum
  if (!is.null(out) && str_detect(lx, paste0(prefix, "\\d+", sfx, " or (fewer|less)\\b"))) {
    out$value_type[out$value_type == "mean"] <- "maximum"
  }
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
  # trait_value keeps every source term that maps to a level, so the copy-down
  # check sees each alternative ("imparipinnate or paripinnate"); levels are
  # kept once, and the whole-value substitutions give exactly those levels
  out <- s |>
    filter(trait_value_clean != "") |>
    pmap_dfr(\(cq, trait_value, trait_value_clean, trait) {
      terms <- str_split_1(trait_value, " ")
      lv <- map_chr(terms, \(t) term_level(trait, t))
      keep <- lv != ""
      row(
        trait,
        paste(terms[keep], collapse = " "),
        paste(unique(lv[keep]), collapse = " "),
        cq = cq,
        em = em,
        raw = x
      )
    })
  if (!nrow(out)) {
    return(out)
  }
  # "pari- or rarely imparipinnate": alternatives with one level leave no
  # alternative for that trait; every source term stays in trait_value, so the
  # copy-down check does not read the dropped one as unscored
  if (nrow(out) > 1 && n_distinct(out$trait_value_clean) == 1) {
    terms <- unique(unlist(str_split(out$trait_value, " ")))
    out <- out[1, ] |>
      mutate(trait_value = paste(terms, collapse = " "), cq = NA_character_)
  }
  # "terminal and sometimes axillary": the added state is rarer, the first is
  # not an alternative to it, so it gets no `usually`
  lxx <- str_to_lower(x)
  if (str_detect(lxx, "\\band (sometimes|rarely|occasionally|exceptionally)\\b") && !str_detect(lxx, "\\busually\\b")) {
    out$cq[out$cq %in% "usually"] <- NA_character_
  }
  # "exceptionally" is not a shared qualifier word
  if (str_detect(str_to_lower(x), "^(or |and )?exceptionally\\b")) {
    out$cq <- coalesce(out$cq, "exceptionally")
  }
  out
}

# level of one matched source term (as written by flora_score_cat())
term_level <- function(trait, term) {
  d <- dict[[trait]]
  t <- str_replace_all(term, "_", " ")
  hit <- which(map_lgl(names(d), \(re) str_detect(t, regex(re, ignore_case = TRUE))))
  if (length(hit)) d[[hit[1]]] else ""
}

rare_more_re <- "\\b(rarely|sometimes|occasionally|exceptionally) (more|fewer|less)\\b"
rare_q <- c("sometimes", "rarely", "occasionally", "exceptionally", "very rarely", "less often")

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
  pos = NA,
  prev = NA
) {
  lx <- str_to_lower(x)
  if (hedged(lx)) {
    return(NULL)
  }
  # "8 (rarely fewer)": the stated count is the usual one ("4 or 5, rarely more"
  # across chunks is handled in parse_description())
  usual_count <- str_detect(lx, rare_more_re)
  # "rounded in the lower, ... in the upper leaflets", "ovate (lower ones) to
  # elliptic (upper ones)" inside a chunk: the states belong to some leaflets
  # only and cannot be attributed here. An aside about some leaflets ("elliptic
  # (or in the lowermost pair ovate)") is dropped and the rest scored.
  pos_label <- "\\((lower|upper|lowermost|uppermost) (ones|leaflets?|pair)\\)"
  if (organ %in% c("leaflets", leaf_labels) && !str_detect(lx, pos_label)) {
    lx <- str_squish(str_remove_all(lx, "\\([^)]*\\b(lowermost|lower|upper|uppermost) (pair|ones|leaflets?)\\b[^)]*\\)"))
  }
  pos_mid <- paste0("\\b(in|of) (the )?(lower|upper|lowermost|uppermost|basal|terminal|lateral|asymmetric)( (pair|ones|leaflets?)\\b|$)|", pos_label)
  part_leaf <- str_detect(lx, pos_mid) & !str_detect(lx, "^(the )?(lowermost|lower|upper|uppermost|basal|apical|terminal|lateral) ")
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
      # "Tree mostly less than 10 m": the qualifier is the height's
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b((mostly|usually|often|sometimes) )?(less than|up to|to|c\\.)\\s*$")
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
        dr <- num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?diam.*|\\s?dbh"), cq_text = x)
        # "Tree to 40 m tall, 90 cm diameter": the height's "to" (and qualifier)
        # also bounds the diameter in the next chunk; "c. 1 m diameter" stays a mean
        lp <- str_to_lower(coalesce(prev, ""))
        if (!is.null(dr) && !str_detect(lx, "^c\\.") && str_detect(lp, "\\bto (c\\.\\s?)?\\d[^,;]*\\bm (tall|high)\\b")) {
          dr <- dr |>
            mutate(
              value_type = if_else(value_type == "mean", "maximum", value_type),
              cq = coalesce(cq, qualifier(lp))
            )
        }
        add(dr)
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
      # spots, patches and lenticels are not the bark colour: drop a "with dark
      # patches" phrase, and skip chunks that are about them
      bc <- str_remove(lx, "\\bwith [^,;]*\\b(spot|patch|mottl|lenticel)[a-z]*")
      if (!str_detect(bc, "spot|patch|mottl|lenticel|blaze|slash")) {
        add(cat_rows("bark_colour", bc))
      }
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (str_detect(str_remove(lx, "^(young )?(twigs?|branchlets?) "), hairs_simple)) {
      add(cat_rows("stem_hairs", str_remove(lx, "^(young )?(twigs?|branchlets?) ")))
    },
    leaves = {
      # "pari- or rarely imparipinnate"
      lx <- str_replace(lx, "\\bpari- or ", "paripinnate or ")
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
      if (is.null(grab_dims(x)) && !str_detect(lx, "gland|domati|hair|scale|pseudostipule") && !part_leaf) {
        add(cat_rows("leaf_shape", lx, em = em))
      }
      if (str_detect(str_squish(core), hairs_simple)) {
        add(cat_rows("leaf_hairs_adult_leaves", str_squish(core), em = em))
      }
      # "dull (sometimes even glaucous) to shiny": the aside is another character
      add(cat_rows("leaf_surface_reflectivity", str_remove_all(lx, "\\([^)]*\\)"), em = em))
    },
    # base, margin and apex labels count only for a leaf or leaflet, not for a
    # pseudostipule, petal or sepal in the same sentence
    base = if (leafy && !part_leaf) {
      # "(in sessile simple leaves subcordate, ...)" is about other leaves
      add(cat_rows("leaf_base_shape", str_remove_all(lx, "\\(in [^)]*\\)"), em = coalesce(em_leaf, "leaf")))
    },
    margin = if (leafy && !part_leaf) {
      add(cat_rows("leaf_margin", lx, em = coalesce(em_leaf, "leaf")))
    },
    apex = if (leafy && !part_leaf && !str_detect(lx, "^(the )?acumen")) {
      # "apex tapering into an acute acumen", "with an obtuse acumen", "obtuse- to
      # acute-acuminate", and a comma-split "..., rounded or acute acumen": the
      # tip shape is the acumen's
      a <- lx |>
        str_replace("\\b[a-z]+- to [a-z]+-acuminate\\b", "acuminate") |>
        str_replace("\\b(abruptly |gradually )?(tapering )?(into|with) (an? |the )?[a-z ,-]*acumen.*", "acuminate") |>
        str_replace("^[a-z ,-]*\\bacumen\\b.*", "acuminate")
      add(cat_rows("leaf_apex_shape", a, em = coalesce(em_leaf, "leaf")))
    },
    inflorescences = {
      em <- em_sex
      # "to 20 cm long if solitary", "flowers solitary" and "inflorescences
      # solitary" (one per axil) are not the single-flowered level; "subterminal"
      # is not terminal; "umbel-like" is a likeness
      no_branch <- lx |>
        str_remove_all("\\bif (solitary|tufted)\\b|\\bflowers? solitary\\b|\\b((usually|mostly|often) )?solitary\\b|\\bsub-?terminal\\b|\\b[a-z]+-like\\b") |>
        str_remove("\\b(cymes|cymules|branches|cyme-like)\\b.*")
      add(cat_rows("inflorescence_type", no_branch, em = em))
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("inflorescence_length", l, em = em, cq_text = x))
    },
    # a pedicel in the fruit sentence is a fruiting pedicel
    pedicels = if (!str_detect(lx, "fruit") && !identical(sent_organ, "fruits")) {
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
      # "radially or rarely bilaterally symmetrical": each state takes the
      # qualifier just before it
      q_before <- \(term) qualifier(str_extract(lx, paste0("\\b(\\w+ ){0,2}(", term, ")")))
      if ((sym && str_detect(lx, "radially")) || str_detect(lx, "actinomorphic")) {
        add(row("flower_perianth_symmetry", "radially_symmetrical", "actinomorphic_general", cq = q_before("radially|actinomorphic"), raw = x))
      }
      if ((sym && str_detect(lx, "bilaterally")) || str_detect(lx, "zygomorphic")) {
        add(row("flower_perianth_symmetry", "bilaterally_symmetrical", "zygomorphic", cq = q_before("bilaterally|zygomorphic"), raw = x))
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
        # "2 outer smaller ones 1–2.8 x 1–2.3 mm": the size is the outer ones'
        str_detect(lx, "^(sepals? )?(\\d+ )?outer\\b") ~ "outer sepals",
        str_detect(lx, "^(sepals? )?(\\d+ )?inner\\b") ~ "inner sepals",
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
    # a blade's colour is the petal colour; its size is not the petal length
    `petal blade` = add(cat_rows("flower_colour", lx, em = "petals")),
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
      # colour of the ripe fruit itself, not of its pulp, septa, seeds or hairs
      # ("brown-tomentulose", "brown short-velvety"), nor when young, unripe or
      # turning colour on drying
      fc <- lx |>
        str_remove_all("\\b[a-z]+[- ](short-)?(velvety|tomentul[a-z]*|tomentose|hairy|pubescent|puberul[a-z]*|sericeous)\\b") |>
        str_remove_all("[^,;]*\\bwhen (young|unripe|immature)\\b") |>
        str_remove_all("\\bturning (to )?[^,;]*\\bwhen dry\\b")
      if (!str_detect(lx, "pulp|sept|seed|aril|hair|inside")) {
        add(cat_rows("fruit_colour", fc))
      }
      core <- str_squish(str_remove(lx, organs$re[organs$organ == "fruits"]))
      if (str_detect(core, hairs_simple)) add(cat_rows("fruit_surface_hairs", core))
    },
    # Cubilia "Fruit parts ..., green to brown": the parts' colour is the fruit's
    `fruit parts` = add(cat_rows("fruit_colour", lx)),
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
  out <- bind_rows(out)
  if (usual_count && nrow(out)) {
    out <- out |> mutate(cq = if_else(unit %in% "{count}" & is.na(cq), "usually", cq))
  }
  # the verbatim clause, not the matched fragment
  out |> mutate(trait_value_raw = x)
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
  cl <- cl |>
    group_by(sentence, piece) |>
    mutate(prev = lag(text), nxt = lead(text)) |>
    ungroup()
  cl$rows <- pmap(cl, \(sentence, piece, chunk, sent_organ, organ, parent, named, side, sex, pos, piece_text, text, id, prev, nxt) {
    if (is.na(organ)) {
      return(NULL)
    }
    r <- score_chunk(organ, text, sent_organ, leaf_kind, parent, side, sex, pos, prev)
    # "sepals 4 or 5, rarely more": the stated count is the usual one
    if (!is.null(r) && nrow(r) && str_detect(str_to_lower(coalesce(nxt, "")), paste0("^(or )?", rare_more_re))) {
      r <- r |> mutate(cq = if_else(unit %in% "{count}" & is.na(cq), "usually", cq))
    }
    # A chunk that inherits its organ may only give a number when the number
    # comes first ("5–25 cm long"); "branches to 4 cm long" belongs to an
    # unlisted part.
    if (!is.null(r) && nrow(r) && !named && !str_detect(str_to_lower(text), lead_num_re)) {
      r <- r |> filter(!value_type %in% c("minimum", "maximum", "mean"))
    }
    r
  })
  # locate each chunk after the previous one, so a short chunk ("glabrous") is
  # not placed at an earlier occurrence on the previous page
  pos <- rep(NA_integer_, nrow(cl))
  from <- 1L
  for (i in seq_len(nrow(cl))) {
    k <- str_locate(str_sub(text, from), fixed(str_sub(cl$text[i], 1, 25)))[1, 1]
    if (!is.na(k)) {
      pos[i] <- from + k - 1L
      from <- pos[i]
    }
  }
  pages <- map(coalesce(pos, 1L), \(k) flora_page_at(p, k))
  cl$pdf_page <- map_int(pages, \(x) as.integer(x[["pdf"]]))
  cl$printed_page <- map_int(pages, \(x) as.integer(x[["printed"]]))
  mapped <- cl |>
    filter(map_lgl(rows, \(r) !is.null(r) && nrow(r) > 0)) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    # "Tree, rarely shrub" is split into two chunks: the unqualified
    # alternative becomes `usually`, as within one chunk. Only a rarer
    # alternative does this: "mostly acuminate", "often sickle-shaped" and
    # "terminal and sometimes axillary" add a state, not an alternative.
    # Arrangement and phyllotaxis share one statement ("spirally arranged,
    # rarely opposite or whorled").
    mutate(tgroup = if_else(trait %in% c("leaf_arrangement", "leaf_phyllotaxis"), "leaf_arrangement", trait)) |>
    group_by(sentence, piece, tgroup, entity_measured) |>
    mutate(
      cq = if_else(
        value_type == "mode" & is.na(cq) &
          any(cq %in% rare_q & !str_detect(str_to_lower(trait_value_raw), "\\band (sometimes|rarely|occasionally|exceptionally)\\b")),
        "usually",
        cq
      )
    ) |>
    # "usually loculicidal, rarely septicidal": alternatives with one level
    # leave no alternative for that trait
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(one_level = n() > 1 & all(value_type == "mode") & n_distinct(trait_value_clean) == 1) |>
    filter(!one_level | row_number() == 1) |>
    mutate(cq = if_else(one_level, NA_character_, cq)) |>
    ungroup() |>
    select(-tgroup, -one_level) |>
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

# a conditional family or genus statement ("when capsular usually
# loculicidal") does not hold for every member
copied <- flora_copy_down(
  higher_rows = own |> filter(!str_detect(str_to_lower(coalesce(trait_value_raw, "")), "^when ")),
  own_rows = own,
  members = members
)
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
