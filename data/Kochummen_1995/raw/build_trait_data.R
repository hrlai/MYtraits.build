# Kochummen_1995: extract traits from the Tree Flora of Sabah and Sarawak
# Burseraceae treatment (K.M. Kochummen 1995, TFSS vol. 1: 45--100).
#
# Writes (run from the repository root):
#   data/Kochummen_1995/raw/trait_data.csv      mapped and unmapped records
#   data/Kochummen_1995/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Kochummen_1995/raw/taxon_flags.csv     headings processed, rank, flags
#
# Values come from the genus and species descriptions (clause parser below),
# from the month statements in the Ecology paragraphs, and from the family
# description and other statements written by hand in raw/manual_rows.csv.
# Keys and figures are not read. Names are kept as printed.

suppressPackageStartupMessages(library(tidyverse))
source("data/Kochummen_1995/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Kochummen_1995"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c"),
  na = character()
)
dict_file <- ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
dict <- flora_load_dict(dict_file)
region <- "Sabah and Sarawak"
num <- .flora_num
rng_re <- paste0(
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
infra_re <- "\\((var|forma|subsp)\\. [a-z]+\\)|\\b(sunken )?in (var|forma|subsp)\\. [a-z]+"
hair_re <- "glabr|hairy|hairs|pubescent|puberul|tomentose|tomentum|velvety|woolly|silky|villous|pilose|hispid"

# ---- text preparation -------------------------------------------------------

prep <- function(x, page) {
  x |>
    # "(sometimes) with hairs", "(rarely ivory white or red)": keep the word
    str_replace_all("\\((sometimes)\\)", "\\1") |>
    str_replace_all(
      "\\s?\\((rarely|sometimes|usually|often) ([a-z][^()]*)\\)",
      ", \\1 \\2"
    ) |>
    str_replace_all("(?<=[\\d)])\\s?x\\s?(?=[\\d(]|c\\. )", " × ") |>
    # line-break joins in the text layer ("3- merous", "asym-metric")
    str_replace_all("(?<=\\d)- (?=[a-z])", "-") |>
    str_replace_all("asym-metric", "asymmetric") |>
    str_replace_all("tri-angular", "triangular") |>
    flora_norm()
}

# Source terms that the dictionary maps to an empty level are not values.
term_level <- function(trait, term) {
  d <- dict[[trait]]
  t <- str_replace_all(term, "_", " ")
  hit <- which(map_lgl(names(d), \(re) {
    str_detect(t, regex(re, ignore_case = TRUE))
  }))
  if (length(hit)) d[[hit[1]]] else NA_character_
}
score <- function(txt, tr) {
  s <- flora_score_cat(txt, tr, dict)
  if (is.null(s)) {
    return(NULL)
  }
  s |>
    mutate(
      terms = str_split(trait_value, " "),
      lv = map(terms, \(t) map_chr(t, \(x) term_level(tr, x))),
      unmapped_terms = map2_chr(terms, lv, \(t, l) {
        paste(str_replace_all(t[l %in% ""], "_", " "), collapse = ", ")
      }),
      trait_value = map2_chr(terms, lv, \(t, l) paste(t[!l %in% ""], collapse = " "))
    ) |>
    filter(trait_value != "") |>
    select(-terms, -lv)
}

# ---- organ detection -----------------------------------------------------------

organ_re <- c(
  infructescence = "^infructescences?",
  habit = "^(very |small|medium|large|big|emergent|tall|trees?\\b|treelet|shrubs?|deciduous|dioecious)",
  buttress = "^(buttresses|stilt-roots|bole)",
  inner_bark = "^(inner|middle) bark",
  bark = "^bark",
  sapwood = "^sapwood",
  pith = "^pith",
  twigs = "^(ultimate leafy |young )?twigs?",
  bud = "^(terminal )?buds?",
  stipules = "^stipules?",
  seedling = "^(leaves (in|of) young seedlings?|seedlings?|first two leaves|subsequent leaves)",
  leaves = "^leaves|^leaf\\b",
  stalk = "^stalks?\\b",
  petiolule = "^(petiolules?|leaflet stalks?|stalks \\(petiolules\\))",
  pedicels = "^pedicels?\\b",
  blade = "^(blade|leaflets?)\\b",
  rachis = "^rachis",
  petiole = "^petioles?",
  lamina_parts = "^(base|margin|apex)\\b",
  tip = "^tip\\b",
  midrib = "^midrib",
  veins = "^(lateral |intercostal |intercostsal )?veins|^reticulations",
  inflorescence = "^(inflorescences?|panicles?)",
  flowers = "^flowers?",
  calyx = "^(calyx|sepals|calyx-lobes|receptacle)",
  petals = "^petals",
  stamens = "^(rudimentary )?stamens?\\b",
  filaments = "^filaments",
  anthers = "^anthers",
  disc = "^disc",
  ovary = "^(ovary|pistil|rudimentary (ovary|pistil))",
  style = "^(style|stigma)",
  fruits = "^fruits?",
  pericarp = "^(pericarp|rind)",
  stone = "^(stones?|pyrenes?)",
  testa = "^testa",
  seeds = "^seeds?",
  cotyledons = "^cotyledons",
  germination = "^germination",
  saplings = "^saplings?"
)
detect_organ <- function(x) {
  x <- str_to_lower(str_remove(x, "^\\((male|female|immature)\\)\\s*"))
  hit <- names(organ_re)[str_detect(x, organ_re)]
  if (length(hit)) hit[1] else NA_character_
}

# Unmapped character labels, by organ, for rows written to trait_data.csv.
unmapped_char <- c(
  infructescence = "infructescence length / position",
  habit = "habit (other)",
  buttress = "buttress height / shape",
  inner_bark = "inner bark colour / texture / exudate / smell",
  bark = "bark (other)",
  sapwood = "sapwood colour",
  pith = "pith vascular strands",
  twigs = "twig thickness / colour / shape / lenticels",
  bud = "terminal bud size / indumentum",
  stipules = "stipule presence / persistence / shape / position",
  leaves = "leaf (other)",
  blade = "leaflet (other)",
  rachis = "rachis colour / indumentum / shape / length",
  stalk = "fruit or flower stalk length",
  pedicels = "pedicel (other)",
  petiolule = "petiolule length / swelling / indumentum",
  petiole = "petiole shape / indumentum",
  lamina_parts = "leaflet base / margin / apex (other)",
  tip = "leaflet tip (acumen) length",
  midrib = "midrib relief",
  veins = "vein number / relief / pattern",
  inflorescence = "inflorescence (other)",
  flowers = "flower (other)",
  calyx = "calyx / receptacle shape / indumentum",
  petals = "petal shape / indumentum",
  stamens = "stamen fusion / staminodes",
  filaments = "filament shape / fusion",
  anthers = "anther (other)",
  disc = "disc shape",
  ovary = "ovary shape / indumentum",
  style = "style / stigma",
  fruits = "fruit (other)",
  pericarp = "pericarp texture",
  stone = "stone (pyrene)",
  testa = "seed coat",
  seeds = "seed number / shape",
  cotyledons = "cotyledons",
  germination = "germination",
  seedling = "seedling leaves",
  saplings = "sapling leaves"
)
piece_char <- function(org, p) {
  pl <- str_to_lower(p)
  case_when(
    str_detect(pl, infra_re) ~ "infraspecific variation",
    org == "leaves" & str_detect(pl, "pairs") ~ "leaflet pair count",
    org %in% c("blade", "leaves") &
      str_detect(pl, "leathery|coriaceous|chartaceous|papery|membranous") ~
      "leaflet texture",
    org %in% c("blade") & str_detect(pl, "dry|dried") ~ "leaflet colour on drying",
    org %in% c("blade") & str_detect(pl, "sessile|stalked") ~ "leaflet sessility",
    org %in% c("blade") & str_detect(pl, "asymmetric|unequal") ~ "leaflet symmetry",
    org %in% c("blade") & str_detect(pl, "galls|domatia") ~ "leaflet galls / domatia",
    org %in% c("blade") & str_detect(pl, "bullate|concave|rugose") ~ "leaflet surface relief",
    org == "lamina_parts" & str_detect(pl, "^apex") ~ "leaflet apex shape (pointed, cuspidate)",
    org == "lamina_parts" & str_detect(pl, "^base") ~ "leaflet base shape (other)",
    org %in% c("flowers", "inflorescence", "fruits", "petals", "calyx", "blade", "twigs", "leaves") &
      str_detect(pl, hair_re) ~
      paste(org, "indumentum"),
    org == "fruits" &
      str_detect(
        pl,
        "globose|globular|ellipsoid|ovoid|oblong|spindle|fusiform|pear|triangular|round|hemispherical|asymmetric|oblique|circular|pointed|apex|base"
      ) ~
      "fruit shape",
    org == "fruits" & str_detect(pl, "calyx") ~ "fruiting calyx",
    org == "fruits" & str_detect(pl, "stigma") ~ "fruit stigma position",
    org == "fruits" & str_detect(pl, "wrinkled|rugose|surface|warty|smooth") ~ "fruit surface",
    TRUE ~ unname(unmapped_char[org])
  )
}

# ---- month calendars ------------------------------------------------------------

months <- c(
  January = 1,
  February = 2,
  March = 3,
  April = 4,
  May = 5,
  June = 6,
  July = 7,
  August = 8,
  September = 9,
  October = 10,
  November = 11,
  December = 12
)
mon_re <- paste(names(months), collapse = "|")
calendar <- function(s) {
  y <- rep("n", 12)
  pairs <- str_match_all(
    s,
    paste0("(", mon_re, ")\\s?(?:–|to)\\s?(", mon_re, ")")
  )[[1]]
  for (i in seq_len(nrow(pairs))) {
    a <- months[[pairs[i, 2]]]
    b <- months[[pairs[i, 3]]]
    y[if (a <= b) a:b else c(a:12, 1:b)] <- "y"
  }
  single <- str_extract_all(s, mon_re)[[1]]
  y[months[single]] <- "y"
  if (all(y == "n")) NA_character_ else paste(y, collapse = "")
}

# ---- clause parser ------------------------------------------------------------------

parse_description <- function(par_row) {
  block <- par_row$block
  rank <- par_row$rank
  txt <- prep(par_row$text, par_row$printed_page)
  rows <- list()
  un <- list()
  cur_pos <- 1

  emit <- function(trait, df, raw, entity = NA_character_, note = NA_character_) {
    if (is.null(df) || !nrow(df)) {
      return(invisible(FALSE))
    }
    pg <- page_at(par_row, cur_pos)
    rows[[length(rows) + 1]] <<- df |>
      mutate(
        trait = trait,
        trait_value_raw = raw,
        entity_measured = entity,
        notes = note,
        pdf_page = pg[["pdf"]],
        printed_page = pg[["printed"]]
      )
    invisible(TRUE)
  }
  unmapped <- function(org, character, text) {
    pg <- page_at(par_row, cur_pos)
    un[[length(un) + 1]] <<- tibble(
      organ = str_replace_all(org, "_", " "),
      character = character,
      trait_value_raw = text,
      pdf_page = pg[["pdf"]],
      printed_page = pg[["printed"]]
    )
  }
  cat_rows <- function(trait, text, raw, entity = NA_character_) {
    s <- score(text, trait)
    if (is.null(s) || !nrow(s)) {
      return(FALSE)
    }
    note <- if_else(
      s$unmapped_terms != "",
      paste0("Unmapped term(s) in the clause: ", s$unmapped_terms, "."),
      NA_character_
    )
    emit(
      trait,
      s |>
        transmute(
          cq,
          trait_value,
          trait_value_clean,
          value_type = "mode",
          unit = NA_character_,
          partial = s$unmapped_terms != ""
        ),
      raw,
      entity,
      note
    )
    TRUE
  }
  num_rows <- function(trait, nr, raw, unit = NULL, entity = NA_character_, note = NA_character_, cq = NULL) {
    if (is.null(nr) || !nrow(nr)) {
      return(FALSE)
    }
    if (!is.null(unit)) {
      nr$unit <- coalesce(nr$unit, unit)
    }
    if (!is.null(cq)) {
      nr$cq <- coalesce(nr$cq, cq)
    }
    emit(
      trait,
      nr |>
        transmute(
          cq,
          trait_value = as.character(value),
          trait_value_clean = as.character(value),
          value_type,
          unit
        ),
      raw,
      entity,
      note
    )
  }
  len_of <- function(p, word = "long", units = "mm|cm|m") {
    hit <- str_extract(p, paste0(rng_re, "\\s?(", units, ")\\s", word))
    if (is.na(hit)) NULL else flora_num_rows(flora_parse_range(hit))
  }

  process_clause <- function(cl, org) {
    raw <- cl
    has_infra <- str_detect(cl, infra_re)
    work <- str_remove(cl, "^[A-Z][a-z]+ \\((male|female|immature)\\)\\s*")
    work <- str_remove(work, "^\\((male|female|immature)\\)\\s*")
    immature <- str_detect(cl, "\\(immature\\)")
    pieces <- flora_split_pieces(work, ",")
    used <- rep(FALSE, length(pieces))
    infra_piece <- str_detect(pieces, infra_re)
    ent_sex <- if (!is.na(sex)) paste(sex, "flowers") else NA_character_
    use <- function(i) used[i] <<- TRUE
    pl <- str_to_lower(pieces)
    # score a categorical trait on selected pieces; mark pieces it used
    cat_on <- function(trait, idx, entity = NA_character_) {
      idx <- idx[!infra_piece[idx]]
      if (!length(idx)) {
        return(FALSE)
      }
      ok <- cat_rows(trait, paste(pieces[idx], collapse = ", "), raw, entity)
      if (ok) {
        for (i in idx) if (!is.null(score(pieces[i], trait))) use(i)
      }
      ok
    }
    all_idx <- seq_along(pieces)
    # labelled runs: "base rounded, margin entire, apex pointed, tip c. 1 cm"
    cat_on_base <- function(ps, idx) {
      ps2 <- str_remove_all(ps, "(on |with )?one half [a-z]+|\\bat base\\b")
      ok <- cat_rows("leaf_base_shape", paste(ps2, collapse = ", "), raw, "leaflet")
      if (ok) for (i in idx) if (!is.null(score(pieces[i], "leaf_base_shape"))) use(i)
    }

    if (org != "leaves" && str_detect(cl, "pairs of leaflets")) {
      emit(
        "leaf_compoundness",
        tibble(cq = NA_character_, trait_value = "compound", trait_value_clean = "compound", value_type = "mode", unit = NA_character_),
        raw
      )
    }

    if (org == "habit") {
      # "tree rarely more than 30 m tall": the qualifier belongs to the height
      gf <- str_remove_all(pieces, "((very )?rarely )?(more than |to |reaching )?[\\d.\u2013()]+ ?m (tall|high).*$")
      if (cat_rows("plant_growth_form", paste(gf, collapse = ", "), raw)) {
        for (i in all_idx) if (!is.null(score(gf[i], "plant_growth_form"))) use(i)
      }
      for (i in which(str_detect(pl, "more than \\d+ m"))) {
        unmapped(org, "plant height (rarely more than)", pieces[i])
      }
      cat_on("stem_base_form", all_idx)
      cat_on("sex_type", all_idx)
      cat_on("leaf_phenology", all_idx)
      for (i in all_idx) {
        p <- pieces[i]
        q <- str_match(pl[i], "^(very rarely|rarely)\\b")[, 2]
        if (str_detect(pl[i], "more than")) {
          next
        }
        h <- str_extract(p, paste0(rng_re, "\\s?m (tall|high)"))
        if (!is.na(h)) {
          num_rows(
            "plant_height",
            flora_num_rows(flora_parse_range(str_remove(h, " (tall|high)"))),
            raw,
            cq = q
          )
          use(i)
        }
        d <- str_extract(p, paste0("(?:to )?", num, " cm (in )?diameter"))
        if (!is.na(d)) {
          v <- str_extract(d, num)
          num_rows(
            "plant_diameter_breast_height",
            tibble(value = as.numeric(v), value_type = "maximum", cq = q, unit = "cm"),
            raw,
            note = "Stem diameter as printed; height of measurement not stated."
          )
          use(i)
        }
      }
      # "Medium-sized tree 30 m tall, very rarely to 50 m tall": the
      # unqualified height becomes the usual value
      hrows <- keep(rows, \(r) r$trait[1] == "plant_height" & identical(r$trait_value_raw[1], raw))
      if (length(hrows) > 1 && any(map_lgl(hrows, \(r) any(!is.na(r$cq))))) {
        rows <<- map(rows, \(r) {
          if (r$trait[1] == "plant_height" && identical(r$trait_value_raw[1], raw)) {
            r$cq <- coalesce(r$cq, "usually")
          }
          r
        })
      }
      for (i in which(str_detect(pl, "^(small|medium|large|very|emergent|tree|shrub)") & !used)) {
        # size words only ("Small to medium-sized tree")
        if (str_detect(pl[i], "\\btrees?\\b|treelet|shrub")) use(i)
      }
    }

    if (org == "buttress") {
      b1 <- cat_on("stem_base_form", all_idx)
      b2 <- cat_on("root_system_type", all_idx)
      if (b1 || b2) {
        rest <- str_detect(pl, "\\d|short|steep|tall|small|spreading")
        if (any(rest)) unmapped(org, "buttress height / shape", raw)
        used[] <- TRUE
      }
    }

    if (org == "bark") {
      ok <- !str_detect(pl, "gum|resin|exudate|sap\\b|smell")
      cat_on("bark_colour", which(ok))
      cat_on("bark_texture", which(ok))
    }

    if (org == "twigs" && !has_infra) {
      ok <- str_detect(pl, hair_re) & !str_detect(pl, "young|tip|leaf-scar")
      if (!str_detect(str_to_lower(cl), "young|tips?\\b")) {
        cat_on("stem_hairs", which(ok))
      }
    }

    if (org == "leaves") {
      for (i in all_idx) {
        if (str_detect(pl[i], "pairs|leaflets? [\\d(]|trifoliolate|pinnate|compound")) {
          emit(
            "leaf_compoundness",
            tibble(cq = NA_character_, trait_value = "compound", trait_value_clean = "compound", value_type = "mode", unit = NA_character_),
            raw
          )
          break
        }
      }
      for (i in which(str_detect(pl, "simple leaves"))) {
        q <- str_match(pl[i], flora_qual_re)[, 2]
        emit(
          "leaf_compoundness",
          tibble(cq = q, trait_value = "simple", trait_value_clean = "simple", value_type = "mode", unit = NA_character_),
          raw
        )
        rows[[length(rows) - 1]]$cq <<- "usually"
        use(i)
      }
      cat_on("leaf_lamina_division", all_idx)
      cat_on("leaf_arrangement", which(str_detect(pl, "spiral|crowded")))
      if (!has_infra) {
        cat_on("leaf_hairs_adult_leaves", which(str_detect(pl, hair_re)), "leaf")
      }
      for (i in all_idx) {
        l <- len_of(pieces[i], "long", "m|cm")
        if (!is.null(l) && !str_detect(pl[i], "rachis|petiol")) {
          num_rows("leaf_length", l, raw, entity = "compound leaf")
          use(i)
        }
      }
    }

    if (org == "blade") {
      # genus-style "Leaflets entire, the base often strongly unequal"
      lab <- case_when(
        str_detect(pl, "^(the )?base\\b|\\bat base$") ~ "base",
        str_detect(pl, "margin|^(leaflets )?entire$") ~ "margin",
        TRUE ~ NA_character_
      )
      # unlabelled alternatives continue a labelled run ("margin entire,
      # dentate, or serrate")
      for (i in seq_along(lab)[-1]) {
        if (
          is.na(lab[i]) &&
            !is.na(lab[i - 1]) &&
            str_detect(pl[i], "^(or |and )?(entire|dentate|serrate|serrulate|toothed|crenate|rounded|cuneate|unequal)\\b")
        ) {
          lab[i] <- lab[i - 1]
        }
      }
      shape_i <- which(str_detect(pl, "elliptic|ovate|obovate|oblong|lanceolate|rhomboid|falcate|orbicular") & is.na(lab))
      cat_on("leaf_shape", shape_i, "leaflet")
      dims <- NULL
      for (i in which(str_detect(pieces, "×"))) {
        dims <- flora_parse_dims(pieces[i])
        if (!is.null(dims)) {
          num_rows("leaflet_length", dims$len, raw, entity = "leaflet")
          num_rows("leaflet_width", dims$wid, raw, entity = "leaflet")
          use(i)
        }
      }
      if (!has_infra) {
        hi <- which(str_detect(pl, hair_re) & is.na(lab))
        surf <- case_when(
          str_detect(pl[hi], "below|beneath|lower|under") ~ "leaflet abaxial surface",
          str_detect(pl[hi], "above|upper") ~ "leaflet adaxial surface",
          TRUE ~ "leaflet"
        )
        for (s in unique(surf)) cat_on("leaf_hairs_adult_leaves", hi[surf == s], s)
      }
      cat_on("leaf_surface_reflectivity", which(str_detect(pl, "shiny|glossy|dull")), "leaflet adaxial surface")
      gi <- which(str_detect(pl, "glaucous|white waxy|whitish (below|beneath)"))
      cat_on("leaf_glaucousness", gi, if (any(str_detect(pl[gi], "below|beneath"))) "leaflet abaxial surface" else "leaflet")
      bi <- which(lab %in% "base")
      if (length(bi)) {
        cat_on_base(pieces[bi], bi)
      }
      mi <- which(lab %in% "margin")
      if (length(mi)) {
        cat_on("leaf_margin", mi, "leaflet")
      }
    }

    if (org == "lamina_parts") {
      lab <- rep(NA_character_, length(pieces))
      cur <- NA_character_
      for (i in all_idx) {
        l <- str_match(pl[i], "^(base|margin|apex|tip)\\b")[, 2]
        cur <- coalesce(l, cur)
        lab[i] <- cur
      }
      bi <- which(lab %in% "base")
      if (length(bi)) cat_on_base(pieces[bi], bi)
      mi <- which(lab %in% "margin")
      cat_on("leaf_margin", mi, "leaflet")
      cat_on("leaf_margin_posture", mi, "leaflet")
      cat_on("leaf_apex_shape", which(lab %in% "apex"), "leaflet")
      for (i in which(lab %in% "tip")) unmapped("tip", unmapped_char[["tip"]], pieces[i])
      used[lab %in% "tip"] <- TRUE
    }

    if (org == "petiole") {
      w <- str_extract(work, paste0(rng_re, "\\s?(mm|cm) (thick|wide)"))
      if (!is.na(w)) {
        num_rows("petiole_width", flora_num_rows(flora_parse_range(str_remove(w, " (thick|wide)"))), raw)
      }
      l <- len_of(work)
      if (!is.null(l)) num_rows("petiole_length", l, raw)
    }

    if (org %in% c("inflorescence", "flowers")) {
      ent_inf <- if (!is.na(sex)) paste(sex, "inflorescences") else NA_character_
      # "male thyrsoid, female racemose to spicate"
      sx <- str_match(pl, "^(male|female)\\b")[, 2]
      inf_i <- which(
        str_detect(pl, "panicle|paniculate|raceme|racemose|spike|spicate|thyrs|axillary|terminal|inflorescence|lateral") &
          !str_detect(pl, "\\bbuds?\\b")
      )
      for (s in unique(sx[inf_i])) {
        ii <- inf_i[sx[inf_i] %in% s]
        e <- if (is.na(s)) ent_inf else paste(s, "inflorescences")
        cat_on("inflorescence_type", ii, e)
      }
      for (i in inf_i) {
        l <- len_of(pieces[i], "long", "cm")
        if (is.null(l)) {
          l2 <- str_match(pieces[i], paste0("(", rng_re, ") cm long|\\(c\\. (", num, ") cm\\)"))
          if (!is.na(l2[1, 1])) l <- flora_num_rows(flora_parse_range(paste(coalesce(l2[1, 2], paste("c.", l2[1, 3])), "cm")))
        }
        if (!is.null(l) && !str_detect(pl[i], "branches")) {
          num_rows("inflorescence_length", l, raw, entity = if (is.na(sx[i])) ent_inf else paste(sx[i], "inflorescences"))
          use(i)
        }
      }
      if (org == "inflorescence") {
        for (i in setdiff(all_idx, inf_i)) {
          l <- len_of(pieces[i], "long", "cm")
          if (!is.null(l) && !str_detect(pl[i], "branches")) {
            num_rows("inflorescence_length", l, raw, entity = if (is.na(sx[i])) ent_inf else paste(sx[i], "inflorescences"))
            use(i)
          }
        }
      }
      if (org == "flowers") {
        # sex type and merism, unless only functional ("functionally unisexual")
        ok <- !str_detect(pl, "functionally|structurally")
        cat_on("flower_structural_sex_type", which(ok & str_detect(pl, "sexual")))
        cat_on("sex_type", which(str_detect(pl, "male and female")))
        for (i in which(str_detect(pl, "-merous"))) {
          m <- as.numeric(str_extract_all(pieces[i], "\\d")[[1]])
          num_rows(
            "flower_perianth_merism",
            tibble(
              value = if (length(m) > 1) range(m) else m,
              value_type = if (length(m) > 1) c("minimum", "maximum") else "mean",
              cq = NA_character_,
              unit = "{count}"
            ),
            raw,
            entity = ent_sex
          )
          use(i)
        }
        col_i <- which(
          !str_detect(pl, "hair|tomentose|pubescent") &
          (!str_detect(pl, "calyx") | str_detect(pl, "petals")) &
            !(seq_along(pl) %in% inf_i)
        )
        col_text <- str_remove(pieces[col_i], "(?i).*with [a-z]+ calyx and ")
        if (length(col_i)) {
          ok <- cat_rows("flower_colour", paste(col_text, collapse = ", "), raw, ent_sex)
          if (ok) for (i in col_i) if (!is.null(score(str_remove(pieces[i], "(?i).*with [a-z]+ calyx and "), "flower_colour"))) use(i)
        }
        for (i in all_idx) {
          l <- len_of(pieces[i], "long", "mm|cm")
          if (!is.null(l) && str_detect(pl[i], "^(flowers? )?(\\(\\w+\\) )?(c\\. )?[\\d(]")) {
            num_rows("flower_length", l, raw, entity = ent_sex)
            use(i)
          }
          st <- str_match(pieces[i], paste0("^stalk (", rng_re, " mm) long"))[, 2]
          if (!is.na(st)) {
            num_rows("pedicel_length", flora_num_rows(flora_parse_range(st)), raw, entity = ent_sex)
            use(i)
          }
        }
      }
    }

    for (i in which(str_detect(pl, "^pedicels? "))) {
      l <- len_of(pieces[i])
      if (!is.null(l)) {
        num_rows("pedicel_length", l, raw, entity = ent_sex)
        use(i)
      }
    }

    if (org == "calyx") {
      for (i in all_idx) {
        l <- len_of(pieces[i])
        if (!is.null(l) && str_detect(pl[i], "^(sepals|calyx)")) {
          num_rows("flower_sepal_length", l, raw, entity = ent_sex)
          use(i)
        }
      }
      if (str_detect(str_to_lower(work), "^sepals and petals free")) {
        emit(
          "flower_perianth_fusion_categorical",
          tibble(cq = NA_character_, trait_value = "sepals_and_petals_free", trait_value_clean = "perianth_parts_unfused", value_type = "mode", unit = NA_character_),
          raw
        )
        used[1] <- TRUE
      }
    }

    if (org == "petals") {
      ok <- !str_detect(pl, hair_re)
      cat_on("flower_colour", which(ok), ent_sex)
      for (i in all_idx) {
        l <- len_of(pieces[i])
        if (!is.null(l)) {
          num_rows("flower_petal_length", l, raw, entity = ent_sex)
          use(i)
        }
      }
    }

    if (org == "stamens" && !str_detect(pl[1], "^rudimentary")) {
      # "stamens 3 in male flowers and 6 in female flowers": female ones sterile
      p1 <- str_remove(pieces[1], " and \\d+ in female flowers.*$")
      e <- if (str_detect(p1, "in male flowers")) "male flowers" else ent_sex
      sc <- str_match(p1, paste0("^stamens? (", num, ")(?:(?:–| or )(", num, "))?\\b"))
      if (!is.na(sc[1, 1]) && !identical(e, "female flowers")) {
        v <- as.numeric(na.omit(sc[1, 2:3]))
        rare <- str_match(work, "(rarely|occasionally) only (\\d+)")
        nr <- tibble(
          value = if (length(v) > 1) range(v) else v,
          value_type = if (length(v) > 1) c("minimum", "maximum") else "mean",
          cq = if (!is.na(rare[1, 1])) "usually" else NA_character_,
          unit = "{count}"
        )
        if (!is.na(rare[1, 1])) {
          nr <- bind_rows(nr, tibble(value = as.numeric(rare[1, 3]), value_type = "mean", cq = rare[1, 2], unit = "{count}"))
          used[str_detect(pl, "only \\d")] <- TRUE
        }
        num_rows("flower_fertile_stamens_count", nr, raw, entity = e)
        if (!str_detect(pieces[1], "and \\d+ in female")) use(1)
      }
      for (i in which(str_detect(pl, "^filaments"))) {
        l <- len_of(pieces[i])
        if (!is.null(l)) {
          num_rows("flower_filament_length", l, raw, entity = ent_sex)
          use(i)
        }
      }
      for (i in which(str_detect(pl, "^anthers"))) {
        cat_on("flower_anther_dehiscence", i)
      }
    }
    if (org == "filaments") {
      l <- len_of(work)
      if (!is.null(l)) {
        num_rows("flower_filament_length", l, raw, entity = ent_sex)
        used[1] <- TRUE
      }
    }
    if (org == "anthers") {
      cat_on("flower_anther_attachment", all_idx)
      cat_on("flower_anther_dehiscence", all_idx)
    }

    if (org == "ovary" && !str_detect(pl[1], "rudimentary")) {
      for (i in all_idx) {
        cc <- str_match(pieces[i], "(\\d)(?:–(\\d))?(?:\\(–(\\d)\\))?-celled")
        if (!is.na(cc[1, 1])) {
          v <- as.numeric(na.omit(cc[1, 2:3]))
          nr <- tibble(
            value = if (length(v) > 1) range(v) else v,
            value_type = if (length(v) > 1) c("minimum", "maximum") else "mean",
            cq = if (!is.na(cc[1, 4])) "usually" else NA_character_,
            unit = "{count}"
          )
          if (!is.na(cc[1, 4])) {
            nr <- bind_rows(nr, tibble(value = as.numeric(cc[1, 4]), value_type = "minimum", cq = "extreme_range", unit = "{count}"))
          }
          num_rows(
            "flower_structural_carpels_count",
            nr,
            raw,
            note = "Ovary cell (locule) number scored as carpel number (one locule per carpel assumed)."
          )
          use(i)
        }
        ov <- str_match(pieces[i], "each cell with (\\d) ")
        if (!is.na(ov[1, 1])) {
          num_rows(
            "flower_ovules_per_functional_carpel_count",
            tibble(value = as.numeric(ov[1, 2]), value_type = "mean", cq = NA_character_, unit = "{count}"),
            raw
          )
          use(i)
        }
      }
      cat_on("flower_ovary_position", all_idx)
      cat_on("flower_gynoecium_placentation", all_idx)
    }

    if (org %in% c("fruits", "pericarp")) {
      e <- if (immature) "immature fruit" else NA_character_
      cat_on("fruit_type", all_idx)
      dry_i <- which(!str_detect(pl, "when dry|drying|dried"))
      cat_on("fruit_fleshiness", dry_i)
      cat_on("fruit_dehiscence", all_idx)
      hi <- which(str_detect(pl, hair_re) & !str_detect(pl, "calyx|young|inside"))
      cat_on("fruit_surface_hairs", hi, e)
      if (!immature) {
        # fresh colour only: "light yellow when fresh drying black" -> light
        # yellow; "black on drying" -> nothing
        fresh <- pieces |>
          str_remove("\\s*\\b(drying|dried)\\b.*$")
        fresh[str_detect(pl, "on drying|when dry")] <- ""
        ci <- which(!str_detect(pl, "hair|tomentose|calyx|inside|stigma|bloom|wax|\\bsap\\b|latex") & fresh != "")
        ct <- paste(fresh[ci], collapse = ", ") |>
          str_remove_all("when (fresh|ripe)") |>
          str_replace("(?i).*\\bripen(ing|s)?( to)?\\s", "") |>
          str_replace("(?i).*\\band then\\s", "")
        if (length(ci) && cat_rows("fruit_colour", ct, raw)) {
          for (i in ci) if (!is.null(score(fresh[i], "fruit_colour"))) use(i)
        }
      }
      if (org == "fruits") {
        for (i in which(str_detect(pieces, "×"))) {
          d <- flora_parse_dims(pieces[i])
          if (!is.null(d)) {
            num_rows("fruit_length", d$len, raw, entity = e)
            num_rows("fruit_width", d$wid, raw, entity = e)
            use(i)
          }
        }
        for (i in which(!used & !str_detect(pl, "stalk|calyx|branches"))) {
          l <- len_of(pieces[i], "long")
          if (!is.null(l)) {
            num_rows("fruit_length", l, raw, entity = e)
            use(i)
          }
          a <- str_extract(pieces[i], paste0(rng_re, "\\s?(mm|cm) across"))
          if (!is.na(a)) {
            nr <- flora_num_rows(flora_parse_range(str_remove(a, " across")))
            note <- "Diameter (across) of a round fruit scored as length and width."
            num_rows("fruit_length", nr, raw, entity = e, note = note)
            num_rows("fruit_width", nr, raw, entity = e, note = note)
            use(i)
          }
        }
      }
    }
    if (org == "testa") {
      cat_on("seed_colour", all_idx)
    }
    if (org == "seeds") {
      cat_on("dispersal_appendage", all_idx)
    }
    if (org == "germination") {
      cat_on("seedling_germination_location", all_idx)
    }

    # every piece no trait used stays as an unmapped record; a clause with
    # nothing scored stays whole
    if (!any(used)) {
      unmapped(org, piece_char(org, work), raw)
    } else {
      for (i in which(!used)) {
        unmapped(org, piece_char(org, pieces[i]), pieces[i])
      }
    }
  }

  sex <- NA_character_
  sentences <- flora_split_sentences(txt)
  cursor <- 1
  sen_organ <- NA_character_
  for (sen in sentences) {
    sen_pos <- coalesce(
      str_locate(str_sub(txt, cursor), fixed(str_sub(sen, 1, 40)))[1, 1] +
        cursor -
        1,
      cursor
    )
    cursor <- sen_pos
    sen <- str_remove(sen, "\\.$")
    # "Flowers (male): calyx ..." / "Leaves: rachis ..."
    lab <- str_match(sen, "^(Flowers|Leaves)(?: \\((male|female)\\))?:\\s*")
    if (!is.na(lab[1, 1])) {
      sen_organ <- if (lab[1, 2] == "Flowers") "flowers" else "leaves"
      sex <- lab[1, 3]
      sen <- str_remove(sen, fixed(lab[1, 1]))
    } else {
      sen_organ <- coalesce(detect_organ(sen), sen_organ)
      sex <- str_match(sen, "^[A-Z][a-z]+ \\((male|female)\\)")[, 2]
    }
    if (str_detect(sen, "^(Inflorescences? and flowers?|Fruits?) unknown|unknown$")) {
      next
    }
    clauses <- str_split(sen, ";\\s*")[[1]]
    prev_org <- sen_organ
    for (cl in clauses) {
      cur_pos <- coalesce(
        str_locate(str_sub(txt, sen_pos), fixed(str_sub(cl, 1, 30)))[1, 1] +
          sen_pos -
          1,
        sen_pos
      )
      org <- coalesce(detect_organ(cl), prev_org)
      prev_org <- org
      # a genus clause about named species ("germination in S. laevigata
      # ... epigeal") is not a genus value; those go in manual_rows.csv
      if (rank == "genus" && str_detect(cl, "\\b[A-Z]\\. [a-z]{3,}")) {
        unmapped(coalesce(org, "description"), "statement about named species", cl)
        next
      }
      if (is.na(org)) {
        unmapped("description", "unparsed clause", cl)
        next
      }
      process_clause(cl, org)
    }
  }

  list(
    rows = bind_rows(rows) |> mutate(block = block, rank = rank),
    unmapped = bind_rows(un) |> mutate(block = block, rank = rank)
  )
}

# ---- run the parser ---------------------------------------------------------------

# The family description has stated exceptions ("except in Garuga"); its
# values are written by hand in raw/manual_rows.csv. Genus and species
# descriptions are parsed.
desc <- paras |> filter(section == "Description", rank != "family")
# source repairs, scoped by printed page (raw/source_fixes.csv)
desc$text <- flora_apply_fixes(desc$text, fixes, desc$printed_page)
parsed <- map(seq_len(nrow(desc)), \(i) parse_description(desc[i, ]))
own <- map_dfr(parsed, "rows")
unm <- map_dfr(parsed, "unmapped")

# Month statements in Ecology paragraphs (species and the single-infra taxa).
eco <- paras |> filter(section == "Ecology", !rank %in% c("family", "genus"))
pheno <- map_dfr(seq_len(nrow(eco)), \(i) {
  e <- eco[i, ]
  s <- str_replace_all(e$text, "\u2013\\s+", "\u2013")
  fl <- str_match(s, "(Flower(?:s|ing)[^.]*?)(?=,? and fruit|\\.|$)")[, 2]
  fr <- str_match(s, "((?:[Ff]ruit(?:s|ing))[^.]*)")[, 2]
  out <- list()
  for (k in list(c("flowering_time", fl), c("fruiting_time", fr))) {
    cal <- if (is.na(k[2])) NA_character_ else calendar(k[2])
    if (!is.na(cal)) {
      pos <- str_locate(e$text, fixed(str_sub(k[2], 1, 12)))[1, 1]
      pg <- flora_page_at(e, coalesce(pos, 1L))
      out[[length(out) + 1]] <- tibble(
        block = e$block,
        rank = e$rank,
        trait = k[1],
        cq = NA_character_,
        trait_value = cal,
        trait_value_clean = cal,
        value_type = "mode",
        unit = NA_character_,
        trait_value_raw = k[2],
        entity_measured = NA_character_,
        notes = "Months of flowering or fruiting collections, as printed.",
        pdf_page = pg[["pdf"]],
        printed_page = pg[["printed"]],
        source_section = "Ecology"
      )
    }
  }
  bind_rows(out)
})

# Ecology, Distribution, Uses, Timber and infraspecific annotation paragraphs:
# no mapped trait applies, so each is kept whole as an unmapped record.
sec_unm <- paras |>
  filter(
    section %in% c("Ecology", "Distribution", "Uses", "Timber", "Infraspecific annotation")
  ) |>
  transmute(
    block,
    rank,
    organ = NA_character_,
    character = case_when(
      section == "Ecology" ~ "habitat / altitude / phenology text",
      section == "Distribution" ~ "distribution",
      section == "Uses" ~ "uses",
      section == "Timber" ~ "timber properties",
      TRUE ~ "infraspecific annotation (distribution, habitat)"
    ),
    trait_value_raw = text,
    pdf_page,
    printed_page,
    source_section = section
  )

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c"),
  na = ""
)

# ---- members and higher-rank values ------------------------------------------------

# Taxa that carry a description: the species, plus the two taxa whose only
# infraspecific taxon in the region holds the description. Infraspecific
# annotations under a species description have no description of their own
# (TFSS guide) and do not inherit the species description.
described <- desc |> filter(rank != "genus") |> distinct(block, rank)
members <- described |>
  transmute(
    block,
    genus = word(block, 1),
    group = NA_character_,
    species = word(block, 1, 2),
    rank
  )

own_rows <- own |>
  mutate(
    partial = coalesce(partial, FALSE),
    source_section = if_else(rank == "genus", "Genus description", "Description")
  )
manual_rows <- manual |>
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
    population_region,
    source_id,
    partial = str_detect(coalesce(notes, ""), "Not copied down")
  )
higher <- bind_rows(
  own_rows |> filter(rank == "genus"),
  manual_rows |> filter(rank %in% c("family", "genus"))
) |>
  filter(!partial)
species_rows <- bind_rows(
  own_rows |> filter(rank != "genus"),
  manual_rows |> filter(!rank %in% c("family", "genus"))
)
copied <- flora_copy_down(higher, species_rows, members)
rejected <- attr(copied, "rejected")

# Family statements with a named exception, applied to members outside it:
# "Fruit a drupe ... or in Triomma a woody capsule"; "The drupaceous fruits are
# dispersed by animals." Only to taxa silent on the trait after copy-down.
except_rows <- manual_rows |>
  filter(rank == "family", str_detect(notes, "Not copied down.*except")) |>
  pmap_dfr(\(...) {
    r <- tibble(...)
    excl <- str_match(r$notes, "except ([A-Z][a-z]+)")[, 2]
    targets <- setdiff(
      members$block[members$genus != excl],
      bind_rows(species_rows, copied) |> filter(trait == r$trait) |> pull(block)
    )
    tibble(block = targets) |>
      mutate(
        trait = r$trait,
        cq = NA_character_,
        trait_value = r$trait_value,
        trait_value_clean = r$trait_value_clean,
        value_type = "mode",
        unit = NA_character_,
        trait_value_raw = r$trait_value_raw,
        entity_measured = r$entity_measured,
        notes = paste0("Copied from the Burseraceae ", str_to_lower(r$source_section), " (all genera except ", excl, ")."),
        pdf_page = r$pdf_page,
        printed_page = r$printed_page,
        rank_scored_from = "family",
        source_section = r$source_section
      )
  })

# ---- assemble ------------------------------------------------------------------------

to_td <- function(d) {
  d |>
    mutate(
      taxon_name = block,
      taxon_rank = rank,
      commonness_qualifier = cq,
      form = NA_character_
    )
}
rank_of_block <- bind_rows(
  paras |> distinct(block, rank),
  members |> select(block, rank)
) |>
  distinct(block, .keep_all = TRUE)

unmapped_rows <- bind_rows(
  unm |> mutate(source_section = if_else(rank == "genus", "Genus description", "Description")),
  sec_unm
) |>
  transmute(
    block,
    rank,
    trait = NA_character_,
    cq = NA_character_,
    trait_value = NA_character_,
    trait_value_clean = NA_character_,
    value_type = "mode",
    unit = NA_character_,
    trait_value_raw,
    entity_measured = NA_character_,
    notes = paste0(
      "UNMAPPED: ",
      if_else(is.na(organ), character, paste(organ, character, sep = " | "))
    ),
    pdf_page,
    printed_page,
    source_section
  )

td <- bind_rows(
  own_rows |>
    mutate(rank_scored_from = if_else(rank == "genus", "genus", NA_character_)),
  pheno,
  copied,
  except_rows |> mutate(rank = NA_character_),
  manual_rows,
  unmapped_rows
) |>
  select(-any_of("rank")) |>
  left_join(rank_of_block, by = "block") |>
  mutate(
    rank = if_else(block == "Burseraceae", "family", rank),
    population_region = case_when(
      !is.na(population_region) ~ population_region,
      rank %in% c("family", "genus") ~ NA_character_,
      coalesce(rank_scored_from, "") %in% c("family", "genus") ~ NA_character_,
      TRUE ~ region
    ),
    source_id = coalesce(source_id, NA_character_)
  ) |>
  to_td() |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits) |>
  # month ranges and other text ranges: a--b, like numeric ranges
  mutate(across(c(trait_value_raw, notes), \(x) str_replace_all(x, "\\s?\u2013\\s?", "--"))) |>
  arrange(
    factor(taxon_rank, c("family", "genus", "species", "subspecies", "variety", "forma")),
    taxon_name,
    is.na(trait),
    trait,
    pdf_page
  )
write_csv(td, file.path(raw_dir, "trait_data.csv"), na = "")
flora_unmapped_summary(td) |>
  write_csv(file.path(raw_dir, "unmapped_terms.csv"), na = "")

# ---- taxon flags ----------------------------------------------------------------------

flags <- paras |>
  filter(!rank %in% c("family", "genus")) |>
  distinct(block, rank) |>
  left_join(td |> count(taxon_name, name = "n_rows"), by = c("block" = "taxon_name")) |>
  transmute(
    taxon_name = block,
    taxon_rank = rank,
    status = "accepted",
    n_rows = coalesce(n_rows, 0L),
    flag = case_when(
      block == "Canarium asperum" ~
        "Species heading only; the description belongs to its one infraspecific taxon in the region (subsp. asperum var. asperum).",
      block %in% single_infra ~
        "Only infraspecific taxon in the region; its description is the local description of the species (TFSS guide).",
      rank %in% c("subspecies", "variety", "forma") ~
        "Infraspecific annotation under a species description (key lead, nomenclature, distribution); no description; species values not inherited.",
      block == "Canarium hirsutum" ~
        "Note names subsp. hirsutum var. hirsutum and forma scabrum in the region without headings; scored at species rank.",
      TRUE ~ NA_character_
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")

if (nrow(rejected)) {
  message("Higher-rank values not copied down (agreement test):")
  print(rejected, n = Inf)
}
