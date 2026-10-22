# Coode_2018: extract traits from the Flora of Peninsular Malaysia
# Elaeocarpaceae treatment (M.J.E. Coode 2018, FPM Series II vol. 7: 33--126).
#
# Writes (run from the repository root):
#   data/Coode_2018/raw/trait_data.csv      long format, traits in config/traits.yml only
#   data/Coode_2018/raw/unmapped_terms.csv  characters with no traits.yml trait (suggestions)
#   data/Coode_2018/raw/taxon_flags.csv     taxa processed, status and name flags
#
# Values come from the description paragraphs (clause parser below), from vetted
# ecology / phenology / pollination statements (raw/manual_rows.csv), and from
# unhedged family / genus / group statements copied to member taxa. Keys,
# maps, plates and the index are not read. Names are kept as printed; matching
# to a backbone is a separate step.

library(tidyverse)
source("data/Coode_2018/raw/segment_source.R")

traits <- yaml::read_yaml("config/traits.yml")$traits$elements
checked <- "Candidate trait, not in config/traits.yml (checked 2026-10)."

# ---- text helpers -----------------------------------------------------------

norm <- function(x) {
  x |>
    # Source fix (p. 68, E. fraseri): lamina "14–18.5 × (7–)9–12" lacks its unit; cm as in every other lamina.
    str_replace_all("14\u2013 ?18\\.5 \u00d7 \\(7\u2013\\)9\u201312,", "14\u201318.5 \u00d7 (7\u2013)9\u201312 cm,") |>
    str_replace_all("\\) by \\(|(?<=\\d) by (?=[\\d(])", function(m) if (str_detect(m, "^\\)")) ") \u00d7 (" else " \u00d7 ") |>
    str_replace_all("\\s*\\(including acumen\\)", "") |>
    str_replace_all("\\(\u2013(\\d+(?:\\.\\d+)?) (cm|mm)\\) long", "(\u2013\\1) \\2 long") |>
    str_replace_all("\\(\u2013(\\d+(?:\\.\\d+)?) (cm|mm)\\)", "(\u2013\\1)") |>
    str_replace_all("^leaves sessile or petioles|(?<=; )leaves sessile or petioles", "petioles") |>
    str_replace_all("\u00ad", "") |>
    str_replace_all("\u2013 ", "\u2013") |> # "elliptic– obovate"
    str_replace_all("(?<=\\d)\\s?\u2013\\s?(?=\\d)", "\u2013") |>
    str_replace_all("\\bs ize\\b", "size") |>
    str_replace_all("axi1s", "axils") |>
    str_squish()
}

# Sentence split protecting abbreviations ("c. 2", "E. glaber", "e.g.").
split_sentences <- function(x) {
  str_split(
    x,
    "(?<!\\bc|\\b[A-Z]|e\\.g|\\bsp|subsp|var|Fig|\\bca)\\.\\s+(?=[A-Z])"
  )[[1]]
}

qual_words <- c(
  "usually",
  "often",
  "sometimes",
  "rarely",
  "occasionally",
  "mostly",
  "generally",
  "frequently",
  "less often",
  "very rarely"
)
qual_re <- paste0("\\b(", paste(qual_words, collapse = "|"), ")\\b")
hedge_re <- "\\b(probably|perhaps|apparently|possibly|presumably)\\b"

# ---- numbers ----------------------------------------------------------------

num <- "\\d+(?:\\.\\d+)?"
# (a–)b–c(–d) with optional "c." and an optional unit
range_re <- paste0(
  "(?:\\((",
  num,
  ")\u2013\\)\\s?)?", # 1 extreme min
  "(c\\.\\s?|to\\s)?", # 2 c. / to
  "(",
  num,
  ")", # 3 low
  "(?:\u2013(",
  num,
  "))?", # 4 high
  "(?:\\s?\\(\u2013(",
  num,
  ")\\))?", # 5 extreme max
  "\\s?(mm|cm|m)?\\b" # 6 unit
)

parse_range <- function(s) {
  m <- str_match(s, range_re)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  tibble(
    xmin = as.numeric(m[, 2]),
    approx = m[, 3],
    lo = as.numeric(m[, 4]),
    hi = as.numeric(m[, 5]),
    xmax = as.numeric(m[, 6]),
    unit = m[, 7]
  )
}

# Rows for one numeric value: typical range, extremes, "c." means, "to" maxima.
num_rows <- function(r, unit = r$unit) {
  out <- list()
  ap <- coalesce(str_trim(r$approx), "")
  if (ap == "c.") {
    out[[1]] <- tibble(value = r$lo, value_type = "mean", cq = NA_character_)
  } else if (ap == "to") {
    out[[1]] <- tibble(value = r$lo, value_type = "maximum", cq = NA_character_)
  } else if (is.na(r$hi)) {
    # single typical value, with or without extremes: one mean row
    out[[1]] <- tibble(value = r$lo, value_type = "mean", cq = NA_character_)
  } else {
    out[[1]] <- tibble(
      value = c(r$lo, r$hi),
      value_type = c("minimum", "maximum"),
      cq = NA_character_
    )
  }
  ext <- !is.na(r$xmin) || !is.na(r$xmax)
  if (ext) {
    out[[1]]$cq <- "usually"
    if (!is.na(r$xmin)) {
      out[[2]] <- tibble(
        value = r$xmin,
        value_type = "minimum",
        cq = "extreme_range"
      )
    }
    if (!is.na(r$xmax)) {
      out[[3]] <- tibble(
        value = r$xmax,
        value_type = "maximum",
        cq = "extreme_range"
      )
    }
  }
  bind_rows(out) |> mutate(unit = unit)
}

# Dimensions "a–b × c–d cm" (extremes allowed on each side; unit from the end).
parse_dims <- function(s) {
  s2 <- str_remove_all(s, "\\((?![\u2013\\d])[^)]*\\)") # drop "(including acumen)"
  m <- str_match(
    s2,
    paste0(
      "(\\(?[\\d.\u2013()c ]+?)\\s?\u00d7\\s?(\\(?[\\d.\u2013()c ]+?)\\s?(mm|cm|m)\\b"
    )
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  a <- parse_range(paste0(str_squish(m[, 2]), " ", m[, 4]))
  b <- parse_range(paste0(str_squish(m[, 3]), " ", m[, 4]))
  if (is.null(a) || is.null(b)) {
    return(NULL)
  }
  list(len = num_rows(a, m[, 4]), wid = num_rows(b, m[, 4]))
}

# ---- categorical dictionaries -------------------------------------------------
# term regex -> traits.yml level ("" = recorded, no level). Order = priority.

dict <- list(
  plant_growth_form = c(
    "\\btrees?\\b|\\btreelets?\\b" = "tree",
    "\\bshrubs?\\b" = "shrub"
  ),
  stem_base_form = c("buttress" = "buttressed", "\\bfluted\\b" = "fluted"),
  root_system_type = c("stilt[ -]root" = "stilt_roots"),
  bark_texture = c(
    "\\bsmooth\\b" = "smooth",
    "\\brough\\b" = "rough",
    "\\bfissured\\b|\\bfurrowed\\b" = "furrowed",
    "\\bflaky\\b|\\bflaking\\b" = "flaky",
    "\\bcorky\\b" = "corky",
    "\\blenticellate\\b" = "",
    "\\bgranular\\b" = ""
  ),
  bark_colour = c(
    "dark brown" = "brown_dark",
    "\\bbrown\\b" = "brown",
    "\\bgrey\\b|\\bgray\\b" = "grey",
    "\\bwhitish\\b|\\bwhite\\b" = "white"
  ),
  stem_hairs = c(
    "\\bglabrous\\b|\\bglabrescent\\b" = "glabrous",
    "hairy|pubescent|puberulous|puberulent|velvety|tomentose|silky|villous|hirsute|felty" = "hairy"
  ),
  leaf_arrangement = c(
    "spiral" = "spiral",
    "crowded" = "crowded",
    "scattered" = "scattered",
    "grouped|clustered|bunches" = "clustered"
  ),
  leaf_phyllotaxis = c(
    "subopposite" = "",
    "\\balternate\\b" = "alternate",
    "\\bopposite\\b" = "opposite"
  ),
  leaf_shape = c(
    "narrow(ly)?[- ]elliptic" = "narrowly_elliptical",
    "broad(ly)?[- ]elliptic" = "widely_elliptical",
    "narrow(ly)?[- ]obovate" = "narrowly_obovate",
    "broad(ly)?[- ]obovate" = "widely_obovate",
    "narrow(ly)?[- ]ovate" = "narrowly_ovate",
    "broad(ly)?[- ]ovate" = "widely_ovate",
    "narrow(ly)?[- ]oblong" = "narrowly_oblong",
    "ovate-elliptic|elliptic-ovate|obovate-elliptic|elliptic[- ]obovate|oblong-elliptic|elliptic-oblong|obovate-oblong|oblong-obovate|obovate-panduriform|ovate-panduriform|ovate-lanceolate" = "",
    "oblanceolate" = "oblanceolate",
    "lanceolate" = "lanceolate",
    "obovate" = "obovate",
    "ovate" = "ovate",
    "elliptic" = "elliptical",
    "oblong" = "oblong",
    "orbicular|round" = "orbicular",
    "spathulate" = "spathulate",
    "linear" = "linear",
    "panduriform" = ""
  ),
  leaf_base_shape = c(
    "cuneate" = "cuneate",
    "rounded" = "rounded",
    "cordate" = "cordate",
    "truncate" = "truncate",
    "attenuate" = "attenuate",
    "obtuse" = "obtuse",
    "\\bacute\\b" = "acute",
    "decurrent" = "",
    "oblique|asymmetric" = "oblique",
    "peltate" = ""
  ),
  leaf_margin = c(
    "subentire|more-or-less entire|nearly entire" = "",
    "\\bentire\\b" = "entire",
    "serrate|serrulate" = "toothed_serrate",
    "crenate|crenulate" = "toothed_crenate",
    "dentate|denticulate" = "toothed_dentate",
    "toothed" = "toothed",
    "sinuate|undulate" = ""
  ),
  leaf_apex_shape = c(
    "acuminate|acumen" = "acuminate",
    "\\bacute\\b|subacute" = "acute",
    "obtuse" = "obtuse",
    "rounded" = "rounded",
    "apiculate" = "apiculate",
    "emarginate|retuse" = "",
    "cuspidate" = ""
  ),
  leaf_surface_reflectivity = c(
    "\\bdull\\b" = "dull",
    "glossy|shiny|shining" = "shiny"
  ),
  leaf_hairs_adult_leaves = c(
    "\\bglabrous\\b|\\bglabrescent\\b" = "glabrous",
    "hairy|pubescent|puberulous|puberulent|velvety|tomentose|silky|villous|felty|adpressed-hairy" = "hairy"
  ),
  inflorescence_type = c(
    "raceme|racemose" = "raceme",
    "cyme|cymose" = "cyme",
    "panicle|paniculate" = "panicle",
    "fascicle|fasciculate" = "",
    "solitary" = "solitary",
    "axillary" = "axillary",
    "terminal" = "terminal"
  ),
  flower_structural_sex_type = c(
    "bisexual" = "bisexual", "unisexual" = "unisexual",
    "\\bmale\\b" = "unisexual"
  ),
  flower_colour = c(
    "(?<![a-z])(white|whitish|cream|creamy)\\b" = "white_cream",
    "(?<![a-z])(yellow|yellowish)(?![a-z-]*\\s?white)" = "yellow_orange",
    "(?<![a-z])(green|greenish)(?![a-z-]*\\s?white)" = "green",
    "(?<![a-z])(pink|pinkish)\\b" = "pink",
    "(?<![a-z])(red|reddish)\\b" = "red_brown"
  ),
  flower_scent_production = c("fragrant|scented|perfumed" = "scent_produced"),
  flower_orientation = c(
    "face[- ]down|pendent|pendulous" = "down",
    "face[- ]up" = "up"
  ),
  flower_perianth_fusion_categorical = c(
    "^sepals free|^petals free" = "perianth_parts_unfused"
  ),
  flower_anther_attachment = c("basifixed" = "basifixed"),
  flower_anther_dehiscence = c(
    "apical pores" = "poricidal",
    "slits" = "short_apical_slits"
  ),
  flower_ovary_position = c("superior" = "superior"),
  fruit_type = c("drupe" = "drupe", "capsule" = "capsule"),
  fruit_dehiscence = c(
    "dehiscent|loculicidal" = "dehiscent",
    "indehiscent" = "indehiscent"
  ),
  fruit_colour = c(
    "(?<![a-z])(blue|bluish)" = "blue",
    "(?<![a-z])(green|greenish)" = "green",
    "(?<![a-z])(red|reddish)" = "red",
    "(?<![a-z])black" = "black",
    "(?<![a-z])(yellow|yellowish)" = "yellow",
    "(?<![a-z])(purple|purplish)" = "purple",
    "(?<![a-z])brown" = "brown",
    "(?<![a-z])(orange)" = "orange"
  ),
  fruit_surface_hairs = c(
    "\\bglabrous\\b" = "glabrous",
    "hairy|pubescent|puberulous|velvety|tomentose|silky" = "hairy"
  ),
  seed_colour = c(
    "(?<![a-z])black" = "black",
    "(?<![a-z])(brown|red|reddish)" = "red_brown"
  ),
  dispersal_appendage = c(
    "\\baril\\b|arillate" = "aril",
    "sarcotesta" = "sarcotesta"
  )
)

# ---- organ detection -----------------------------------------------------------

organ_re <- c(
  habit = "^(small|medium|big|large|tall|shrubs?|trees?|treelet)",
  bole = "^bole",
  bark = "^bark",
  sapwood = "^sapwood",
  twigs = "^twigs?",
  buds = "^(terminal )?buds?",
  stipules = "^stipules?",
  leaves = "^leaves|^leaf\\b",
  petioles = "^petioles?",
  lamina = "^lamina",
  midrib = "^midrib",
  veins = "^(lateral )?veins|^venation",
  domatia = "^domatia|^without domatia",
  inflorescence = "^(racemes?|inflorescences?)",
  axis = "^(axis|rachis)",
  bracts = "^bracts",
  bracteoles = "^bracteoles",
  pedicels = "^(flowering )?pedicels?",
  peduncle = "^peduncles?",
  flowers = "^flowers?",
  sepals = "^sepals?",
  petals = "^petals?",
  nectary = "^nectary",
  stamens = "^stamens",
  filaments = "^filaments",
  anthers = "^anthers",
  ovary = "^ovar(y|ies)",
  ovules = "^ovules",
  style = "^styles?",
  fruits = "^fruits?|^capsules?",
  stalk = "^stalk",
  mesocarp = "^mesocarp",
  stones = "^stones?",
  seeds = "^seeds?",
  valves = "^valves",
  indumentum = "^indumentum"
)
detect_organ <- function(x) {
  x <- str_to_lower(str_remove(x, "^(flowers|leaf|leaves):\\s*"))
  hit <- names(organ_re)[str_detect(x, organ_re)]
  if (length(hit)) hit[1] else NA_character_
}

# Lamina sub-features introduced by a comma piece.
sub_re <- c(
  base = "^base\\b|^(lamina )?base\\b",
  margin = "^margins?\\b",
  apex = "^apex\\b",
  tip = "^tip\\b",
  above = "^(dull|glossy|shiny)",
  beneath = "beneath$"
)

organ_traits <- list(
  habit = c("plant_growth_form"),
  bole = c("stem_base_form", "root_system_type"),
  bark = c("bark_texture", "bark_colour"),
  twigs = c("stem_hairs"),
  leaves = c("leaf_arrangement", "leaf_phyllotaxis"),
  inflorescence = c("inflorescence_type"),
  flowers = c(
    "flower_structural_sex_type",
    "flower_colour",
    "flower_scent_production",
    "flower_orientation"
  ),
  petals = c("flower_colour"),
  anthers = c("flower_anther_attachment", "flower_anther_dehiscence"),
  ovary = c("flower_ovary_position"),
  fruits = c(
    "fruit_type",
    "fruit_dehiscence",
    "fruit_colour",
    "fruit_surface_hairs"
  ),
  seeds = c("seed_colour", "dispersal_appendage")
)

# ---- categorical scoring with commonness rows ----------------------------------


score_cat <- function(txt, trait) {
  d <- dict[[trait]]
  # Commas and "or/and/to <qualifier>" separate alternatives.
  chunks <- unlist(str_split(txt, paste0(",\\s*|\\s(?=(or|and|to) (", paste(qual_words, collapse = "|"), ")\\b)")))
  out <- list()
  for (ci in seq_along(chunks)) {
    ch <- chunks[ci]
    if (str_detect(ch, hedge_re)) next
    q <- str_match(ch, qual_re)[, 2]
    work <- ch
    for (k in seq_along(d)) {
      repeat {
        loc <- str_locate(work, regex(names(d)[k], ignore_case = TRUE))
        if (is.na(loc[1, 1])) break
        out[[length(out) + 1]] <- tibble(ci = ci, pos = loc[1, 1], term = str_sub(work, loc[1, 1], loc[1, 2]), level = d[[k]], cq = q)
        str_sub(work, loc[1, 1], loc[1, 2]) <- strrep("#", loc[1, 2] - loc[1, 1] + 1)
      }
    }
  }
  if (!length(out)) return(NULL)
  r <- bind_rows(out) |>
    arrange(ci, pos) |>
    mutate(term = str_replace_all(str_to_lower(term), " ", "_"), cq = if (any(!is.na(cq))) coalesce(cq, "usually") else cq)
  r |>
    group_by(cq) |>
    summarise(
      trait_value = paste(unique(term), collapse = " "),
      trait_value_clean = paste(unique(level[level != ""]), collapse = " "),
      first = min(ci * 1000 + pos),
      .groups = "drop"
    ) |>
    arrange(cq != "usually" | is.na(cq), first) |>
    mutate(trait = trait) |>
    select(-first)
}

# Mesocarp thickness: "mesocarp usually 2-4 mm thick", "2 mm thick or less",
# "to 2 mm thick". "or less" / "to" give a maximum; "usually" a usually row.
meso_rows <- function(s) {
  m <- str_match(s, paste0("(?i)mesocarp[^,;]*?(usually )?((?:to |c\\. )?", num, "(?:\u2013", num, ")?)\\s?mm thick( or less| or more)?"))
  if (is.na(m[1, 1])) return(NULL)
  r <- num_rows(parse_range(paste(m[1, 3], "mm")))
  if (!is.na(m[1, 4]) && nrow(r) == 1) r$value_type <- if (str_detect(m[1, 4], "less")) "maximum" else "minimum"
  if (!is.na(m[1, 2])) r$cq <- "usually"
  r
}

# Colour words apply to the clause organ only: stop at the first comma piece
# naming another organ, and keep only the final state of "X ripening Y".
colour_scope <- function(s) {
  p <- str_split(s, ",\\s*")[[1]]
  stop_at <- which(str_detect(str_to_lower(p), "inside|valves?|mesocarp|stone|aril|calyx|sepals?|anthers?|filaments?"))
  stop_at <- stop_at[stop_at > 1]
  if (length(stop_at)) p <- p[seq_len(min(stop_at) - 1)]
  p |> paste(collapse = ", ") |>
    str_replace("(?i).*\\bripen(ing|s)?( a)?\\s", "") |>
    # "yellowish or greenish white" and "greenish white": head colour only
    str_replace_all("(?i)\\b[a-z]+ish or (?=[a-z]+ish[ -](white|grey|green|blue|black)\\b)", "") |>
    str_replace_all("(?i)\\b(yellowish|greenish|creamy|bluish|reddish)[ -](?=(white|grey|green|blue|black)\\b)", "")
}
# ---- clause parser -----------------------------------------------------------------

unmapped <- list()
note_unmapped <- function(block, organ, character, text) {
  unmapped[[length(unmapped) + 1]] <<- tibble(
    block = block,
    organ = organ,
    character = character,
    text = text
  )
}

parse_description <- function(par_row) {
  block <- par_row$block
  txt <- norm(par_row$text)
  rows <- list()
  add <- function(trait, value_df, raw, entity = NA, note = NA, pos = 1) {
    pg <- page_at(par_row, pos)
    if (is.na(entity) && !is.na(flower_sex)) entity <- paste(str_to_lower(flower_sex), "flowers")
    rows[[length(rows) + 1]] <<- value_df |>
      mutate(
        trait = trait,
        trait_value_raw = raw,
        entity_measured = entity,
        notes = note,
        pdf_page = pg[["pdf"]],
        printed_page = pg[["printed"]]
      )
  }
  add_num <- function(
    trait,
    nr,
    raw,
    unit_default = NA,
    entity = NA,
    note = NA,
    pos = 1
  ) {
    if (is.null(nr) || !nrow(nr)) {
      return()
    }
    nr <- nr |> mutate(unit = coalesce(unit, unit_default))
    add(
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
      note,
      pos
    )
  }
  add_cat <- function(trait, piece, raw, pos, entity = NA, note = NA) {
    s <- score_cat(piece, trait)
    if (is.null(s)) {
      return()
    }
    add(
      trait,
      s |> mutate(value_type = "mode", unit = NA_character_),
      raw,
      entity,
      note,
      pos
    )
  }

  flower_sex <- NA_character_
  sentences <- split_sentences(txt)
  cursor <- 1
  for (sen in sentences) {
    sen_pos <- coalesce(
      str_locate(str_sub(txt, cursor), fixed(str_sub(sen, 1, 40)))[1, 1] +
        cursor -
        1,
      cursor
    )
    cursor <- sen_pos
    sen <- str_remove(sen, "\\.$")
    sen_organ <- detect_organ(sen)
    clauses <- str_split(sen, ";\\s*")[[1]]
    for (cl in clauses) {
      cl_pos <- coalesce(
        str_locate(str_sub(txt, sen_pos), fixed(str_sub(cl, 1, 30)))[1, 1] +
          sen_pos -
          1,
        sen_pos
      )
      flower_sex <- str_match(sen, "^(Male|Female) flowers:")[, 2]
      cl <- str_remove(cl, "^((Male|Female) flowers|Flowers|Leaf|Leaves):\\s*")
      org <- coalesce(detect_organ(cl), sen_organ)
      if (is.na(org)) {
        note_unmapped(block, NA, "unparsed clause", cl)
        next
      }
      raw <- cl
      if (str_detect(cl, hedge_re)) {
        # hedged clause: score only the unhedged comma pieces
        cl_scoring <- str_split(str_remove_all(cl, "\\([^()]*\\)"), ",\\s*")[[1]] |>
          discard(\(p) str_detect(p, hedge_re)) |>
          paste(collapse = ", ")
      } else {
        cl_scoring <- cl
      }
      # drop parenthetical asides that name other taxa or specimens
      cl_scoring <- str_remove_all(
        cl_scoring,
        "\\((?:[^()]*\\b(E|S)\\. [a-z]+|[^()]*[A-Z]{2,} \\d+|[^()]*\\bin (var|subsp)\\.)[^()]*\\)"
      )
      # "in var. x ..." segments describe one infraspecific taxon only
      cl_scoring <- str_remove(cl_scoring, ",?\\s*in (var|subsp)\\. [a-z]+.*$")

      pieces <- str_split(cl_scoring, ",\\s*(?![^()]*\\))")[[1]]

      # categorical traits for the organ
      for (tr_name in organ_traits[[org]] %||% character()) {
        if (tr_name == "flower_structural_sex_type" && !str_detect(cl, "^Flowers?\\b")) next
        if (tr_name == "seed_colour" && str_detect(cl, "testa [a-z ]*(brown|black|red)") && !str_detect(cl, "^Seeds [a-z]+,")) { add_cat(tr_name, str_extract(cl, "testa [a-z ]*(brown|black|red)[a-z ]*"), raw, cl_pos); next }
        if (tr_name == "seed_colour" && str_detect(cl, "aril")) cl_scoring <- str_remove(cl_scoring, "(?i),?\\s*[a-z ]*(enclosed|covered)\\b.*$")
        if (tr_name == "stem_hairs") { if (!str_detect(cl, "^Twigs")) next; add_cat(tr_name, str_remove_all(cl_scoring, "(?i)(,\\s*)?(with |and )?[^,]*\\b(buds?|leaf[- ]scars?)\\b[^,]*"), raw, cl_pos); next }
        add_cat(tr_name, if (str_detect(tr_name, "colour")) colour_scope(cl_scoring) else cl_scoring, raw, cl_pos)
      }

      # numeric traits
      low <- str_to_lower(cl_scoring)
      first_piece <- pieces[1]
      measure_pieces <- pieces[
        !str_detect(
          str_to_lower(pieces),
          "^(with|the |teeth|divisions|awn|keel|hairs|acumen|spines|sutures|wall|locules?\\b)"
        )
      ]
      mp <- paste(measure_pieces, collapse = ", ")
      mpl <- str_to_lower(mp)
      get_len <- function(word = "long") {
        hit <- str_extract(
          mp,
          paste0(
            "(?:\\(",
            num,
            "\u2013\\)\\s?)?(?:c\\.\\s?|to\\s)?",
            num,
            "(?:\u2013",
            num,
            ")?(?:\\s?\\(\u2013",
            num,
            "\\))?\\s?(mm|cm|m)\\s",
            word
          )
        )
        if (is.na(hit)) NULL else num_rows(parse_range(hit))
      }
      usually_par <- str_match(
        cl_scoring,
        paste0("\\((usually|mostly) (c\\. )?(",
        num,
        ")\\s?(mm|cm)?\\)")
      )

      if (org == "habit" || (sen_organ %in% "habit" && str_detect(cl, "beginning to flower"))) {
        h <- str_extract(
          str_remove(str_replace(mp, "(?<=\\d)\u2013c\\. ", "\u2013"), "(?i)beginning to flower.*$"),
          paste0(
            "(?:\\(",
            num,
            "\u2013\\))?(?:c\\.\\s?|to\\s)?",
            num,
            "(?:\u2013",
            num,
            ")?\\s?m (tall|high)"
          )
        )
        if (!is.na(h)) {
          add_num("plant_height", num_rows(parse_range(h)), raw, pos = cl_pos)
        }
        # bole details inside the habit sentence
        for (tr_name in c("stem_base_form", "root_system_type")) {
          add_cat(tr_name, cl_scoring, raw, cl_pos)
        }
        if (str_detect(cl, "beginning to flower")) {
          note_unmapped(block, "habit", "height at first flowering", cl)
        }
      }
      if (org == "bole") {
        dbh <- str_extract(mp, paste0("to ", num, " cm diameter"))
        if (!is.na(dbh)) {
          add_num(
            "plant_diameter_breast_height",
            num_rows(parse_range(dbh)),
            raw,
            "cm",
            pos = cl_pos,
            note = "Bole diameter; height of measurement not stated."
          )
        }
        if (str_detect(low, "columnar")) {
          note_unmapped(block, "bole", "bole shape", cl)
        }
      }
      if (org == "twigs") {
        note_unmapped(block, "twigs", "twig thickness / colour / shape", cl)
      }
      if (org == "petioles") {
        add_num("petiole_length", get_len("long"), raw, pos = cl_pos)
        w <- str_extract(
          mp,
          paste0(
            "(?:c\\.\\s?)?",
            num,
            "(?:\u2013",
            num,
            ")?\\s?mm (wide|thick)"
          )
        )
        if (!is.na(w)) {
          add_num("petiole_width", num_rows(parse_range(w)), raw, pos = cl_pos)
        }
        if (str_detect(low, "swollen|geniculate|pulvin|groove|canalicul")) {
          note_unmapped(block, "petioles", "petiole swelling / grooving", cl)
        }
      }
      if (org %in% c("lamina", "leaves")) {
        # pieces: shape (first unlabelled), base, margin, apex/tip, surface
        dims <- parse_dims(mp)
        if (!is.null(dims)) {
          add_num(
            "leaf_length",
            dims$len,
            raw,
            pos = cl_pos,
            note = "Lamina length (source excludes petiole)."
          )
          add_num("leaf_width", dims$wid, raw, pos = cl_pos)
        } else if (org == "lamina") {
          add_num(
            "leaf_length",
            get_len("long"),
            raw,
            pos = cl_pos,
            note = "Lamina length (source excludes petiole)."
          )
        }
        ratio <- str_extract(
          mp,
          paste0(
            "(?:\\(",
            num,
            "\u2013\\))?(?:to )?",
            num,
            "(?:\u2013",
            num,
            ")?(?:\\(\u2013",
            num,
            "\\))? times as long as (wide|broad)"
          )
        )
        if (!is.na(ratio)) {
          add_num(
            "leaf_length_to_width_ratio",
            num_rows(parse_range(str_remove(ratio, " times.*"))) |>
              mutate(unit = "{dimensionless}"),
            raw,
            pos = cl_pos
          )
        }
        shape_piece <- pieces[
          !str_detect(
            str_to_lower(pieces),
            "^(base|margin|apex|tip|lamina base)|beneath|above|times as long|\u00d7|\\bcm\\b|^\\d"
          )
        ]
        shape_piece <- shape_piece[str_detect(
          str_to_lower(shape_piece),
          "elliptic|ovate|obovate|oblong|lanceolate|orbicular|round|spathulate|linear|panduriform"
        )]
        if (org == "lamina" && length(shape_piece)) {
          add_cat(
            "leaf_shape",
            paste(shape_piece, collapse = ", "),
            raw,
            cl_pos
          )
        }
        if (org == "leaves" && length(shape_piece)) {
          add_cat(
            "leaf_shape",
            paste(shape_piece, collapse = ", "),
            raw,
            cl_pos
          )
        }
        for (p in pieces) {
          pl <- str_to_lower(p)
          if (str_detect(pl, "^(lamina )?base\\b")) {
            add_cat("leaf_base_shape", p, raw, cl_pos)
          }
          if (str_detect(pl, "^margins?\\b")) {
            add_cat("leaf_margin", p, raw, cl_pos)
            if (str_detect(pl, "teeth")) {
              note_unmapped(block, "lamina", "leaf tooth spacing", p)
            }
          }
          if (str_detect(pl, "^apex\\b")) {
            ap_tip <- paste(c(p, pieces[str_detect(str_to_lower(pieces), "^tip\\b")]), collapse = ", ")
            add_cat("leaf_apex_shape", ap_tip, raw, cl_pos)
          }
          if (str_detect(pl, "^tip\\b") && !any(str_detect(str_to_lower(pieces), "^apex\\b"))) {
            add_cat("leaf_apex_shape", p, raw, cl_pos)
          }
          if (str_detect(pl, "\\b(dull|glossy|shiny|shining)\\b")) {
            add_cat(
              "leaf_surface_reflectivity",
              p,
              raw,
              cl_pos,
              entity = "adaxial surface"
            )
          }
          if (
            str_detect(pl, "beneath") &&
              str_detect(
                pl,
                "glabr|hairy|pubesc|puberul|velvet|tomentose|silky|villous"
              )
          ) {
            add_cat(
              "leaf_hairs_adult_leaves",
              p,
              raw,
              cl_pos,
              entity = "abaxial surface"
            )
          }
          if (
            str_detect(pl, "chartaceous|coriaceous|papery|membranous|leathery")
          ) {
            note_unmapped(block, "lamina", "leaf texture", p)
          }
          if (str_detect(pl, "verrucose|dots")) {
            note_unmapped(block, "lamina", "leaf verrucae / dots", p)
          }
        }
        if (org == "leaves") {
          # sentence-level "Leaves ..." pieces with margins/arrangement
          if (str_detect(low, "margins?")) {
            add_cat(
              "leaf_margin",
              str_extract(cl_scoring, "margins?[^,;]*(?:,[^,;]*)?"),
              raw,
              cl_pos
            )
          }
          if (str_detect(low, "simple")) {
            add(
              "leaf_compoundness",
              tibble(
                cq = NA,
                trait_value = "simple",
                trait_value_clean = "simple",
                value_type = "mode",
                unit = NA
              ),
              raw,
              pos = cl_pos
            )
          }
        }
      }
      if (org == "veins") {
        note_unmapped(block, "veins", "lateral vein count / prominence", cl)
      }
      if (org == "domatia") {
        note_unmapped(block, "domatia", "domatia presence", cl)
      }
      if (org == "stipules") {
        note_unmapped(
          block,
          "stipules",
          "stipule presence / persistence / shape / length",
          cl
        )
      }
      if (org == "inflorescence") {
        il <- get_len("long")
        if (!is.null(il)) {
          add_num("inflorescence_length", il, raw, pos = cl_pos)
        }
        if (str_detect(cl, "-flowered")) {
          note_unmapped(block, "inflorescence", "flowers per inflorescence", cl)
        }
        if (str_detect(low, "among|behind|axils")) {
          note_unmapped(
            block,
            "inflorescence",
            "inflorescence position relative to leaves",
            cl
          )
        }
      }
      if (org == "bracts") {
        add_num("flower_bract_length", get_len("long"), raw, pos = cl_pos)
        note_unmapped(block, "bracts", "bract persistence / shape", cl)
      }
      if (org == "pedicels") {
        mp <- str_remove_all(mp, "\\([^)]*in fruit[^)]*\\)"); low <- str_to_lower(mp)
        if (!str_detect(low, "in fruit")) {
          add_num("pedicel_length", get_len("long"), raw, pos = cl_pos)
        }
      }
      if (org == "flowers") {
        mer <- str_match(
          cl_scoring,
          "(\\d)(?:[\u2013-]| or |\\- or )?(\\d)?-merous"
        )
        if (!is.na(mer[1, 1])) {
          v <- as.numeric(na.omit(mer[1, 2:3]))
          add(
            "flower_perianth_merism",
            tibble(
              cq = NA,
              trait_value = as.character(range(v)),
              trait_value_clean = as.character(range(v)),
              value_type = if (length(v) > 1) {
                c("minimum", "maximum")
              } else {
                c("mean", NA)
              },
              unit = "{count}"
            ) |>
              distinct() |>
              filter(!is.na(value_type)),
            raw,
            pos = cl_pos
          )
        }
      }
      if (org == "sepals") {
        d2 <- parse_dims(mp)
        if (!is.null(d2)) {
          add_num("flower_sepal_length", d2$len, raw, pos = cl_pos)
        } else {
          add_num("flower_sepal_length", get_len("long"), raw, pos = cl_pos)
        }
        if (str_detect(low, "^sepals free")) {
          add_cat("flower_perianth_fusion_categorical", low, raw, cl_pos)
        }
        if (str_detect(low, "persistent|falling|caducous")) {
          note_unmapped(block, "sepals", "sepal persistence", cl)
        }
        if (str_detect(low, "hairy|glabrous|velvet|silky")) {
          note_unmapped(block, "sepals", "sepal hairs", cl)
        }
      }
      if (org == "petals") {
        d2 <- parse_dims(mp)
        if (!is.null(d2)) {
          add_num("flower_petal_length", d2$len, raw, pos = cl_pos)
        } else {
          add_num("flower_petal_length", get_len("long"), raw, pos = cl_pos)
        }
        if (str_detect(low, "wide at widest|\u00d7")) {
          note_unmapped(block, "petals", "petal width", cl)
        }
        if (
          str_detect(low, "division|teeth|toothed|laciniate|fringed|entire")
        ) {
          note_unmapped(
            block,
            "petals",
            "petal apical divisions (count / length / shape)",
            cl
          )
        }
        if (str_detect(low, "hairy|glabrous|silky|pilose|ciliate")) {
          note_unmapped(
            block,
            "petals",
            "petal hairs (outside / inside / margins)",
            cl
          )
        }
        if (str_detect(low, "keel|pocket|pit")) {
          note_unmapped(block, "petals", "petal keel / basal pockets", cl)
        }
      }
      if (org == "nectary") {
        note_unmapped(block, "nectary", "nectary form / height / hairs", cl)
      }
      if (org == "stamens") {
        sc <- str_match(
          cl_scoring,
          paste0(
            "^stamens\\s+((?:\\(",
            num,
            "\u2013\\))?(?:c\\. )?",
            num,
            "(?:\u2013",
            num,
            ")?(?:\\s?\\(\u2013",
            num,
            "\\))?)"
          )
        )
        if (!is.na(sc[1, 1])) {
          add_num(
            "flower_fertile_stamens_count",
            num_rows(parse_range(sc[1, 2])) |> mutate(unit = "{count}"),
            raw,
            pos = cl_pos
          )
        }
        if (!is.na(usually_par[1, 1])) {
          add(
            "flower_fertile_stamens_count",
            tibble(
              cq = usually_par[1, 2],
              trait_value = usually_par[1, 4],
              trait_value_clean = usually_par[1, 4],
              value_type = "mode",
              unit = "{count}"
            ),
            raw,
            pos = cl_pos
          )
        }
        if (str_detect(low, "falling|persist")) {
          note_unmapped(block, "stamens", "stamen persistence", cl)
        }
        # genus/family "stamens 8–many / numerous": no count scored
        if (str_detect(low, "numerous|many")) {
          note_unmapped(block, "stamens", "stamen number (numerous)", cl)
        }
        # anthers/filaments described inside the stamens clause
        for (p in pieces) {
          pl <- str_to_lower(p)
          if (str_detect(pl, "^filaments")) {
            fl <- str_extract(
              p,
              paste0(
                "(?:\\(",
                num,
                "\u2013\\))?(?:c\\. )?",
                num,
                "(?:\u2013",
                num,
                ")?(?:\\(\u2013",
                num,
                "\\))?\\s?mm"
              )
            )
            if (!is.na(fl)) {
              add_num(
                "flower_filament_length",
                num_rows(parse_range(fl)),
                raw,
                pos = cl_pos
              )
            }
          }
          if (str_detect(pl, "^anthers")) {
            al <- str_extract(
              paste(pieces[which(pieces == p):length(pieces)], collapse = ", "),
              paste0(
                "(?:\\(",
                num,
                "\u2013\\))?(?:c\\. )?",
                num,
                "(?:\u2013",
                num,
                ")?(?:\\s?\\(\u2013",
                num,
                "\\))?\\s?mm"
              )
            )
            if (!is.na(al) && !str_detect(p, "awn")) {
              add_num(
                "flower_anther_length",
                num_rows(parse_range(al)),
                raw,
                pos = cl_pos
              )
            }
            add_cat("flower_anther_attachment", p, raw, cl_pos)
          }
        }
      }
      if (org == "filaments") {
        fl <- get_len("long")
        if (is.null(fl)) {
          fl0 <- str_extract(
            mp,
            paste0(
              "^filaments\\s(?:\\(",
              num,
              "\u2013\\))?(?:c\\. )?",
              num,
              "(?:\u2013",
              num,
              ")?(?:\\(\u2013",
              num,
              "\\))?\\s?mm"
            )
          )
          if (!is.na(fl0)) {
            fl <- num_rows(parse_range(str_remove(fl0, "^filaments\\s")))
          }
        }
        add_num("flower_filament_length", fl, raw, pos = cl_pos)
        # anther length later in the same clause
        ap <- str_extract(
          cl_scoring,
          paste0(
            "anthers[^;]*?((?:\\(",
            num,
            "\u2013\\))?(?:c\\. )?",
            num,
            "(?:\u2013",
            num,
            ")?(?:\\s?\\(\u2013",
            num,
            "\\))?\\s?mm)(?= long|,|$)"
          ),
          group = 1
        )
        if (!is.na(ap)) {
          add_num(
            "flower_anther_length",
            num_rows(parse_range(ap)),
            raw,
            pos = cl_pos
          )
        }
      }
      if (org == "anthers") {
        al <- get_len("long")
        if (is.null(al)) {
          a0 <- str_extract(
            mp,
            paste0(
              "^anthers\\s(?:\\(",
              num,
              "\u2013\\))?(?:c\\. )?",
              num,
              "(?:\u2013",
              num,
              ")?\\s?mm"
            )
          )
          if (!is.na(a0)) {
            al <- num_rows(parse_range(str_remove(a0, "^anthers\\s")))
          }
        }
        add_num("flower_anther_length", al, raw, pos = cl_pos)
        add_cat("flower_anther_attachment", cl_scoring, raw, cl_pos)
        if (str_detect(low, "pores|slits")) {
          add_cat("flower_anther_dehiscence", cl_scoring, raw, cl_pos)
        }
        if (str_detect(low, "awn|beak|setae|tooth")) {
          note_unmapped(block, "anthers", "anther awn / teeth / setae", cl)
        }
      }
      if (org == "ovary") {
        loc <- str_match(
          cl_scoring,
          paste0(
            "((?:\\(",
            num,
            "\u2013\\))?",
            num,
            "(?:\u2013",
            num,
            ")?(?:\\(\u2013",
            num,
            "\\))?)-locular"
          )
        )
        loc2 <- str_match(
          cl_scoring,
          paste0("with (", num, "(?:\u2013", num, ")?) (loculi|locules)")
        )
        lv <- coalesce(loc[1, 2], loc2[1, 2])
        if (!is.na(lv)) {
          add_num(
            "flower_structural_carpels_count",
            num_rows(parse_range(lv)) |> mutate(unit = "{count}"),
            raw,
            pos = cl_pos,
            note = "Ovary locule number scored as carpel number (syncarpous ovary, one locule per carpel)."
          )
        }
        add_cat("flower_ovary_position", cl_scoring, raw, cl_pos)
        if (str_detect(low, "hairy|glabrous|silky|velvety")) {
          note_unmapped(block, "ovary", "ovary hairs", cl)
        }
        ov <- str_match(
          cl_scoring,
          paste0(
            "ovules\\s(c\\. )?(",
            num,
            "(?:\u2013",
            num,
            ")?)\\s?(?:\\(\u2013(",
            num,
            ")\\))? per locule"
          )
        )
        if (!is.na(ov[1, 1])) {
          add_num(
            "flower_ovules_per_functional_carpel_count",
            num_rows(parse_range(paste0(
              coalesce(ov[1, 2], ""),
              ov[1, 3],
              if (!is.na(ov[1, 4])) paste0("(\u2013", ov[1, 4], ")") else ""
            ))) |>
              mutate(unit = "{count}"),
            raw,
            pos = cl_pos
          )
        }
        st <- str_extract(
          cl_scoring,
          paste0(
            "styles?[^,;]*?((?:c\\. )?",
            num,
            "(?:\u2013",
            num,
            ")?\\s?mm) long"
          ),
          group = 1
        )
        if (!is.na(st)) {
          add_num(
            "flower_style_length",
            num_rows(parse_range(st)),
            raw,
            pos = cl_pos
          )
        }
      }
      if (org == "ovules") {
        ov <- str_match(
          cl_scoring,
          paste0(
            "^ovules\\s(c\\. )?(",
            num,
            "(?:\u2013",
            num,
            ")?)\\s?(\\(\u2013(",
            num,
            ")\\))?\\s?per locule"
          )
        )
        if (!is.na(ov[1, 1])) {
          add_num(
            "flower_ovules_per_functional_carpel_count",
            num_rows(parse_range(paste0(
              coalesce(ov[1, 2], ""),
              ov[1, 3],
              if (!is.na(ov[1, 5])) paste0(" (\u2013", ov[1, 5], ")") else ""
            ))) |>
              mutate(unit = "{count}"),
            raw,
            pos = cl_pos
          )
        }
      }
      if (org == "style") {
        st <- get_len("long")
        add_num("flower_style_length", st, raw, pos = cl_pos)
      }
      if (org == "fruits") {
        d2 <- parse_dims(mp)
        if (!is.null(d2)) {
          add_num("fruit_length", d2$len, raw, pos = cl_pos)
          add_num("fruit_width", d2$wid, raw, pos = cl_pos)
        } else {
          fl <- get_len("long")
          if (!is.null(fl)) {
            add_num("fruit_length", fl, raw, pos = cl_pos)
          }
          dm <- str_extract(
            mp,
            paste0(
              "(?:c\\. |to )?",
              num,
              "(?:\u2013",
              num,
              ")?\\s?cm diam(eter)?"
            )
          )
          if (!is.na(dm)) {
            add_num(
              "fruit_length",
              num_rows(parse_range(dm)),
              raw,
              pos = cl_pos,
              note = "Diameter of a round fruit scored as length."
            )
            add_num("fruit_width", num_rows(parse_range(dm)), raw, pos = cl_pos)
          }
        }
        if (!is.na(usually_par[1, 1])) {
          add(
            "fruit_length",
            tibble(
              cq = usually_par[1, 2],
              trait_value = usually_par[1, 4],
              trait_value_clean = usually_par[1, 4],
              value_type = "mean",
              unit = usually_par[1, 5]
            ),
            raw,
            pos = cl_pos
          )
        }
        add_num("fruit_wall_thickness", meso_rows(cl_scoring), raw, pos = cl_pos, note = "Mesocarp thickness.")
        shp <- str_extract(
          pieces[1],
          "(?i)(round|globose|ellipsoid|ovoid|obovoid|fusiform|oblong|subglobose)[a-z -]*"
        )
        if (!is.na(shp)) {
          note_unmapped(block, "fruits", "fruit shape", cl)
        }
        if (str_detect(low, "spin")) {
          note_unmapped(block, "fruits", "fruit spines", cl)
        }
      }
      if (org == "mesocarp") {
        add_num("fruit_wall_thickness", meso_rows(cl_scoring), raw, pos = cl_pos, note = "Mesocarp thickness.")
        if (str_detect(low, "fleshy|oily")) {
          note_unmapped(block, "mesocarp", "mesocarp texture", cl)
        }
      }
      if (org == "stalk") {
        note_unmapped(block, "fruit stalk", "fruit stalk length", cl)
      }
      if (org == "stones") {
        note_unmapped(
          block,
          "stones",
          "stone (endocarp) size / shape / sutures / sculpture / wall / locules",
          cl
        )
      }
      if (org == "seeds") {
        d2 <- parse_dims(mp)
        if (!is.null(d2)) {
          add_num("seed_length", d2$len, raw, pos = cl_pos)
          add_num("seed_width", d2$wid, raw, pos = cl_pos)
        }
        if (str_detect(low, "embryo|endosperm")) {
          note_unmapped(block, "seeds", "embryo shape / endosperm", cl)
        }
        if (str_detect(low, "per locule")) {
          note_unmapped(block, "seeds", "seeds per locule", cl)
        }
      }
      if (org == "valves") {
        note_unmapped(block, "capsule valves", "valve count / spines", cl)
      }
      if (
        org %in%
          c(
            "buds",
            "axis",
            "bracteoles",
            "sapwood",
            "midrib",
            "indumentum",
            "peduncle"
          )
      ) {
        note_unmapped(block, org, paste(org, "characters"), cl)
      }
    }
  }
  if (!length(rows)) {
    return(NULL)
  }
  bind_rows(rows) |>
    mutate(
      entity_measured = if_else(trait == "stem_hairs", "twigs", entity_measured)
    )
}

# ---- run over descriptions ---------------------------------------------------------

desc <- paras |> filter(section == "Description")
desc_rows <- desc |>
  mutate(row = row_number()) |>
  group_split(row) |>
  map_dfr(\(p) {
    r <- parse_description(p)
    if (is.null(r)) {
      return(NULL)
    }
    r |>
      mutate(
        block = p$block,
        rank = p$rank,
        status = p$status,
        source_section = "Description"
      )
  })

# bole clauses in habit sentences may also hold DBH ("bole to 70 cm diameter")
desc_rows <- desc_rows |>
  distinct() |>
  # female-flower filaments/anthers belong to staminodes, not fertile stamens
  filter(!(entity_measured %in% "female flowers" & trait %in% c("flower_filament_length", "flower_anther_length")))


# ---- taxa ----------------------------------------------------------------------------

# Group membership as listed in the group paragraphs (pp. 35--37). E. kunstleri
# is placed in Monocera "still not certain" and E. sp. nov. only "suggested"
# for Coilopetalum: neither receives group copy-down.
groups <- tribble(
  ~group, ~species,
  "Acronodia", "acronodia ferrugineus knuthii mastersii nanus reticosus",
  "Coilopetalum", "griffithii palembanicus pedunculatus petiolatus pseudopaniculatus symingtonii",
  "Elaeocarpus", "acmosepalus eriobotryoides floribundus fraseri glaber nitidus robustus salicifolius sallehianus stipularis submonoceras tahanensis valetonii wrayi",
  "Ganitrus", "angustifolius",
  "Monocera", "cruciatus grandiflorus macrocerus ngii obtusus",
  "Polystachyus", "cupreus polystachyus"
) |>
  separate_longer_delim(species, " ") |>
  mutate(group = paste("Elaeocarpus", group, "Group"), species = paste("Elaeocarpus", species))

taxa <- paras |>
  filter(rank %in% c("species", "subspecies", "variety", "informal_form")) |>
  distinct(block, rank, status) |>
  mutate(
    genus = word(block, 1),
    species = str_extract(block, "^\\S+ (sp\\. nov\\.|\\S+)"),
    has_description = block %in% desc$block
  ) |>
  left_join(groups, by = "species")

# Taxa scored: species and infraspecific taxa recorded for Peninsular Malaysia.
# Species with infraspecific taxa treated here keep their own description rows.
# Excluded and incompletely known names are listed in taxon_flags.csv only.
scored_taxa <- taxa |> filter(status %in% c("accepted", "unnamed")) |>
  # "intermediate" form: synonym only, no description; ovalis form: not yet collected in Peninsular Malaysia
  filter(!str_detect(block, "\\((intermediate|ovalis form)\\)"))

# ---- copy-down ------------------------------------------------------------------------
# A categorical value from a family / genus / group description is copied to
# member taxa when (a) it is unqualified on the higher-rank row (no commonness
# word, a single mapped level) and (b) every member taxon that scores the trait
# itself includes that level. Never numbers. Applied only to taxa silent on the trait.

higher <- desc_rows |>
  filter(rank %in% c("family", "genus", "infrageneric_group"), value_type == "mode", is.na(cq),
         trait_value_clean != "", !str_detect(trait_value_clean, " "),
         # parenthetical-genus or species-scoped statements
         !str_detect(trait_value_raw, "\\((Elaeocarpus|Sloanea)\\)|\\bin some\\b|\\bmost\\b|\\bmany\\b"))

members_of <- function(b, r) {
  if (r == "family") return(scored_taxa$block)
  if (r == "genus") return(scored_taxa$block[scored_taxa$genus == b])
  scored_taxa$block[scored_taxa$group %in% b]
}

own <- desc_rows |> filter(rank %in% c("species", "subspecies", "variety", "informal_form"))

copy_rows <- higher |>
  pmap_dfr(\(...) {
    h <- list(...)
    mem <- members_of(h$block, h$rank)
    scored <- own |> filter(block %in% mem, trait == h$trait)
    ok <- all(map_lgl(split(scored$trait_value_clean, scored$block), \(v) any(str_detect(v, paste0("\\b", h$trait_value_clean, "\\b")))))
    if (!ok) {
      return(tibble(trait = h$trait, value = h$trait_value_clean, from = h$block, copied_to = NA_character_, rejected = TRUE))
    }
    tibble(trait = h$trait, value = h$trait_value_clean, from = h$block,
           copied_to = setdiff(mem, unique(scored$block)), rejected = FALSE)
  })

copied <- copy_rows |>
  filter(!rejected) |>
  left_join(higher |> select(from = block, trait, value = trait_value_clean, trait_value, trait_value_raw, printed_page, pdf_page, from_rank = rank),
            by = c("from", "trait", "value")) |>
  transmute(
    block = copied_to, trait, cq = NA_character_, trait_value, trait_value_clean = value,
    value_type = "mode", unit = NA_character_, trait_value_raw, entity_measured = NA_character_,
    notes = paste0("Copied from the ", from, " description."),
    pdf_page, printed_page,
    rank_scored_from = recode(from_rank, infrageneric_group = "species_group"),
    source_section = paste(recode(from_rank, family = "Family", genus = "Genus", infrageneric_group = "Group"), "description")
  )

# ---- species -> infraspecific inheritance -----------------------------------------------
# A subspecies / variety / form silent on a trait inherits its species' own values.

infra <- scored_taxa |> filter(rank != "species")
inherited <- infra |>
  select(block, species) |>
  inner_join(own |> filter(rank == "species") |> rename(species = block), by = "species", relationship = "many-to-many") |>
  anti_join(own |> distinct(block, trait), by = c("block", "trait")) |>
  mutate(rank_scored_from = "species", source_section = "Species description",
         notes = coalesce(paste(notes, "Inherited from the species description."), "Inherited from the species description.")) |>
  select(-species)

# ---- manual (vetted) rows ---------------------------------------------------------------------

manual <- read_csv("data/Coode_2018/raw/manual_rows.csv", col_types = cols(.default = "c")) |>
  rename(cq = commonness_qualifier) |>
  mutate(printed_page = as.integer(printed_page))
pdf_of_printed <- lines |> distinct(printed_page, pdf_page) |> filter(!is.na(printed_page))
manual <- manual |> left_join(pdf_of_printed, by = "printed_page")

# ---- assemble ------------------------------------------------------------------------------------

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
  inherited,
  manual
) |>
  filter(block %in% scored_taxa$block) |>
  left_join(scored_taxa |> select(block, taxon_rank = rank, status), by = "block") |>
  # Informal forms (E. angustifolius) are merged into the species with a form
  # context (user, 2026-10-22): keep only their own values, not the species
  # values inherited or copied down to them.
  filter(!(taxon_rank %in% "informal_form" & !is.na(rank_scored_from))) |>
  mutate(
    form = coalesce(if ("form" %in% names(pick(everything()))) form else NA_character_,
                    str_match(block, "\\((.*form)\\)$")[, 2]),
    taxon_name = str_remove(block, " \\(.*form\\)$"),
    taxon_rank = if_else(taxon_rank == "informal_form", "species", taxon_rank),
    entity_type = "species",
    commonness_qualifier = cq,
    in_traits.yml = if_else(trait %in% names(traits), "x", ""),
    value_type = coalesce(value_type, "mode"),
    source_section = paste0(coalesce(source_section, "Description"), ", p.", printed_page),
    # ranges as a--b so spreadsheets don't read dates
    across(c(trait_value_raw, trait_value, notes), \(x) str_replace_all(x, "(?<=\\d)\\s?[\u2013-]\\s?(?=\\d)", "--")),
    trait_value_raw = str_replace_all(trait_value_raw, "\u00d7", "x")
  )
if (!"population_region" %in% names(out)) out$population_region <- NA_character_

trait_data <- out |>
  arrange(taxon_name, trait, factor(commonness_qualifier, levels = unique(c(NA, "usually", qual_words, "extreme_range")), exclude = NULL), value_type) |>
  transmute(
    taxon_name, taxon_rank, entity_type, rank_scored_from, commonness_qualifier, population_region, form,
    entity_measured, trait, in_traits.yml, trait_value_raw, trait_value, trait_value_clean,
    value_type, unit, source_section, pdf_page, printed_page, notes
  ) |>
  distinct()

stopifnot(all(trait_data$in_traits.yml == "x"))
write_csv(trait_data, "data/Coode_2018/raw/trait_data.csv", na = "")

# ---- unmapped characters (suggestions, not traits) -----------------------------------------------

unmapped_terms <- bind_rows(unmapped) |>
  filter(character != "unparsed clause") |>
  group_by(organ, character) |>
  summarise(n_taxa = n_distinct(block), example_taxon = first(block),
            example_text = str_trunc(first(text), 200), .groups = "drop") |>
  arrange(desc(n_taxa))
write_csv(unmapped_terms, "data/Coode_2018/raw/unmapped_terms.csv", na = "")

# ---- taxon flags ------------------------------------------------------------------------------------

flags <- taxa |>
  left_join(trait_data |> count(taxon_name, name = "n_rows"), by = c("block" = "taxon_name")) |>
  transmute(
    taxon_name = block, taxon_rank = rank, status, n_rows = coalesce(n_rows, 0L),
    flag = case_when(
      status == "unnamed" ~ "Unnamed taxon (sp. nov., material inadequate for naming); kept with its printed label.",
      status == "excluded" ~ "Excluded species; not scored.",
      status == "incompletely_known" ~ "Incompletely known species; not scored.",
      str_detect(block, "intermediate\\)") ~ "Informal form with no description (synonym E. cyanocarpus only); not scored.",
      str_detect(block, "ovalis form") ~ "Informal form not yet collected in Peninsular Malaysia; not scored.",
      rank == "informal_form" ~ "Informal form; merged into the species with a form context (own values only).",
      !has_description & rank == "species" ~ "Species heading only; description given at infraspecific rank.",
      !has_description ~ "No description; values inherited from species or taken from notes only.",
      TRUE ~ ""
    )
  )
write_csv(flags, "data/Coode_2018/raw/taxon_flags.csv", na = "")
