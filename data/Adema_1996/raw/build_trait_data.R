# Sapindaceae, TFSS vol. 2. Run from repository root.
suppressPackageStartupMessages(library(tidyverse))
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Adema_1996"
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
genera <- c(
  "Alectryon",
  "Allophylus",
  "Arytera",
  "Cardiospermum",
  "Cubilia",
  "Dictyoneura",
  "Dimocarpus",
  "Dodonaea",
  "Ganophyllum",
  "Glenniea",
  "Guioa",
  "Harpullia",
  "Lepisanthes",
  "Litchi",
  "Mischocarpus",
  "Nephelium",
  "Paranephelium",
  "Pometia",
  "Rhysotoechia",
  "Trigonachras",
  "Tristiropsis",
  "Xerospermum",
  "Zollingeria"
)
sp_re <- paste0(
  "^(?:\\d{1,2}\\. )?(",
  paste(genera, collapse = "|"),
  ") (sp\\. [AB]|[a-z][a-z-]+)\\b"
)

pages <- pdftools::pdf_text("inst/extdata/TFSS/Sapindaceae.pdf")
lines <- map2_dfr(pages, seq_along(pages), \(x, pg) {
  tibble(pdf_page = pg, text = strsplit(x, "\n")[[1]])
}) |>
  mutate(text = str_squish(text), printed_page = pdf_page + 256L) |>
  filter(
    between(pdf_page, 7, 118),
    text != "",
    !str_detect(text, "^TREE FLORA OF SABAH AND SARAWAK"),
    !str_detect(text, "^\\d{3}$")
  )

starts <- which(
  str_detect(lines$text, sp_re) &
    !str_detect(lines$text, "^(Litchi remains|Paranephelium nitidum King)")
)
ends <- c(starts[-1] - 1L, nrow(lines))
blocks <- map2_dfr(starts, ends, \(a, b) {
  m <- str_match(lines$text[a], sp_re)
  tibble(
    taxon_name = paste(m[1, 2], m[1, 3]),
    pdf_page = lines$pdf_page[a],
    printed_page = lines$printed_page[a],
    text = paste(lines$text[a:b], collapse = " ") |>
      str_replace_all("(?<=\\d)\\s*x\\s*(?=\\(?\\d)", " × ") |>
      flora_norm()
  )
})

new_row <- function(x, trait, raw, value, type, unit, entity = NA_character_) {
  tibble(
    taxon_name = x$taxon_name,
    taxon_rank = "species",
    entity_type = "species",
    rank_scored_from = "own",
    commonness_qualifier = NA_character_,
    population_region = "Sabah and Sarawak",
    form = NA_character_,
    entity_measured = entity,
    trait,
    in_traits.yml = "x",
    trait_value_raw = raw,
    trait_value = value,
    trait_value_clean = value,
    value_type = type,
    unit,
    source_section = "Description",
    pdf_page = x$pdf_page,
    printed_page = x$printed_page,
    notes = NA_character_,
    source_id = NA_character_
  )
}

categorical_row <- function(
  x,
  trait,
  raw,
  value,
  clean,
  entity = NA_character_
) {
  z <- new_row(x, trait, raw, value, "mode", NA_character_, entity)
  z$trait_value_clean <- clean
  z
}

last_match <- function(text, pattern) {
  hit <- str_extract_all(text, pattern)[[1]]
  if (length(hit)) last(hit) else NA_character_
}

number_rows <- function(x, trait, raw, entity = NA_character_) {
  r <- flora_parse_range(raw)
  if (is.null(r)) {
    return(tibble())
  }
  flora_num_rows(r) |>
    pmap_dfr(\(value, value_type, cq, unit) {
      z <- new_row(x, trait, raw, as.character(value), value_type, unit, entity)
      z$commonness_qualifier <- cq
      z
    })
}

extract_taxon <- function(x) {
  z <- x$text
  out <- list()
  add <- function(y) if (nrow(y)) out[[length(out) + 1]] <<- y
  habit <- str_extract(
    str_to_lower(z),
    "\\b(tree|shrub|liana|woody climber|herbaceous climber)s?\\b"
  )
  if (!is.na(habit)) {
    clean <- case_when(
      str_detect(habit, "tree") ~ "tree",
      str_detect(habit, "shrub") ~ "shrub",
      str_detect(habit, "liana|woody") ~ "climber_woody",
      TRUE ~ "climber_herbaceous"
    )
    add(categorical_row(
      x,
      "plant_growth_form",
      habit,
      str_replace_all(habit, " ", "_"),
      clean
    ))
  }
  if (
    str_detect(
      z,
      "(?i)leaves? each with|leaves? (?:bi|tri)?pinnate|leaves? digitate"
    )
  ) {
    add(categorical_row(
      x,
      "leaf_compoundness",
      "compound leaves",
      "compound",
      "compound"
    ))
  }
  h <- str_match(
    z,
    "(?i)(?:to |up to )?((?:c\\. )?\\d+(?:–\\d+)? m) (?:tall|high)"
  )[, 2]
  if (!is.na(h)) {
    add(number_rows(x, "plant_height", h))
  }
  petiole <- str_match(
    z,
    "(?i)petioles? ((?:\\([^)]*\\))?\\d+(?:–\\d+)?(?:\\([^)]*\\))? (?:mm|cm)) long"
  )[, 2]
  if (!is.na(petiole)) {
    add(number_rows(x, "petiole_length", petiole, "petiole"))
  }
  inflorescence <- str_match(
    z,
    "(?i)inflorescences?[^.;]{0,100}?((?:to |c\\. )?\\d+(?:–\\d+)? (?:mm|cm)) long"
  )[, 2]
  if (!is.na(inflorescence)) {
    add(number_rows(x, "inflorescence_length", inflorescence, "inflorescence"))
  }
  for (spec in list(
    c("sepals", "flower_sepal_length"),
    c("petals", "flower_petal_length"),
    c("anthers", "flower_anther_length"),
    c("filaments", "flower_filament_length")
  )) {
    hit <- str_match(
      z,
      paste0(
        "(?i)",
        spec[1],
        "[^.;]{0,100}?((?:c\\. |to )?\\d+(?:–\\d+)? (?:mm|cm)) long"
      )
    )[, 2]
    if (!is.na(hit)) add(number_rows(x, spec[2], hit, spec[1]))
  }
  leaf <- last_match(
    z,
    "(?i)leaflets?[^.;]{0,250}?\\d+(?:–\\d+)? ?× ?\\d+(?:–\\d+)? (?:mm|cm)"
  )
  dim <- flora_parse_dims(leaf)
  if (!is.null(dim)) {
    for (spec in list(c("leaflet_length", "len"), c("leaflet_width", "wid"))) {
      add(
        dim[[spec[2]]] |>
          pmap_dfr(\(value, value_type, cq, unit) {
            a <- new_row(
              x,
              spec[1],
              leaf,
              as.character(value),
              value_type,
              unit,
              "leaflet"
            )
            a$commonness_qualifier <- cq
            a
          })
      )
    }
  }
  leaf_clause <- last_match(z, "(?i)leaflets?[^.;]{0,400}")
  if (!is.na(leaf_clause)) {
    for (spec in list(
      c("leaf_shape", "elliptic", "elliptical"),
      c("leaf_shape", "obovate", "obovate"),
      c("leaf_shape", "ovate", "ovate"),
      c("leaf_shape", "oblong", "oblong"),
      c("leaf_shape", "lanceolate", "lanceolate"),
      c("leaf_shape", "falcate", "falcate"),
      c("leaf_base_shape", "cuneate", "cuneate"),
      c("leaf_base_shape", "rounded", "rounded"),
      c("leaf_base_shape", "obtuse", "obtuse"),
      c("leaf_base_shape", "acute", "acute"),
      c("leaf_base_shape", "attenuate", "attenuate"),
      c("leaf_apex_shape", "acuminate", "acuminate"),
      c("leaf_apex_shape", "acute", "acute"),
      c("leaf_apex_shape", "obtuse", "obtuse"),
      c("leaf_apex_shape", "rounded", "rounded"),
      c("leaf_apex_shape", "apiculate", "apiculate"),
      c("leaf_margin", "entire", "entire"),
      c("leaf_margin", "serrate|serrulate", "toothed_serrate"),
      c("leaf_margin", "dentate", "toothed_dentate"),
      c("leaf_margin", "crenate", "toothed_crenate")
    )) {
      term <- str_extract(leaf_clause, paste0("(?i)\\b(?:", spec[2], ")\\b"))
      if (!is.na(term)) {
        add(categorical_row(
          x,
          spec[1],
          leaf_clause,
          str_to_lower(term),
          spec[3],
          "leaflet"
        ))
      }
    }
    if (str_detect(leaf_clause, "(?i)\\bglabrous\\b")) {
      add(categorical_row(
        x,
        "leaf_hairs_adult_leaves",
        leaf_clause,
        "glabrous",
        "glabrous",
        "leaflet"
      ))
    }
  }
  fruit <- last_match(
    z,
    "(?i)fruit(?:s)? (?!\\d+(?:–\\d+)?(?:\\([^)]*\\))?-lobed)[^.;]{0,180}?\\d+(?:–\\d+)? ?× ?\\d+(?:–\\d+)? (?:mm|cm)"
  )
  dim <- flora_parse_dims(fruit)
  if (!is.null(dim)) {
    for (spec in list(c("fruit_length", "len"), c("fruit_width", "wid"))) {
      add(
        dim[[spec[2]]] |>
          pmap_dfr(\(value, value_type, cq, unit) {
            a <- new_row(
              x,
              spec[1],
              fruit,
              as.character(value),
              value_type,
              unit,
              "fruit"
            )
            a$commonness_qualifier <- cq
            a
          })
      )
    }
  }
  fruit_clause <- last_match(z, "(?i)fruit(?:s)?[^.;]{0,300}")
  if (!is.na(fruit_clause)) {
    if (
      str_detect(fruit_clause, "(?i)\\bglabrous\\b") &&
        !str_detect(
          fruit_clause,
          "(?i)glabrous (?:inside|within|internally)|fruit appendages[^.;]*glabrous"
        )
    ) {
      add(categorical_row(
        x,
        "fruit_surface_hairs",
        fruit_clause,
        "glabrous",
        "glabrous",
        "fruit"
      ))
    }
    if (
      str_detect(
        fruit_clause,
        "(?i)(?<!in)dehiscent|loculicidal|septicidal|septifragal"
      )
    ) {
      add(categorical_row(
        x,
        "fruit_dehiscence",
        fruit_clause,
        "dehiscent",
        "dehiscent",
        "fruit"
      ))
    }
    if (str_detect(fruit_clause, "(?i)indehiscent")) {
      add(categorical_row(
        x,
        "fruit_dehiscence",
        fruit_clause,
        "indehiscent",
        "indehiscent",
        "fruit"
      ))
    }
    for (spec in list(
      c("capsul", "capsule"),
      c("drup", "drupe"),
      c("samar", "samara")
    )) {
      if (str_detect(fruit_clause, paste0("(?i)", spec[1]))) {
        add(categorical_row(
          x,
          "fruit_type",
          fruit_clause,
          spec[2],
          spec[2],
          "fruit"
        ))
      }
    }
  }
  bind_rows(out)
}

month_names <- c(
  January = 1L,
  February = 2L,
  March = 3L,
  April = 4L,
  May = 5L,
  June = 6L,
  July = 7L,
  August = 8L,
  September = 9L,
  October = 10L,
  November = 11L,
  December = 12L
)
month_calendar <- function(text) {
  if (is.na(text)) {
    return(NA_character_)
  }
  if (str_detect(text, "(?i)throughout the year")) {
    return(strrep("y", 12))
  }
  found <- str_match_all(text, paste(names(month_names), collapse = "|"))[[1]][,
    1
  ]
  idx <- unname(month_names[found])
  if (!length(idx)) {
    return(NA_character_)
  }
  y <- rep("n", 12)
  if (length(idx) == 1) {
    y[idx] <- "y"
  } else {
    pairs <- str_match_all(
      text,
      paste0(
        "(?i)\\b(",
        paste(names(month_names), collapse = "|"),
        ")\\s*(?:to|–|-)\\s*(",
        paste(names(month_names), collapse = "|"),
        ")"
      )
    )[[1]]
    if (nrow(pairs)) {
      for (i in seq_len(nrow(pairs))) {
        a <- month_names[[str_to_title(pairs[i, 2])]]
        b <- month_names[[str_to_title(pairs[i, 3])]]
        y[if (a <= b) a:b else c(a:12, 1:b)] <- "y"
      }
    }
    y[idx] <- "y"
  }
  paste0(y, collapse = "")
}

phenology_rows <- function(x) {
  eco <- str_extract(
    x$text,
    "(?i)Ecology\\. .*?(?=(?:Uses|Vernacular names|Distribution|Taxonomy)\\.|$)"
  )
  if (is.na(eco)) {
    return(tibble())
  }
  out <- list()
  for (spec in list(
    c("Flowering", "flowering_time"),
    c("Fruiting", "fruiting_time")
  )) {
    hit <- str_extract(eco, paste0("(?i)", spec[1], "[^.]*\\."))
    cal <- month_calendar(hit)
    if (!is.na(cal)) {
      z <- new_row(x, spec[2], hit, cal, "mode", NA_character_)
      z$trait_value_clean <- cal
      z$source_section <- "Ecology"
      out[[length(out) + 1]] <- z
    }
  }
  bind_rows(out)
}

rows <- blocks |>
  split(seq_len(nrow(blocks))) |>
  map_dfr(extract_taxon) |>
  bind_rows(
    blocks |> split(seq_len(nrow(blocks))) |> map_dfr(phenology_rows)
  ) |>
  mutate(trait_value_raw = str_replace_all(trait_value_raw, "–", "--")) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)
write_csv(rows, file.path("data", id, "raw", "trait_data.csv"), na = "")
flora_unmapped_summary(rows) |>
  write_csv(file.path("data", id, "raw", "unmapped_terms.csv"), na = "")
blocks |>
  transmute(
    taxon_name,
    taxon_rank = "species",
    status = "accepted",
    n_rows = 0L,
    flag = if_else(
      str_detect(taxon_name, "sp\\."),
      "Unnamed species as printed.",
      NA_character_
    )
  ) |>
  write_csv(file.path("data", id, "raw", "taxon_flags.csv"), na = "")
