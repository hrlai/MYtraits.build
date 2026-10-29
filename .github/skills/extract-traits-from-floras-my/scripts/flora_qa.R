# QA for a flora dataset's raw/trait_data.csv (and, when present, data.csv and
# metadata.yml). Read-only: prints tables and returns them in a list; writes
# nothing. Run after every family, before dataset_test().
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_qa.R")
#      qa <- flora_qa("Coode_2018")
# Run from the repository root.

suppressPackageStartupMessages(library(tidyverse))
# The project .Rprofile attaches traits.build (and so dplyr) before stats, so
# stats::filter and stats::lag would otherwise mask the dplyr verbs.
filter <- dplyr::filter
lag <- dplyr::lag

flora_qa <- function(
  id,
  traits = yaml::read_yaml("config/traits.yml")$traits$elements,
  unit_conversions = read_csv(
    "config/unit_conversions.csv",
    col_types = cols(.default = "c")
  )
) {
  td <- read_csv(
    file.path("data", id, "raw", "trait_data.csv"),
    col_types = cols(.default = "c"),
    na = ""
  )
  mapped <- td |> filter(!is.na(trait))
  out <- list()

  # 1. traits that are not in config/traits.yml (the user's rule: never invent)
  out$unknown_traits <- mapped |>
    filter(!trait %in% names(traits)) |>
    count(trait)

  # 2. duplicate keys: these fail dataset_test() with "cannot pivot wider"
  key <- c(
    "taxon_name",
    "trait",
    "commonness_qualifier",
    "population_region",
    "form",
    "entity_measured",
    "trait_scoring_method",
    "value_type",
    "trait_value_clean"
  )
  # trait_scoring_method is built from rank_scored_from (flora_make_data), so
  # rows that differ only in rank_scored_from are distinct keys at build time.
  mapped2 <- mapped |>
    mutate(trait_scoring_method = rank_scored_from)
  key <- intersect(key, names(mapped2))
  out$duplicate_keys <- mapped2 |>
    group_by(across(all_of(key))) |>
    filter(n() > 1) |>
    ungroup() |>
    count(trait, name = "n_rows")

  # 3. different values for the same key (cannot be merged automatically)
  k2 <- setdiff(intersect(key, names(mapped2)), "trait_value_clean")
  out$conflicting_keys <- mapped2 |>
    group_by(across(all_of(k2))) |>
    filter(n_distinct(trait_value_clean) > 1, value_type == "mode") |>
    ungroup() |>
    count(trait, name = "n_rows")

  # 4. numeric ranges per trait and unit, min > max, mixed units in one trait
  num <- mapped |>
    filter(
      trait %in% names(traits),
      map_lgl(trait, \(t) identical(traits[[t]]$type, "numeric"))
    ) |>
    mutate(v = suppressWarnings(as.numeric(trait_value_clean)))
  out$numeric_ranges <- num |>
    group_by(trait, unit) |>
    summarise(
      n = n(),
      min = min(v, na.rm = TRUE),
      max = max(v, na.rm = TRUE),
      .groups = "drop"
    )
  out$units_per_trait <- num |>
    group_by(trait) |>
    summarise(
      units = paste(sort(unique(unit)), collapse = "/"),
      .groups = "drop"
    ) |>
    filter(str_detect(units, "/"))
  out$min_gt_max <- num |>
    filter(commonness_qualifier %in% c(NA, "usually")) |>
    group_by(
      taxon_name,
      trait,
      entity_measured,
      form,
      trait_scoring_method = rank_scored_from
    ) |>
    summarise(
      lo = suppressWarnings(min(v[value_type == "minimum"])),
      hi = suppressWarnings(max(v[value_type == "maximum"])),
      .groups = "drop"
    ) |>
    filter(is.finite(lo), is.finite(hi), lo > hi)
  # allowed range in traits.yml (these rows are excluded at build). The range
  # is in the trait's unit, so convert each value first, as the build does
  # (`unit_in: unit`); a unit with no conversion is listed with a blank value.
  to_trait_unit <- \(v, from, to) {
    if (is.na(v) || is.na(from) || is.na(to) || from == to) {
      return(v)
    }
    f <- unit_conversions$`function`[
      unit_conversions$unit_from == from & unit_conversions$unit_to == to
    ]
    if (length(f) != 1) {
      return(NA_real_)
    }
    eval(parse(text = f), list(x = v))
  }
  out$outside_allowed <- num |>
    mutate(
      trait_unit = map_chr(trait, \(t) traits[[t]]$units %||% NA_character_),
      value_in_trait_unit = pmap_dbl(list(v, unit, trait_unit), to_trait_unit),
      lo = map_dbl(trait, \(t) traits[[t]]$allowed_values_min %||% -Inf),
      hi = map_dbl(trait, \(t) traits[[t]]$allowed_values_max %||% Inf)
    ) |>
    filter(
      !is.na(v),
      is.na(value_in_trait_unit) |
        value_in_trait_unit < lo |
        value_in_trait_unit > hi
    ) |>
    select(
      taxon_name,
      trait,
      trait_value_clean,
      unit,
      value_in_trait_unit,
      trait_unit,
      lo,
      hi
    )

  # 5. categorical term -> level table: read it all
  levels_ok <- \(t, v) {
    al <- names(traits[[t]]$allowed_values_levels)
    if (is.null(al) || is.na(v) || v == "") {
      return(NA)
    }
    all(str_split_1(v, " ") %in% al)
  }
  cat_rows <- mapped |>
    filter(
      map_lgl(trait, \(t) identical(traits[[t]]$type, "categorical")),
      !trait %in% c("flowering_time", "fruiting_time")
    )
  out$categorical_map <- cat_rows |>
    count(trait, trait_value, trait_value_clean, sort = FALSE) |>
    mutate(level_in_traits_yml = map2_lgl(trait, trait_value_clean, levels_ok))

  # What the build sees: `value` = trait_value, mapped by the metadata
  # substitutions (whole-value find -> replace, per trait). Terms still outside
  # the allowed levels reach excluded_data.
  subs <- NULL
  mf0 <- file.path("data", id, "metadata.yml")
  if (file.exists(mf0)) {
    ms <- yaml::read_yaml(mf0)$substitutions %||% list()
    subs <- tibble(
      trait = map_chr(ms, \(s) s$trait_name),
      trait_value = map_chr(ms, \(s) as.character(s$find)),
      replace = map_chr(ms, \(s) as.character(s$replace))
    )
  }
  out$values_not_allowed_at_build <- cat_rows |>
    count(trait, trait_value) |>
    left_join(
      subs %||%
        tibble(
          trait = character(),
          trait_value = character(),
          replace = character()
        ),
      by = c("trait", "trait_value")
    ) |>
    mutate(
      value_built = coalesce(replace, trait_value),
      ok = map2_lgl(trait, value_built, levels_ok)
    ) |>
    filter(ok %in% FALSE) |>
    select(trait, trait_value, value_built, n)

  # Same key and value scored from two ranks (family/genus/group and species):
  # not a build error, but redundant. Keep the most specific rank only.
  out$same_value_two_ranks <- mapped |>
    filter(value_type == "mode") |>
    group_by(
      taxon_name,
      trait,
      commonness_qualifier,
      population_region,
      form,
      entity_measured,
      trait_value_clean
    ) |>
    filter(n_distinct(rank_scored_from, na.rm = FALSE) > 1) |>
    ungroup() |>
    count(trait, name = "n_rows")

  # 6. calendars must be 12 characters of y/n
  out$bad_calendars <- mapped |>
    filter(
      trait %in% c("flowering_time", "fruiting_time"),
      !str_detect(trait_value_clean, "^[yn]{12}$")
    )

  # 7. leftovers: digit-digit hyphens or en dashes in text columns, unit-less numbers
  txt <- td |> select(trait_value_raw, trait_value, notes)
  out$hyphen_leftovers <- sum(map_int(txt, \(x) {
    sum(str_detect(x, "[0-9]\\s?[\u2013-]\\s?[0-9]"), na.rm = TRUE)
  }))
  out$endash_leftovers <- sum(map_int(txt, \(x) {
    sum(str_detect(x, "\u2013"), na.rm = TRUE)
  }))
  out$numeric_without_unit <- num |>
    filter(is.na(unit) | unit == "") |>
    count(trait)

  # 8. measurement misses: an organ named with a number, but no numeric row on that clause
  clauses_with_numbers <- td |>
    filter(str_detect(trait_value_raw, "\\d+(\\.\\d+)?\\s?(mm|cm|m)\\b")) |>
    distinct(taxon_name, trait_value_raw, source_section)
  scored <- td |>
    filter(!is.na(trait), value_type %in% c("minimum", "maximum", "mean")) |>
    distinct(taxon_name, trait_value_raw)
  out$measurement_misses <- anti_join(
    clauses_with_numbers,
    scored,
    by = c("taxon_name", "trait_value_raw")
  )

  # 9. unmapped share, and copied-down rows by rank
  out$unmapped_rows <- td |>
    filter(is.na(trait)) |>
    count(source_section = str_remove(source_section, ", p\\..*"))
  out$rank_scored_from <- td |> count(rank_scored_from)

  # 10. coverage of the core traits per taxon
  core <- intersect(
    c(
      "plant_growth_form",
      "plant_height",
      "leaf_length",
      "leaf_width",
      "leaf_phyllotaxis",
      "flower_colour",
      "fruit_type",
      "fruit_length",
      "seed_length"
    ),
    names(traits)
  )
  out$core_coverage <- expand_grid(
    taxon_name = unique(td$taxon_name),
    trait = core
  ) |>
    left_join(
      mapped |> count(taxon_name, trait),
      by = c("taxon_name", "trait")
    ) |>
    mutate(has = !is.na(n)) |>
    group_by(trait) |>
    summarise(taxa_with_value = sum(has), taxa = n(), .groups = "drop")

  # 11. metadata consistency: contexts, traits and sources used in data.csv
  mf <- file.path("data", id, "metadata.yml")
  df <- file.path("data", id, "data.csv")
  if (file.exists(mf) && file.exists(df)) {
    m <- yaml::read_yaml(mf)
    dd <- read_csv(df, col_types = cols(.default = "c"), na = "")
    ctx <- map_chr(m$contexts %||% list(), \(c) c$var_in)
    ctx_used <- intersect(
      c("commonness_qualifier", "population_region", "form", "entity_measured"),
      names(dd)
    )
    ctx_used <- ctx_used[map_lgl(ctx_used, \(c) any(!is.na(dd[[c]])))]
    out$contexts_missing_in_metadata <- setdiff(ctx_used, ctx)
    out$traits_missing_in_metadata <- setdiff(
      unique(dd$trait),
      map_chr(m$traits, \(t) t$trait_name)
    )
    known <- c(
      m$source$primary$key,
      map_chr(m$source[setdiff(names(m$source), "primary")] %||% list(), \(s) {
        s$key %||% NA_character_
      })
    )
    if (!is.null(m$source$secondary)) {
      known <- c(known, m$source$secondary$key)
    }
    out$source_ids_missing_in_metadata <- setdiff(unique(dd$source_id), known)
    out$yaml_parse_ok <- TRUE
    out$n_questions <- length(m$questions %||% list())
  }

  for (nm in names(out)) {
    x <- out[[nm]]
    cat("\n##", nm, "\n")
    if (is.data.frame(x)) {
      if (nrow(x)) print(x, n = 30) else cat("(none)\n")
    } else if (length(x) == 0) {
      cat("(none)\n")
    } else {
      print(x)
    }
  }
  invisible(out)
}

# dataset_test() without the progress log. Returns passed / failed counts and
# prints the failure messages. dataset_process() is a separate call: inspect
# its excluded_data, because allowed-value mismatches survive schema tests.
flora_dataset_test <- function(id) {
  rep <- testthat::ListReporter$new()
  traits.build::dataset_test(
    id,
    path_config = "config",
    path_data = "data",
    reporter = rep
  )
  res <- rep$get_results()
  failed <- keep(res, \(r) {
    any(map_lgl(r$results, \(x) {
      inherits(x, c("expectation_failure", "expectation_error"))
    }))
  })
  cat("failed:", length(failed), "\n")
  walk(failed, \(r) {
    walk(r$results, \(x) {
      if (inherits(x, c("expectation_failure", "expectation_error"))) {
        cat("-", r$test, ":", conditionMessage(x), "\n")
      }
    })
  })
  invisible(res)
}
