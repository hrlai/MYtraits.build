# Yao_2010: builds data.csv from raw/trait_data.csv (written by
# raw/build_trait_data.R). Unmapped rows (blank trait) stay in trait_data.csv only.
# Adds: value (source term for categorical traits, mapped by substitutions in
# metadata.yml; trait_value_clean for numeric traits and flowering_time),
# trait_scoring_method (from rank_scored_from) and source_id.
# Run from the repository root; the build never reads raw/.

traits <- yaml::read_yaml("config/traits.yml")$traits$elements

trait_data <- readr::read_csv(
  "data/Yao_2010/raw/trait_data.csv",
  col_types = readr::cols(.default = "c"),
  na = ""
) |>
  dplyr::filter(!is.na(trait))

is_numeric_trait <- vapply(
  trait_data$trait,
  \(t) identical(traits[[t]]$type, "numeric"),
  logical(1)
)

data <- trait_data |>
  dplyr::mutate(
    value = dplyr::if_else(
      is_numeric_trait | trait == "flowering_time",
      trait_value_clean,
      trait_value
    ),
    trait_scoring_method = dplyr::case_when(
      rank_scored_from == "family" ~ "inferred_from_family",
      rank_scored_from == "genus" ~ "inferred_from_genus",
      TRUE ~ NA_character_
    ),
    source_id = "Yao_2010"
  )

readr::write_csv(data, "data/Yao_2010/data.csv", na = "")
