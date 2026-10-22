# Coode_2018: builds data.csv from raw/trait_data.csv (written by
# raw/build_trait_data.R). data.csv keeps every trait_data.csv column and adds:
#   value                the build column: the source term (trait_value) for
#                        categorical traits, mapped through substitutions in
#                        metadata.yml; trait_value_clean for numeric traits and
#                        the flowering / fruiting y/n calendars
#   trait_scoring_method from rank_scored_from (values copied from the family,
#                        genus or species-group description, or inherited by an
#                        infraspecific taxon from its species description)
#   source_id            the study an Ecology statement cites, else Coode_2018
# Run from the repository root; the build itself never reads raw/.

traits <- yaml::read_yaml("config/traits.yml")$traits$elements

trait_data <- readr::read_csv(
  "data/Coode_2018/raw/trait_data.csv",
  col_types = readr::cols(.default = "c"),
  na = ""
)

is_numeric_trait <- vapply(
  trait_data$trait,
  \(t) identical(traits[[t]]$type, "numeric"),
  logical(1)
)

data <- trait_data |>
  dplyr::mutate(
    value = dplyr::if_else(
      is_numeric_trait | trait %in% c("flowering_time", "fruiting_time"),
      trait_value_clean,
      trait_value
    ),
    trait_scoring_method = dplyr::case_when(
      rank_scored_from == "family" ~ "inferred_from_family",
      rank_scored_from == "genus" ~ "inferred_from_genus",
      rank_scored_from == "species_group" ~ "inferred_from_species_group",
      rank_scored_from == "species" ~ "inferred_from_species",
      TRUE ~ NA_character_
    ),
    source_id = dplyr::case_when(
      grepl("Roubik et al\\. \\(2006\\)", notes) ~ "Roubik_2006",
      grepl("Ng \\(2014\\)", notes) ~ "Ng_2014",
      grepl("^Corner \\(1988\\)", trait_value_raw) ~ "Corner_1988",
      TRUE ~ "Coode_2018"
    )
  )

readr::write_csv(data, "data/Coode_2018/data.csv", na = "")
