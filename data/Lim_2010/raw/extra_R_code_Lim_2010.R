# Lim_2010: builds data.csv from the hand-transcribed raw/trait_data.csv.
# data.csv is trait_data.csv with two added columns: trait_scoring_method, derived from
# rank_scored_from (values copied from the family or genus description, or inferred typical values),
# and source_id, naming the study an Ecology statement cites (Yii 1995, Whitmore 1975).
# Run from the repository root; the build itself never reads raw/.

trait_data <- readr::read_csv(
  "data/Lim_2010/raw/trait_data.csv",
  col_types = readr::cols(.default = "c"),
  na = ""
)

data <- trait_data |>
  dplyr::mutate(
    trait_scoring_method = dplyr::case_when(
      rank_scored_from == "family" ~ "inferred_from_family",
      rank_scored_from == "genus" ~ "inferred_from_genus",
      rank_scored_from == "inferred" ~ "inferred",
      TRUE ~ NA_character_
    ),
    source_id = dplyr::case_when(
      grepl("cited from Yii \\(1995\\)", notes) ~ "Yii_1995",
      grepl("Cited from Whitmore \\(1975\\)", notes) ~ "Whitmore_1975",
      TRUE ~ "Lim_2010"
    )
  )

readr::write_csv(data, "data/Lim_2010/data.csv", na = "")
