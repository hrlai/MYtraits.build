source("R/extract_species_list.R")

standardise_taxonomy <- function(
  database_path = "export/data/curr/MYtraits.rds",
  species_output = "data/MYtraits_raw_species_list.csv"
) {
  extract_species_list(
    database_path = database_path,
    output_path = species_output
  )
}

standardise_taxonomy()
