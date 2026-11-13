# Rebuild the MYtraits taxonomy from scratch: reference, alignments of every
# dataset, and config/taxon_list.csv. See inst/notes/taxonomy/README.md.
#
# Run from the project root: uvr run inst/notes/taxonomy/rebuild_taxon_list.R
# Copy export/data/curr/MYtraits.rds aside first, to compare afterwards.

source("R/build_taxonomic_reference.R")
source("R/build_align_taxon_names.R")
source("R/build_update_taxon_list.R")

database_path <- "export/data/curr/MYtraits.rds"
review_path <- "inst/taxonomy/alignment_review.csv"

build_mytraits_reference(wcvp_version = 16, countries = c("MY", "BN", "SG"))
resources <- load_mytraits_resources()

if (!file.exists(database_path)) source("build.R", local = new.env())
database <- readRDS(database_path)

# Align every dataset and write its taxonomic_updates
dataset_ids <- sort(unique(database$taxonomic_updates$dataset_id))
aligned <- lapply(dataset_ids, function(id) {
  x <- build_align_taxon_names(database, id, resources)
  dplyr::mutate(x, dataset_id = id)
}) |>
  dplyr::bind_rows()

# Names to review by hand: fuzzy matches and names with no match
aligned |>
  dplyr::filter(is.na(.data$replace) | .data$chars_changed > 0) |>
  readr::write_csv(review_path, na = "")
message("Review ", review_path, " and fix entries in data/*/metadata.yml.")

# Rebuild so the database holds the aligned names, then rewrite the list
source("build.R", local = new.env())
database <- readRDS(database_path)
build_update_taxon_list(database, resources, replace = TRUE)
source("build.R", local = new.env())
