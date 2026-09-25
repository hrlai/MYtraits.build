source("R/extract_species_list.R")
source("R/align_to_wfo.R")
source("R/build_align_taxon_names.R")

database_path <- "export/data/curr/MYtraits.rds"
species_output_path <- "inst/taxonomy/MYtraits_raw_species_list.csv"
candidate_output_path <- "inst/taxonomy/MYtraits_taxon_alignment_detail.csv"
review_output_path <- "inst/taxonomy/MYtraits_alignment_review_needed.csv"
wfo_backbone_path <- "inst/extdata/classification.csv"
taxon_list_output_path <- "config/taxon_list.csv"
allow_unchecked_species <- TRUE

extract_species_list(
  database_path = database_path,
  output_path = species_output_path
)

align_to_wfo(
  species_list_path = species_output_path,
  wfo_backbone_path = wfo_backbone_path,
  candidate_output_path = candidate_output_path,
  review_output_path = review_output_path,
  allow_unchecked_species = allow_unchecked_species
)

build_mytraits_taxon_list(
  candidate_output_path = candidate_output_path,
  review_output_path = review_output_path,
  output_path = taxon_list_output_path,
  allow_unchecked_species = allow_unchecked_species
)
