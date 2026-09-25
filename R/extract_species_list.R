#' Normalize taxon names for comparison and classification.
#'
#' Replaces control whitespace with single spaces and squishes repeated spaces so
#' downstream matching works on a consistent name form.
#'
#' @param x A character vector of taxon names.
#'
#' @return A character vector with normalized whitespace.
#' @keywords internal
normalize_taxon_name <- function(x) {
  x |>
    # Replace line breaks and tabs with spaces so each name is on one line.
    stringr::str_replace_all("[\r\n\t]", " ") |>
    # Trim leading/trailing whitespace and collapse repeated internal spaces.
    stringr::str_squish()
}

#' Classify submitted taxon names by taxonomic form.
#'
#' Distinguishes missing names, genus placeholders, informal names,
#' infraspecific names, and standard species names.
#'
#' @param x A character vector of taxon names.
#'
#' @return A character vector of name classes.
#' @keywords internal
classify_taxon_name <- function(x) {
  cleaned <- normalize_taxon_name(x)

  dplyr::case_when(
    # NA and empty names
    is.na(cleaned) | cleaned == "" ~ "missing",
    # Treat `Genus sp.` and `Genus spp.` as genus-level placeholders.
    stringr::str_detect(
      cleaned,
      stringr::regex("^[[:alpha:]-]+ spp?\\.$", ignore_case = TRUE)
    ) ~ "genus",
    # Flag informal names such as quoted morphotypes, `sp nov`, `indet.`,
    # `cf.`, and `aff.`; `\\b` marks word boundaries and
    # `ignore_case = TRUE` makes the check case-insensitive.
    stringr::str_detect(
      cleaned,
      stringr::regex(
        '"|\\bnov\\b|nov\\.|indet\\.|\\bindet\\b|\\bcf\\.?\\b|\\baff\\.?\\b',
        ignore_case = TRUE
      )
    ) ~ "informal",
    # If there is no space left after cleaning, treat the name as genus-only.
    !stringr::str_detect(cleaned, " ") ~ "genus",
    # Detect common infraspecific ranks such as `subsp.`, `var.`, `forma`,
    # and `f.` anywhere in the submitted name.
    stringr::str_detect(
      cleaned,
      stringr::regex(" subsp\\.| var\\.| forma | f\\.", ignore_case = TRUE)
    ) ~ "infraspecific",
    .default = "species"
  )
}

#' Extract a reviewable species inventory from the MYtraits database.
#'
#' Collapses the exported traits table to one row per unique `original_name`,
#' counts dataset coverage and record counts, and classifies each submitted name
#' before writing the raw inventory for taxonomic review.
#'
#' @param database_path Path to `export/data/curr/MYtraits.rds`.
#' @param output_path Path for `inst/taxonomy/MYtraits_raw_species_list.csv`.
#'
#' @return The extracted inventory as a data frame, invisibly written to disk.
#' @export
extract_species_list <- function(
  database_path = "export/data/curr/MYtraits.rds",
  output_path = "inst/taxonomy/MYtraits_raw_species_list.csv"
) {
  # Load the exported MYtraits database that still contains all submitted names.
  database <- read_rds(database_path)

  inventory <-
    database$traits |>
    # Keep only the fields needed to count names and track dataset coverage.
    dplyr::select(dataset_id, original_name) |>
    # Normalize names before grouping so whitespace-only differences collapse.
    dplyr::mutate(
      original_name = normalize_taxon_name(original_name)
    ) |>
    # Reduce the traits table to one row per submitted name.
    dplyr::group_by(original_name) |>
    dplyr::summarise(
      # Count how many distinct datasets used the name.
      n_datasets = dplyr::n_distinct(dataset_id),
      # Preserve the contributing dataset ids for quick review.
      datasets = paste(sort(unique(dataset_id)), collapse = "; "),
      # Keep the raw record count behind each submitted name.
      n_records = dplyr::n(),
      # Classify the name once so later steps know how to handle it.
      name_class = dplyr::first(classify_taxon_name(original_name)),
      .groups = "drop"
    ) |>
    # Sort for stable output and easier manual inspection.
    dplyr::arrange(original_name)

  # Write the reviewable raw inventory for the alignment step.
  readr::write_csv(inventory, output_path, na = "")
  inventory
}
