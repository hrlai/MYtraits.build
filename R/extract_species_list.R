normalize_taxon_name <- function(x) {
  x |>
    # Replace line breaks and tabs with spaces so each name is on one line.
    stringr::str_replace_all("[\r\n\t]", " ") |>
    # Trim leading/trailing whitespace and collapse repeated internal spaces.
    stringr::str_squish()
}

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

extract_species_list <- function(
  database_path = "export/data/curr/MYtraits.rds",
  output_path = "data/MYtraits_raw_species_list.csv"
) {
  database <- read_rds(database_path)

  inventory <-
    database$taxonomic_updates |>
    dplyr::mutate(
      original_name = normalize_taxon_name(original_name),
      aligned_name = normalize_taxon_name(aligned_name)
    ) |>
    dplyr::distinct(dataset_id, original_name, aligned_name) |>
    dplyr::group_by(aligned_name) |>
    dplyr::summarise(
      original_name = dplyr::first(original_name),
      n_datasets = dplyr::n_distinct(dataset_id),
      datasets = paste(sort(unique(dataset_id)), collapse = "; "),
      n_records = dplyr::n(),
      name_class = dplyr::first(classify_taxon_name(aligned_name)),
      .groups = "drop"
    ) |>
    dplyr::arrange(aligned_name)

  readr::write_csv(inventory, output_path, na = "")
  inventory
}
