normalize_taxon_name <- function(x) {
  x |>
    stringr::str_replace_all("[\r\n\t]", " ") |>
    stringr::str_squish()
}

assert_unique_key <- function(x, key, object_name) {
  duplicates <- x |>
    dplyr::count(dplyr::across(dplyr::all_of(key))) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    cli::cli_abort(c(
      "{.strong {object_name}} is not unique on {.code {paste(key, collapse = ', ')}}.",
      "x" = "Duplicate keys: {paste(duplicates[[key[1]]], collapse = ', ')}",
      "i" = "Review the duplicate names before continuing."
    ))
  }
}

#' Build a MYtraits taxon list from `align_to_wfo()` output.
#'
#' Treats the `align_to_wfo()` review checkpoint as the gate for final output.
#' This function builds `config/taxon_list.csv` from the preferred WFO matches
#' and stops whenever unresolved review rows remain or any name lacks a
#' preferred match.
#'
#' @param candidate_output_path Path to `data/MYtraits_taxon_alignment_detail.csv`.
#' @param review_output_path Path to `data/MYtraits_alignment_review_needed.csv`.
#' @param output_path Path for the final `config/taxon_list.csv` output.
#'
#' @return Invisibly returns a list with the final `taxon_list`, the current
#'   `review_table`, and the `preferred_matches` used to build the output.
build_mytraits_taxon_list <- function(
  candidate_output_path = "data/MYtraits_taxon_alignment_detail.csv",
  review_output_path = "data/MYtraits_alignment_review_needed.csv",
  output_path = "config/taxon_list.csv",
  allow_unchecked_species = FALSE
) {
  alignment <- readr::read_csv(candidate_output_path, show_col_types = FALSE)
  review_table <- readr::read_csv(review_output_path, show_col_types = FALSE)

  if (nrow(review_table) > 0) {
    cli::cli_abort(c(
      "Taxon list not written.",
      "x" = "{nrow(review_table)} review row{?s} remain in {.file {review_output_path}}.",
      "i" = "Resolve them in {.code align_to_wfo()} before running {.code build_mytraits_taxon_list()}."
    ))
  }

  preferred_matches <- alignment |>
    dplyr::filter(wfo_preferred) |>
    dplyr::mutate(
      original_name = normalize_taxon_name(original_name),
      dplyr::across(
        c(
          taxon_id,
          scientific_name_id,
          taxon_name,
          taxon_rank,
          scientific_name_authorship,
          genus,
          family,
          taxonomic_status,
          taxon_id_genus,
          taxon_id_family,
          aligned_name_taxon_id,
          aligned_name_taxonomic_status
        ),
        ~ dplyr::na_if(.x, "")
      ),
      taxon_name = dplyr::if_else(
        allow_unchecked_species &
          aligned_name_taxonomic_status == "unchecked" &
          is.na(taxon_name),
        scientificName,
        taxon_name
      ),
      taxon_rank = dplyr::if_else(
        allow_unchecked_species &
          aligned_name_taxonomic_status == "unchecked" &
          is.na(taxon_rank),
        taxonRank,
        taxon_rank
      ),
      taxonomic_status = dplyr::if_else(
        allow_unchecked_species &
          aligned_name_taxonomic_status == "unchecked" &
          is.na(taxonomic_status),
        aligned_name_taxonomic_status,
        taxonomic_status
      ),
      genus = dplyr::if_else(
        allow_unchecked_species &
          aligned_name_taxonomic_status == "unchecked" &
          is.na(genus),
        stringr::word(scientificName, 1),
        genus
      )
    ) |>
    dplyr::arrange(original_name)

  assert_unique_key(
    preferred_matches,
    "original_name",
    "Preferred `align_to_wfo()` matches"
  )

  names_without_preferred_match <- alignment |>
    dplyr::distinct(original_name) |>
    dplyr::anti_join(
      preferred_matches |>
        dplyr::select(original_name),
      by = "original_name"
    )

  if (nrow(names_without_preferred_match) > 0) {
    cli::cli_abort(c(
      "Taxon list not written.",
      "x" = "No preferred {.code align_to_wfo()} match exists for: {paste(names_without_preferred_match$original_name, collapse = ', ')}",
      "i" = "Resolve or exclude the unmatched names before building the final taxon list."
    ))
  }

  preferred_matches <- preferred_matches |>
    dplyr::select(
      aligned_name = original_name,
      taxon_name,
      taxon_rank,
      taxonomic_status,
      genus,
      family,
      scientific_name_authorship,
      taxon_id,
      taxon_id_genus,
      taxon_id_family,
      scientific_name_id,
      aligned_name_taxon_id,
      aligned_name_taxonomic_status
    ) |>
    dplyr::mutate(
      aligned_name = normalize_taxon_name(aligned_name)
    )

  unresolved_preferred_matches <- preferred_matches |>
    dplyr::filter(
      is.na(taxon_id) |
        is.na(taxon_name) |
        is.na(taxon_rank) |
        is.na(taxonomic_status)
    ) |>
    dplyr::filter(
      !(allow_unchecked_species & aligned_name_taxonomic_status == "unchecked")
    )

  if (nrow(unresolved_preferred_matches) > 0) {
    cli::cli_abort(c(
      "Taxon list not written.",
      "x" = "Preferred {.code align_to_wfo()} matches are missing accepted-taxon fields for: {paste(unresolved_preferred_matches$aligned_name, collapse = ', ')}",
      "i" = "Inspect the WFO match output and resolve the missing accepted-name metadata."
    ))
  }

  taxon_list <- preferred_matches |>
    dplyr::mutate(
      taxonomic_dataset = "WFO",
      taxon_name_alternatives = NA_character_,
      binomial = dplyr::case_when(
        taxon_rank == "genus" ~ NA_character_,
        stringr::str_count(taxon_name, " ") >= 1 ~ paste(
          stringr::word(taxon_name, 1),
          stringr::word(taxon_name, 2)
        ),
        .default = NA_character_
      ),
      trinomial = dplyr::case_when(
        taxon_rank %in% c("subspecies", "variety", "form") ~ taxon_name,
        .default = NA_character_
      ),
      taxon_distribution = NA_character_,
      establishment_means = NA_character_,
      scientific_name = dplyr::case_when(
        is.na(scientific_name_authorship) |
          scientific_name_authorship == "" ~ taxon_name,
        .default = paste(taxon_name, scientific_name_authorship)
      ),
      taxon_id_genus = dplyr::case_when(
        taxon_rank == "genus" ~ taxon_id,
        .default = taxon_id_genus
      )
    ) |>
    dplyr::arrange(aligned_name) |>
    # Keep this column schema stable for traits.build compatibility.
    # Upstream reference for future maintenance:
    # https://github.com/traitecoevo/austraits.build/blob/07d133f87520ecce9fb9b775cd498706a84bf2f7/R/build_update_taxon_list.R#L219-L223
    dplyr::select(
      aligned_name,
      taxon_name,
      taxon_rank,
      taxonomic_status,
      taxonomic_dataset,
      taxon_name_alternatives,
      genus,
      family,
      binomial,
      trinomial,
      taxon_distribution,
      establishment_means,
      scientific_name,
      taxon_id,
      taxon_id_genus,
      taxon_id_family,
      scientific_name_id,
      aligned_name_taxon_id,
      aligned_name_taxonomic_status
    )

  assert_unique_key(taxon_list, "aligned_name", "Final taxon list")

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(taxon_list, output_path, na = "")

  invisible(list(
    taxon_list = taxon_list,
    review_table = review_table,
    preferred_matches = preferred_matches
  ))
}

#' Align submitted taxon names to a taxon list.
#'
#' Matches `dataset$original_name` against `taxon_list$aligned_name` and returns
#' the original data with a populated `taxon_name` column.
#'
#' @param dataset A data frame containing an `original_name` column.
#' @param taxon_list A data frame or path to `config/taxon_list.csv`.
#' @param strict If `TRUE`, stop when any submitted names are not found.
#'
#' @return The input `dataset` with a `taxon_name` column.
align_taxon_names <- function(dataset, taxon_list, strict = TRUE) {
  if (is.character(taxon_list) && length(taxon_list) == 1) {
    taxon_list <- readr::read_csv(taxon_list, show_col_types = FALSE)
  }

  if (!"original_name" %in% names(dataset)) {
    cli::cli_abort(c(
      "Dataset is missing the required {.code original_name} column.",
      "i" = "Check the input data frame before calling {.code align_taxon_names()}."
    ))
  }

  if (!"taxon_name" %in% names(taxon_list)) {
    cli::cli_abort(c(
      "Taxon list is missing the required {.code taxon_name} column.",
      "i" = "Use a taxon table with the expected schema before matching names."
    ))
  }

  dataset_normalized <- dataset |>
    dplyr::mutate(original_name = normalize_taxon_name(original_name))

  matched_names <- tibble::tibble(
    original_name = dataset_normalized$original_name
  ) |>
    dplyr::left_join(
      taxon_list |>
        dplyr::mutate(
          aligned_name = normalize_taxon_name(aligned_name)
        ) |>
        dplyr::transmute(
          original_name = aligned_name,
          taxon_name
        ),
      by = "original_name",
      relationship = "many-to-one"
    )

  unmatched <- matched_names |>
    dplyr::filter(is.na(taxon_name)) |>
    dplyr::distinct(original_name) |>
    dplyr::pull(original_name)

  if (length(unmatched) > 0 && isTRUE(strict)) {
    cli::cli_abort(c(
      "Unmatched taxon names remain after alignment.",
      "x" = "{paste(unmatched, collapse = ', ')}",
      "i" = "Inspect the source names or set {.code strict = FALSE} to continue with warnings only."
    ))
  }

  if (length(unmatched) > 0 && !isTRUE(strict)) {
    cli::cli_warn(c(
      "Unmatched taxon names remain after alignment.",
      "x" = "{paste(unmatched, collapse = ', ')}",
      "i" = "These rows were left unmatched; review them before downstream use."
    ))
  }

  dataset_normalized$taxon_name <- matched_names$taxon_name
  dataset_normalized
}
