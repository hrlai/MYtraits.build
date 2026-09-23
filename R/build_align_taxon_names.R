source("R/extract_species_list.R")

assert_unique_key <- function(x, key, object_name) {
  duplicates <- x |>
    dplyr::count(dplyr::across(dplyr::all_of(key))) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop(
      object_name,
      " is not unique on ",
      paste(key, collapse = ", "),
      "."
    )
  }
}

#' Build a MYtraits taxon list from Step 2 WFO alignment output.
#'
#' Treats Step 2 as the review checkpoint. This function builds
#' `config/taxon_list.csv` from the preferred Step 2 matches and stops whenever
#' unresolved Step 2 review rows remain or any name lacks a preferred match.
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
  output_path = "config/taxon_list.csv"
) {
  alignment <- readr::read_csv(candidate_output_path, show_col_types = FALSE)

  required_cols <- c(
    "original_name",
    "wfo_preferred",
    "taxon_id",
    "scientific_name_id",
    "taxon_name",
    "taxon_rank",
    "scientific_name_authorship",
    "genus",
    "family",
    "taxonomic_status",
    "taxon_id_genus",
    "taxon_id_family",
    "aligned_name_taxon_id",
    "aligned_name_taxonomic_status"
  )

  missing_cols <- setdiff(required_cols, names(alignment))
  if (length(missing_cols) > 0) {
    stop(
      "Step 2 candidate table is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!file.exists(review_output_path)) {
    stop(
      "Step 2 review file is missing: `",
      review_output_path,
      "`. Run Step 2 before running Step 3."
    )
  }

  review_table <- readr::read_csv(review_output_path, show_col_types = FALSE)

  if (nrow(review_table) > 0) {
    stop(
      "Taxon list not written. ",
      nrow(review_table),
      " Step 2 review row(s) remain in `",
      review_output_path,
      "`. Resolve them in Step 2 before running Step 3."
    )
  }

  preferred_matches <- alignment |>
    dplyr::filter(isTRUE(wfo_preferred)) |>
    dplyr::mutate(
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
      )
    ) |>
    dplyr::arrange(original_name)

  assert_unique_key(
    preferred_matches,
    "original_name",
    "Preferred Step 2 matches"
  )

  names_without_preferred_match <- alignment |>
    dplyr::distinct(original_name) |>
    dplyr::anti_join(
      preferred_matches |>
        dplyr::select(original_name),
      by = "original_name"
    )

  if (nrow(names_without_preferred_match) > 0) {
    stop(
      "Taxon list not written. No preferred Step 2 match exists for: ",
      paste(names_without_preferred_match$original_name, collapse = ", ")
    )
  }

  preferred_matches <- preferred_matches |>
    dplyr::transmute(
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
    )

  unresolved_preferred_matches <- preferred_matches |>
    dplyr::filter(
      is.na(taxon_id) |
        is.na(taxon_name) |
        is.na(taxon_rank) |
        is.na(taxonomic_status)
    )

  if (nrow(unresolved_preferred_matches) > 0) {
    stop(
      "Taxon list not written. Preferred Step 2 matches lacked accepted-taxon fields for: ",
      paste(unresolved_preferred_matches$aligned_name, collapse = ", ")
    )
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
    stop("Dataset must contain an `original_name` column.")
  }

  if (!"taxon_name" %in% names(taxon_list)) {
    stop("Taxon list must contain a `taxon_name` column.")
  }

  matched_names <- tibble::tibble(
    original_name = normalize_taxon_name(dataset$original_name)
  ) |>
    dplyr::left_join(
      taxon_list |>
        dplyr::transmute(
          original_name = normalize_taxon_name(aligned_name),
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
    stop(
      "Unmatched taxon names: ",
      paste(unmatched, collapse = ", ")
    )
  }

  if (length(unmatched) > 0 && !isTRUE(strict)) {
    warning(
      "Unmatched taxon names: ",
      paste(unmatched, collapse = ", ")
    )
  }

  dataset$taxon_name <- matched_names$taxon_name
  dataset
}
