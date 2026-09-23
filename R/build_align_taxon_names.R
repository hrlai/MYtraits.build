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

lookup_aligned_name_status <- function(x, wfo_backbone) {
  candidates <- x |>
    dplyr::select(aligned_name, taxon_id) |>
    dplyr::left_join(
      wfo_backbone |>
        dplyr::transmute(
          aligned_name = scientificName,
          aligned_name_taxon_id = taxonID,
          aligned_name_taxonomic_status = stringr::str_to_lower(
            taxonomicStatus
          ),
          aligned_name_accepted_id = acceptedNameUsageID
        ),
      by = "aligned_name",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      match_priority = dplyr::case_when(
        aligned_name_taxon_id == taxon_id ~ 1L,
        aligned_name_accepted_id == taxon_id ~ 2L,
        .default = 3L
      )
    ) |>
    dplyr::filter(match_priority < 3L)

  ambiguous_matches <- candidates |>
    dplyr::count(aligned_name, match_priority) |>
    dplyr::filter(n > 1)

  if (nrow(ambiguous_matches) > 0) {
    stop(
      "Multiple World Flora Online submitted-name matches remain for: ",
      paste(ambiguous_matches$aligned_name, collapse = ", ")
    )
  }

  candidates |>
    dplyr::group_by(aligned_name) |>
    dplyr::summarise(
      aligned_name_taxon_id = dplyr::first(aligned_name_taxon_id),
      aligned_name_taxonomic_status = dplyr::first(
        aligned_name_taxonomic_status
      ),
      .groups = "drop"
    )
}

#' Build a MYtraits taxon list from Step 2 WFO alignment output.
#'
#' Treats Step 2 as the review checkpoint. This function builds
#' `config/taxon_list.csv` from the preferred Step 2 matches and stops whenever
#' unresolved Step 2 review rows remain or any name lacks a preferred match.
#'
#' @param candidate_output_path Path to `data/MYtraits_taxon_alignment_detail.csv`.
#' @param review_output_path Path to `data/MYtraits_alignment_review_needed.csv`.
#' @param wfo_backbone_path Path to the WFO backbone file.
#' @param output_path Path for the final `config/taxon_list.csv` output.
#'
#' @return Invisibly returns a list with the final `taxon_list`, the current
#'   `review_table`, and the `preferred_matches` used to build the output.
build_mytraits_taxon_list <- function(
  candidate_output_path = "data/MYtraits_taxon_alignment_detail.csv",
  review_output_path = "data/MYtraits_alignment_review_needed.csv",
  wfo_backbone_path = "inst/extdata/classification.csv",
  output_path = "config/taxon_list.csv"
) {
  alignment <- readr::read_csv(candidate_output_path, show_col_types = FALSE)

  required_cols <- c(
    "original_name",
    "wfo_preferred",
    "taxonID",
    "acceptedNameUsageID"
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

  WorldFlora::WFO.remember(wfo_backbone_path)
  wfo_backbone <- tibble::as_tibble(WFO.data)

  accepted_taxa <- wfo_backbone |>
    dplyr::filter(taxonomicStatus == "Accepted") |>
    dplyr::transmute(
      taxon_id = taxonID,
      scientific_name_id = scientificNameID,
      taxon_name = scientificName,
      taxon_rank = taxonRank,
      scientific_name_authorship = scientificNameAuthorship,
      genus,
      family,
      taxonomic_status = stringr::str_to_lower(taxonomicStatus)
    )

  assert_unique_key(accepted_taxa, "taxon_id", "Accepted WFO taxa")

  preferred_matches <- alignment |>
    dplyr::filter(isTRUE(wfo_preferred)) |>
    dplyr::mutate(
      dplyr::across(
        c(taxonID, acceptedNameUsageID),
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
      taxon_id = dplyr::coalesce(acceptedNameUsageID, taxonID)
    )

  unresolved_preferred_matches <- preferred_matches |>
    dplyr::filter(is.na(taxon_id))

  if (nrow(unresolved_preferred_matches) > 0) {
    stop(
      "Taxon list not written. Preferred Step 2 matches lacked an accepted taxon ID for: ",
      paste(unresolved_preferred_matches$aligned_name, collapse = ", ")
    )
  }

  taxon_list <- preferred_matches |>
    dplyr::left_join(
      accepted_taxa,
      by = "taxon_id",
      relationship = "many-to-one"
    )

  missing_backbone_rows <- taxon_list |>
    dplyr::filter(is.na(taxon_name) | is.na(taxon_rank))

  if (nrow(missing_backbone_rows) > 0) {
    stop(
      "Preferred Step 2 matches did not resolve to accepted WFO taxa: ",
      paste(sort(unique(missing_backbone_rows$taxon_id)), collapse = ", ")
    )
  }

  genus_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "genus") |>
    dplyr::semi_join(
      taxon_list |>
        dplyr::filter(!is.na(genus)) |>
        dplyr::distinct(genus),
      by = c("taxon_name" = "genus")
    ) |>
    dplyr::transmute(genus = taxon_name, taxon_id_genus = taxon_id)

  family_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "family") |>
    dplyr::semi_join(
      taxon_list |>
        dplyr::filter(!is.na(family)) |>
        dplyr::distinct(family),
      by = c("taxon_name" = "family")
    ) |>
    dplyr::transmute(family = taxon_name, taxon_id_family = taxon_id)

  assert_unique_key(genus_ids, "genus", "Accepted WFO genera used in MYtraits")
  assert_unique_key(
    family_ids,
    "family",
    "Accepted WFO families used in MYtraits"
  )

  aligned_name_status <- lookup_aligned_name_status(
    x = taxon_list,
    wfo_backbone = wfo_backbone
  )

  taxon_list <- taxon_list |>
    dplyr::left_join(
      genus_ids,
      by = "genus",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      family_ids,
      by = "family",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      aligned_name_status,
      by = "aligned_name",
      relationship = "one-to-one"
    ) |>
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
      ),
      aligned_name_taxon_id = dplyr::coalesce(
        aligned_name_taxon_id,
        dplyr::if_else(aligned_name == taxon_name, taxon_id, NA_character_)
      ),
      aligned_name_taxonomic_status = dplyr::coalesce(
        aligned_name_taxonomic_status,
        dplyr::if_else(
          aligned_name == taxon_name,
          taxonomic_status,
          NA_character_
        )
      )
    ) |>
    dplyr::arrange(aligned_name) |>
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
