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

refresh_review_table <- function(review_table, review_output_path) {
  template <- review_table |>
    dplyr::transmute(
      aligned_name,
      name_class,
      review_reason,
      match_strategy,
      current_taxon_name = scientificName,
      current_taxon_id = taxonID,
      current_taxon_rank = taxonRank,
      current_taxonomic_status = taxonomicStatus,
      decision = NA_character_,
      manual_taxon_id = NA_character_,
      notes = NA_character_
    ) |>
    dplyr::arrange(aligned_name)

  dir.create(
    dirname(review_output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  readr::write_csv(template, review_output_path, na = "")
  template
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
#' Creates or refreshes the user-facing review file for names that still
#' require explicit review. A replacement taxon list is written only when every
#' unresolved name is covered by a manual decision.
#'
#' @param candidate_output_path Path to `data/MYtraits_taxon_alignment_detail.csv`.
#' @param wfo_backbone_path Path to the WFO backbone file.
#' @param review_output_path Path to the user-facing review CSV.
#' @param output_path Path for the final `config/taxon_list.csv` output.
#' @param strict If `TRUE`, stop when unresolved names remain after applying
#'   manual decisions.
#'
#' @return Invisibly returns a list with the final `taxon_list`, the current
#'   `review_table`, and any `remaining_unresolved` names.
build_mytraits_taxon_list <- function(
  candidate_output_path = "data/MYtraits_taxon_alignment_detail.csv",
  wfo_backbone_path = "inst/extdata/classification.csv",
  review_output_path = "data/MYtraits_alignment_review_needed.csv",
  output_path = "config/taxon_list.csv",
  strict = TRUE
) {
  alignment <- readr::read_csv(candidate_output_path, show_col_types = FALSE)
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

  review_table <- alignment |>
    dplyr::filter(!is.na(review_reason)) |>
    dplyr::arrange(aligned_name)

  unresolved <- alignment |>
    dplyr::filter(
      is.na(taxonID) |
        is.na(scientificName)
    ) |>
    dplyr::arrange(aligned_name)

  genus_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "genus") |>
    dplyr::semi_join(
      alignment |>
        dplyr::filter(!is.na(genus)) |>
        dplyr::distinct(genus),
      by = c("taxon_name" = "genus")
    ) |>
    dplyr::transmute(genus = taxon_name, taxon_id_genus = taxon_id)

  family_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "family") |>
    dplyr::semi_join(
      alignment |>
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

  review_table <- refresh_review_table(
    review_table = review_table,
    review_output_path = review_output_path
  )

  manual_decisions_path <- review_output_path
  manual_decisions <- review_table

  valid_decisions <- c(NA_character_, "use_step2_match", "use_manual_taxon_id")
  invalid_decisions <- review_table |>
    dplyr::filter(!is.na(decision) & !decision %in% valid_decisions)

  if (nrow(invalid_decisions) > 0) {
    stop(
      "Invalid manual decision values found: ",
      paste(sort(unique(invalid_decisions$decision)), collapse = ", "),
      ". Use `use_step2_match` or `use_manual_taxon_id`."
    )
  }

  auto_resolved <- alignment |>
    dplyr::anti_join(
      unresolved |>
        dplyr::select(aligned_name),
      by = "aligned_name"
    ) |>
    dplyr::transmute(
      aligned_name,
      taxon_id,
      submitted_wfo_name,
      submitted_wfo_status
    )

  approved_step2 <- unresolved |>
    dplyr::inner_join(
      manual_decisions |>
        dplyr::filter(decision == "use_step2_match") |>
        dplyr::select(aligned_name),
      by = "aligned_name"
    )

  unusable_step2 <- approved_step2 |>
    dplyr::filter(is.na(taxon_id) | is.na(wfo_scientific_name))

  if (nrow(unusable_step2) > 0) {
    stop(
      "`use_step2_match` was selected for names without a usable Step 2 match: ",
      paste(unusable_step2$aligned_name, collapse = ", ")
    )
  }

  approved_step2 <- approved_step2 |>
    dplyr::transmute(
      aligned_name,
      taxon_id,
      submitted_wfo_name,
      submitted_wfo_status
    )

  manual_taxa <- unresolved |>
    dplyr::inner_join(
      manual_decisions |>
        dplyr::filter(decision == "use_manual_taxon_id") |>
        dplyr::select(aligned_name, manual_taxon_id),
      by = "aligned_name"
    ) |>
    dplyr::transmute(
      aligned_name,
      taxon_id = manual_taxon_id,
      submitted_wfo_name = NA_character_,
      submitted_wfo_status = NA_character_
    )

  resolved_names <- dplyr::bind_rows(auto_resolved, approved_step2, manual_taxa)

  remaining_unresolved <- unresolved |>
    dplyr::anti_join(
      resolved_names |>
        dplyr::select(aligned_name),
      by = "aligned_name"
    )

  if (isTRUE(strict) && nrow(remaining_unresolved) > 0) {
    stop(
      "Taxon list not written. ",
      nrow(remaining_unresolved),
      " names still need decisions in `",
      review_output_path,
      "`."
    )
  }

  taxon_list <- resolved_names |>
    dplyr::left_join(
      accepted_taxa,
      by = "taxon_id",
      relationship = "many-to-one"
    )

  missing_backbone_rows <- taxon_list |>
    dplyr::filter(is.na(taxon_name) | is.na(taxon_rank))

  if (nrow(missing_backbone_rows) > 0) {
    stop(
      "Manual taxon IDs were not found as accepted WFO taxa: ",
      paste(sort(unique(missing_backbone_rows$taxon_id)), collapse = ", ")
    )
  }

  aligned_name_status <- lookup_aligned_name_status(taxon_list, wfo_backbone)

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

  readr::write_csv(taxon_list, output_path, na = "")

  invisible(list(
    taxon_list = taxon_list,
    review_table = review_table,
    remaining_unresolved = remaining_unresolved
  ))
}

#' Align submitted taxon names to a taxon list.
#'
#' Detects a submitted-name column in `dataset`, matches it against
#' `taxon_list$aligned_name`, and returns the original data with a populated
#' `taxon_name` column.
#'
#' @param dataset A data frame containing a submitted taxon-name column.
#' @param taxon_list A data frame or path to `config/taxon_list.csv`.
#' @param strict If `TRUE`, stop when any submitted names are not found.
#'
#' @return The input `dataset` with a `taxon_name` column.
align_taxon_names <- function(dataset, taxon_list, strict = TRUE) {
  if (is.character(taxon_list) && length(taxon_list) == 1) {
    taxon_list <- readr::read_csv(taxon_list, show_col_types = FALSE)
  }

  required_cols <- c("aligned_name", "taxon_name")
  missing_cols <- setdiff(required_cols, names(taxon_list))
  if (length(missing_cols) > 0) {
    stop(
      "Taxon list is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  name_column <- dplyr::case_when(
    "species" %in% names(dataset) ~ "species",
    "aligned_name" %in% names(dataset) ~ "aligned_name",
    "taxon_name" %in% names(dataset) ~ "taxon_name",
    .default = NA_character_
  )

  if (is.na(name_column)) {
    stop("Dataset must contain one of: species, aligned_name, taxon_name.")
  }

  matched_names <- tibble::tibble(
    aligned_name = normalize_taxon_name(dataset[[name_column]])
  ) |>
    dplyr::left_join(
      taxon_list |>
        dplyr::select(aligned_name, taxon_name),
      by = "aligned_name",
      relationship = "many-to-one"
    )

  unmatched <- matched_names |>
    dplyr::filter(is.na(taxon_name)) |>
    dplyr::distinct(aligned_name) |>
    dplyr::pull(aligned_name)

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
