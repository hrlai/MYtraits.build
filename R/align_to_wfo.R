align_to_wfo <- function(
  species_list_path = "data/MYtraits_raw_species_list.csv",
  wfo_backbone_path = "inst/extdata/classification.csv",
  candidate_output_path = "data/MYtraits_taxon_alignment_detail.csv",
  review_output_path = "data/MYtraits_alignment_review_needed.csv"
) {
  species_list <- readr::read_csv(species_list_path, show_col_types = FALSE)

  required_cols <- c(
    "aligned_name",
    "original_name",
    "n_datasets",
    "datasets",
    "n_records",
    "name_class"
  )

  missing_cols <- setdiff(required_cols, names(species_list))
  if (length(missing_cols) > 0) {
    stop(
      "Species list is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  WorldFlora::WFO.remember(wfo_backbone_path)

  build_unmatched_rows <- function(x, review_reason, strategy) {
    if (nrow(x) == 0) {
      return(tibble::tibble())
    }

    tibble::tibble(
      aligned_name = x$aligned_name,
      original_name = x$original_name,
      n_datasets = x$n_datasets,
      datasets = x$datasets,
      n_records = x$n_records,
      name_class = x$name_class,
      match_strategy = strategy,
      candidate_count = 0L,
      wfo_preferred = FALSE,
      one_reason = NA_character_,
      review_reason = review_reason,
      submitted_name = x$aligned_name,
      matched = FALSE,
      fuzzy = FALSE,
      fuzzy_distance = NA_real_,
      unique_name = TRUE,
      taxon_id = NA_character_,
      scientific_name_id = NA_character_,
      wfo_scientific_name = NA_character_,
      taxon_rank = NA_character_,
      family = NA_character_,
      genus = NA_character_,
      taxonomic_status = NA_character_,
      accepted_name_usage_id = NA_character_,
      parent_name_usage_id = NA_character_,
      nomenclatural_status = NA_character_,
      scientific_name_authorship = NA_character_,
      submitted_wfo_name = NA_character_,
      submitted_wfo_status = NA_character_,
      accepted_name_changed = NA,
      references = NA_character_,
      source = NA_character_
    )
  }

  format_candidate_table <- function(all_candidates, x, strategy) {
    preferred <- all_candidates |>
      as.data.frame() |>
      WorldFlora::WFO.one(spec.name = "aligned_name", verbose = FALSE) |>
      tibble::as_tibble() |>
      dplyr::select(
        aligned_name,
        preferred_taxon_id = taxonID,
        one_reason = One.Reason
      ) |>
      dplyr::distinct()

    candidate_counts <- all_candidates |>
      dplyr::count(aligned_name, name = "candidate_count")

    candidate_table <- all_candidates |>
      dplyr::left_join(x, by = "aligned_name") |>
      dplyr::left_join(candidate_counts, by = "aligned_name") |>
      dplyr::left_join(
        preferred,
        by = "aligned_name",
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        match_strategy = strategy,
        review_reason = dplyr::case_when(
          !Matched ~ "no_wfo_match",
          candidate_count > 1 ~ "multiple_wfo_candidates",
          Fuzzy ~ "fuzzy_match_used",
          !is.na(Old.status) & Old.status != "" ~ "submitted_name_is_synonym",
          TRUE ~ NA_character_
        )
      )

    preferred_rows <- candidate_table |>
      dplyr::filter(Matched, taxonID == preferred_taxon_id) |>
      dplyr::group_by(aligned_name) |>
      dplyr::mutate(preferred_row = dplyr::row_number() == 1) |>
      dplyr::ungroup() |>
      dplyr::select(aligned_name, taxonID, preferred_row)

    candidate_table |>
      dplyr::left_join(
        preferred_rows,
        by = c("aligned_name", "taxonID"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(wfo_preferred = dplyr::coalesce(preferred_row, FALSE)) |>
      dplyr::select(-preferred_row) |>
      dplyr::transmute(
        aligned_name,
        original_name,
        n_datasets,
        datasets,
        n_records,
        name_class,
        match_strategy,
        candidate_count,
        wfo_preferred,
        one_reason,
        review_reason,
        submitted_name = spec.name.ORIG,
        matched = Matched,
        fuzzy = Fuzzy,
        fuzzy_distance = Fuzzy.dist,
        unique_name = Unique,
        taxon_id = taxonID,
        scientific_name_id = scientificNameID,
        wfo_scientific_name = scientificName,
        taxon_rank = taxonRank,
        family,
        genus,
        taxonomic_status = taxonomicStatus,
        accepted_name_usage_id = acceptedNameUsageID,
        parent_name_usage_id = parentNameUsageID,
        nomenclatural_status = nomenclaturalStatus,
        scientific_name_authorship = scientificNameAuthorship,
        submitted_wfo_name = Old.name,
        submitted_wfo_status = Old.status,
        accepted_name_changed = New.accepted,
        references,
        source
      )
  }

  match_wfo_names <- function(x, strategy, fuzzy = 0) {
    if (nrow(x) == 0) {
      return(tibble::tibble())
    }

    all_candidates <- WorldFlora::WFO.match(
      spec.data = data.frame(
        spec.name = x$aligned_name,
        stringsAsFactors = FALSE
      ),
      WFO.data = WFO.data,
      Fuzzy = fuzzy,
      verbose = FALSE
    ) |>
      tibble::as_tibble() |>
      dplyr::rename(aligned_name = spec.name)

    format_candidate_table(
      all_candidates = all_candidates,
      x = x,
      strategy = strategy
    )
  }

  species_input <- species_list |>
    dplyr::filter(name_class %in% c("species", "infraspecific"))

  species_exact <- match_wfo_names(
    x = species_input,
    strategy = "species_exact",
    fuzzy = 0
  )

  unresolved_species <- species_exact |>
    dplyr::group_by(aligned_name) |>
    dplyr::summarise(any_matched = any(matched), .groups = "drop") |>
    dplyr::filter(!any_matched) |>
    dplyr::select(aligned_name) |>
    dplyr::inner_join(species_input, by = "aligned_name")

  species_fuzzy <- match_wfo_names(
    x = unresolved_species,
    strategy = "species_fuzzy",
    fuzzy = 0.1
  )

  species_candidates <- species_exact |>
    dplyr::bind_rows(species_fuzzy)

  genus_candidates <- species_list |>
    dplyr::filter(name_class == "genus") |>
    match_wfo_names(strategy = "genus_exact", fuzzy = 0) |>
    dplyr::mutate(
      review_reason = dplyr::case_when(
        !matched ~ "genus_no_wfo_match",
        taxon_rank != "genus" ~ "genus_input_not_matched_to_genus_rank",
        candidate_count > 1 ~ "multiple_wfo_candidates",
        TRUE ~ review_reason
      )
    )

  informal_candidates <- species_list |>
    dplyr::filter(name_class == "informal") |>
    build_unmatched_rows(
      review_reason = "informal_name_manual_review",
      strategy = "informal_skipped"
    )

  missing_candidates <- species_list |>
    dplyr::filter(name_class == "missing") |>
    build_unmatched_rows(
      review_reason = "missing_name_manual_review",
      strategy = "missing_skipped"
    )

  candidate_table <- dplyr::bind_rows(
    species_candidates,
    genus_candidates,
    informal_candidates,
    missing_candidates
  ) |>
    dplyr::arrange(
      aligned_name,
      dplyr::desc(wfo_preferred),
      wfo_scientific_name
    )

  review_table <- candidate_table |>
    dplyr::filter(!is.na(review_reason)) |>
    dplyr::group_by(aligned_name) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(aligned_name)

  readr::write_csv(candidate_table, candidate_output_path, na = "")
  readr::write_csv(review_table, review_output_path, na = "")

  invisible(list(
    candidate_table = candidate_table,
    review_table = review_table
  ))
}
