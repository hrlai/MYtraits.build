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

    x |>
      dplyr::mutate(
        raw_candidate_rows = 0L,
        spec.name = aligned_name,
        spec.name.ORIG = aligned_name,
        match_strategy = strategy,
        wfo_preferred = FALSE,
        review_reason = review_reason,
        matched = FALSE,
        fuzzy = FALSE,
        fuzzy_distance = NA_real_
      )
  }

  format_candidate_table <- function(all_candidates, x, strategy) {
    preferred <- all_candidates |>
      as.data.frame() |>
      WorldFlora::WFO.one(spec.name = "spec.name.ORIG", verbose = FALSE) |>
      tibble::as_tibble()

    raw_candidate_rows <- all_candidates |>
      dplyr::group_by(spec.name.ORIG) |>
      dplyr::summarise(
        raw_candidate_rows = dplyr::n(),
        .groups = "drop"
      )

    x |>
      dplyr::left_join(
        raw_candidate_rows,
        by = c("aligned_name" = "spec.name.ORIG"),
        relationship = "many-to-one"
      ) |>
      dplyr::left_join(
        preferred,
        by = c("aligned_name" = "spec.name.ORIG"),
        relationship = "one-to-one",
        keep = TRUE
      ) |>
      dplyr::mutate(
        match_strategy = strategy,
        wfo_preferred = !is.na(taxonID),
        review_reason = dplyr::case_when(
          is.na(taxonID) ~ "no_wfo_match",
          taxonRank == "family" ~ "original_name_is_family_rank",
          taxonRank == "genus" & name_class == "genus" ~ NA_character_,
          .default = NA_character_
        ),
        matched = !is.na(taxonID),
        fuzzy = identical(strategy, "species_fuzzy"),
        fuzzy_distance = NA_real_
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
      tibble::as_tibble()

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

  species_candidates <- dplyr::bind_rows(species_exact, species_fuzzy)

  genus_candidates <- species_list |>
    dplyr::filter(name_class == "genus") |>
    match_wfo_names(strategy = "genus_exact", fuzzy = 0) |>
    dplyr::mutate(
      review_reason = dplyr::case_when(
        !matched ~ "genus_no_wfo_match",
        taxonRank == "family" ~ "original_name_is_family_rank",
        taxonRank != "genus" ~ "genus_input_not_matched_to_genus_rank",
        .default = NA_character_
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

  candidate_inputs <- list(
    species_candidates,
    genus_candidates,
    informal_candidates,
    missing_candidates
  )

  candidate_table <- candidate_inputs |>
    purrr::keep(~ nrow(.x) > 0) |>
    dplyr::bind_rows() |>
    dplyr::arrange(
      aligned_name,
      dplyr::desc(wfo_preferred),
      scientificName
    )

  review_table <- candidate_table |>
    dplyr::filter(!is.na(review_reason)) |>
    dplyr::arrange(aligned_name)

  readr::write_csv(candidate_table, candidate_output_path, na = "")
  readr::write_csv(review_table, review_output_path, na = "")

  if (nrow(review_table) > 0) {
    cli::cli_warn(c(
      "{.code align_to_wfo} produced unresolved taxon review entries.",
      "x" = "{nrow(review_table)} row{?s} in {.file {review_output_path}} require attention before next step.",
      "i" = "Fix the upstream metadata schema, especially the {.code exclude_observations} section in source metadata files."
    ))
  }

  invisible(list(
    candidate_table = candidate_table,
    review_table = review_table
  ))
}
