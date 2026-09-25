align_to_wfo <- function(
  species_list_path = "inst/taxonomy/MYtraits_raw_species_list.csv",
  wfo_backbone_path = "inst/extdata/classification.csv",
  candidate_output_path = "inst/taxonomy/MYtraits_taxon_alignment_detail.csv",
  review_output_path = "inst/taxonomy/MYtraits_alignment_review_needed.csv",
  allow_unchecked_species = FALSE
) {
  species_list <- readr::read_csv(species_list_path, show_col_types = FALSE)

  required_cols <- c(
    "original_name",
    "n_datasets",
    "datasets",
    "n_records",
    "name_class"
  )

  missing_cols <- setdiff(required_cols, names(species_list))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Species list is missing required columns.",
      "x" = "Missing: {paste(missing_cols, collapse = ', ')}",
      "i" = "Check the input file for the expected schema before running {.code align_to_wfo}."
    ))
  }

  WorldFlora::WFO.remember(wfo_backbone_path)

  accepted_taxa <- WFO.data |>
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

  genus_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "genus") |>
    dplyr::transmute(genus = taxon_name, taxon_id_genus = taxon_id)

  family_ids <- accepted_taxa |>
    dplyr::filter(taxon_rank == "family") |>
    dplyr::transmute(family = taxon_name, taxon_id_family = taxon_id)

  build_unmatched_rows <- function(x, review_reason, strategy) {
    if (nrow(x) == 0) {
      return(tibble::tibble())
    }

    x |>
      dplyr::mutate(
        raw_candidate_rows = 0L,
        spec.name = original_name,
        spec.name.ORIG = original_name,
        match_strategy = strategy,
        wfo_preferred = FALSE,
        review_reason = review_reason,
        matched = FALSE,
        fuzzy = FALSE,
        fuzzy_distance = NA_real_,
        taxon_id = NA_character_,
        scientific_name_id = NA_character_,
        taxon_name = NA_character_,
        taxon_rank = NA_character_,
        scientific_name_authorship = NA_character_,
        genus = NA_character_,
        family = NA_character_,
        taxonomic_status = NA_character_,
        taxon_id_genus = NA_character_,
        taxon_id_family = NA_character_,
        aligned_name_taxon_id = NA_character_,
        aligned_name_taxonomic_status = NA_character_
      )
  }

  format_candidate_table <- function(all_candidates, x, strategy) {
    preferred <- all_candidates |>
      as.data.frame() |>
      WorldFlora::WFO.one(spec.name = "spec.name.ORIG", verbose = FALSE) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        dplyr::across(
          c(taxonID, acceptedNameUsageID),
          ~ dplyr::na_if(.x, "")
        ),
        aligned_name_taxon_id = taxonID,
        aligned_name_taxonomic_status = dplyr::if_else(
          is.na(taxonID),
          NA_character_,
          stringr::str_to_lower(taxonomicStatus)
        ),
        taxon_id = dplyr::coalesce(acceptedNameUsageID, taxonID)
      ) |>
      dplyr::select(
        original_name = spec.name.ORIG,
        scientificName,
        taxonRank,
        aligned_name_taxon_id,
        aligned_name_taxonomic_status,
        taxon_id,
        matched = Matched,
        fuzzy = Fuzzy,
        fuzzy_distance = Fuzzy.dist,
        fuzzy_one = Fuzzy.one,
        fuzzy_two = Fuzzy.two
      ) |>
      dplyr::left_join(accepted_taxa, by = join_by(taxon_id)) |>
      dplyr::left_join(genus_ids, by = join_by(genus)) |>
      dplyr::left_join(family_ids, by = join_by(family))

    x |>
      dplyr::left_join(
        preferred,
        by = "original_name",
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(
        matched = dplyr::coalesce(matched, FALSE),
        fuzzy = dplyr::coalesce(fuzzy, FALSE),
        fuzzy_distance = as.numeric(fuzzy_distance),
        fuzzy_one = dplyr::coalesce(fuzzy_one, FALSE),
        fuzzy_two = dplyr::coalesce(fuzzy_two, FALSE),
        match_strategy = dplyr::case_when(
          !matched ~ paste0(strategy, "_unmatched"),
          fuzzy ~ paste0(strategy, "_fuzzy"),
          .default = paste0(strategy, "_exact")
        ),
        wfo_preferred = !is.na(aligned_name_taxon_id),
        review_reason = dplyr::case_when(
          is.na(aligned_name_taxon_id) ~ "no_wfo_match",
          taxonRank == "family" ~ "original_name_is_family_rank",
          taxonRank == "species" &
            aligned_name_taxonomic_status == "unchecked" &
            !allow_unchecked_species ~ "unchecked_wfo_match_manual_review",
          taxonRank == "genus" & name_class == "genus" ~ NA_character_,
          .default = NA_character_
        )
      )
  }

  match_wfo_names <- function(
    x,
    strategy,
    fuzzy = 0,
    fuzzy_one = FALSE,
    fuzzy_two = TRUE
  ) {
    if (nrow(x) == 0) {
      return(tibble::tibble())
    }

    all_candidates <- WorldFlora::WFO.match(
      spec.data = data.frame(
        spec.name = x$original_name,
        stringsAsFactors = FALSE
      ),
      WFO.data = WFO.data,
      Fuzzy = fuzzy,
      Fuzzy.one = fuzzy_one,
      Fuzzy.two = fuzzy_two,
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
    strategy = "species",
    fuzzy = 0,
    fuzzy_one = FALSE,
    fuzzy_two = FALSE
  )
  gc(verbose = FALSE)

  species_unresolved <- species_exact |>
    dplyr::filter(!matched) |>
    dplyr::select(original_name)

  if (nrow(species_unresolved) > 0) {
    species_fuzzy <- match_wfo_names(
      x = species_unresolved |>
        dplyr::left_join(
          species_input,
          by = "original_name",
          relationship = "one-to-one"
        ),
      strategy = "species",
      fuzzy = 0.1,
      fuzzy_one = FALSE,
      fuzzy_two = FALSE
    )
    gc(verbose = FALSE)

    species_candidates <- species_exact |>
      dplyr::anti_join(
        species_unresolved,
        by = "original_name"
      ) |>
      dplyr::bind_rows(species_fuzzy) |>
      dplyr::distinct(original_name, .keep_all = TRUE)
  } else {
    species_candidates <- species_exact
  }

  genus_candidates <- species_list |>
    dplyr::filter(name_class == "genus") |>
    match_wfo_names(strategy = "genus", fuzzy = 0) |>
    dplyr::mutate(
      review_reason = dplyr::case_when(
        !matched ~ "genus_no_wfo_match",
        taxonRank == "family" ~ "original_name_is_family_rank",
        taxonRank != "genus" ~ "genus_input_not_matched_to_genus_rank",
        .default = NA_character_
      )
    )
  gc(verbose = FALSE)

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
      original_name,
      dplyr::desc(wfo_preferred),
      scientificName
    )

  review_table <- candidate_table |>
    dplyr::filter(!is.na(review_reason)) |>
    dplyr::arrange(original_name)

  readr::write_csv(candidate_table, candidate_output_path, na = "")
  readr::write_csv(review_table, review_output_path, na = "")

  if (nrow(review_table) > 0) {
    cli::cli_warn(c(
      "{.code align_to_wfo} produced unresolved taxon review entries.",
      "x" = "{nrow(review_table)} row{?s} in {.file {review_output_path}} require attention before next step.",
      "i" = "Fix the upstream metadata schema, especially the {.code exclude_observations} section in source metadata files."
    ))
  } else {
    cli::cli_inform(c(
      "{.code align_to_wfo} found no unresolved taxon review entries.",
      "i" = "Nothing needs manual review before the next step."
    ))
  }

  invisible(list(
    candidate_table = candidate_table,
    review_table = review_table
  ))
}
