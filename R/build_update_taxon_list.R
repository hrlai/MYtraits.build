#' Update `config/taxon_list.csv` from the aligned names in a database.
#'
#' Port of the AusTraits function of the same name, using taxonAlign and the
#' MYtraits references instead of APCalign. Each distinct `aligned_name` in
#' `database$taxonomic_updates` is matched exactly to the references, updated
#' to its current accepted name with `taxonAlign::update_taxa()`, and given the
#' columns `traits.build` expects in a taxon list.
#'
#' Names aligned only to genus or family (`taxonomic_resolution`) keep their
#' aligned name (e.g. `Cleistanthus sp. [Cleistanthus indet; Both_2019]`) as
#' `taxon_name`, with the identifiers of their genus or family. WCVP has no
#' family records; taxonAlign adds one per family when it prepares the
#' resources (`taxon_ID` `"<taxonomic_dataset>_family_<family>"`), and family
#' names are matched to those. Names with no exact match in the references are
#' left out: align them first with `build_align_taxon_names()`.
#'
#' @param database A compiled MYtraits database, e.g.
#'   `readRDS("export/data/curr/MYtraits.rds")`.
#' @param resources The list returned by `load_mytraits_resources()`.
#' @param replace If `TRUE`, rewrite the taxon list from scratch. If `FALSE`,
#'   only add aligned names not yet in it.
#' @param path Path of the taxon list.
#'
#' @return The new taxon list, invisibly.
build_update_taxon_list <- function(database, resources, replace = FALSE,
                                    path = "config/taxon_list.csv") {
  higher_ranks <- c("family", "genus")
  infraspecific <- c("subspecies", "variety", "form")

  all_taxa <- database$taxonomic_updates |>
    dplyr::filter(!is.na(.data$aligned_name)) |>
    dplyr::select(dplyr::all_of(c("aligned_name", "taxonomic_resolution"))) |>
    dplyr::arrange(.data$aligned_name, .data$taxonomic_resolution) |>
    dplyr::distinct(.data$aligned_name, .keep_all = TRUE) |>
    # Bracketed notes and genus-level names are matched on the name itself
    dplyr::mutate(
      name_to_match_to = dplyr::case_when(
        taxonomic_resolution %in% higher_ranks ~
          stringr::word(aligned_name, 1),
        stringr::str_detect(aligned_name, stringr::fixed("[")) &
          taxonomic_resolution %in% "species" ~
          stringr::word(aligned_name, 1, 2),
        stringr::str_detect(aligned_name, stringr::fixed("[")) &
          taxonomic_resolution %in% infraspecific ~
          stringr::word(aligned_name, 1, 4),
        TRUE ~ aligned_name
      )
    )

  # One record per name: regional before global, then accepted first. Family
  # records exist only in the prepared resources.
  families <- resources$combined$family |>
    dplyr::mutate(source = "family")
  reference <- resources$reference |>
    dplyr::bind_rows(families) |>
    dplyr::arrange(
      .data$canonical_name, .data$source != "regional",
      .data$taxonomic_status != "accepted"
    )
  matched <- all_taxa |>
    dplyr::inner_join(
      reference |>
        dplyr::distinct(.data$canonical_name, .keep_all = TRUE) |>
        dplyr::select(dplyr::all_of(c(
          name_to_match_to = "canonical_name", "taxon_rank",
          "taxonomic_status", "taxonomic_dataset", "genus", "family",
          "taxon_ID", "accepted_name_usage_ID"
        ))),
      by = "name_to_match_to"
    )

  accepted_families <- families |>
    dplyr::filter(.data$taxonomic_status == "accepted")

  # update_taxa() returns the accepted record's `taxon_ID` and keeps the
  # matched record's (joined above) as `aligned_taxon_ID`
  updated <- matched |>
    dplyr::rename(match_name = "aligned_name", aligned_name = "name_to_match_to") |>
    taxonAlign::update_taxa(resources = resources$combined) |>
    dplyr::rename(name_to_match_to = "aligned_name", aligned_name = "match_name")

  taxon_list_new <- updated |>
    dplyr::mutate(
      is_higher = .data$taxonomic_resolution %in% higher_ranks,
      taxon_name = ifelse(
        .data$is_higher, .data$aligned_name,
        stringr::str_trim(stringr::str_remove(
          .data$suggested_name, " \\[alternative possible names:.*\\]$"
        ))
      ),
      taxon_rank = ifelse(.data$is_higher, .data$taxonomic_resolution,
                          .data$taxon_rank),
      # family records have no `family` of their own
      family = ifelse(
        .data$taxon_rank == "family",
        dplyr::coalesce(.data$accepted_name, .data$name_to_match_to),
        .data$family
      ),
      taxon_name_alternatives = .data$alternative_possible_names,
      genus = ifelse(.data$taxon_rank == "family", NA_character_,
                     stringr::word(.data$taxon_name, 1)),
      binomial = dplyr::case_when(
        taxon_rank == "species" ~ taxon_name,
        taxon_rank %in% infraspecific ~ stringr::word(taxon_name, 1, 2)
      ),
      trinomial = ifelse(.data$taxon_rank %in% infraspecific,
                         .data$taxon_name, NA_character_),
      # shortcut: load_WCVP() does not read WCVP's distributions, so
      # update_taxa() returns NA; fill these if needed
      taxon_distribution = .data$taxon_distribution,
      establishment_means = NA_character_,
      # A `Genus sp. [...]` name is not the genus itself: only the genus and
      # family identifiers apply
      scientific_name = ifelse(.data$is_higher, NA_character_,
                               .data$scientific_name),
      taxon_id = ifelse(.data$is_higher, NA_character_, .data$taxon_ID),
      taxon_id_genus = .data$taxon_ID_genus,
      taxon_id_family = accepted_families$taxon_ID[
        match(.data$family, accepted_families$canonical_name)
      ],
      scientific_name_id = ifelse(.data$is_higher, NA_character_,
                                  .data$scientific_name_ID),
      aligned_name_taxon_id = .data$aligned_taxon_ID,
      aligned_name_taxonomic_status = .data$taxonomic_status_aligned
    ) |>
    dplyr::select(dplyr::all_of(c(
      "aligned_name", "taxon_name", "taxon_rank", "taxonomic_status",
      "taxonomic_dataset", "taxon_name_alternatives", "genus", "family",
      "binomial", "trinomial", "taxon_distribution", "establishment_means",
      "scientific_name", "taxon_id", "taxon_id_genus", "taxon_id_family",
      "scientific_name_id", "aligned_name_taxon_id",
      "aligned_name_taxonomic_status"
    )))

  if (replace) {
    taxon_list <- taxon_list_new
  } else {
    taxon_list <- readr::read_csv(path, col_types = readr::cols(
      .default = "c"
    ))
    taxon_list <- taxon_list |>
      dplyr::bind_rows(
        taxon_list_new |>
          dplyr::filter(!.data$aligned_name %in% taxon_list$aligned_name)
      )
  }

  taxon_list <- taxon_list |>
    dplyr::arrange(.data$taxon_name, .data$aligned_name) |>
    dplyr::distinct(.data$aligned_name, .keep_all = TRUE)

  readr::write_csv(taxon_list, path, na = "")
  invisible(taxon_list)
}
