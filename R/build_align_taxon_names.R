#' Align a dataset's taxon names and record the alignments in its metadata.
#'
#' Port of the AusTraits function of the same name, using taxonAlign and the
#' MYtraits reference (see `R/build_taxonomic_reference.R`) instead of APCalign.
#' Names that already match a reference name exactly, or that already have a
#' `taxonomic_resolution` in `metadata.yml`, are skipped. Each other name is
#' aligned in two passes:
#'
#' 1. `taxonAlign::align_taxa()` against the regional (MY/BN/SG) reference,
#'    with fuzzy matching.
#' 2. Names that pass 1 matched only fuzzily, only to genus or higher, or not
#'    at all are aligned again against the whole WCVP with exact matching
#'    only. An exact match there replaces the pass-1 result. Fuzzy matching
#'    against the whole WCVP is avoided because it finds look-alike names in
#'    unrelated genera from other continents.
#'
#' Every name whose aligned form differs from the original is written to the
#' dataset's `taxonomic_updates` (spelling and formatting fixes,
#' `Genus sp. [...]` names). Synonym updates are not written here: they live
#' in `config/taxon_list.csv`.
#'
#' The `taxonomic_updates` block is written as text, so the rest of the
#' `metadata.yml` keeps its formatting (`traits.build::write_metadata()`
#' rewrites the whole file).
#'
#' @param database A compiled MYtraits database, e.g.
#'   `readRDS("export/data/curr/MYtraits.rds")`.
#' @param dataset_id Identifier of the dataset to align.
#' @param resources The list returned by `load_mytraits_resources()`.
#' @param write If `FALSE`, return the alignments without writing them.
#'
#' @return Invisibly, a tibble with one row per aligned name: `find`,
#'   `replace`, `reason`, `taxonomic_resolution` and `chars_changed`. Names
#'   with no match have `replace = NA` and are not written; review them.
build_align_taxon_names <- function(database, dataset_id, resources,
                                    write = TRUE) {
  names_to_align <- database$taxonomic_updates |>
    dplyr::filter(.data$dataset_id == .env$dataset_id) |>
    dplyr::filter(!.data$aligned_name %in% resources$canonical_names) |>
    dplyr::filter(is.na(.data$taxonomic_resolution)) |>
    dplyr::distinct(.data$original_name)

  if (nrow(names_to_align) == 0) {
    return(invisible(dplyr::tibble(
      find = character(), replace = character(), reason = character(),
      taxonomic_resolution = character(), chars_changed = numeric()
    )))
  }

  pass_1 <- safe_align_taxa(
    names_to_align$original_name,
    resources = resources$regional,
    identifier = dataset_id
  )

  retry <- is.na(pass_1$aligned_name) |
    pass_1$chars_changed %in% 1:1000 |
    !pass_1$taxon_rank %in% c("species", "subspecies", "variety", "form")
  exact <- pass_1[0, ]
  if (any(retry)) {
    # With fuzzy matching off, any match differs only in formatting
    exact <- safe_align_taxa(
      pass_1$original_name[retry],
      resources = resources$combined,
      identifier = dataset_id,
      fuzzy_matches = FALSE
    ) |>
      dplyr::filter(
        !is.na(.data$aligned_name),
        .data$taxon_rank %in% c("species", "subspecies", "variety", "form")
      )
  }

  names_aligned <- pass_1 |>
    dplyr::filter(!.data$original_name %in% exact$original_name) |>
    dplyr::bind_rows(exact) |>
    dplyr::select(dplyr::all_of(c(
      find = "original_name", replace = "aligned_name",
      reason = "aligned_reason", taxonomic_resolution = "taxon_rank",
      chars_changed = "chars_changed"
    ))) |>
    dplyr::arrange(.data$find)

  if (write) {
    names_aligned |>
      dplyr::filter(!is.na(.data$replace), .data$find != .data$replace) |>
      dplyr::select(-"chars_changed") |>
      write_taxonomic_updates(dataset_id = dataset_id)
  }

  invisible(names_aligned)
}

#' Write rows into the `taxonomic_updates` block of a `metadata.yml`.
#'
#' Merges `updates` with any rows already in the block (a new row replaces an
#' existing row with the same `find`), sorts by `find`, and replaces only the
#' block's lines in the file.
#'
#' @param updates A data frame with columns `find`, `replace`, `reason` and
#'   `taxonomic_resolution`.
#' @param dataset_id Identifier of the dataset.
#' @param path_data Folder holding the dataset folders.
#'
#' @return The path of the `metadata.yml`, invisibly.
#' @keywords internal
write_taxonomic_updates <- function(updates, dataset_id, path_data = "data") {
  path <- file.path(path_data, dataset_id, "metadata.yml")
  if (nrow(updates) == 0) return(invisible(path))

  lines <- readLines(path, encoding = "UTF-8")
  start <- which(stringr::str_detect(lines, "^taxonomic_updates:"))
  if (length(start) != 1) {
    stop("Expected one `taxonomic_updates:` line in ", path)
  }
  # The block runs until the next top-level key
  top_level <- which(stringr::str_detect(lines, "^[^[:space:]#]"))
  end <- min(c(top_level[top_level > start], length(lines) + 1)) - 1

  existing <- yaml::yaml.load(paste(lines[start:end], collapse = "\n"))
  existing <- existing$taxonomic_updates
  if (is.list(existing)) {
    existing <- dplyr::bind_rows(lapply(existing, dplyr::as_tibble))
    updates <- existing |>
      dplyr::filter(!.data$find %in% updates$find) |>
      dplyr::bind_rows(updates)
  }

  updates <- updates |>
    dplyr::select(dplyr::all_of(
      c("find", "replace", "reason", "taxonomic_resolution")
    )) |>
    dplyr::arrange(.data$find)

  block <- yaml::as.yaml(
    list(taxonomic_updates = unname(split(updates, seq_len(nrow(updates))) |>
      lapply(as.list))),
    indent.mapping.sequence = TRUE,
    line.sep = "\n"
  ) |>
    stringr::str_remove("\n$") |>
    strsplit("\n")

  lines <- c(
    lines[seq_len(start - 1)],
    block[[1]],
    lines[-seq_len(end)]
  )
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}
