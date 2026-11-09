#' Build the MYtraits taxonomic reference.
#'
#' There is no complete checklist of the Malaysian flora yet. Until there is,
#' MYtraits uses the World Checklist of Vascular Plants (WCVP), restricted to
#' the taxa GBIF has occurrence records of in the given countries. Accepted
#' names keep all their synonyms, so older names in a dataset still update.
#' See `inst/notes/taxonomy/README.md`.
#'
#' Downloads (about 90 MB for the WCVP) and GBIF names are cached by
#' taxonAlign.dataprep in `tools::R_user_dir("taxonAlign", "cache")`.
#'
#' @param wcvp_version WCVP version number. Pin it so an alignment can be
#'   repeated.
#' @param countries Two-letter ISO country codes passed to
#'   `taxonAlign.dataprep::filter_to_country()`.
#' @param output_path Where to write the reference (parquet).
#'
#' @return The reference table, invisibly.
build_mytraits_reference <- function(
  wcvp_version = 16,
  countries = c("MY", "BN", "SG"),
  output_path = "inst/taxonomy/taxonomic_reference.parquet"
) {
  wcvp <- taxonAlign.dataprep::load_WCVP(wcvp_version)
  reference <- taxonAlign.dataprep::filter_to_country(wcvp, countries)
  taxonAlign::check_taxonomic_reference(reference)

  reference <- reference |>
    dplyr::mutate(taxonomic_dataset = paste0("WCVP v", wcvp_version))

  arrow::write_parquet(reference, output_path)
  message(
    "Wrote ", nrow(reference), " records (WCVP v", wcvp_version, "; ",
    paste(countries, collapse = ", "), ") to ", output_path
  )
  invisible(reference)
}

#' Load the MYtraits taxonomic references, prepared for taxonAlign.
#'
#' Returns two prepared references:
#' - `regional`: the MY/BN/SG reference from `build_mytraits_reference()`. Use
#'   it for fuzzy matching, so a misspelt name only matches a regional taxon.
#' - `combined`: the regional reference first, then the whole WCVP (same
#'   version). Use it for exact matching of names missing from the regional
#'   list, and for updating names to their accepted names.
#'
#' Preparing the whole WCVP takes a few minutes. Load once per session.
#'
#' @param path Path to the reference written by `build_mytraits_reference()`.
#'
#' @return A list with `regional` and `combined` (prepared with
#'   `taxonAlign::prepare_taxonomic_resources()`), `reference` (the regional
#'   and whole-WCVP tables bound together, with a `source` column) and
#'   `canonical_names` (every name in `reference`).
load_mytraits_resources <- function(
  path = "inst/taxonomy/taxonomic_reference.parquet"
) {
  if (!file.exists(path)) {
    stop(path, " not found. Build it with build_mytraits_reference().")
  }
  regional <- arrow::read_parquet(path) |> dplyr::as_tibble()
  dataset <- unique(regional$taxonomic_dataset)
  wcvp_version <- as.numeric(sub("^WCVP v", "", dataset))
  if (length(dataset) != 1 || is.na(wcvp_version)) {
    stop("Expected one `taxonomic_dataset` like 'WCVP v16' in ", path)
  }
  global <- taxonAlign.dataprep::load_WCVP(wcvp_version) |>
    dplyr::mutate(taxonomic_dataset = dataset)

  list(
    regional = taxonAlign::prepare_taxonomic_resources(regional),
    combined = taxonAlign::prepare_taxonomic_resources(
      list(regional = regional, global = global)
    ),
    reference = dplyr::bind_rows(regional = regional, global = global,
                                 .id = "source"),
    canonical_names = unique(c(regional$canonical_name, global$canonical_name))
  )
}

#' Align names, crash-safe.
#'
#' `taxonAlign::align_taxa()` stops on some names (e.g. the hyphenated genus
#' "Neo-uvaria"). If the whole call fails, align names one at a time and
#' return `NA` for those that fail.
#'
#' @param original_name Names to align.
#' @param ... Passed to `taxonAlign::align_taxa()`.
#'
#' @return The output of `taxonAlign::align_taxa()`, one row per name.
#' @keywords internal
safe_align_taxa <- function(original_name, ...) {
  tryCatch(
    taxonAlign::align_taxa(original_name, ...),
    error = function(e) {
      # shortcut: name-by-name is slow; remove once taxonAlign fixes the crash
      rows <- lapply(original_name, function(x) {
        tryCatch(
          taxonAlign::align_taxa(x, ...),
          error = function(e) {
            warning("taxonAlign::align_taxa() failed on '", x, "': ",
                    conditionMessage(e), call. = FALSE)
            dplyr::tibble(original_name = x)
          }
        )
      })
      dplyr::bind_rows(rows)
    }
  )
}
