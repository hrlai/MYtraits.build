# Hand-entry helpers for tiny families (one to a few taxa) and for single taxon
# description papers (for example Gardens' Bulletin Singapore). No segmenter or
# clause parser: each row is written by hand with its verbatim clause and page.
# Pattern taken from data/Yao_2010/raw/build_trait_data.R (row / un / tri / rng);
# ext from data/Kiew_2015.
#
# Use:
#   source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
#   h <- flora_hand("Pentaphylax euryoides",
#                   pages = list(sp1 = c(9, 162), genus1 = c(8, 161)))
#   d <- bind_rows(
#     h$row("leaf_arrangement", "Leaves simple, spirally arranged, ...",
#           value = "spirally arranged", clean = "spiral", rank = "genus",
#           p = "genus1", sec = "Genus description"),
#     h$tri("leaf_length", raw, lo = 5, mid = 8, hi = 12, unit = "cm", p = "sp1"),
#     h$ext("leaf_width", "(2-)3-5(-7) cm", a = 2, b = 3, c = 5, d = 7,
#           unit = "cm", p = "sp1"),
#     h$un(raw, "stipules absent (negative phrase)", p = "sp1")
#   )
# `pages` maps a short key to c(pdf page, printed page). Pass `source_id` for
# values the flora cites from another work. Run from the repository root.

suppressPackageStartupMessages(library(tidyverse))
# The project .Rprofile attaches traits.build (and so dplyr) before stats, so
# stats::filter and stats::lag would otherwise mask the dplyr verbs.
filter <- dplyr::filter
lag <- dplyr::lag

flora_hand <- function(taxon, pages, rank = "species") {
  row <- function(
    trait,
    raw,
    value = "",
    clean = "",
    vt = "mode",
    unit = "",
    q = "",
    em = "",
    rank_from = "",
    sec = "Description",
    p,
    region = "",
    form = "",
    notes = "",
    source_id = ""
  ) {
    stopifnot(p %in% names(pages))
    tibble(
      taxon_name = taxon,
      taxon_rank = rank,
      entity_type = "species",
      rank_scored_from = rank_from,
      commonness_qualifier = q,
      population_region = region,
      form = form,
      entity_measured = em,
      trait = trait,
      trait_value_raw = raw,
      trait_value = as.character(value),
      trait_value_clean = as.character(clean),
      value_type = vt,
      unit = unit,
      source_section = sec,
      pdf_page = pages[[p]][1],
      printed_page = pages[[p]][2],
      notes = notes,
      source_id = source_id
    )
  }
  # Unmapped clause: blank trait and level, verbatim text kept. `what` is
  # "<organ> | <character>" so flora_unmapped_summary() can group it; `notes`
  # becomes the optional third "| <detail>" field.
  un <- function(raw, what, notes = "", ...) {
    if (nzchar(notes)) {
      stopifnot(grepl("|", what, fixed = TRUE))
      what <- paste(what, notes, sep = " | ")
    }
    row("", raw, notes = paste0("UNMAPPED: ", what), ...)
  }
  # Typical (a-)b(-c): mean on `usually`, extremes on `extreme_range`.
  tri <- function(trait, raw, lo, mid, hi, unit, ...) {
    bind_rows(
      row(trait, raw, mid, mid, "mean", unit, "usually", ...),
      row(trait, raw, lo, lo, "minimum", unit, "extreme_range", ...),
      row(trait, raw, hi, hi, "maximum", unit, "extreme_range", ...)
    )
  }
  rng <- function(trait, raw, lo, hi, unit, ...) {
    bind_rows(
      row(trait, raw, lo, lo, "minimum", unit, ...),
      row(trait, raw, hi, hi, "maximum", unit, ...)
    )
  }
  # Typical range with optional parenthetical extremes (a-)b-c(-d): b and c on
  # `usually`, a and d (when given) on `extreme_range`.
  ext <- function(trait, raw, a = NA, b, c, d = NA, unit, ...) {
    bind_rows(
      rng(trait, raw, b, c, unit, q = "usually", ...),
      if (!is.na(a)) row(trait, raw, a, a, "minimum", unit, "extreme_range", ...),
      if (!is.na(d)) row(trait, raw, d, d, "maximum", unit, "extreme_range", ...)
    )
  }
  list(row = row, un = un, tri = tri, rng = rng, ext = ext)
}
