# One-page coverage summary of the compiled MYtraits database, for
# taxonomists and botanists.
#
# Run from the project root after building the database:
#   uvr run build.R
#   uvr run inst/notes/coverage/make_summary.R
#
# Input:  export/data/curr/MYtraits.rds
# Output: inst/notes/coverage/summary.md (regenerated, never edit by hand)
#
# Rows are split by basis_of_record: "preserved_specimen" rows come from flora
# treatments; all other rows (field, captive_cultivated) are field-measured.

library(dplyr)
library(purrr)
library(stringr)

# .Rprofile attaches dplyr before Rscript attaches stats, so stats::filter
# would win.
filter <- dplyr::filter

dir_out <- "inst/notes/coverage"
n_top <- 20

db <- readRDS("export/data/curr/MYtraits.rds")

traits <- db$traits |>
  mutate(
    source = if_else(basis_of_record == "preserved_specimen",
                     "Flora", "Field")
  ) |>
  left_join(
    db$taxa |> select(taxon_name, taxon_rank, family),
    by = "taxon_name"
  )

stopifnot(!anyNA(traits$source))

no_family <- "(no family: name not aligned)"
n_unaligned <- n_distinct(traits$taxon_name[is.na(traits$family)])

fmt <- function(x) format(x, big.mark = ",", trim = TRUE)

md_table <- function(df, align) {
  c(
    paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(align, collapse = "|"), "|"),
    df |>
      mutate(across(everything(), as.character)) |>
      pmap_chr(function(...) paste0("| ", paste(c(...), collapse = " | "),
                                    " |"))
  )
}

# Overview ----------------------------------------------------------------

count_all <- function(df) {
  tibble(
    Datasets = n_distinct(df$dataset_id),
    Taxa = n_distinct(df$taxon_name),
    `Species-rank taxa` = n_distinct(
      df$taxon_name[df$taxon_rank %in% "species"]
    ),
    Families = n_distinct(na.omit(df$family)),
    Traits = n_distinct(df$trait_name),
    Rows = nrow(df)
  )
}

overview <- bind_rows(
  traits |> filter(source == "Flora") |> count_all(),
  traits |> filter(source == "Field") |> count_all(),
  count_all(traits)
) |>
  mutate(across(everything(), fmt)) |>
  mutate(Source = c("Flora", "Field-measured", "All"), .before = 1)

# Overlap -----------------------------------------------------------------

taxa_by_source <- split(traits$taxon_name, traits$source) |> map(unique)
taxa_both <- intersect(taxa_by_source$Flora, taxa_by_source$Field)

pairs_by_source <- split(
  paste(traits$taxon_name, traits$trait_name),
  traits$source
) |>
  map(unique)
pairs_both <- intersect(pairs_by_source$Flora, pairs_by_source$Field)

traits_both <- traits |>
  filter(taxon_name %in% taxa_both) |>
  distinct(source, taxon_name, trait_name) |>
  count(source, taxon_name)

overlap_families <- traits |>
  filter(taxon_name %in% taxa_both) |>
  distinct(family = coalesce(family, no_family), taxon_name) |>
  count(family, name = "Taxa in both", sort = TRUE) |>
  rename(Family = family) |>
  slice_head(n = 10)

# Coverage by family ------------------------------------------------------

family_cov <- traits |>
  group_by(Family = coalesce(family, no_family)) |>
  summarise(
    Taxa = n_distinct(taxon_name),
    `Flora taxa` = n_distinct(taxon_name[source == "Flora"]),
    `Field taxa` = n_distinct(taxon_name[source == "Field"]),
    Traits = n_distinct(trait_name),
    `Taxon × trait` = n_distinct(paste(taxon_name, trait_name)),
    Rows = n(),
    .groups = "drop"
  ) |>
  mutate(
    `Fill (%)` = round(100 * `Taxon × trait` / (Taxa * Traits))
  ) |>
  arrange(desc(Taxa), desc(`Taxon × trait`))

# Traits with most taxa ---------------------------------------------------

trait_cov <- traits |>
  group_by(Trait = trait_name) |>
  summarise(
    Taxa = n_distinct(taxon_name),
    `Flora taxa` = n_distinct(taxon_name[source == "Flora"]),
    `Field taxa` = n_distinct(taxon_name[source == "Field"]),
    Families = n_distinct(na.omit(family)),
    Datasets = n_distinct(dataset_id),
    .groups = "drop"
  ) |>
  arrange(desc(Taxa)) |>
  mutate(Trait = paste0("`", Trait, "`"))

top_field <- trait_cov |>
  filter(`Field taxa` > 0) |>
  arrange(desc(`Field taxa`)) |>
  slice_head(n = 10) |>
  select(Trait, `Field taxa`, `Flora taxa`, Datasets)

# Write -------------------------------------------------------------------

version <- db$build_info$version %||% "unknown"
sha <- db$build_info$git_SHA %||% "unknown"

summary_md <- c(
  "# What MYtraits holds: a coverage summary",
  "",
  paste0(
    "Generated ", format(Sys.Date()), " by `", dir_out, "/make_summary.R` ",
    "from `export/data/curr/MYtraits.rds` (version ", version, ", commit ",
    str_sub(sha, 1, 7), "). Do not edit by hand."
  ),
  "",
  paste(
    "MYtraits compiles plant trait data for Malaysia from two kinds of",
    "source. **Flora** data are characters transcribed from family",
    "treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and",
    "Sarawak) and taxon descriptions, so each value describes a species",
    "from herbarium specimens. **Field-measured** data come from studies",
    "that measured plants in plots, gardens or nurseries. Taxon names are",
    "aligned to World Flora Online; a taxon is one aligned name, which may",
    "be a species, an infraspecific taxon or a genus."
  ),
  "",
  "## Overview",
  "",
  md_table(overview, c("---", rep("---:", 6))),
  "",
  paste0(
    fmt(n_unaligned), " taxa are not yet aligned to World Flora Online, so ",
    "they have no family and are counted under their original name. ",
    "Family counts and the family table understate coverage until they are."
  ),
  "",
  paste0(
    "A further ", fmt(nrow(db$excluded_data)), " values were read from the ",
    "sources but excluded during the build and are not counted above: ",
    db$excluded_data |>
      count(error, sort = TRUE) |>
      with(paste0(str_to_lower(error), " (", fmt(n), ")")) |>
      paste(collapse = "; "),
    "."
  ),
  "",
  "## Overlap between flora and field data",
  "",
  paste0(
    "- **", fmt(length(taxa_both)), "** taxa have both flora and ",
    "field-measured data (", round(100 * length(taxa_both) /
      length(taxa_by_source$Field)), "% of field-measured taxa, ",
    round(100 * length(taxa_both) / length(taxa_by_source$Flora)),
    "% of flora taxa)."
  ),
  paste0(
    "- **", fmt(length(pairs_both)), "** taxon × trait combinations have ",
    "values from both sources, so flora and field values can be compared ",
    "directly."
  ),
  paste0(
    "- For taxa in both, the median taxon has ",
    median(traits_both$n[traits_both$source == "Flora"]),
    " flora traits and ",
    median(traits_both$n[traits_both$source == "Field"]),
    " field-measured traits."
  ),
  "",
  "Families with the most taxa in both:",
  "",
  md_table(overlap_families, c("---", "---:")),
  "",
  "## Coverage by family",
  "",
  paste0(
    "The ", n_top, " families with the most taxa, of ",
    fmt(n_distinct(na.omit(traits$family))), ". *Taxon × trait* counts the combinations ",
    "with at least one value. *Fill* is that count as a share of ",
    "taxa × traits recorded for the family."
  ),
  "",
  md_table(slice_head(family_cov, n = n_top), c("---", rep("---:", 7))),
  "",
  "## Traits with the most taxa",
  "",
  paste0(
    "The ", n_top, " traits with the most taxa, of ",
    fmt(nrow(trait_cov)), "."
  ),
  "",
  md_table(slice_head(trait_cov, n = n_top), c("---", rep("---:", 5))),
  "",
  "The 10 traits with the most field-measured taxa:",
  "",
  md_table(top_field, c("---", rep("---:", 3)))
)

writeLines(summary_md, file.path(dir_out, "summary.md"))
message("Wrote ", file.path(dir_out, "summary.md"))
