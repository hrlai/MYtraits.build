# Assembly steps shared by every FPM / TFSS family: higher-rank copy-down,
# species -> infraspecific inheritance, duplicate-key merge, final column
# order, and the trait_data.csv -> data.csv step.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
# Run from the repository root. This script loads flora_parse.R because
# flora_finalise() uses its text-normalisation helpers.
#
# Row tables use the column names of the family parsers:
#   block, rank, trait, trait_value, trait_value_clean, value_type, unit, cq
#   (commonness qualifier), entity_measured, trait_value_raw, pdf_page,
#   printed_page, notes, population_region, form
# `rank` is one of family, genus, infrageneric_group, species, subspecies,
# variety, informal_form.

suppressPackageStartupMessages(library(tidyverse))
# The project .Rprofile attaches traits.build (and so dplyr) before stats, so
# stats::filter and stats::lag would otherwise mask the dplyr verbs.
filter <- dplyr::filter
lag <- dplyr::lag
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

# Standard order of columns in raw/trait_data.csv (all three FPM datasets).
flora_trait_data_cols <- c(
  "taxon_name",
  "taxon_rank",
  "entity_type",
  "rank_scored_from",
  "commonness_qualifier",
  "population_region",
  "form",
  "entity_measured",
  "trait",
  "in_traits.yml",
  "trait_value_raw",
  "trait_value",
  "trait_value_clean",
  "value_type",
  "unit",
  "source_section",
  "pdf_page",
  "printed_page",
  "notes",
  "source_id"
)

# members: one row per scored taxon with columns
#   block, genus, group (NA when none), species (the parent species for an
#   infraspecific taxon, else itself), rank
# Built from the treatment's own group paragraphs and headings, never guessed.

.flora_members_of <- function(members, b, r) {
  switch(
    r,
    family = members$block,
    genus = members$block[members$genus %in% b],
    infrageneric_group = members$block[members$group %in% b],
    character()
  )
}

# Copy an unqualified categorical value from a family / genus / group
# description to member taxa that are silent on the trait. Rules (user rules,
# written down in the notes and used by Coode_2018, Lim_2010 and Yao_2010):
#   - only value_type == "mode", no commonness word, exactly one mapped level;
#   - drop statements scoped to some members ("most", "many", "in some");
#   - never numbers; never a "sometimes" statement from genus level;
#   - copy only if every member that scores the trait itself includes the level;
#   - entity_measured (e.g. male / female flowers) is part of the trait: test
#     and copy each entity separately, so male and female values never mix;
#   - when several ranks give a value, the most specific rank wins
#     (group > genus > family), so family and genus alternatives are not both
#     copied.
# Returns the copied rows plus attr(, "rejected"): higher-rank rows that failed
# the agreement test, to be read in the QA step.
flora_copy_down <- function(higher_rows, own_rows, members) {
  higher <- higher_rows |>
    filter(
      rank %in% c("family", "genus", "infrageneric_group"),
      value_type == "mode",
      is.na(cq),
      !is.na(trait_value_clean),
      trait_value_clean != "",
      !str_detect(trait_value_clean, " "),
      !str_detect(trait_value_raw, "\\b(in some|most|many|some species)\\b")
    )
  tests <- pmap_dfr(higher, \(...) {
    h <- list(...)
    mem <- .flora_members_of(members, h$block, h$rank)
    scored <- own_rows |>
      filter(
        block %in% mem,
        trait == h$trait,
        coalesce(entity_measured, "") == coalesce(h$entity_measured, "")
      )
    ok <- all(map_lgl(
      split(scored$trait_value_clean, scored$block),
      \(v) any(str_detect(v, paste0("\\b", h$trait_value_clean, "\\b")))
    ))
    tibble(
      from = h$block,
      from_rank = h$rank,
      trait = h$trait,
      entity_measured = h$entity_measured,
      value = h$trait_value_clean,
      ok = ok,
      copied_to = list(
        if (ok) setdiff(mem, unique(scored$block)) else character()
      )
    )
  })
  rejected <- tests |> filter(!ok) |> select(-copied_to)
  src <- higher |>
    transmute(
      from = block,
      from_rank = rank,
      trait,
      value = trait_value_clean,
      trait_value,
      trait_value_raw,
      entity_measured,
      pdf_page,
      printed_page
    ) |>
    distinct(from, from_rank, trait, entity_measured, value, .keep_all = TRUE)
  copied <- tests |>
    filter(ok) |>
    unnest(copied_to) |>
    rename(block = copied_to) |>
    left_join(
      src,
      by = c("from", "from_rank", "trait", "entity_measured", "value")
    ) |>
    mutate(
      lvl = match(from_rank, c("infrageneric_group", "genus", "family"))
    ) |>
    group_by(block, trait, entity_measured) |>
    filter(lvl == min(lvl)) |>
    ungroup() |>
    transmute(
      block,
      trait,
      cq = NA_character_,
      trait_value,
      trait_value_clean = value,
      value_type = "mode",
      unit = NA_character_,
      trait_value_raw,
      entity_measured,
      notes = paste0("Copied from the ", from, " description."),
      pdf_page,
      printed_page,
      rank_scored_from = recode(
        from_rank,
        infrageneric_group = "species_group"
      ),
      source_section = paste(
        recode(
          from_rank,
          family = "Family",
          genus = "Genus",
          infrageneric_group = "Group"
        ),
        "description"
      )
    )
  attr(copied, "rejected") <- rejected
  copied
}

# An infraspecific taxon (subsp., var., informal form) silent on a trait
# inherits its species' own values (rank_scored_from = "species"). Informal
# forms that are merged into the species with a `form` context should be
# excluded from `infra_members` and from this step (Coode_2018 rule).
flora_inherit_infraspecific <- function(own_rows, members) {
  infra <- members |> filter(rank != "species") |> select(block, species)
  infra |>
    inner_join(
      own_rows |> filter(rank == "species") |> rename(species = block),
      by = "species",
      relationship = "many-to-many"
    ) |>
    anti_join(own_rows |> distinct(block, trait), by = c("block", "trait")) |>
    mutate(
      rank_scored_from = "species",
      source_section = "Species description",
      notes = if_else(
        is.na(notes),
        "Inherited from the species description.",
        paste(notes, "Inherited from the species description.")
      )
    ) |>
    select(-species)
}

# Merge categorical rows that share every key column, including
# rank_scored_from, but differ in value (two clauses of one description that
# map to one trait; Yao_2010 bark texture, "rarely ovate or falcate" split in
# two). `dataset_test()` fails on these with "cannot pivot wider". Levels are
# merged space-delimited in text order and raw clauses joined with " | ".
# Numeric clashes are left as they are and reported by flora_qa().
#
# Rows that repeat one value from two ranks (Coode_2018: 23 pairs, e.g. genus
# and group both say "raceme") are NOT merged here; use
# flora_drop_redundant_ranks().
flora_merge_duplicate_keys <- function(d) {
  key <- c(
    "taxon_name",
    "trait",
    "rank_scored_from",
    "commonness_qualifier",
    "population_region",
    "form",
    "entity_measured",
    "value_type",
    "unit"
  )
  key <- intersect(key, names(d))
  mergeable <- d$value_type %in% "mode" & !is.na(d$trait) & d$trait != ""
  keyed <- d[mergeable, ] |>
    group_by(across(all_of(key))) |>
    summarise(
      across(
        any_of(c("trait_value", "trait_value_clean")),
        \(x) paste(unique(unlist(str_split(x[!is.na(x)], " "))), collapse = " ")
      ),
      trait_value_raw = paste(unique(trait_value_raw), collapse = " | "),
      across(everything(), dplyr::first),
      .groups = "drop"
    )
  bind_rows(keyed, d[!mergeable, ]) |>
    select(all_of(names(d)))
}

# Final shape of raw/trait_data.csv. Mapped and unmapped records share the file:
# unmapped rows have a blank `trait`, blank levels and notes starting
# "UNMAPPED:". Verbatim text is kept for every row.
flora_finalise <- function(rows, traits, source_default = NA_character_) {
  tn <- names(traits)
  rows |>
    mutate(
      entity_type = "species",
      in_traits.yml = if_else(!is.na(trait) & trait %in% tn, "x", ""),
      value_type = coalesce(value_type, "mode"),
      source_section = paste0(source_section, ", p.", printed_page),
      trait_value_raw = flora_ascii_raw(trait_value_raw),
      across(
        c(trait_value_raw, trait_value, notes),
        \(x) {
          str_replace_all(x, "(?<=[0-9)(])\\s?[\u2013-]\\s?(?=[0-9()])", "--")
        }
      )
    ) |>
    select(any_of(flora_trait_data_cols))
}

# trait_data.csv -> data.csv for a flora dataset. Adds the three build columns:
#   value                trait_value_clean for numeric traits and the y/n
#                        calendars, else the source term trait_value (mapped to
#                        levels by `substitutions:` in metadata.yml)
#   trait_scoring_method from rank_scored_from
#   source_id            the `source_id` column of trait_data.csv, else `id`
# Unmapped rows (blank trait) stay in trait_data.csv only. Writes only
# data/<id>/data.csv; the build never reads raw/.
flora_make_data <- function(
  id,
  traits = yaml::read_yaml("config/traits.yml")$traits$elements,
  out_file = file.path("data", id, "data.csv")
) {
  td <- read_csv(
    file.path("data", id, "raw", "trait_data.csv"),
    col_types = cols(.default = "c"),
    na = ""
  ) |>
    filter(!is.na(trait), trait != "")
  stopifnot(all(td$trait %in% names(traits)))
  numeric_trait <- map_lgl(td$trait, \(t) {
    identical(traits[[t]]$type, "numeric")
  })
  if (!"source_id" %in% names(td)) {
    td$source_id <- NA_character_
  }
  data <- td |>
    mutate(
      value = if_else(
        numeric_trait | trait %in% c("flowering_time", "fruiting_time"),
        trait_value_clean,
        trait_value
      ),
      trait_scoring_method = case_when(
        is.na(rank_scored_from) |
          rank_scored_from %in% c("", "own") ~ "inferred_from_species",
        rank_scored_from == "family" ~ "inferred_from_family",
        rank_scored_from == "genus" ~ "inferred_from_genus",
        rank_scored_from == "species_group" ~ "inferred_from_species_group",
        rank_scored_from == "species" ~ "inferred_from_species",
        .default = NA_character_
      ),
      source_id = coalesce(source_id, id)
    )
  write_csv(data, out_file, na = "")
  invisible(data)
}

# raw/unmapped_terms.csv: a summary of the unmapped rows in trait_data.csv (blank
# `trait`), never the value store. Unmapped rows carry notes of the form
# "UNMAPPED: <organ> | <character> | <detail>" (organ and detail optional), the
# verbatim clause in trait_value_raw, and the page. Yao_2010 wrote organ and
# character as the same string; Coode_2018 kept only one example per character
# and no verbatim rows. This summary fixes both.
flora_unmapped_summary <- function(trait_data) {
  trait_data |>
    filter(is.na(trait) | trait == "") |>
    mutate(
      spec = str_remove(notes, "^UNMAPPED:\\s*"),
      parts = str_split(spec, "\\s*\\|\\s*"),
      organ = if_else(
        map_int(parts, length) > 1,
        map_chr(parts, 1),
        NA_character_
      ),
      character = map_chr(parts, \(p) if (length(p) > 1) p[2] else p[1])
    ) |>
    group_by(organ, character) |>
    summarise(
      n_taxa = n_distinct(taxon_name),
      example_taxon = first(taxon_name),
      example_text = str_trunc(first(trait_value_raw), 200),
      printed_page = first(printed_page),
      .groups = "drop"
    ) |>
    arrange(desc(n_taxa), organ, character)
}

# Keep one row where the same taxon, trait, qualifier, context and level was
# scored from several ranks (genus + group, species + group). The most specific
# source wins: own text > species > species_group > genus > family. Coode_2018
# kept both (23 pairs); the build accepts that, but the table is cleaner
# without it.
flora_drop_redundant_ranks <- function(d) {
  order_rank <- c("own", "species", "species_group", "genus", "family")
  d |>
    mutate(
      .lvl = match(coalesce(na_if(rank_scored_from, ""), "own"), order_rank)
    ) |>
    group_by(
      taxon_name,
      trait,
      commonness_qualifier,
      population_region,
      form,
      entity_measured,
      value_type,
      trait_value_clean
    ) |>
    filter(
      is.na(trait) | trait == "" | value_type != "mode" | .lvl == min(.lvl)
    ) |>
    ungroup() |>
    select(-.lvl)
}
