# Rank unmapped flora characters and write the upstream trait request.
#
# Run from the project root after each wave of flora datasets:
#   uvr run inst/notes/upstream_traits/make_report.R
#
# Inputs:
#   data/*/raw/unmapped_terms.csv      written by the flora extraction skill
#   inst/notes/upstream_traits/decisions.csv   hand-edited, one row per
#     organ + character: decision (existing / propose / ignore), tier for
#     proposals (easy / medium / hard), template_trait, proposed_name,
#     proposed_type, status (draft / sent / accepted / rejected), sent_date
#   config/traits.yml in austraits.build (develop branch), to spot proposals
#     that now exist upstream; falls back to the local config/traits.yml.
#
# Outputs (both regenerated, never edit by hand):
#   inst/notes/upstream_traits/unmapped_pooled.csv   every pooled pair with its
#     decision; undecided pairs have a blank decision
#   inst/notes/upstream_traits/requests.md   the page sent to APD colleagues

library(dplyr)
library(purrr)
library(readr)
library(stringr)

# .Rprofile attaches dplyr before Rscript attaches stats, so stats::filter
# would win.
filter <- dplyr::filter

dir_out <- "inst/notes/upstream_traits"
upstream_url <- paste0(
  "https://raw.githubusercontent.com/traitecoevo/austraits.build/",
  "develop/config/traits.yml"
)
# Free-text bins the parser has not split into characters; not traits.
catch_all <- c("description", "text")

# Pool unmapped terms ---------------------------------------------------------

files <- Sys.glob("data/*/raw/unmapped_terms.csv")
dataset_of <- function(f) basename(dirname(dirname(f)))

unmapped <- files |>
  set_names(dataset_of) |>
  map(\(f) read_csv(f, col_types = cols(.default = "c"))) |>
  list_rbind(names_to = "dataset_id") |>
  mutate(
    organ = str_squish(str_to_lower(organ)),
    character = str_squish(str_to_lower(character)),
    n_taxa = as.integer(n_taxa)
  )

# Families per dataset: the family-rank rows of each trait_data.csv.
families <- files |>
  str_replace("unmapped_terms\\.csv$", "trait_data.csv") |>
  set_names(dataset_of) |>
  keep(file.exists) |>
  map(\(f) {
    read_csv(f, col_types = cols(.default = "c")) |>
      filter(taxon_rank == "family") |>
      distinct(family = taxon_name)
  }) |>
  list_rbind(names_to = "dataset_id")

pooled <- unmapped |>
  filter(!character %in% catch_all) |>
  left_join(families, by = "dataset_id", relationship = "many-to-many") |>
  group_by(organ, character) |>
  summarise(
    n_datasets = n_distinct(dataset_id),
    n_families = n_distinct(family, na.rm = TRUE),
    datasets = paste(sort(unique(dataset_id)), collapse = "; "),
    families = paste(sort(unique(na.omit(family))), collapse = "; "),
    .groups = "drop"
  ) |>
  left_join(
    # n_taxa summed per dataset before the family join duplicated rows;
    # the example comes from the dataset with most taxa.
    unmapped |>
      filter(!character %in% catch_all) |>
      arrange(desc(n_taxa), dataset_id) |>
      group_by(organ, character) |>
      summarise(
        n_taxa = sum(n_taxa),
        example_text = first(example_text),
        example_dataset = first(dataset_id),
        example_page = first(printed_page),
        .groups = "drop"
      ),
    by = c("organ", "character")
  ) |>
  relocate(n_taxa, .after = character) |>
  arrange(desc(n_taxa), organ, character)

# Decisions -------------------------------------------------------------------

decisions <- read_csv(
  file.path(dir_out, "decisions.csv"),
  col_types = cols(.default = "c")
)

# readr reads UTF-8 whatever the locale; yaml::read_yaml() stops at the first
# non-ASCII character under a POSIX locale.
trait_names <- function(path) {
  names(yaml::yaml.load(read_file(path))$traits$elements)
}
local_traits <- trait_names("config/traits.yml")
upstream_traits <- tryCatch(
  trait_names(upstream_url),
  error = function(e) {
    warning("Upstream traits.yml not read; using config/traits.yml: ",
            conditionMessage(e))
    NULL
  }
)
upstream_source <- if (is.null(upstream_traits)) {
  "local `config/traits.yml` (upstream could not be read)"
} else {
  "austraits.build `develop` branch"
}
if (is.null(upstream_traits)) upstream_traits <- local_traits

# Self-checks: catch typos in the hand-edited file before they reach the page.
stopifnot(
  "duplicate organ + character in decisions.csv" =
    !anyDuplicated(decisions[c("organ", "character")]),
  "decision must be existing, propose or ignore" =
    all(decisions$decision %in% c("existing", "propose", "ignore")),
  "proposals need tier easy, medium or hard" =
    all(decisions$tier[decisions$decision == "propose"] %in%
          c("easy", "medium", "hard")),
  "proposals need a proposed_name" =
    all(!is.na(decisions$proposed_name[decisions$decision == "propose"])),
  "status must be draft, sent, accepted or rejected" =
    all(decisions$status %in% c(NA, "draft", "sent", "accepted", "rejected"))
)
bad_template <- setdiff(na.omit(decisions$template_trait), local_traits)
if (length(bad_template) > 0) {
  stop("template_trait not in config/traits.yml: ",
       paste(bad_template, collapse = ", "))
}

table_all <- pooled |>
  full_join(decisions, by = c("organ", "character")) |>
  mutate(
    n_taxa = coalesce(n_taxa, 0L),
    now_upstream = !is.na(proposed_name) & proposed_name %in% upstream_traits
  ) |>
  arrange(desc(n_taxa), organ, character)

write_csv(table_all, file.path(dir_out, "unmapped_pooled.csv"), na = "")

# Report ----------------------------------------------------------------------

proposals <- table_all |>
  filter(decision == "propose", !status %in% c("accepted", "rejected"))

# One row per proposed trait; several flora wordings can feed one trait.
by_trait <- function(d) {
  d |>
    group_by(proposed_name) |>
    summarise(
      proposed_type = first(na.omit(proposed_type)),
      template_trait = first(na.omit(template_trait)),
      wording = paste0(organ, " | ", character, collapse = "; "),
      n_taxa = sum(n_taxa),
      n_datasets = length(unique(unlist(str_split(datasets, "; ")))),
      n_families = length(setdiff(unique(unlist(str_split(families, "; "))),
                                  "")),
      example = paste0(
        "\"", first(example_text), "\" (", first(example_dataset),
        ", p. ", first(example_page), ")"
      ),
      status = first(status),
      notes = paste(na.omit(notes), collapse = " "),
      .groups = "drop"
    ) |>
    arrange(desc(n_taxa))
}

md_cell <- function(x) {
  x |>
    coalesce("") |>
    str_replace_all("\\|", "\\\\|") |>
    str_squish()
}

ask <- proposals |> filter(tier == "easy", !now_upstream) |> by_trait()
later <- proposals |> filter(tier != "easy", !now_upstream) |> by_trait()
landed <- table_all |>
  filter(decision == "propose", now_upstream) |>
  distinct(proposed_name)

ask_table <- c(
  paste(
    "| Proposed trait | Type | Modelled on | Flora wording (organ \\|",
    "character) | Taxa | Datasets | Families | Example (source, printed",
    "page) | Notes |"
  ),
  "|---|---|---|---|---:|---:|---:|---|---|",
  with(ask, paste0(
    "| `", proposed_name, "` | ", md_cell(proposed_type), " | ",
    if_else(is.na(template_trait), "", paste0("`", template_trait, "`")),
    " | ", md_cell(wording), " | ", n_taxa, " | ", n_datasets, " | ",
    n_families, " | ",
    md_cell(example), " | ", md_cell(notes), " |"
  ))
)

report <- c(
  "# Trait requests from MYtraits to the AusTraits Plant Dictionary",
  "",
  paste0("Generated ", format(Sys.Date()), " by `", dir_out,
         "/make_report.R`. Do not edit by hand."),
  "",
  paste(
    "MYtraits extracts traits from Malaysian flora treatments (Flora of",
    "Peninsular Malaysia, Tree Flora of Sabah and Sarawak). These characters",
    "are described often but have no trait in `traits.yml`. We ask only for",
    "the easy ones: each copies an existing APD trait for a different organ,",
    "so the definition, type and units can follow that template."
  ),
  "",
  paste0(
    "Counts come from ", length(files), " flora datasets. Taxa are summed ",
    "across datasets. Upstream trait list checked against ", upstream_source,
    "."
  ),
  "",
  "## Requested now",
  "",
  if (nrow(ask) > 0) ask_table else "Nothing to request right now.",
  "",
  "## Not requested now",
  "",
  if (nrow(later) > 0) {
    paste0(
      nrow(later), " further characters need new categories or concepts, ",
      "so we are not asking for them yet: ",
      paste0("`", later$proposed_name, "`", collapse = ", "), "."
    )
  } else {
    "None."
  },
  "",
  "## Already in AusTraits since our last request",
  "",
  if (nrow(landed) > 0) {
    paste0("- `", landed$proposed_name, "`")
  } else {
    "None yet."
  },
  ""
)

write_lines(report, file.path(dir_out, "requests.md"))

# Maintainer summary: what still needs a decision.
undecided <- table_all |> filter(is.na(decision)) |> slice_head(n = 15)
message(
  nrow(ask), " traits requested, ", nrow(later), " deferred, ",
  nrow(landed), " now upstream. ",
  sum(is.na(table_all$decision)), " pairs undecided; top by taxa:"
)
print(select(undecided, organ, character, n_taxa, n_datasets, example_text))
