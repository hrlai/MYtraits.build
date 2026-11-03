# Upstream trait requests

This folder tracks flora characters that MYtraits extracts but cannot map to
a trait in `config/traits.yml`. It ranks them and writes a page,
`requests.md`, that we send to the AusTraits Plant Dictionary (APD) and
`traits.build` maintainers. They update `traits.yml` upstream; we then copy
the file in. Never edit `config/traits.yml` here.

## Files

| File | Edited by | Purpose |
|---|---|---|
| `decisions.csv` | hand | One row per organ + character: what to do with it. |
| `make_report.R` | hand (rarely) | Pools the unmapped terms, joins decisions, writes the two outputs. |
| `unmapped_pooled.csv` | script | Every pooled organ + character with counts, example and decision. |
| `requests.md` | script | The page sent to APD colleagues. |
| `README.md` | hand | This file. |

Never edit `unmapped_pooled.csv` or `requests.md` by hand. The script
overwrites them.

## Regenerate

Run from the project root:

```bash
uvr run inst/notes/upstream_traits/make_report.R
```

Inputs:

- `data/*/raw/unmapped_terms.csv`, written by the flora extraction skill
  (`flora_unmapped_summary()`).
- `data/*/raw/trait_data.csv`, for the families in each dataset.
- `decisions.csv`.
- `config/traits.yml`, to check that each `template_trait` exists.
- `config/traits.yml` on the austraits.build `develop` branch, to spot
  proposals that now exist upstream. If the download fails, the script warns
  and uses the local file. `requests.md` states which list it used.

The script prints how many traits are requested and deferred, and the top 15
undecided pairs by taxa.

## Update cycle

Run this after each wave of new flora datasets.

1. Run the script.
2. Read the undecided pairs it prints. For the full list, filter
   `unmapped_pooled.csv` for a blank `decision`, sorted by `n_taxa`.
3. Add a row to `decisions.csv` for each pair worth a decision (see below).
4. Run the script again. Fix any error it stops on.
5. Commit `decisions.csv`, `unmapped_pooled.csv` and `requests.md` together.
6. Send colleagues a permalink to `requests.md` at that commit. On GitHub,
   press `y` on the file page to get it.
7. In `decisions.csv`, set `status = sent` and `sent_date` (YYYY-MM-DD) on
   the rows you sent. Commit.
8. When upstream adds a trait, set `status = accepted`. Copy the new
   `config/traits.yml` in and map the trait in the extraction skill. If
   upstream declines, set `status = rejected`.

You do not need to decide every pair. Most of the tail has few taxa. Decide
the top of the list and leave the rest blank.

To list undecided pairs as stub rows to paste into `decisions.csv`:

```r
library(dplyr)
library(readr)

read_csv("inst/notes/upstream_traits/unmapped_pooled.csv",
         col_types = cols(.default = "c")) |>
  dplyr::filter(is.na(decision)) |>
  mutate(n_taxa = as.integer(n_taxa)) |>
  slice_max(n_taxa, n = 20) |>
  select(organ, character, n_taxa, n_datasets, example_text)
```

## `decisions.csv` columns

Key: `organ` + `character`, lower case with single spaces, spelled exactly
as in `unmapped_pooled.csv`. Each key appears once.

| Column | Values | Notes |
|---|---|---|
| `organ` | text | From `unmapped_terms.csv`. |
| `character` | text | From `unmapped_terms.csv`. |
| `decision` | `existing`, `propose`, `ignore` | Required. |
| `tier` | `easy`, `medium`, `hard` | Required for `propose`; blank otherwise. |
| `template_trait` | APD trait name | Must exist in `config/traits.yml`. For `existing`, the trait it maps to. For `propose`, the trait the proposal copies. |
| `proposed_name` | new trait name | Required for `propose`. Rows with the same name merge into one request. |
| `proposed_type` | e.g. `numeric (mm)`, `categorical (glabrous / hairy)` | Free text shown in the request. |
| `status` | blank, `draft`, `sent`, `accepted`, `rejected` | `accepted` and `rejected` rows leave the request. |
| `sent_date` | YYYY-MM-DD | When the request was sent. |
| `notes` | text | Shown in the request for proposals. Quote it if it has a comma. |

### Choosing a decision

- `existing`: an APD trait already covers the character. The extraction
  skill should map it; these rows feed a skill fix, not a request.
- `propose`: no APD trait fits and the character is common enough to ask
  for.
- `ignore`: too rare, too family-specific, or not a trait (for example, a
  count of fruit lobes recorded as `fruits | count`).

### Choosing a tier

Only `easy` proposals go in "Requested now". `medium` and `hard` are listed
by name only.

- `easy`: copies an existing APD trait for another organ, so definition,
  type and units follow that template. Example: `flower_petal_hairs` from
  `fruit_surface_hairs`. Set `template_trait`.
- `medium`: needs a new set of categorical levels, e.g. `leaf_texture`.
- `hard`: needs a new concept or vocabulary, e.g. tertiary venation pattern.

## Checks the script runs

It stops when:

- an organ + character appears twice;
- `decision` is not `existing`, `propose` or `ignore`;
- a `propose` row has no valid `tier` or no `proposed_name`;
- `status` is not blank, `draft`, `sent`, `accepted` or `rejected`;
- a `template_trait` is not in `config/traits.yml`.

## Known limits

- Organ names are not merged across spellings. `style` and `styles` are
  separate pairs; give each its own row.
- The characters `description` and `text` are dropped. They are free-text
  bins, not characters.
- The example is the first clause from the dataset with most taxa. It can
  be uninformative (e.g. "glabrous").
- Families come from family-rank rows in each `trait_data.csv`. A dataset
  without one adds no families.

## Notes for agents

- Edit `decisions.csv` and this README only. Regenerate the rest with the
  script.
- Do not edit `config/traits.yml`; proposals go upstream.
- `make_report.R` sets `filter <- dplyr::filter` because `stats::filter`
  masks it under `uvr run`. Keep that line.
- It reads YAML with `readr::read_file()` then `yaml::yaml.load()`.
  `yaml::read_yaml()` stops at the first non-ASCII character under a POSIX
  locale and silently drops most traits.
- In R run with `uvr run`, call `dplyr::filter()` explicitly or set
  `filter <- dplyr::filter`.
- Stage files by path when committing.
