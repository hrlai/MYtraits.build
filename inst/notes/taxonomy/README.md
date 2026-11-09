# Taxonomy: rebuilding the taxon list

This folder documents how MYtraits aligns and updates taxon names, and how to
rebuild `config/taxon_list.csv`. It is written for curators and AI agents.

## Background

There is no complete checklist of the Malaysian flora yet. Until there is,
MYtraits uses a stand-in reference: the World Checklist of Vascular Plants
(WCVP, Royal Botanic Gardens, Kew), restricted to the taxa GBIF has occurrence
records of in Malaysia (`MY`), Brunei (`BN`) and Singapore (`SG`). Each kept
accepted name keeps all its synonyms, so an older name in a dataset still
updates to the current one.

The reference is built with
[taxonAlign.dataprep](https://github.com/traitecoevo/taxonAlign.dataprep) and
names are matched with [taxonAlign](https://github.com/traitecoevo/taxonAlign).
This follows the AusTraits workflow, which uses APCalign with the Australian
Plant Census.

The reference takes its taxonomy from WCVP. For example, WCVP v16 splits
*Shorea* (into *Anthoshorea*, *Rubroshorea*, *Richetia* and others) and moves
most *Cyathea* species to *Alsophila*, *Sphaeropteris* and *Gymnosphaera*.
`original_name` always keeps the name as printed.

The reference (`inst/taxonomy/taxonomic_reference.parquet`) is git-ignored.
On a new machine, build it first with `build_mytraits_reference()` (about 5
minutes the first time: a 90 MB WCVP download and GBIF queries, both cached).
GBIF's records change over time, so a reference rebuilt later can differ
slightly.

### How names are matched

`build_align_taxon_names()` aligns each name in two passes:

1. Against the MY/BN/SG reference, with fuzzy matching.
2. Names that pass 1 matched only fuzzily, only to genus or family, or not at
   all are matched again against the whole WCVP, exact matches only. An exact
   match replaces the pass-1 result.

Fuzzy matching against the whole WCVP is avoided on purpose: it finds
look-alike names in unrelated genera from other continents (for example
*Shorea parviflora* to *Sloanea parviflora*).

## How names flow through the build

1. A dataset's `data.csv` keeps names as printed in the source
   (`original_name`).
2. **Alignment** (spelling, formatting, `Genus sp.` names) is recorded per
   dataset in `metadata.yml` under `taxonomic_updates`, one `find` /
   `replace` / `reason` / `taxonomic_resolution` entry per name, as in
   AusTraits. `build_align_taxon_names()` writes these.
3. **Updating** an aligned name to its currently accepted name (synonymy) is
   done by `config/taxon_list.csv`, keyed by `aligned_name`.
   `build_update_taxon_list()` writes it.
4. `traits.build::dataset_update_taxonomy()` in `build.R` applies both.

Never put spelling fixes in `config/taxon_list.csv`. Never put synonym
updates in `metadata.yml`.

## Files

| File | Purpose |
|---|---|
| `R/build_taxonomic_reference.R` | `build_mytraits_reference()` builds the reference; `load_mytraits_resources()` loads it for taxonAlign. |
| `R/build_align_taxon_names.R` | `build_align_taxon_names()` writes a dataset's `taxonomic_updates`. |
| `R/build_update_taxon_list.R` | `build_update_taxon_list()` writes `config/taxon_list.csv`. |
| `inst/taxonomy/taxonomic_reference.parquet` | The reference used for matching. |
| `config/taxon_list.csv` | One row per aligned name, with its accepted name and identifiers. |
| `inst/notes/taxonomy/rebuild_taxon_list.R` | Runs the whole rebuild. |

## Add a new dataset's names

Run once `dataset_process()` passes. Either the `dataset_process()` output
or a whole compiled database works as `database`; the functions use only the
rows of the dataset (or datasets) it holds.

```r
source("R/build_taxonomic_reference.R")
source("R/build_align_taxon_names.R")
source("R/build_update_taxon_list.R")

resources <- load_mytraits_resources()  # a few minutes; load once
processed <- dataset_process("data/<dataset_id>/data.csv", ...)  # as in build.R

aligned <- build_align_taxon_names(processed, "<dataset_id>", resources)
dplyr::filter(aligned, is.na(replace) | chars_changed > 0)  # review these
```

Review the alignments (next section). Then rerun `dataset_process()` so the
aligned names reach `taxonomic_updates`, and add them to the taxon list:

```r
processed <- dataset_process("data/<dataset_id>/data.csv", ...)
build_update_taxon_list(processed, resources)
```

With `replace = FALSE` (the default), only aligned names not yet in
`config/taxon_list.csv` are added. Every name in the dataset should now have
a `family` after `dataset_update_taxonomy()`.

## Review alignments

Check every fuzzy match (`chars_changed > 0`) and every name with no match
(`replace` is `NA`, not written). A fuzzy match that changes the genus is
often wrong (*Mesua borneensis* to *Musa borneensis*); one that corrects a
known spelling of the genus is usually right (*Aporusa* to *Aporosa*).

Fix a wrong match by editing its entry in the dataset's `metadata.yml`, or
add an entry for an unmatched name. Use the AusTraits forms:

```yaml
taxonomic_updates:
  - find: Magnolia candolii
    replace: Magnolia candollei
    reason: 'Manual alignment: fuzzy match was to a different taxon (MYtraits curation, 2026-11-09)'
    taxonomic_resolution: species
  - find: Mesua borneensis
    replace: Mesua sp. [Mesua borneensis; Both_2019]
    reason: 'Manual alignment: fuzzy match was to a different taxon (MYtraits curation, 2026-11-09)'
    taxonomic_resolution: genus
```

A name that can only be placed in a genus or family becomes
`Genus sp. [<original name>; <dataset_id>]` or
`Family sp. [<original name>; <dataset_id>]`, with `taxonomic_resolution`
`genus` or `family`. Rerunning `build_align_taxon_names()` keeps manual
entries: it skips names already aligned to a reference name or given a
`taxonomic_resolution`.

## Rebuild everything

Do this when the reference changes: a new WCVP version, other countries, or a
new taxonAlign that changes matching.

```bash
uvr run inst/notes/taxonomy/rebuild_taxon_list.R
```

The script rebuilds the reference, aligns every dataset, rebuilds the
database, and rewrites `config/taxon_list.csv` from scratch
(`replace = TRUE`). Before running it, copy `export/data/curr/MYtraits.rds`
aside. Afterwards:

1. Review the names in `inst/taxonomy/alignment_review.csv` (previous
   section) and fix entries in `metadata.yml`.
2. If you fixed any, run `uvr run build.R`, then
   `build_update_taxon_list(readRDS("export/data/curr/MYtraits.rds"), resources, replace = TRUE)`,
   then `uvr run build.R` again.
3. Compare with `waldo::compare(old, new)` and review the `git diff` of
   `data/*/metadata.yml` and `config/taxon_list.csv`. Trait values should not
   change; only `taxon_name` and the taxonomy tables should.

To change the WCVP version or the countries, edit the arguments to
`build_mytraits_reference()` in that script. Record the change in the PR.

## Keeping up with taxonAlign

`taxonAlign` and `taxonAlign.dataprep` change often. In `uvr.toml` they track
their default branches (no `rev`); `uvr.lock` fixes the commit. To move to the
latest commits:

```bash
uvr lock -P taxonAlign -P taxonAlign.dataprep
uvr sync
```

Then run the full rebuild above and report any change in names.

Known upstream issues (as of 2026-11-09). Recheck after each bump:

- `rgbif` is pinned below 3.9.0 in `uvr.toml`. In `rgbif` 3.9.0,
  `name_backbone()` returns GBIF's new backbone IDs (for example `"TP"` for
  Tracheophyta), which `taxonAlign.dataprep::filter_to_country()` reads as
  `NA`, and then it keeps no records. Remove the pin once taxonAlign.dataprep
  supports the new IDs.
- `taxonAlign::align_taxa("Neo-uvaria")` stops with "Can't assign rows with
  `i`" (a hyphenated genus). `safe_align_taxa()` in
  `R/build_taxonomic_reference.R` works around it by aligning name by name.
- `load_WCVP()` drops the IPNI ID, so `scientific_name_id` in the taxon list
  is blank. WCVP has no family records, so family-level names and
  `taxon_id_family` have no identifiers.
- The taxon list has no `taxon_distribution` or `establishment_means`.

## To explore: World Flora Online

An earlier MYtraits workflow aligned names to World Flora Online (WFO) with the
WorldFlora package. It was retired in favour of taxonAlign. WFO is worth
exploring as a reference source for taxonAlign: it is the global consensus
flora, and regional treatments (such as the Flora of Peninsular Malaysia) may
feed into it. Proposal to the taxonAlign maintainers: add a
`load_WFO()` to taxonAlign.dataprep, alongside `load_WCVP()` and
`load_CoL()`.
