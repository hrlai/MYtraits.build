# Folder Structure

## Repository layout

- `build.R` — main build script that compiles the MYtraits database
- `.Rprofile` — project startup code loading `traits.build`
- `MYtraits.build.Rproj` — project editor settings
- `config/` — shared metadata, trait definitions, conversions, and taxonomy
  inputs
- `R/` — reusable helper functions and support code
- `data/` — per-dataset source folders
- `inst/figures/` — image assets
- `export/data/curr/` — generated compiled output

## `data/` conventions

Each dataset directory under `data/` typically follows this pattern:

- `data/<Dataset_Name>/metadata.yml`
- `data/<Dataset_Name>/data.csv`
- `data/<Dataset_Name>/raw/` for raw source files and wrangling scripts, when
  needed

Dataset directory names follow an `Author_Year` style, with suffixes such as
`_2` when a source is split into multiple outputs.

## Where changes belong

### Add or revise a dataset import

Work primarily in:

- `data/<Dataset_Name>/raw/` for wrangling logic
- `data/<Dataset_Name>/metadata.yml` for dataset mapping and provenance
- `build.R` to register the dataset in the build pipeline

### Add reusable parsing or helper logic

Put it in `R/` rather than embedding the same logic in multiple dataset scripts.

### Update shared schema-like inputs

Edit `config/` files carefully because they can affect many dataset builds.

### Update generated output

Do not edit `export/data/curr/` directly unless the task is explicitly about
generated artifacts.

## Structure preferences for Copilot

When generating new files or code:

- Prefer fitting work into the existing structure rather than creating new
  top-level directories.
- Avoid introducing package-only folders like `man/`, `tests/testthat/`, or
  `NAMESPACE` unless explicitly requested.
- Avoid creating application-style directories such as `app/`, `src/`, or
  `services/` unless the task clearly requires a structural change.
- Match the organization of neighboring dataset folders when adding a new
  source.

## Special cases

- `remake.yml` appears to be generated and may not reflect the current
  hand-maintained build script exactly.
- Some dataset `raw/` folders may include a local `README.md` with
  source-specific notes; preserve that local documentation style when extending
  those folders.
