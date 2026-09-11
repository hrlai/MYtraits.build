# Architecture

## Overview

This repository is a monolithic R-based data build project for compiling the
MYtraits database. It is not structured as an R package or application. The
central workflow is a build pipeline that standardizes multiple source datasets
into one compiled RDS output.

## Main flow

`build.R` is the orchestration entry point.

It performs these stages:

1. Load shared schema, metadata, trait definitions, unit conversions, and taxon
   lists from `config/`.
2. For each dataset directory under `data/`, configure the dataset with its
   `metadata.yml`.
3. Process each cleaned `data.csv` through `traits.build`.
4. Update taxonomy using the shared taxon list.
5. Combine all processed datasets with `austraits::bind_databases(...)`.
6. Add repository version and Git SHA metadata.
7. Save the compiled artifact to `export/data/curr/MYtraits.rds`.

## Architectural units

### Shared configuration

`config/` contains repository-wide inputs used across datasets:

- `metadata.yml`
- `traits.yml`
- `unit_conversions.csv`
- `taxon_list.csv`

Changes here have cross-cutting effects.

### Reusable helper code

`R/` contains helper functions used to support data parsing and project assets.
Helpers are small, task-specific, and documented with roxygen comments.

### Dataset modules

Each `data/<Dataset_Name>/` directory acts like a small dataset module.

Typical contents:

- `metadata.yml`: dataset-specific mapping and provenance metadata
- `data.csv`: cleaned, standardized data ready for processing
- `raw/`: source files and wrangling scripts used to derive `data.csv`

Dataset modules are independent inputs that converge in `build.R`.

### Generated outputs

`export/data/curr/` contains compiled output and should be treated as generated.

## Data flow expectations

The repository expects standardized, long-format trait data suitable for
`traits.build`. Existing code suggests that:

- wrangling happens close to the raw sources in dataset-local scripts
- shared normalization inputs live in `config/`
- final assembly happens centrally in `build.R`

## Boundary guidance

When making changes:

- Put dataset-specific logic in that dataset's folder, usually
  `data/<Dataset_Name>/raw/`.
- Put reusable logic in `R/`.
- Put shared definitions in `config/`.
- Keep `build.R` as the central orchestrator.

## Generated-file caution

`remake.yml` states that it is auto-generated. Treat it as derived configuration
rather than the main source for architectural decisions unless explicitly asked
to work on its generator.
