# GitHub Copilot Instructions

## Priority Guidelines

When generating code for this repository:

1. Follow any more specific guidance in `.github/copilot/` first.
2. Match patterns already established in nearby repository files before applying
   general R advice.
3. Prefer consistency with the existing `traits.build` workflow over introducing
   new abstractions or tooling.
4. Be conservative about version-sensitive features because this repository does
   not pin most dependency versions.
5. If a requested change would require a new package, a new project structure,
   or a newer API than the repository shows, keep the change minimal and note
   the dependency explicitly.

## Repository Technology and Version Evidence

Observed evidence in this repository:

- Primary language: R
- Supporting formats: YAML, CSV, Markdown, Quarto (`README.qmd`)
- Project editor settings in `MYtraits.build.Rproj` indicate UTF-8, spaces for
  tabs, and two-space indentation.
- Project metadata version is `0.0.1` in `config/metadata.yml`.
- `.pre-commit-config.yaml` references `yamlfmt` `v0.19.0`.
- Repository scripts use `traits.build`, `austraits`, `tidyverse`, `readxl`,
  `sf`, and `hexSticker`.
- No project-local R version file, `renv.lock`, `DESCRIPTION`, `NAMESPACE`, or
  other lockfile/manifests were found.
- The current interactive session may be newer, but it should not be treated as
  the repository's minimum required R version unless the repo declares that
  explicitly.

Implication for Copilot:

- Prefer conservative R syntax and functions already used in nearby files.
- Do not assume that modern package APIs or recent R features are acceptable
  unless the same style already appears in the repository.
- Where multiple valid R styles exist, prefer the one already used in the file
  being edited.

## R-Specific Guidance

### Match the file's style

- If a file uses tidyverse-style pipelines, continue in that style.
- If a file uses `%>%`, keep using `%>%` in that file rather than switching to
  `|>` mid-script.
- If a file uses `library(...)` at the top, that is acceptable in repository
  code.
- In prose or generic examples outside repository files, qualifying non-base
  calls is acceptable, but repository edits should match local practice.

### Naming and structure

- Use `lower_snake_case` for functions and objects.
- Avoid introducing dotted names for new helpers unless extending an existing
  external column naming scheme.
- Keep helpers small and single-purpose, as in `R/custom_R_code.R`.
- Prefer direct transformation steps over clever metaprogramming.

### Assignment, spacing, and layout

- Use `<-` for assignment.
- Use two-space indentation.
- Keep line length to 80 characters.
- Keep pipelines vertically readable, with one verb per line when a
  transformation chain is non-trivial.
- Use comment banners and short section comments in data-wrangling scripts when
  they improve scanability.

### Tidyverse usage

Observed repository style is tidyverse-first in wrangling scripts. Prefer:

- `readr` readers such as `read_delim()` and `write_csv()`
- `dplyr` verbs such as `mutate()`, `select()`, `group_by()`, `summarise()`,
  `ifelse()` inside `mutate()`, and `bind_rows()`
- `tidyr` verbs such as `pivot_longer()` and `separate()`
- `purrr` mapping helpers such as `map()`, `map_dfr()`, and `pmap()` when
  iteration is needed in tidyverse-style code

When editing legacy code:

- Preserve working patterns already present, including older verbs such as
  `summarise_at()` and `%>%`, unless the task explicitly asks for modernization.
- Do not refactor stable code only to replace older tidyverse idioms with newer
  ones.

### Reproducibility and side effects

- Never introduce `setwd()`.
- Keep file paths project-relative, matching the repository's current style.
- Avoid hidden side effects; write outputs explicitly and near the end of a
  script.
- Treat `export/data/curr/` as generated output.

### Validation and safety

- Keep validation lightweight unless the task asks for stronger checks.
- Prefer repository-consistent handling of irregular values, including returning
  `NA` where nearby helper code does so.
- Avoid `eval(parse())`, unvalidated shell calls, or unnecessary system-level
  dependencies.
- Do not hardcode secrets, tokens, or credentials.

### Documentation in R files

- In `R/`, use roxygen comments matching `R/custom_R_code.R`.
- In dataset wrangling scripts, use brief comments that explain why a
  transformation exists, not line-by-line narration.
- Match the minimal root documentation style unless editing a dataset folder
  that already contains richer notes.

## Architecture and Project Shape

This repository is best understood as a
**monolithic, file-oriented trait-data build pipeline** centered on
`traits.build`.

Observed structure:

- `build.R` orchestrates the full build.
- `R/` contains reusable helper functions.
- `config/` contains shared metadata, trait definitions, unit conversions, and
  taxonomy inputs.
- `data/<Dataset_Name>/` contains per-source metadata, cleaned data, and often a
  `raw/` folder with source files and wrangling scripts.
- `inst/figures/` contains static image assets.
- `export/data/curr/` contains generated output.

Do not reinterpret this repository as an R package, web app, or service-oriented
system unless explicitly asked.

## Required Build Workflow Patterns

When editing `build.R` or adding a new source, mirror the established workflow:

1. Load shared schema/config resources near the top.
2. For each dataset, use:
   - `dataset_configure()`
   - `dataset_process()`
   - `dataset_update_taxonomy()`
3. Combine datasets with `austraits::bind_databases(...)`.
4. Add version metadata with `build_add_version(...)` using `util_get_version()`
   and `util_get_SHA()`.
5. Save the final object with `saveRDS()` into `export/data/curr/`.

Prefer the repository's explicit repeated dataset blocks over introducing loops,
code generation, or new abstraction layers unless requested.

## File-by-File Guidance

### `build.R`

- Preserve the current top-to-bottom orchestration style.
- Keep dataset blocks explicit and easy to scan.
- Do not convert the script into package infrastructure or a generalized
  framework.

### `R/`

- Place reusable helpers here.
- Use roxygen comments.
- Follow existing patterns for string handling, explicit condition checks, and
  pragmatic returns.

### `data/*/raw/wrangle.R`

- Load needed libraries at the top.
- Read raw data, transform in clear stages, and write standardized CSV outputs.
- Use meaningful intermediate objects when they correspond to real processing
  stages.
- Prefer code that makes data provenance legible over compact but opaque
  pipelines.

### `config/`

- Keep shared metadata and schema-like inputs here.
- Edit carefully because these files affect multiple datasets.

### Generated or template-derived files

- Treat generated files conservatively.
- `remake.yml` states that it is auto-generated; do not treat it as the primary
  hand-maintained source unless the task explicitly targets its generation
  process.

## Documentation and Testing Guidance

Observed repository state:

- Formal automated tests were not found.
- CI workflows were not found.
- Validation seems to rely mainly on the `traits.build` processing pipeline and
  schema expectations.

Implications:

- Do not assume this repository wants a full testing framework by default.
- If asked to add checks, prefer local validation close to the transformation or
  build step.
- Preserve compatibility with `traits.build` expectations, especially
  long-format outputs and the handling of `individual_id` noted in existing
  wrangling comments.

## Wording for Uncertain Version Evidence

Because version evidence is limited, prefer wording like:

- "match existing repository patterns"
- "prefer APIs already used nearby"
- "avoid introducing version-sensitive features unless the repository already
  uses them"

Avoid overstated claims like:

- "always use the latest tidyverse idiom"
- "assume native pipe support"
- "require renv, lintr, styler, or testthat"

Those tools may be good ideas, but they are not currently established by
repository evidence.

## Examples of Local Patterns to Mirror

- Repeated dataset build blocks in `build.R`
- Roxygen-documented utility helpers in `R/custom_R_code.R`
- Tidyverse wrangling stages in `data/Wright_2024/raw/wrangle.R`
- Minimal root documentation in `README.md`
- More detailed, dataset-local notes in `data/Wright_2024/raw/README.md`

## Ambiguous Cases

If guidance is missing:

1. Check for a more specific file in `.github/copilot/`.
2. Inspect nearby files in the same folder.
3. Follow the most repeated local pattern.
4. Keep the change minimal if the repository does not show a clear preference.

## Copilot Goal

Generate R code and related project edits that fit this repository's existing
trait-database build workflow, preserve its file-oriented structure, and stay
close to patterns actually observed in the codebase.
