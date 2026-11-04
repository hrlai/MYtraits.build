# Rerun and search flora datasets already processed with this skill.
# Run from the repository root. A flora dataset is any data/<id>/ with
# raw/build_trait_data.R.

suppressPackageStartupMessages(library(tidyverse))
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

flora_ids <- function() {
  basename(dirname(dirname(Sys.glob("data/*/raw/build_trait_data.R"))))
}

# Search the unmapped rows of every flora dataset: the committed text index
# of what each treatment says but did not score. Start here before adding a
# trait, to size the evidence without reopening any PDF.
flora_unmapped_find <- function(pattern, ids = flora_ids()) {
  map(ids, \(id) {
    read_csv(
      file.path("data", id, "raw", "trait_data.csv"),
      col_types = cols(.default = "c"),
      na = ""
    ) |>
      filter(is.na(trait) | trait == "") |>
      filter(str_detect(trait_value_raw, regex(pattern, ignore_case = TRUE))) |>
      transmute(
        dataset_id = id,
        taxon_name,
        taxon_rank,
        source_section,
        printed_page,
        trait_value_raw,
        notes
      )
  }) |>
    list_rbind()
}

# Rebuild raw/trait_data.csv and data.csv for each dataset, one at a time.
# A failure (most often a missing local PDF) is reported, not fatal. Check
# `git diff --stat data/` afterwards: only intended changes may remain.
flora_rebuild <- function(ids = flora_ids()) {
  res <- map(ids, \(id) {
    # A fresh R process per build: scripts edit helper globals
    # (Kochummen_2000 extends flora_qual_words), which a local environment
    # hides from the helpers and a shared session leaks to the next script.
    # The project .Rprofile links the uvr library in the child process.
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      file.path("data", id, "raw", "build_trait_data.R"),
      stdout = FALSE,
      stderr = FALSE
    )
    msg <- if (status != 0) {
      paste("build_trait_data.R exited with status", status)
    } else {
      tryCatch(
        {
          flora_make_data(id)
          "ok"
        },
        error = \(e) conditionMessage(e)
      )
    }
    message(id, ": ", msg)
    tibble(dataset_id = id, result = msg)
  })
  list_rbind(res)
}
