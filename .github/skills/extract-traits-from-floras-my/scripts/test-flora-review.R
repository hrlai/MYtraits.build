source(".github/skills/extract-traits-from-floras-my/scripts/flora_review.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")

root <- tempfile("flora-review-")
dir.create(root)
old <- getwd()
tryCatch(
  {
    dir.create(file.path(root, "data", "Trial", "raw"), recursive = TRUE)
    dir.create(file.path(dirname(root), paste0(basename(root), "-images")))
    images <- file.path(dirname(root), paste0(basename(root), "-images"))
    pdf <- file.path(root, "trial.pdf")
    grDevices::pdf(pdf)
    for (i in 1:4) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, i)
    }
    grDevices::dev.off()
    setwd(root)
    task <- flora_review_prepare(
      "Trial",
      "trial.pdf",
      1:4,
      images,
      batch_size = 2L
    )
    stopifnot(
      task$batch$batch_id == "B001",
      task$remaining == 2L,
      !any(task$pages$transcription_checked),
      !any(task$pages$rendered)
    )
    stopifnot(inherits(
      try(
        flora_review_prepare("Trial", "trial.pdf", 1:4, images),
        silent = TRUE
      ),
      "try-error"
    ))
    stopifnot(flora_review_next("Trial")$batch$batch_id == "B001")
    flora_review_render_next("Trial")
    state <- flora_review_read("Trial")
    stopifnot(all(file.exists(unlist(
      state$pages[1:2, c("full_image", "top_image", "bottom_image")],
      use.names = FALSE
    ))))
    flora_review_handoff("Trial")
    stopifnot(file.exists("data/Trial/raw/review_handoff.md"))
    state$batches$status[1] <- "verified"
    write.csv(
      state$batches,
      "data/Trial/raw/review_batches.csv",
      row.names = FALSE
    )
    stopifnot(inherits(
      try(flora_review_next("Trial"), silent = TRUE),
      "try-error"
    ))
    state$batches$status[1] <- "not_started"
    write.csv(
      state$batches,
      "data/Trial/raw/review_batches.csv",
      row.names = FALSE
    )
    for (col in c("full_image", "top_image", "bottom_image")) {
      file.create(state$pages[[col]])
    }
    writeLines(
      c("@page 1 101", "Sample checked text", "@page 2 102", "More text"),
      "data/Trial/raw/trial_transcription.txt"
    )
    state$pages$printed_page <- 101:104
    state$pages$printed_page_verified <- TRUE
    for (col in c(
      "transcription_checked",
      "headings_checked",
      "sections_checked",
      "clauses_checked",
      "vouchers_checked"
    )) {
      state$pages[[col]][1:2] <- TRUE
    }
    write.csv(
      state$pages,
      "data/Trial/raw/page_manifest.csv",
      row.names = FALSE,
      na = ""
    )
    state$batches$status[1] <- "verified"
    state$batches$boundary_checked[1] <- TRUE
    state$batches$blocker[1] <- ""
    write.csv(
      state$batches,
      "data/Trial/raw/review_batches.csv",
      row.names = FALSE,
      na = ""
    )
    stopifnot(flora_review_next("Trial")$batch$batch_id == "B002")
    flora_review_update(
      "Trial",
      pages = 3:4,
      printed_page = 103:104,
      notes = c("First note", "Second note")
    )
    state <- flora_review_read("Trial")
    stopifnot(identical(state$pages$notes[3:4], c("First note", "Second note")))
    writeLines("changed", pdf)
    stopifnot(inherits(
      try(flora_review_next("Trial"), silent = TRUE),
      "try-error"
    ))
  },
  finally = {
    setwd(old)
    unlink(root, recursive = TRUE)
    unlink(
      file.path(dirname(root), paste0(basename(root), "-images")),
      recursive = TRUE
    )
  }
)
