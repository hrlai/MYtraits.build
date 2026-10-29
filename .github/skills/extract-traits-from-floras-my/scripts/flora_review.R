# Resumable source-review queue. Source from the repository root.
flora_review_paths <- function(id) {
  raw <- file.path("data", id, "raw")
  if (!dir.exists(raw)) {
    stop("Unknown dataset: ", id)
  }
  list(
    manifest = file.path(raw, "page_manifest.csv"),
    batches = file.path(raw, "review_batches.csv"),
    handoff = file.path(raw, "review_handoff.md")
  )
}

# Path relative to the repository root, so the manifest can be committed. A path
# on another drive stays absolute.
flora_review_relpath <- function(path) {
  path <- strsplit(normalizePath(path, winslash = "/"), "/")[[1]]
  root <- strsplit(normalizePath(".", winslash = "/"), "/")[[1]]
  n <- 0L
  while (
    n < min(length(path), length(root)) && path[n + 1L] == root[n + 1L]
  ) {
    n <- n + 1L
  }
  if (!n) {
    return(paste(path, collapse = "/"))
  }
  paste(c(rep("..", length(root) - n), path[-seq_len(n)]), collapse = "/")
}

flora_review_read <- function(id) {
  paths <- flora_review_paths(id)
  if (!file.exists(paths$manifest) || !file.exists(paths$batches)) {
    stop("Review queue not prepared: ", id)
  }
  list(
    pages = read.csv(paths$manifest, na.strings = "", stringsAsFactors = FALSE),
    batches = read.csv(paths$batches, na.strings = "", stringsAsFactors = FALSE)
  )
}

flora_review_prepare <- function(
  id,
  pdf,
  pages,
  image_dir,
  batch_size = 10L,
  dpi = 220L
) {
  paths <- flora_review_paths(id)
  if (file.exists(paths$manifest) || file.exists(paths$batches)) {
    stop("Review queue exists; use flora_review_next() to resume: ", id)
  }
  if (!file.exists(pdf)) {
    stop("Missing PDF: ", pdf)
  }
  if (
    !length(pages) ||
      anyNA(pages) ||
      anyDuplicated(pages) ||
      any(diff(pages) < 1L) ||
      min(pages) < 1L ||
      max(pages) > pdftools::pdf_info(pdf)$pages
  ) {
    # Gaps are allowed, e.g. a plate page elsewhere in the volume.
    stop("pages must be increasing PDF pages within the PDF")
  }
  if (
    length(batch_size) != 1L ||
      is.na(batch_size) ||
      batch_size < 1L ||
      length(dpi) != 1L ||
      is.na(dpi) ||
      dpi < 1L
  ) {
    stop("batch_size and dpi must be positive")
  }
  if (!dir.exists(image_dir)) {
    stop("Create a persistent image directory first: ", image_dir)
  }
  if (dir.exists(image_dir) && length(list.files(image_dir))) {
    stop("Choose an empty image directory for this treatment: ", image_dir)
  }
  if (file.exists(paths$handoff)) {
    stop("Existing handoff needs review before preparation: ", paths$handoff)
  }
  image_dir <- flora_review_relpath(image_dir)
  if (!startsWith(image_dir, "../") && !grepl("^([A-Za-z]:)?/", image_dir)) {
    stop("image_dir must be outside the repository")
  }
  pdf <- flora_review_relpath(pdf)
  hash <- unname(tools::md5sum(pdf))
  batch <- ceiling(seq_along(pages) / batch_size)
  batches <- data.frame(
    batch_id = sprintf("B%03d", seq_len(max(batch))),
    first_pdf_page = as.integer(tapply(pages, batch, min)),
    last_pdf_page = as.integer(tapply(pages, batch, max)),
    status = "not_started",
    boundary_checked = FALSE,
    blocker = "",
    stringsAsFactors = FALSE
  )
  images <- function(suffix) {
    file.path(image_dir, sprintf("p%02d_%s.png", pages, suffix))
  }
  manifest <- data.frame(
    pdf = rep(pdf, length(pages)),
    pdf_md5 = rep(hash, length(pages)),
    pdf_page = pages,
    printed_page = NA_integer_,
    printed_page_verified = FALSE,
    dpi = rep(dpi, length(pages)),
    full_image = images("full"),
    top_image = images("top"),
    bottom_image = images("bottom"),
    transcription_checked = FALSE,
    headings_checked = FALSE,
    sections_checked = FALSE,
    clauses_checked = FALSE,
    vouchers_checked = FALSE,
    notes = "",
    batch_id = batches$batch_id[batch],
    stringsAsFactors = FALSE
  )
  readr::write_csv(manifest, paths$manifest, na = "")
  readr::write_csv(batches, paths$batches, na = "")
  flora_review_next(id)
}

flora_review_next <- function(id) {
  state <- flora_review_read(id)
  pages <- state$pages
  batches <- state$batches
  if (!nrow(pages) || !nrow(batches)) {
    stop("Empty review queue")
  }
  if (
    anyNA(batches$status) ||
      any(
        !batches$status %in%
          c("not_started", "in_review", "blocked", "verified")
      ) ||
      !"boundary_checked" %in% names(batches) ||
      anyNA(batches$boundary_checked) ||
      !is.logical(batches$boundary_checked)
  ) {
    stop("Unknown batch status")
  }
  expected <- data.frame(
    batch_id = sprintf("B%03d", seq_len(nrow(batches))),
    first_pdf_page = as.integer(tapply(pages$pdf_page, pages$batch_id, min)),
    last_pdf_page = as.integer(tapply(pages$pdf_page, pages$batch_id, max)),
    stringsAsFactors = FALSE
  )
  if (
    !identical(
      batches[c("batch_id", "first_pdf_page", "last_pdf_page")],
      expected
    )
  ) {
    stop("Batch ledger does not match the page manifest")
  }
  if (
    anyNA(pages$pdf_page) ||
      anyDuplicated(pages$pdf_page) ||
      anyNA(pages$batch_id) ||
      anyDuplicated(batches$batch_id) ||
      anyNA(pages$dpi) ||
      any(pages$dpi < 1L) ||
      anyNA(pages$full_image) ||
      anyNA(pages$top_image) ||
      anyNA(pages$bottom_image) ||
      any(!pages$batch_id %in% batches$batch_id) ||
      anyNA(pages$pdf_md5) ||
      anyNA(pages$pdf)
  ) {
    stop("Invalid page manifest")
  }
  sources <- unique(pages$pdf)
  if (
    length(sources) != 1L ||
      !file.exists(sources) ||
      !identical(unname(tools::md5sum(sources)), unique(pages$pdf_md5))
  ) {
    stop("Source PDF missing or changed; resolve before resuming")
  }
  checks <- c(
    "printed_page_verified",
    "transcription_checked",
    "headings_checked",
    "sections_checked",
    "clauses_checked",
    "vouchers_checked"
  )
  for (col in checks) {
    if (
      !col %in% names(pages) || anyNA(pages[[col]]) || !is.logical(pages[[col]])
    ) {
      stop("Invalid review flag: ", col)
    }
  }
  if (any(pages$printed_page_verified & is.na(pages$printed_page))) {
    stop("Verified printed page lacks number")
  }
  if (any(pages$transcription_checked)) {
    transcription <- file.path(
      "data",
      id,
      "raw",
      paste0(
        tools::file_path_sans_ext(basename(sources)),
        "_transcription.txt"
      )
    )
    if (!file.exists(transcription)) {
      stop("Checked transcription file missing")
    }
    markers <- grep(
      "^@page [0-9]+ [0-9]+$",
      readLines(transcription, warn = FALSE),
      value = TRUE
    )
    marked <- sub("^@page ([0-9]+) .*$", "\\1", markers)
    for (i in which(pages$transcription_checked)) {
      marker <- paste("@page", pages$pdf_page[i], pages$printed_page[i])
      if (
        !marker %in% markers ||
          sum(as.integer(marked) == pages$pdf_page[i]) != 1L
      ) {
        stop(
          "Checked transcription lacks unique page marker: ",
          pages$pdf_page[i],
          " (add one '",
          marker,
          "' line; figure-only pages need one too)"
        )
      }
    }
  }
  if (any(pages$transcription_checked & !pages$printed_page_verified)) {
    stop("Checked transcription requires verified printed page")
  }
  if (
    any(
      (pages$headings_checked |
        pages$sections_checked |
        pages$clauses_checked) &
        !pages$transcription_checked
    )
  ) {
    stop("Heading, section and clause review require checked transcription")
  }
  checked <- pages$printed_page_verified |
    pages$transcription_checked |
    pages$headings_checked |
    pages$sections_checked |
    pages$clauses_checked |
    pages$vouchers_checked
  if (
    any(
      checked &
        !(file.exists(pages$full_image) &
          file.exists(pages$top_image) &
          file.exists(pages$bottom_image))
    )
  ) {
    stop("Image-checked page has missing render files")
  }
  if (
    any(
      batches$status == "verified" &
        vapply(
          batches$batch_id,
          function(key) {
            sub <- pages[pages$batch_id == key, , drop = FALSE]
            !nrow(sub) || !all(vapply(sub[checks], all, logical(1)))
          },
          logical(1)
        )
    )
  ) {
    stop("Verified batch has unchecked pages")
  }
  if (
    any(
      batches$status == "verified" &
        !is.na(batches$blocker) &
        nzchar(trimws(as.character(batches$blocker)))
    )
  ) {
    stop("Verified batch has an unresolved blocker")
  }
  if (any(batches$status == "verified" & !batches$boundary_checked)) {
    stop("Verified batch has unchecked boundary")
  }
  available <- which(batches$status != "verified")
  if (!length(available)) {
    return(list(status = "all_batches_reviewed", remaining = 0L))
  }
  i <- available[1]
  selected <- pages[pages$batch_id == batches$batch_id[i], , drop = FALSE]
  selected$rendered <- file.exists(selected$full_image) &
    file.exists(selected$top_image) &
    file.exists(selected$bottom_image)
  list(
    status = batches$status[i],
    batch = batches[i, , drop = FALSE],
    pages = selected,
    remaining = length(available)
  )
}

flora_review_update <- function(
  id,
  pages = NULL,
  printed_page = NULL,
  flags = list(),
  notes = NULL,
  batch_id = NULL,
  status = NULL,
  boundary_checked = NULL,
  blocker = NULL
) {
  paths <- flora_review_paths(id)
  state <- flora_review_read(id)
  page_flags <- c(
    "printed_page_verified",
    "transcription_checked",
    "headings_checked",
    "sections_checked",
    "clauses_checked",
    "vouchers_checked"
  )
  if (!is.list(flags) || !all(names(flags) %in% page_flags)) {
    stop("flags must be a named list of page review flags")
  }
  if (length(flags) && any(!vapply(flags, is.logical, logical(1)))) {
    stop("Review flags must be logical")
  }
  if (!is.null(pages)) {
    rows <- match(as.integer(pages), state$pages$pdf_page)
    if (anyNA(rows)) {
      stop("Unknown PDF page in update")
    }
    n <- length(rows)
    if (!is.null(printed_page)) {
      if (length(printed_page) != n || anyNA(printed_page)) {
        stop("printed_page must supply one number per PDF page")
      }
      state$pages$printed_page[rows] <- as.integer(printed_page)
    }
    if (!is.null(notes)) {
      if (length(notes) != n) {
        stop("notes must supply one value per PDF page")
      }
      state$pages$notes[rows] <- as.character(notes)
    }
    for (flag in names(flags)) {
      value <- flags[[flag]]
      if (length(value) != 1L && length(value) != n) {
        stop("Each review flag must have length one or match pages")
      }
      state$pages[[flag]][rows] <- rep(value, length.out = n)
    }
  } else if (length(flags) || !is.null(printed_page) || !is.null(notes)) {
    stop("Page updates require pages")
  }
  if (!is.null(batch_id)) {
    row <- match(batch_id, state$batches$batch_id)
    if (is.na(row)) {
      stop("Unknown batch: ", batch_id)
    }
    if (!is.null(status)) {
      if (
        length(status) != 1L ||
          !status %in%
            c(
              "not_started",
              "in_review",
              "blocked",
              "verified"
            )
      ) {
        stop("Unknown batch status")
      }
      state$batches$status[row] <- status
    }
    if (!is.null(boundary_checked)) {
      if (length(boundary_checked) != 1L || is.na(boundary_checked)) {
        stop("boundary_checked must be one logical value")
      }
      state$batches$boundary_checked[row] <- boundary_checked
    }
    if (!is.null(blocker)) {
      if (length(blocker) != 1L) {
        stop("blocker must be one value")
      }
      state$batches$blocker[row] <- as.character(blocker)
    }
  } else if (
    !is.null(status) || !is.null(boundary_checked) || !is.null(blocker)
  ) {
    stop("Batch updates require batch_id")
  }
  readr::write_csv(state$pages, paths$manifest, na = "")
  readr::write_csv(state$batches, paths$batches, na = "")
  flora_review_next(id)
}

flora_review_status <- function(id) {
  state <- flora_review_read(id)
  next_task <- flora_review_next(id)
  list(
    batches = table(factor(
      state$batches$status,
      levels = c("not_started", "in_review", "blocked", "verified")
    )),
    pages_rendered = sum(
      file.exists(state$pages$full_image) &
        file.exists(state$pages$top_image) &
        file.exists(state$pages$bottom_image)
    ),
    pages_total = nrow(state$pages),
    pages_fully_checked = sum(
      vapply(
        state$pages[c(
          "printed_page_verified",
          "transcription_checked",
          "headings_checked",
          "sections_checked",
          "clauses_checked",
          "vouchers_checked"
        )],
        identity,
        logical(nrow(state$pages))
      ) |>
        apply(1, all)
    ),
    next_batch = if (is.null(next_task$batch)) {
      NA_character_
    } else {
      next_task$batch$batch_id
    }
  )
}

flora_review_render_next <- function(id) {
  next_task <- flora_review_next(id)
  if (is.null(next_task$pages)) {
    return(invisible(next_task))
  }
  selected <- next_task$pages
  missing <- selected$pdf_page[!selected$rendered]
  if (!length(missing)) {
    return(invisible(next_task))
  }
  if (!exists("flora_render_pages", mode = "function")) {
    source(
      ".github/skills/extract-traits-from-floras-my/scripts/flora_source.R"
    )
  }
  pdf <- selected$pdf[1]
  hash <- selected$pdf_md5[1]
  if (!identical(unname(tools::md5sum(pdf)), hash)) {
    stop("Source PDF changed")
  }
  for (page in missing) {
    row <- selected[selected$pdf_page == page, , drop = FALSE]
    files <- unlist(
      row[c("full_image", "top_image", "bottom_image")],
      use.names = FALSE
    )
    if (any(file.exists(files))) {
      stop(
        "Incomplete render for PDF page ",
        page,
        "; inspect files before retrying"
      )
    }
    expected <- file.path(
      dirname(row$full_image),
      sprintf("p%02d_%s.png", page, c("full", "top", "bottom"))
    )
    if (!identical(unname(files), unname(expected))) {
      stop("Image paths do not match renderer for PDF page ", page)
    }
  }
  for (page in missing) {
    row <- selected[selected$pdf_page == page, , drop = FALSE]
    flora_render_pages(pdf, page, dirname(row$full_image), dpi = row$dpi)
  }
  invisible(flora_review_next(id))
}

flora_review_handoff <- function(id) {
  task <- flora_review_next(id)
  path <- flora_review_paths(id)$handoff
  if (identical(task$status, "all_batches_reviewed")) {
    lines <- c(
      paste0("# ", id, " review handoff"),
      "",
      "All batches verified; page review is complete.",
      "Extraction status, open questions and QA results are in metadata.yml."
    )
  } else {
    pages <- task$pages
    flags <- c(
      "printed_page_verified",
      "transcription_checked",
      "headings_checked",
      "sections_checked",
      "clauses_checked",
      "vouchers_checked"
    )
    pending <- flags[!vapply(pages[flags], all, logical(1))]
    lines <- c(
      paste0("# ", id, " review handoff"),
      "",
      paste0("PDF: ", pages$pdf[1]),
      paste0("PDF MD5: ", pages$pdf_md5[1]),
      paste0(
        "Batch: ",
        task$batch$batch_id,
        " (PDF pages ",
        min(pages$pdf_page),
        "--",
        max(pages$pdf_page),
        "; ",
        task$status,
        ")"
      ),
      paste0("Images rendered: ", sum(pages$rendered), "/", nrow(pages)),
      paste0("Pending checks: ", paste(pending, collapse = ", ")),
      paste0(
        "Boundary checked: ",
        if (task$batch$boundary_checked) "yes" else "no"
      ),
      paste0(
        "Blocker: ",
        if (is.na(task$batch$blocker) || !nzchar(task$batch$blocker)) {
          "none"
        } else {
          task$batch$blocker
        }
      ),
      paste0("Remaining batches: ", task$remaining),
      "",
      paste0("Manifest: ", flora_review_paths(id)$manifest),
      paste0("Batch ledger: ", flora_review_paths(id)$batches),
      "Image-check the OCR draft. Save checked text with @page PDF PRINTED markers.",
      "Use flora_review_update() for reviewed pages and batch decisions; do not infer completion from rendering."
    )
  }
  writeLines(lines, path)
  invisible(path)
}
