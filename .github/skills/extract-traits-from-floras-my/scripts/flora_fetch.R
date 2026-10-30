# Download one MyFlora PDF into the gitignored inst/extdata/myflora/pdf/, where
# flora_pdf_check() and flora_review_prepare() expect it. Gentle on the FRIM
# server by design: an existing file is never fetched again, a download is tried
# once with no retry, and any attempt in the last 10 minutes blocks the next one
# (delete <dest>/.last_fetch to override). On failure, stop and report; do not
# call it again or try another family.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_fetch.R")
#      flora_fetch("Cycadaceae", "FPM")
# or:  uvr run .github/skills/extract-traits-from-floras-my/scripts/flora_fetch.R Cycadaceae FPM
# Run from the repository root.

.flora_fetch_url <- function(filename) {
  paste0("https://myflora.frim.gov.my/PDF/", sub("^pdf/", "", filename))
}

flora_fetch <- function(family, flora = NULL, dest = "inst/extdata/myflora/pdf") {
  meta <- utils::read.csv(
    ".github/skills/extract-traits-from-floras-my/assets/myflora_metadata.csv"
  )
  hit <- meta[meta$family == family, ]
  if (!is.null(flora)) {
    hit <- hit[startsWith(hit$filename, paste0("pdf/", flora, "/")), ]
  }
  if (nrow(hit) == 0) {
    near <- unique(meta$family[agrep(family, meta$family, ignore.case = TRUE)])
    stop(
      "No MyFlora PDF for '", family, "'", if (!is.null(flora)) paste0(" in ", flora),
      if (length(near)) paste0(". Close names: ", paste(near, collapse = ", "))
    )
  }
  if (nrow(hit) > 1) {
    stop(
      "'", family, "' is in more than one flora; set flora to one of: ",
      paste(sub("^pdf/([^/]+)/.*", "\\1", hit$filename), collapse = ", ")
    )
  }
  path <- file.path(dest, sub("^pdf/", "", hit$filename))
  if (file.exists(path)) {
    n <- pdftools::pdf_info(path)$pages
    if (n != hit$pages) {
      warning(path, " has ", n, " pages, metadata says ", hit$pages, "; not downloading again")
    }
    return(path)
  }

  stamp <- file.path(dest, ".last_fetch")
  if (file.exists(stamp)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(stamp), units = "mins"))
    if (age < 10) {
      stop(
        "Last FRIM download attempt was ", round(age, 1), " min ago (",
        readLines(stamp, 1), "). Wait 10 min between downloads, or delete ",
        stamp, " to override."
      )
    }
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(format(Sys.time()), hit$filename), stamp)

  url <- .flora_fetch_url(hit$filename)
  part <- paste0(path, ".part")
  on.exit(unlink(part))
  old <- options(HTTPUserAgent = "MYtraits-build (R)", timeout = 600)
  on.exit(options(old), add = TRUE)
  fail <- function(why) {
    stop("Download of ", url, " failed (", why, "). Do not retry; report this to the user.",
         call. = FALSE)
  }
  status <- tryCatch(
    utils::download.file(url, part, mode = "wb", quiet = TRUE),
    error = function(e) fail(conditionMessage(e)),
    warning = function(w) fail(conditionMessage(w))
  )
  if (status != 0) fail(paste("status", status))
  n <- tryCatch(pdftools::pdf_info(part)$pages, error = function(e) NA)
  if (!isTRUE(n == hit$pages)) {
    fail(paste0("got ", n, " pages, metadata says ", hit$pages, "; not a complete PDF"))
  }
  file.rename(part, path)
  path
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  cat(flora_fetch(args[1], if (length(args) > 1) args[2]), "\n")
}
