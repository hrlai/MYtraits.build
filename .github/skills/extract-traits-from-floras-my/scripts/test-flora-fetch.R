source(".github/skills/extract-traits-from-floras-my/scripts/flora_fetch.R")

# No network: every case below must stop or return before download.file().
fails <- function(expr, pattern) {
  msg <- tryCatch({ expr; "" }, error = conditionMessage)
  grepl(pattern, msg)
}
dest <- tempfile("flora-fetch-")
dir.create(file.path(dest, "FPM"), recursive = TRUE)
tryCatch(
  {
    stopifnot(
      fails(flora_fetch("Anisophylleaceae", dest = dest), "FPM, TFSS"),
      fails(flora_fetch("Cycadacea", dest = dest), "Close names: Cycadaceae"),
      .flora_fetch_url("pdf/FPM/Cycadaceae.pdf") ==
        "https://myflora.frim.gov.my/PDF/FPM/Cycadaceae.pdf",
      .flora_fetch_url("pdf/TFSS/Anisophylleaceae.pdf") ==
        "https://myflora.frim.gov.my/PDF/TFSS/Anisophylleaceae.pdf"
    )

    meta <- utils::read.csv(
      ".github/skills/extract-traits-from-floras-my/assets/myflora_metadata.csv"
    )
    pdf <- file.path(dest, "FPM", "Cycadaceae.pdf")
    grDevices::pdf(pdf)
    for (i in seq_len(meta$pages[meta$filename == "pdf/FPM/Cycadaceae.pdf"])) {
      graphics::plot.new()
    }
    grDevices::dev.off()
    stopifnot(
      flora_fetch("Cycadaceae", "FPM", dest = dest) == pdf,
      !file.exists(file.path(dest, ".last_fetch"))
    )

    writeLines("earlier attempt", file.path(dest, ".last_fetch"))
    stopifnot(fails(
      flora_fetch("Anisophylleaceae", "TFSS", dest = dest),
      "min ago \\(earlier attempt\\)"
    ))
    cat("flora_fetch tests passed\n")
  },
  finally = unlink(dest, recursive = TRUE)
)
