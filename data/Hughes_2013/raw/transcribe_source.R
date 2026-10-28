# This treatment was rendered to page images in the agent scratch directory.
# The PDF is born-digital (typeset text, no page scans). Its text layer is
# retained here only as a reproducible transcription draft. Each measurement and
# name used by build_trait_data.R was checked against images.
# Do not rerun: it overwrites the image-verified Memecylaceae_transcription.txt.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")

dir.create("data/Hughes_2013/raw", showWarnings = FALSE, recursive = TRUE)
pdf <- "inst/extdata/myflora/pdf/FPM/Memecylaceae.pdf"
txt <- pdftools::pdf_text(pdf)

out <- c(
  "# M. Hughes (2013), Memecylaceae, Flora of Peninsular Malaysia, Series II, vol. 4, pp. 269--324.",
  "# Reproducible working transcription of the born-digital PDF text layer; treatment pages were rendered at 220 dpi.",
  "# Repairs and image checks belong in source_fixes.csv. Do not score unverified text."
)
for (i in 7:62) {
  printed <- 262 + i
  lines <- stringr::str_squish(stringr::str_split(txt[[i]], "\\n")[[1]])
  out <- c(out, sprintf("@page %d %d", i, printed), lines)
}
readr::write_lines(out, "data/Hughes_2013/raw/Memecylaceae_transcription.txt")
