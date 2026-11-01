# Segment the Fagaceae treatment (E. Soepadmo, S. Julia & Rusea Go 2000, Tree
# Flora of Sabah and Sarawak vol. 3: 1--118). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Fagaceae.pdf, the 2007 electronic
# version of the 2000 volume (L.G. Saw & B. Aslina), born-digital text. PDF
# pages 7--124 are printed pages 1--118 (printed = pdf - 6). The 17 figure
# plates (pdf 15, 17, 22, 24, 27, 31, 35, 48, 59, 71, 73, 77, 95, 97, 113,
# 117, 123) are images with no text layer beyond the running head.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/Fagaceae.pdf"
genera <- c("Castanopsis", "Lithocarpus", "Quercus", "Trigonobalanus")
gen_re <- paste(genera, collapse = "|")
plate_pages <- c(15, 17, 22, 24, 27, 31, 35, 48, 59, 71, 73, 77, 95, 97, 113, 117, 123)

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^FAGACEAE$",
  last_pdf_page = 124,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 3 \\(2000\\))?|",
    "FAGACEAE \\(SOEPADMO, JULIA & GO\\))$"
  ),
  # captions are inside the figure images
  caption_re = "^Current address: ",
  side_caption_re = "(?!x)x",
  # plate pages: running head (garbled on pdf 71) and a stray "vi"
  drop_pages = plate_pages
) |>
  # the page range dash is a minus sign on some pages
  mutate(t = str_replace_all(t, "−", "–"))

headers <- list(
  list(
    name = "family",
    re = "^FAGACEAE$",
    rank = "family",
    block = \(x, sp) "Fagaceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d\\. (", str_to_upper(gen_re), ")\\b"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    # Trigonobalanus verticillata, the one species of its genus, is unnumbered
    re = paste0("^(\\d{1,2}\\. (", gen_re, ") [a-z-]+ [A-Z(]|Trigonobalanus verticillata Forman)"),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy and Classification|Uses?|Vernacular names?)\\. ",
  desc_start_re = "^(Monoecious|Trees?|Shrubs?|Stunted|Treelets?)\\b",
  status_ranks = character()
)
page_at <- flora_page_at
