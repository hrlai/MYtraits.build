# Segment the Selaginellaceae treatment (K.M. Wong 2010, Flora of Peninsular
# Malaysia Series I vol. 1: 49--86). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Selaginellaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--44 the treatment (printed 49--86, offset 42), pages 45--48 the
# plates (printed 215, 217--219; photographs with credits only) and page 49 is
# blank. flora_layer_flags() flags pdf 17 and 22 (full-page Figures 1 and 2,
# low text); their captions are in the layer and only supply figure vouchers.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

lines <- flora_read_pdf_lines(
  "inst/extdata/myflora/pdf/FPM/Selaginellaceae.pdf",
  first_line_re = "^2\\. SELAGINELLACEAE$",
  last_pdf_page = 44,
  running_head_re = "^(FLORA OF PENINSULAR MALAYSIA|FERNS & LYCOPHYTES VOL\\. 1 \\(2010\\)|SELAGINELLACEAE \\(WONG\\))$"
)

headers <- list(
  list(
    name = "family",
    re = "^2\\. SELAGINELLACEAE$",
    rank = "family",
    block = \(x, sp) "Selaginellaceae"
  ),
  list(
    name = "genus",
    re = "^SELAGINELLA Beauv\\.$",
    rank = "genus",
    block = \(x, sp) "Selaginella"
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x, sp)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. Selaginella [a-z]+ ",
    rank = "species",
    block = \(x, sp) str_extract(x, "Selaginella [a-z]+"),
    sets_species = TRUE
  ),
  list(
    name = "variety",
    re = "^\\d{1,2}[ab]\\. var\\. [a-z]+",
    rank = "variety",
    block = \(x, sp) paste(sp, str_extract(x, "var\\. [a-z]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|Chromosome numbers)\\. ",
  # Species descriptions open "Plant ..."; the genus "Plants herbaceous";
  # var. strigosa "Differs from ..." and var. ciliata "Its stoloniferous ...".
  desc_start_re = "^(Plants?|Differs|Its stoloniferous)\\b"
)
