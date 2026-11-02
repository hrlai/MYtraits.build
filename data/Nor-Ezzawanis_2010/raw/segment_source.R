# Segment the Cibotiaceae treatment (A.T. Nor-Ezzawanis 2010, Flora of
# Peninsular Malaysia Series I vol. 1: 119--122). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Cibotiaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--10 the treatment (printed 119--122, offset 112), page 9 Figure 1
# (drawing and caption only), page 11 the "PLATES" divider, pages 12--13
# Plates 9 and 10 (printed 225--226; photographs with captions) and page 14
# is blank. flora_layer_flags() flags only the front matter, Figure 1, the
# plates and the blank page; no bad glyphs and no page-number breaks inside
# the treatment.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Cibotiaceae.pdf"

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^17\\. CIBOTIACEAE$",
  last_pdf_page = 10,
  running_head_re = paste0(
    "^(FLORA OF PENINSULAR MALAYSIA|FERNS & LYCOPHYTES VOL\\. 1 \\(2010\\)|",
    "CIBOTIACEAE \\(NOR-EZZAWANIS\\))$"
  )
)

headers <- list(
  list(
    name = "family",
    re = "^17\\. CIBOTIACEAE$",
    rank = "family",
    block = \(x, sp) "Cibotiaceae"
  ),
  list(
    name = "genus",
    re = "^CIBOTIUM Kaulf\\.$",
    rank = "genus",
    block = \(x, sp) "Cibotium"
  ),
  list(
    name = "species",
    re = "^Cibotium barometz \\(L\\.\\) J\\.Sm\\.",
    rank = "species",
    block = \(x, sp) "Cibotium barometz",
    sets_species = TRUE
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?|English name)\\. "
  ),
  # Genus "Large to very large terrestrial ferns."; species "Large
  # terrestrial fern."
  desc_start_re = "^Large\\b"
)
