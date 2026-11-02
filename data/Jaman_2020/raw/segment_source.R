# Segment the Cyatheaceae treatment (R. Jaman 2020, Flora of Peninsular
# Malaysia Series I vol. 3: 77--107). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Cyatheaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--37 the treatment (printed 77--107, offset 70), page 38 the "PLATES"
# divider and pages 39--50 Plates 11--22 (printed 257--268; photographs with
# credits and taxon captions only) and page 51 is blank.
# flora_layer_flags() flags only the front matter, the plates and pdf 51; no
# bad glyphs and no page-number breaks inside the treatment.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Cyatheaceae.pdf"

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^18\\. CYATHEACEAE$",
  last_pdf_page = 37,
  running_head_re = paste0(
    "^(FLORA OF PENINSULAR MALAYSIA|FERNS & LYCOPHYTES VOL\\. 3 \\(2020\\)|",
    "CYATHEACEAE \\(JAMAN\\))$"
  )
)

headers <- list(
  list(
    name = "family",
    re = "^18\\. CYATHEACEAE$",
    rank = "family",
    block = \(x, sp) "Cyatheaceae"
  ),
  list(
    name = "genus",
    re = "^CYATHEA Sm\\.$",
    rank = "genus",
    block = \(x, sp) "Cyathea"
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. Cyathea [a-z]+ ",
    rank = "species",
    block = \(x, sp) str_extract(x, "Cyathea [a-z]+"),
    sets_species = TRUE
  ),
  list(
    name = "section",
    re = "^Excluded species$",
    rank = "section",
    block = \(x, sp) paste("SECTION", x),
    status = "excluded"
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?|English name)\\. "
  ),
  # Family "Scaly tree ferns."; genus and species "Trunk ..."; C.
  # recommutata "Small and slender tree fern, trunk ...".
  desc_start_re = "^(Scaly tree ferns|Trunk|Small and slender tree fern)\\b"
)
