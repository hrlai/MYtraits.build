# Segment the Nepenthaceae treatment (M. Cheek & M. Jebb 2012, Flora of
# Peninsular Malaysia Series II vol. 3: 249--275). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Nepenthaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--33 the treatment (printed 249--275, offset 242), page 34 the
# "PLATES" divider and pages 35--39 the plates (printed 362--366; photographs
# with credits only; pdf 39 also holds Ochnaceae). flora_layer_flags() flags
# only the front matter, the plates and pdf 20 and 27 (full-page Figures 1 and
# 2, low text); no bad glyphs and no page-number breaks inside the treatment.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Nepenthaceae.pdf"

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^NEPENTHACEAE$",
  last_pdf_page = 33,
  running_head_re = "^(FLORA OF PENINSULAR MALAYSIA|SEED PLANTS VOL\\. 3 \\(2012\\)|NEPENTHACEAE \\(CHEEK & JEBB\\))$"
)

headers <- list(
  list(
    name = "family",
    re = "^NEPENTHACEAE$",
    rank = "family",
    block = \(x, sp) "Nepenthaceae"
  ),
  list(
    name = "genus",
    re = "^NEPENTHES L\\.$",
    rank = "genus",
    block = \(x, sp) "Nepenthes"
  ),
  list(name = "key", re = "^Key to Nepenthes", rank = "key", block = \(x, sp) {
    "KEY"
  }),
  # "Named Hybrids" and "Excluded Species" set the status of the numbered
  # headings that follow them.
  list(
    name = "hybrids",
    re = "^Named Hybrids$",
    rank = "section",
    block = \(x, sp) "SECTION hybrids",
    status = "hybrid"
  ),
  list(
    name = "excluded",
    re = "^Excluded Species$",
    rank = "section",
    block = \(x, sp) "SECTION excluded",
    status = "excluded"
  ),
  list(
    name = "species",
    re = "^\\d{1,2}\\. Nepenthes ×?[a-z]+ ",
    rank = "species",
    block = \(x, sp) str_extract(x, "Nepenthes ×?[a-z]+"),
    sets_species = TRUE
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation [Ss]tatus|Conservation|Ecology|Taxonomy|",
    "Notes?|Uses|Vernacular names?|Reproductive biology|Morphology|",
    "Pitcher function|The Pitcher Ecosystem|Trapping of prey|Hybridization|",
    "Collecting methods)\\. "
  ),
  # Genus description opens "Carnivorous"; species "Terrestrial"; the three
  # hybrids give a comparative paragraph opening "Intermediate".
  desc_start_re = "^(Carnivorous|Terrestrial|Intermediate)\\b"
) |>
  mutate(section = str_replace(section, "Conservation Status", "Conservation status"))
