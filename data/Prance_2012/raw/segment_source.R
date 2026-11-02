# Segment the Chrysobalanaceae treatment (G.T. Prance 2012, Flora of
# Peninsular Malaysia Series II vol. 3: 13--46). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Chrysobalanaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--40 the treatment (printed 13--46, offset 6), page 41 the "PLATES"
# divider and page 42 Plate 1 (printed 335; photographs with credits only).
# flora_layer_flags() flags only the front matter, the plates and the
# full-page Figures 1--6 and a map page (pdf 16, 19, 22, 26, 28, 37, 40; low
# text); no bad glyphs and no page-number breaks inside the treatment.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Chrysobalanaceae.pdf"
genera <- c("Atuna", "Kostermanthus", "Licania", "Maranthes", "Parastemon", "Parinari")
gen_re <- paste(genera, collapse = "|")

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^CHRYSOBALANACEAE$",
  last_pdf_page = 40,
  running_head_re = paste0(
    "^(FLORA OF PENINSULAR MALAYSIA|SEED PLANTS VOL\\. 3 \\(2012\\)|",
    "CHRYSOBALANACEAE \\(PRANCE\\))$"
  )
)

headers <- list(
  list(
    name = "family",
    re = "^CHRYSOBALANACEAE$",
    rank = "family",
    block = \(x, sp) "Chrysobalanaceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d\\. (", str_to_upper(gen_re), ") [A-Z]"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    # Licania, Maranthes and Parastemon have one species each, unnumbered;
    # synonym citations also start lines with a binomial, so name those three
    re = paste0(
      "^(\\d{1,2}\\. (", gen_re, ") [a-z]+ |",
      "Licania splendens \\(Korth|Maranthes corymbosa Blume|",
      "Parastemon urophyllus \\(Wall)"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z]+")),
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = "^\\d{1,2}[a-z]\\. subsp\\. [a-z]+",
    rank = "subspecies",
    block = \(x, sp) paste(sp, str_extract(x, "subsp\\. [a-z]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?)\\. "
  ),
  # a page turn inside a description (P. oblongifolia, printed 42) starts
  # with "Inflorescences"
  desc_start_re = paste0(
    "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Twigs?|Leaves|Leaf(?=:)|",
    "Inflorescences?|Flowers?|Fruits?|Bark)\\b"
  )
)
