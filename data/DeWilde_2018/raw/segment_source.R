# Segment the Myristicaceae treatment (W.J.J.O. de Wilde & B.E.E. Duyfjes
# 2018, Flora of Peninsular Malaysia Series II vol. 7: 127--256). Run from the
# repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Myristicaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--6 are the volume front matter,
# pages 7--136 the treatment (printed 127--256, offset 120) and pages 137--143
# plates (printed 295--306). flora_layer_flags() flags only the front matter,
# the plates and the full-page Figures and map pages inside the treatment
# (low text); one bad glyph on pdf 73 (see source_fixes.csv); no page-number
# breaks inside the treatment.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Myristicaceae.pdf"
genera <- c("Endocomia", "Gymnacranthera", "Horsfieldia", "Knema", "Myristica")
gen_re <- paste(genera, collapse = "|")

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^MYRISTICACEAE$",
  last_pdf_page = 136,
  running_head_re = paste0(
    "^(FLORA OF PENINSULAR MALAYSIA|SEED PLANTS VOL\\. 7 \\(2018\\)|",
    "MYRISTICACEAE \\(DE WILDE & DUYFJES\\))$"
  )
)

headers <- list(
  list(
    name = "family",
    re = "^MYRISTICACEAE$",
    rank = "family",
    block = \(x, sp) "Myristicaceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d\\. (", str_to_upper(gen_re), ") [A-Z(]"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = paste0("^\\d{1,2}\\. (", gen_re, ") [a-z-]+ "),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = "^(\\d{1,2}[a-z]\\. )?(subsp|var)\\. [a-z-]+( |$)",
    rank = \(x) if_else(str_detect(x, "subsp\\."), "subspecies", "variety"),
    block = \(x, sp) paste(sp, str_extract(x, "(subsp|var)\\. [a-z-]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?|English names?)\\. "
  ),
  desc_start_re = paste0(
    "^(Dioecious|Monoecious|Small|Medium|Large|Big|Slender|Usually a|Tree|Trees|Treelet|Shrubs?|",
    "Twigs?|Leaves|Leaf(?=:)|Inflorescences?|Flowers?|Fruits?|Bark)\\b"
  ),
  # varieties named in Taxonomy paragraphs and a synonym citation
  # (H. polyspherula var. sumatrana, printed 161) are not headings
  cancel_header = \(l) {
    str_detect(l$t, ", and var\\.|^var\\. [a-z]+\\. |Tr\\. Fl\\. Malaya")
  }
)
