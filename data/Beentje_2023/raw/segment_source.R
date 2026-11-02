# Segment the Pandanaceae treatment (H.J. Beentje & M.W. Callmander 2023,
# Flora of Peninsular Malaysia Series II vol. 10: 117--215). Run from the
# repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Pandanaceae.pdf, born-digital
# (assets/myflora_metadata.csv). PDF pages 1--7 are the volume front matter,
# pages 8--106 the treatment (printed 117--215, offset 109), page 107 blank,
# page 108 the "PLATES" divider and pages 109--137 the Pandanaceae plates
# (photographs with credits only). flora_layer_flags() flags no bad glyphs and
# no page-number breaks inside the treatment; its low_text pages (pdf 24, 28,
# 45, 51, 55, 57, 72, 82, 86, 91, 95) hold the full-page Figures 3--13.
# Family keys, "Spot characters", "Specific habitats" and the list of taxa
# with unknown male inflorescences (pdf 12--18) are key material, not traits.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

pdf_file <- "inst/extdata/myflora/pdf/FPM/Pandanaceae.pdf"
gen_re <- "Benstonea|Freycinetia|Pandanus"

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^PANDANACEAE$",
  last_pdf_page = 106,
  running_head_re = paste0(
    "^(FLORA OF PENINSULAR MALAYSIA|SEED PLANTS VOL\\. 10 \\(2023\\)|",
    "PANDANACEAE \\(BEENTJE & CALLMANDER\\))$"
  ),
  caption_re = "^(Map \\d+\\.|Figure \\d+\\.)"
)

headers <- list(
  list(
    name = "family",
    re = "^PANDANACEAE$",
    rank = "family",
    block = \(x, sp) "Pandanaceae"
  ),
  list(
    name = "genus",
    re = "^\\d\\. (BENSTONEA|FREYCINETIA|PANDANUS) ",
    rank = "genus",
    block = \(x, sp) str_to_title(str_match(x, "^\\d\\. ([A-Z]+)")[, 2]),
    reset_status = TRUE
  ),
  # Keys and the family's key-like lists ("Spot characters", "Specific
  # habitats", "Taxa for which the male inflorescence is still unknown")
  list(
    name = "key",
    re = paste0(
      "^(Key to |Spot characters$|Specific habitats$|",
      "Taxa for which the male inflorescence)"
    ),
    rank = "key",
    block = \(x, sp) "KEY"
  ),
  list(
    name = "cultivated",
    re = "^CULTIVATED PLANTS$",
    rank = "section",
    block = \(x, sp) "SECTION cultivated",
    status = "cultivated"
  ),
  list(
    name = "not_native",
    re = "^NOT ACCEPTED AS NATIVE$",
    rank = "section",
    block = \(x, sp) "SECTION not native",
    status = "not accepted as native"
  ),
  list(
    name = "species",
    re = paste0("^\\d{1,2}\\. (", gen_re, ") [a-z-]+ "),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  # Unnumbered headings of the cultivated and not-native sections
  list(
    name = "species_unnumbered",
    re = paste0(
      "^Pandanus (amaryllifolius Roxb\\.|kaida Kurz|tectorius Parkinson|",
      "polycephalus Lam\\.|nitidus \\(Miq\\.\\) Kurz)$"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, "Pandanus [a-z]+"),
    sets_species = TRUE
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?|Terminology used)[.:] ?"
  ),
  desc_start_re = paste0(
    "^(Arborescent|Acaulescent|Woody climbers|Erect|Bushy|Climber|Climbing|",
    "Clustering|Shrubs?|Small|Stemless|Solitary|Tufted|Robust|Pandans?|",
    "Plants?|Trees?|Tall|Medium|Epiphyt[a-z]*|Decumbent|Monopodial|Short|Large|",
    "Understorey|Massive|Low|Slender|Climbers|Thicket-forming|Stems|Leaves|",
    "Male inflorescences)\\b"
  )
) |>
  mutate(section = str_replace(section, "^Notes?$", "Notes"))
