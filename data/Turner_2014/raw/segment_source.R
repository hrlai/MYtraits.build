# Segment the Annonaceae treatment (I.M. Turner et al. 2014, Tree Flora of
# Sabah and Sarawak vol. 8: 1--200). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Annonaceae.pdf, born-digital text.
# PDF pages 12--211 are printed pages 1--200 (printed = pdf - 11); PDF
# 212--219 are the colour plates (printed 228--235), not read. Figure pages
# carry only a caption ("Fig. 1. ...") under the image; captions are dropped
# here. The eight climbing genera (Uvaria, Artabotrys, Desmos, Sphaerocoryne,
# Friesodielsia, Fissistigma, Pyramidanthe, Mitrella) are described only as
# annotations inside the key to genera (pp. 7--10); the key is skipped, so
# they are not segmented (listed in taxon_flags.csv).
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/Annonaceae.pdf"
genera <- c(
  "Alphonsea", "Anaxagorea", "Cananga", "Cyathocalyx", "Dasymaschalon",
  "Disepalum", "Drepananthus", "Goniothalamus", "Hubera", "Maasia",
  "Marsypopetalum", "Meiogyne", "Mezzettia", "Miliusa", "Mitrephora",
  "Monocarpia", "Monoon", "Neo-uvaria", "Orophea", "Phaeanthus",
  "Polyalthia", "Popowia", "Pseuduvaria", "Sageraea", "Stelechocarpus",
  "Trivalvaria", "Xylopia"
)
gen_re <- paste(genera, collapse = "|")
# the one species of its genus in Sabah and Sarawak: heading unnumbered
sole_species <- c(
  "Cananga odorata", "Cyathocalyx magnifructus", "Marsypopetalum pallidum",
  "Miliusa macropoda", "Trivalvaria macrophylla"
)

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^ANNONACEAE$",
  last_pdf_page = 211,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 8 \\(201[34]\\))?|",
    "ANNONACEAE \\(TURNER et al\\.\\))$"
  ),
  caption_re = "^Fig\\. \\d+\\. ",
  side_caption_re = "(?!x)x"
) |>
  mutate(
    # author block and the footnote to it (p. 1)
    drop = drop |
      (pdf_page == 12 & !str_detect(t, "^ANNONACEAE$") &
        line < min(line[str_detect(t, "^Juss\\., Gen\\. Pl\\.")])) |
      str_detect(t, "^Current address: 103, Waller|^Hathorn Road, University|^1$")
  )

headers <- list(
  list(
    name = "family",
    re = "^ANNONACEAE$",
    rank = "family",
    block = \(x, sp) "Annonaceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d{1,2}\\. (", str_to_upper(gen_re), ") [A-Z(]"),
    rank = "genus",
    # "NEO-UVARIA" -> "Neo-uvaria", as in the species headings
    block = \(x, sp) str_to_sentence(str_extract(x, "[A-Z-]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x, sp)
  }),
  list(
    name = "species",
    re = paste0(
      # authorities may start "van der Heijden", "van Heusden"
      "^\\d{1,3}\\. (", gen_re, ") [a-z-]+ (\\(|[A-Z]|van )|",
      "^(", paste(sole_species, collapse = "|"), ") (\\(|[A-Z])"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Ecology|Uses?|Notes?|Vernacular names?|Phenology|",
    "Conservation status)\\. "
  ),
  desc_start_re = paste0(
    "^(Trees?|Treelets?|Shrubs?|Shrublets?|Small|Large|Medium|Monopodial|",
    "Monoecious)\\b"
  ),
  skip_ranks = c("key", "drop"),
  status_ranks = character()
)

# Labelled paragraphs keep their section; an unlabelled paragraph after a
# label is a note, not a continuation of Ecology or Distribution. A
# description that continues after a figure page restarts with an organ word.
paras <- paras |>
  group_by(block) |>
  mutate(
    section = if_else(
      section == "Unlabelled note" & lag(section) %in% "Description",
      "Description",
      section
    )
  ) |>
  ungroup() |>
  mutate(
    section = case_when(
      label_len == 0 &
        !section %in% c("Description", "Nomenclature", "Unlabelled note") ~
        "Note",
      TRUE ~ section
    )
  )
page_at <- flora_page_at
