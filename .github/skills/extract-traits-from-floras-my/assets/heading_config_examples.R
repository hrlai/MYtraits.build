# Heading configurations for flora_segment(), taken from the two families that
# were segmented as reference examples. Adapt the closer one in the family's
# raw/segment_source.R; do not copy its page range, input reader or heading
# patterns without checking the source images. Both examples reproduce their
# respective `paras` tables.
#
# Review headings, captions, page numbering, sections and description starts
# for each family. TFSS layouts vary; do not assume either FPM example fits.

suppressPackageStartupMessages(library(tidyverse))
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

# ---- Simple family: one genus, species only (Lim_2010, Araucariaceae) --------
# Input is a hand transcription, so there are no running heads to drop.
lim_lines <- flora_read_transcription(
  "data/Lim_2010/raw/Araucariaceae_transcription.txt"
)
lim_headers <- list(
  list(
    name = "family",
    re = "^ARAUCARIACEAE$",
    rank = "family",
    block = \(x, sp) "Araucariaceae"
  ),
  list(
    name = "genus",
    re = "^\\d\\. AGATHIS ",
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. Agathis [a-z]+",
    rank = "species",
    block = \(x, sp) str_extract(x, "Agathis [a-z]+"),
    sets_species = TRUE
  )
)
lim_paras <- flora_segment(
  lim_lines,
  lim_headers,
  # add family-specific habit / organ words that open a description
  desc_start_re = "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark|Bole)\\b"
)

# ---- Large family: groups, infraspecific taxa, informal forms, special sections
# (Coode_2018, Elaeocarpaceae). Input is a born-digital PDF.
co_lines <- flora_read_pdf_lines(
  "inst/extdata/FPM/Elaeocarpaceae.pdf",
  first_line_re = "^ELAEOCARPACEAE$",
  last_pdf_page = 100, # pp. 101-105 are plates
  running_head_re = "^(SEED PLANTS VOL\\. 7 \\(2018\\)|ELAEOCARPACEAE \\(COODE\\))$"
)
epithet <- \(x) str_match(x, "(subsp|var)\\. ([a-z]+)")[, 3]
co_headers <- list(
  list(
    name = "family",
    re = "^ELAEOCARPACEAE$",
    rank = "family",
    block = \(x, sp) "Elaeocarpaceae"
  ),
  list(
    name = "genus",
    re = "^\\d\\. (ELAEOCARPUS|SLOANEA) L\\.$",
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}")),
    reset_status = TRUE
  ),
  list(
    name = "group",
    re = "^(Acronodia|Coilopetalum|Elaeocarpus|Ganitrus|Monocera|Polystachyus) Group$",
    rank = "infrageneric_group",
    block = \(x, sp) paste("Elaeocarpus", x)
  ),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. (Elaeocarpus|Sloanea) [a-z]+",
    rank = "species",
    block = \(x, sp) {
      str_extract(x, "(Elaeocarpus|Sloanea) (sp\\. nov\\.|[a-z]+)")
    },
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = "^(\\d{1,2}[a-c]\\. )?(subsp|var)\\. [a-z]+",
    rank = \(x) {
      if (str_extract(x, "subsp|var") == "subsp") "subspecies" else "variety"
    },
    block = \(x, sp) {
      paste0(sp, " ", str_extract(x, "subsp|var"), ". ", epithet(x))
    }
  ),
  list(
    name = "form",
    re = "^\\d[a-c]\\. E\\. angustifolius \u2013",
    rank = "informal_form",
    block = \(x, sp) {
      paste0(sp, " (", str_extract(x, "(?<=\u2013 ).*?form|intermediate"), ")")
    }
  ),
  list(
    name = "formp",
    re = "^Ovalis form$",
    rank = "informal_form",
    block = \(x, sp) "Elaeocarpus petiolatus (ovalis form)"
  ),
  list(
    name = "section",
    re = "^(Incompletely Known Species|Excluded Species)$",
    rank = "section",
    block = \(x, sp) paste("SECTION", x),
    status = \(x) {
      if (str_detect(x, "Excluded")) "excluded" else "incompletely_known"
    }
  ),
  list(
    name = "oddsp",
    re = "^Elaeocarpus (macrocarpus|euneurus|serratus) ",
    rank = "species",
    block = \(x, sp) str_extract(x, "Elaeocarpus [a-z]+")
  )
)
co_paras <- flora_segment(
  co_lines,
  co_headers,
  form_label_re = "^(Typical form|Western form of subsp\\. apiculatus)$",
  # an indented "subsp. x" line is a map caption, not a heading
  cancel_header = \(l) l$hdr %in% "infra" & l$indent > 15,
  # a wrapped key line ("subsp. ferrugineus" alone) is not a heading
  ignore_header = \(h, x, rk) {
    h == "infra" &&
      identical(rk, "key") &&
      !str_detect(x, "^\\d{1,2}[a-c]\\. |Map \\d")
  },
  post_blocks = \(l) {
    l$status[str_detect(l$block, "sp\\. nov")] <- "unnamed"
    l
  }
)
