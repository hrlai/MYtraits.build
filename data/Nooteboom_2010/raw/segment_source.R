# Segment the Symplocaceae treatment (H.P. Nooteboom 2010, Flora of
# Peninsular Malaysia Series II vol. 1: 219--264). Run from the repository
# root.
#
# Source: inst/extdata/myflora/pdf/FPM/Symplocaceae.pdf, scanned_ocr
# (assets/myflora_metadata.csv). PDF pages 8--53 are the treatment (printed
# 219--264, offset 211); pdf 63 is Plate 16 (printed 308, photograph of
# S. fasciculata, no voucher). Input is the image-checked transcription
# raw/Symplocaceae_transcription.txt, not the OCR layer.
#
# One genus (Symplocos); the family has no description ("A monogeneric
# family."). 22 numbered species; infraspecific headings are numbered "7a."
# etc. or, for a single taxon in the region, unnumbered ("var. adenophylla").
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

lines <- flora_read_transcription(
  "data/Nooteboom_2010/raw/Symplocaceae_transcription.txt"
)

infra_name <- function(x, sp) {
  ss <- str_match(x, "subsp\\. ([a-z]+)")[, 2]
  va <- str_match(x, "var\\. ([a-z]+)")[, 2]
  paste0(
    sp,
    if_else(is.na(ss), "", paste0(" subsp. ", ss)),
    if_else(is.na(va), "", paste0(" var. ", va))
  )
}

headers <- list(
  list(
    name = "family",
    re = "^SYMPLOCACEAE$",
    rank = "family",
    block = \(x, sp) "Symplocaceae"
  ),
  list(
    name = "genus",
    re = "^SYMPLOCOS Jacq\\.",
    rank = "genus",
    block = \(x, sp) "Symplocos"
  ),
  list(name = "key", re = "^(Key to |Addendum$)", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. Symplocos [a-z]+ ",
    rank = "species",
    block = \(x, sp) str_extract(x, "Symplocos [a-z]+"),
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = "^(\\d{1,2}[a-e]\\. )?(subsp|var)\\. [a-z]+",
    rank = \(x) if_else(str_detect(x, "var\\. "), "variety", "subspecies"),
    block = infra_name
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|",
    "Vernacular names?)\\. "
  ),
  desc_start_re = paste0(
    "^(Small|Medium|Medium-sized|Bushy|Tree|Trees|Shrubs?|Twigs?|Leaves|",
    "Leaf(?=:)|Flowers?|Fruits?)\\b"
  )
)
