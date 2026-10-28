# Segment the page-image transcription, not the Araucariaceae PDF OCR layer.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

lines <- flora_read_transcription(
  "data/Lim_2010/raw/Araucariaceae_transcription.txt"
)
headers <- list(
  list(name = "family", re = "^ARAUCARIACEAE$", rank = "family",
       block = \(x, sp) "Araucariaceae"),
  list(name = "genus", re = "^\\d\\. AGATHIS ", rank = "genus",
       block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))),
  list(name = "key", re = "^Key to", rank = "key",
       block = \(x, sp) paste("KEY", x)),
  list(name = "species", re = "^\\d{1,2}\\. Agathis [a-z]+", rank = "species",
       block = \(x, sp) str_extract(x, "Agathis [a-z]+"), sets_species = TRUE)
)
paras <- flora_segment(
  lines, headers,
  desc_start_re = "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark|Bole)\\b"
)
page_at <- flora_page_at
