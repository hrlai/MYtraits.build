# Segmentation of image-verified transcription, not the PDF OCR layer.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

lines <- flora_read_transcription(
  "data/Hughes_2013/raw/Memecylaceae_transcription.txt"
) |>
  mutate(
    drop = str_detect(
      t,
      "^(FLORA OF PENINSULAR MALAYSIA|SEED PLANTS VOL\\. 4 \\(2013\\)|MEMECYLACEAE \\(HUGHES\\)|SeedPlant4-|\\d{3}|Map \\d+\\.|Figure \\d+\\.)"
    )
  )

headers <- list(
  list(
    name = "family",
    re = "^MEMECYLACEAE$",
    rank = "family",
    block = \(x, sp) "Memecylaceae"
  ),
  list(
    name = "genus",
    re = "^\\d\\. (LIJNDENIA|MEMECYLON) ",
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}")),
    reset_status = TRUE
  ),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  list(
    name = "species",
    re = "^\\d{1,2}\\. (Lijndenia|Memecylon) [a-z]+",
    rank = "species",
    block = \(x, sp) str_extract(x, "(Lijndenia|Memecylon) [a-z]+"),
    sets_species = TRUE
  ),
  list(
    name = "variety",
    re = "^(?:(?:\\d{1,2}[a-z]\\. )?var\\. [a-z]+(?: .*?(?:Fig\\.|Map))|(?:\\d{1,2}[a-z]\\. var\\. [a-z]+))",
    rank = "variety",
    block = \(x, sp) paste0(sp, " ", str_extract(x, "var\\. [a-z]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  desc_start_re = "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark|Stems?)\\b"
)
page_at <- flora_page_at
