# Segmentation of the image-checked transcription (raw/Sapindaceae_transcription.txt),
# not the PDF text layer. Source fixes are applied line by line with the
# printed page of each line before segmentation.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

fixes <- read_csv(
  "data/Adema_1996/raw/source_fixes.csv",
  col_types = cols(.default = "c", page = "i")
)
lines <- flora_read_transcription(
  "data/Adema_1996/raw/Sapindaceae_transcription.txt"
) |>
  mutate(t = flora_apply_fixes(t, fixes, printed_page))

genera <- c(
  "Alectryon", "Allophylus", "Arytera", "Cubilia", "Dictyoneura",
  "Dimocarpus", "Ganophyllum", "Glenniea", "Guioa", "Harpullia",
  "Lepisanthes", "Litchi", "Mischocarpus", "Nephelium", "Paranephelium",
  "Pometia", "Rhysotoechia", "Trigonachras", "Tristiropsis", "Xerospermum",
  "Zollingeria"
)
genus_alt <- paste(genera, collapse = "|")
sp_re <- paste0("^(\\d{1,2}\\. )?(", genus_alt, ") (sp\\. [A-Z]|[a-z-]+)\\b")
infra_re <- "^(\\d{1,2}[a-z]\\. |[a-z]\\. )?(subsp\\.|var\\.|forma) [a-z]"

headers <- list(
  list(
    name = "family",
    re = "^SAPINDACEAE$",
    rank = "family",
    block = \(x, sp) "Sapindaceae"
  ),
  list(
    name = "genus",
    re = "^\\d{1,2}\\. [A-Z]{4,}\\b",
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{4,}")),
    reset_status = TRUE
  ),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  # a key lead ending in dot leaders (and a couplet number) inside the
  # infraspecific keys, where each taxon's treatment follows its lead
  list(name = "keylead", re = "\\.{3,}\\s*(\\d+)?\\s*$", rank = "key", block = \(x, sp) {
    "KEY lead"
  }),
  list(
    name = "species",
    re = sp_re,
    rank = "species",
    block = \(x, sp) {
      str_extract(x, paste0("(", genus_alt, ") (sp\\. [A-Z]|[a-z-]+)"))
    },
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = infra_re,
    rank = \(x) {
      case_when(
        str_detect(x, "subvar\\. ") ~ "subvariety",
        str_detect(x, "\\bvar\\. ") ~ "variety",
        str_detect(x, "^subsp\\. ") ~ "subspecies",
        TRUE ~ "form"
      )
    },
    # rank markers and epithets only; authorities can sit between them
    block = \(x, sp) {
      paste(
        sp,
        str_extract_all(x, "(subsp\\.|var\\.|subvar\\.|forma) [a-z-]+")[[1]] |>
          paste(collapse = " ")
      )
    }
  )
)

desc_re <- "^(Small|Medium|Big|Large|Tall|Mostly|Monoecious|Dioecious|Tree|Trees|Shrubs?|Treelets?|Lianas?|Climbers?|Twigs?|Leaves|Leaf(?=:)|Leaflets?|Inflorescences?|Flowers?|Petals|Sepals|Fruits?|Bark)\\b"

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy|Notes?|Uses?|Vernacular names?|Field characters?|Phenology)\\. ",
  desc_start_re = desc_re,
  # species headings are short lines; a long line starting with a binomial is text
  cancel_header = \(l) l$hdr %in% "species" & nchar(l$t) > 110
)

# An unlabelled paragraph after a description (distribution and ecology text in
# the infraspecific treatments) is not description: flora_segment() fills the
# section down, so relabel it here.
paras <- paras |>
  group_by(block) |>
  mutate(
    prev_end = lag(text, default = ""),
    section = if_else(
      section == "Description" &
        lag(section, default = "") == "Description" &
        !str_detect(text, "^[a-z(0-9]") &
        str_detect(prev_end, "\\.$") &
        !str_detect(text, desc_re),
      "Unlabelled note",
      section
    )
  ) |>
  ungroup() |>
  select(-prev_end)

# Infraspecific treatments printed inside a key carry only nomenclature, a
# description and unlabelled distribution text. A labelled section after the
# last of them (Lepisanthes divaricata Notes) belongs to the species.
in_key_species <- c(
  "Dimocarpus longan",
  "Lepisanthes divaricata",
  "Nephelium cuspidatum",
  "Nephelium lappaceum",
  "Xerospermum laevigatum"
)
paras <- paras |>
  mutate(
    sp = str_extract(block, paste0("^(", paste(in_key_species, collapse = "|"), ")(?= )")),
    move = !is.na(sp) &
      !section %in% c("Nomenclature", "Description", "Unlabelled note"),
    block = if_else(move, sp, block),
    rank = if_else(move, "species", rank)
  ) |>
  select(-sp, -move)
