# Segmentation of the transcription (raw/Rutaceae_transcription.txt), drafted
# from the PDF text layer of the electronic edition (vector text; the layer is
# what renders). Source fixes are applied line by line with the printed page
# of each line before segmentation.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

fixes <- read_csv(
  "data/Jones_1995/raw/source_fixes.csv",
  col_types = cols(.default = "c", page = "i")
)
lines <- flora_read_transcription(
  "data/Jones_1995/raw/Rutaceae_transcription.txt"
) |>
  mutate(t = flora_apply_fixes(t, fixes, printed_page))

genera <- c(
  "Acronychia", "Burkillanthus", "Citrus", "Clausena", "Glycosmis",
  "Lunasia", "Maclurodendron", "Melicope", "Merope", "Merrillia",
  "Micromelum", "Monanthocitrus", "Murraya", "Pleiospermium", "Severinia",
  "Tetractomia", "Zanthoxylum"
)
genus_alt <- paste(genera, collapse = "|")
sp_re <- paste0("^(\\d{1,2}\\. )?(", genus_alt, ") [a-z-]+ [A-Z(]")

headers <- list(
  list(
    name = "family",
    re = "^RUTACEAE$",
    rank = "family",
    block = \(x, sp) "Rutaceae"
  ),
  list(
    name = "genus",
    re = "^\\d{1,2}\\. [A-Z]{4,}\\b",
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{4,}"))
  ),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) {
    paste("KEY", x)
  }),
  # a key lead ending in dot leaders (and a couplet number)
  list(name = "keylead", re = "\\.{3,}\\s*(\\d+)?\\s*$", rank = "key", block = \(x, sp) {
    "KEY lead"
  }),
  # taxa printed only inside a key (scandent genera, cultivated species):
  # annotations, not part of the detailed treatment and not scored
  list(
    name = "annotation",
    re = "^((Toddalia|Luvunga|Paramignya) [A-Z]|[A-Z]\\. [a-z-]+ [A-Z(])",
    rank = "annotation",
    block = \(x, sp) {
      g <- c(C = "Citrus", G = "Glycosmis", M = "Murraya", Z = "Zanthoxylum")
      a <- str_extract(x, "^[A-Z]\\. [a-z-]+")
      if (is.na(a)) {
        word(x, 1)
      } else if (str_detect(a, "^C\\. lansium")) {
        "Clausena lansium"
      } else {
        paste(g[[str_sub(a, 1, 1)]], word(a, 2))
      }
    }
  ),
  list(
    name = "species",
    re = sp_re,
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", genus_alt, ") [a-z-]+")),
    sets_species = TRUE
  ),
  list(
    name = "infra",
    re = "^var\\. [a-z]+",
    rank = "variety",
    block = \(x, sp) paste(sp, str_extract(x, "^var\\. [a-z-]+"))
  )
)

desc_re <- "^(Aromatic|Small|Medium|Large|Erect|Scandent|Tree|Trees|Shrubs?)\\b"

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy|Uses|Vernacular names?)\\. ",
  desc_start_re = desc_re,
  skip_ranks = c("key", "annotation"),
  status_ranks = character(),
  # headings are short lines; a long line starting with a binomial is text
  cancel_header = \(l) l$hdr %in% c("species", "annotation") & nchar(l$t) > 70
)

# An unlabelled paragraph after a description (distribution and vernacular
# text in the in-key variety treatments) is not description: flora_segment()
# fills the section down, so relabel it here.
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

# The Glycosmis chlorosperma varieties are printed inside the key to
# varieties. The labelled Distribution paragraph after the last of them
# belongs to the species.
in_key_species <- "Glycosmis chlorosperma"
paras <- paras |>
  mutate(
    sp = str_extract(block, paste0("^", in_key_species, "(?= )")),
    move = !is.na(sp) &
      !section %in% c("Nomenclature", "Description", "Unlabelled note"),
    block = if_else(move, sp, block),
    rank = if_else(move, "species", rank)
  ) |>
  select(-sp, -move)
