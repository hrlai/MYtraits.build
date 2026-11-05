# Segment the Leguminosae-Caesalpinioideae treatment (Ding Hou 2000, Tree
# Flora of Sabah and Sarawak vol. 3: 119--180). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/LeguminosaeCaesalpinioideae.pdf, the
# 2007 electronic version of the 2000 volume (L.G. Saw & B. Aslina),
# born-digital text. PDF pages 7--68 are printed pages 119--180 (printed =
# pdf + 112). The 15 figure plates (pdf 13, 15, 18, 23, 31, 36, 39, 43, 45, 49,
# 52, 62, 65, 67, 69) are images whose only text is the running head and a
# stray "vi".
#
# Bauhinia, Caesalpinia and Pterolobium are described only inside the key to
# genera; keys are not read (key-only genera, raw/taxon_flags.csv). The
# varieties of Dialium indum and D. kunstleri are annotations in a key to
# varieties (citations, distribution and ecology only).
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/LeguminosaeCaesalpinioideae.pdf"
genera <- c(
  "Afzelia", "Cassia", "Copaifera", "Crudia", "Cynometra", "Dialium",
  "Intsia", "Koompassia", "Leucostegane", "Peltophorum", "Saraca", "Sindora",
  "Sympetalandra", "Uittienia"
)
gen_re <- paste(genera, collapse = "|")
plate_pages <- c(13, 15, 18, 23, 31, 36, 39, 43, 45, 49, 52, 62, 65, 67, 69)
# the one species of a genus in the region is unnumbered
unnumbered <- c(
  "Afzelia rhomboidea", "Cassia javanica", "Copaifera palustris",
  "Leucostegane grandis", "Uittienia modesta"
)

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^LEGUMINOSAE$",
  last_pdf_page = 68,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 3 \\(2000\\))?|",
    "LEGUMINOSAE-CAESALPINIOIDEAE \\(DING HOU\\))$"
  ),
  # captions are inside the figure images
  caption_re = "(?!x)x",
  side_caption_re = "(?!x)x",
  drop_pages = plate_pages
) |>
  # subfamily line and the author's address under the family heading
  mutate(drop = drop | str_detect(t, "^\\(Subfamily CAESALPINIOIDEAE\\)$"))

headers <- list(
  list(
    name = "family",
    re = "^LEGUMINOSAE$",
    rank = "family",
    block = \(x, sp) "Caesalpinioideae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d{1,2}\\. (", str_to_upper(gen_re), ") [A-Z(]"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x, sp)
  }),
  list(
    name = "species",
    # authors may start lower case ("de Wit")
    re = paste0(
      "^\\d{1,2}\\. (", gen_re, ") [a-z-]+ ([A-Z(]|de )|",
      "^(", paste(unnumbered, collapse = "|"), ") [A-Z(]"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  # a whole line naming an infraspecific taxon: flush, the one infraspecific
  # taxon in the region (with its own description); indented, an annotation
  # in a key to varieties
  list(
    name = "infra",
    re = "^(subsp|var)\\. [a-z-]+( \\(|( [A-Z][A-Za-z.&-]*)+( Fig\\. .*)?$|$)",
    rank = \(x) if (str_detect(x, "^subsp")) "subspecies" else "variety",
    block = \(x, sp) paste(sp, str_extract(x, "^(subsp|var)\\. [a-z-]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Uses?|Vernacular names?)\\. ",
  desc_start_re = "^(Trees?|Giant tree|Deciduous tree|Shrubs)\\b",
  skip_ranks = c("key", "drop"),
  status_ranks = character(),
  post_blocks = \(l) {
    # A key annotation (indented heading): its flush lines and dot-leader lines
    # are key leads, not annotation text.
    annot <- FALSE
    for (i in seq_len(nrow(l))) {
      if (!is.na(l$hdr[i])) {
        annot <- l$hdr[i] == "infra" && l$indent[i] >= 3
        next
      }
      if (annot && l$t[i] != "" &&
        (l$indent[i] == 0 || str_detect(l$t[i], "…|\\.{4,}"))) {
        l$rank[i] <- "drop"
      }
    }
    l
  }
)

# Flush infraspecific headings: the only infraspecific taxon of the species in
# Sabah and Sarawak; its description is the local description (TFSS guide).
single_infra <- paras |>
  filter(rank %in% c("subspecies", "variety"), section == "Description") |>
  distinct(block) |>
  pull(block)

paras <- paras |>
  mutate(
    section = case_when(
      rank %in% c("subspecies", "variety") & !block %in% single_infra ~
        "Infraspecific annotation",
      label_len == 0 &
        !section %in% c("Description", "Nomenclature", "Unlabelled note") ~
        "Note",
      TRUE ~ section
    )
  )
page_at <- flora_page_at
