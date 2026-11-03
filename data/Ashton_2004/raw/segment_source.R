# Segment the Dipterocarpaceae treatment (P.S. Ashton 2004, Tree Flora of
# Sabah and Sarawak vol. 5: 63--388). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Dipterocarpaceae.pdf, the 2008
# electronic version of the 2004 volume (L.G. Saw & B. Aslina), born-digital
# text. PDF pages 7--332 are printed pages 63--388 (printed = pdf + 56); PDF
# 333--337 are the colour plates (printed 493--498), not read. Figure pages
# carry only a caption ("Fig. 19. ...") under the image; captions are dropped
# here and read by hand for vouchers. Hyphens (U+00AD) and semicolons (U+037E)
# are read as the printed characters by flora_read_pdf_lines().
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/Dipterocarpaceae.pdf"
genera <- c(
  "Anisoptera", "Cotylelobium", "Dipterocarpus", "Dryobalanops", "Hopea",
  "Parashorea", "Shorea", "Upuna", "Vatica"
)
gen_re <- paste(genera, collapse = "|")

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^DIPTEROCARPACEAE$",
  last_pdf_page = 332,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 5 \\(2004\\))?|",
    "DIPTEROCARPACEAE \\(ASHTON\\))$"
  ),
  caption_re = "^Fig\\. \\d+\\. ",
  side_caption_re = "(?!x)x",
  soft_hyphen = "-"
) |>
  mutate(
    # footnote to the family introduction (p. 65)
    drop = drop | str_detect(t, "^\\*Brunig, E\\.F\\. 1966|^Vegetation und Ökologie")
  )

sp_of <- \(b) str_remove(b, " subsp\\. [a-z-]+$")
headers <- list(
  list(
    name = "family",
    re = "^DIPTEROCARPACEAE$",
    rank = "family",
    block = \(x, sp) "Dipterocarpaceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d\\. (", str_to_upper(gen_re), ") [A-Z]"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^Key to ", rank = "key", block = \(x, sp) {
    paste("KEY", x, sp)
  }),
  list(
    name = "species",
    # Upuna borneensis, the one species of its genus, is unnumbered
    re = paste0(
      "^\\d{1,3}\\. (", gen_re, ") [a-z-]+ (\\(|[A-Z])|",
      "^Upuna borneensis Symington"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  # a whole line naming a subspecies: flush, the one subspecies in the region
  # (with its own description); indented, an annotation in a key
  list(
    name = "infra",
    re = "^subsp\\. [a-z-]+( \\([A-Z][^)]*\\))?( [A-Z][A-Za-z.]*)*$",
    rank = "subspecies",
    block = \(x, sp) paste(sp, str_extract(x, "^subsp\\. [a-z-]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = paste0(
    "^(Distribution|Ecology|Uses?|Notes?|Vernacular names?|Classification|",
    "Phytogeography|Conservation status)\\. "
  ),
  desc_start_re = paste0(
    "^(Large|Emergent|Main|Canopy|Subcanopy|Suncanopy|Small|Low|Vast|Huge|",
    "Tall|Magnificent|Immense|Very|Understor(e)?y|Stout|Stocky|Stately|",
    "Noble|Medium|Tree|Mostly)\\b"
  ),
  skip_ranks = c("key", "drop"),
  status_ranks = character(),
  post_blocks = \(l) {
    # A key annotation: lines indented at least as much as the first line
    # after the subspecies heading. A flush section label resumes the species
    # treatment; any other line is a key lead and is dropped. A flush heading
    # (single subspecies) starts an ordinary treatment block.
    head_ind <- NA_integer_
    resume <- FALSE
    for (i in seq_len(nrow(l))) {
      if (!is.na(l$hdr[i])) {
        head_ind <- if (l$hdr[i] == "infra" && l$indent[i] >= 3) -1L else NA_integer_
        resume <- FALSE
        next
      }
      if (is.na(head_ind) || l$t[i] == "" || l$drop[i]) {
        if (resume) {
          l$block[i] <- sp_of(l$block[i])
          l$rank[i] <- "species"
        }
        next
      }
      if (
        !resume &&
          l$indent[i] == 0 &&
          str_detect(l$t[i], "^(Distribution|Ecology|Uses|Notes)\\. ")
      ) {
        resume <- TRUE
      }
      if (head_ind < 0) {
        head_ind <- l$indent[i]
      }
      if (resume) {
        l$block[i] <- sp_of(l$block[i])
        l$rank[i] <- "species"
      } else if (l$indent[i] < head_ind) {
        l$rank[i] <- "drop"
      }
    }
    l
  }
)

# Flush subspecies headings: the only subspecies of the species in Sabah and
# Sarawak; its description is the local description (TFSS guide).
single_infra <- paras |>
  filter(rank == "subspecies", section == "Description") |>
  distinct(block) |>
  pull(block)

# Labelled paragraphs keep their section; an unlabelled paragraph after a
# label is a note, not a continuation of Ecology or Distribution.
# A description that continues after a figure page or page turn starts a new
# paragraph with an organ word (Dipterocarpus confertus p. 99, Hopea
# altocollina p. 148, Parashorea smythiesii p. 189, Shorea monticola p. 294).
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
      rank == "subspecies" & !block %in% single_infra ~
        "Infraspecific annotation",
      label_len == 0 &
        !section %in% c("Description", "Nomenclature", "Unlabelled note") ~
        "Note",
      TRUE ~ section
    )
  )
page_at <- flora_page_at
