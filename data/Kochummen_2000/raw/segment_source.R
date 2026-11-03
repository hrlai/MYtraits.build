# Segment the Moraceae treatment (K.M. Kochummen & Rusea Go 2000, Tree Flora
# of Sabah and Sarawak vol. 3: 181--334). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Moraceae.pdf, the 2007 electronic
# version of the 2000 volume (L.G. Saw & B. Aslina), born-digital text. PDF
# pages 7--160 are printed pages 181--334 (printed = pdf + 174). The 16 figure
# plates inside the treatment are images with no text layer beyond the running
# head and a stray "vi".
#
# Taxa printed only inside a key (the genera Trophis and Maclura, the
# cultivated Artocarpus altilis and A. heterophyllus, and the c. 74 Ficus
# species and varieties annotated in the key to Ficus species) stay inside
# their key block and are not segmented (TFSS guide: keys are not trait
# evidence); they are listed in raw/taxon_flags.csv.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/Moraceae.pdf"
genera <- c("Antiaris", "Artocarpus", "Ficus", "Parartocarpus", "Prainea", "Streblus")
gen_re <- paste(genera, collapse = "|")
plate_pages <- c(12, 19, 21, 32, 35, 91, 93, 97, 101, 105, 129, 141, 148, 150, 154, 158)

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^MORACEAE$",
  last_pdf_page = 160,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 3 \\(2000\\))?|",
    "MORACEAE \\(KOCHUMMEN & GO\\)|vi)$"
  ),
  # captions are inside the figure images
  caption_re = "^Current address: ",
  side_caption_re = "(?!x)x",
  drop_pages = plate_pages
) |>
  mutate(
    # minus signs and U+23AF for range dashes on some pages
    t = str_replace_all(t, "[−⎯]", "–"),
    # section label misprinted in A. excelsus (p. 198)
    t = str_replace(t, "^Disribution\\. ", "Distribution. ")
  )

sp_of <- \(b) str_remove(b, " (subsp\\.|var\\.) \\S+$")
infra_label <- "^(subsp\\.|var\\.) ([a-z-]+|‘[A-Z]’)"
# The one infraspecific taxon of a species in the flora region, printed after
# a species heading with no description: its description is the local
# description of the species (TFSS guide).
single_infra_re <- paste0(
  "^(subsp\\. brevipedunculatus Jarrett|var\\. elongata \\(King\\) Barrett|",
  "var\\. lanceata Corner|subsp\\. gibbosa \\(Blume\\) Corner|",
  "var\\. glabella \\(Blume\\) Corner)$"
)
headers <- list(
  list(
    name = "family",
    re = "^MORACEAE$",
    rank = "family",
    block = \(x, sp) "Moraceae"
  ),
  list(
    name = "genus",
    re = paste0("^\\d\\. (", str_to_upper(gen_re), ") [A-Z]"),
    rank = "genus",
    block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}"))
  ),
  list(name = "key", re = "^(Key|Forest key) to ", rank = "key", block = \(x, sp) {
    paste("KEY", x, sp)
  }),
  # Artocarpus sp. 'A' (pdf 38) is not scored
  list(
    name = "incomplete",
    re = "^Incompletely known species$",
    rank = "section",
    block = \(x, sp) "Incompletely known species"
  ),
  list(
    name = "infra_single",
    re = single_infra_re,
    rank = \(x) if (str_detect(x, "^var\\.")) "variety" else "subspecies",
    block = \(x, sp) paste(sp, str_extract(x, infra_label))
  ),
  list(
    name = "species",
    # numbered, or Antiaris toxicaria, the one species of its genus
    re = paste0("^(\\d{1,2}\\. (", gen_re, ") [a-z-]+ [A-Z(]|Antiaris toxicaria Lesch\\.)"),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  # infraspecific annotations inside a "Key to varieties / subspecies"
  list(
    name = "infra",
    re = infra_label,
    rank = \(x) if (str_detect(x, "^var\\.")) "variety" else "subspecies",
    block = \(x, sp) paste(sp, str_extract(x, infra_label))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy|Notes|Uses?|Vernacular names?)\\. ",
  desc_start_re = paste0(
    "^(Monoecious|Dioecious|Deciduous|Small|Medium-sized|Tree|Trees|Treelets?|",
    "Shrubs?|Syconia|Inflorescences)\\b"
  ),
  skip_ranks = c("key", "section", "drop"),
  status_ranks = character(),
  # an annotation heading counts only when indented (inside a key); flush
  # "var. uncinata King l.c. ..." is a synonym line, flush var. terminalifolia
  # (F. cumingii) a short note after the species description. Variety
  # annotations inside the key to Ficus species are not headings either.
  cancel_header = \(l) {
    last_hdr <- vctrs::vec_fill_missing(
      if_else(is.na(l$hdr) | l$hdr == "infra", NA_character_, l$t),
      direction = "down"
    )
    l$hdr %in% "infra" &
      (l$indent < 3 | !str_detect(coalesce(last_hdr, ""), "^Key to (varieties|subspecies)"))
  },
  post_blocks = \(l) {
    # As in data/Kochummen_1995: inside an annotation block, lines indented at
    # least as much as the first line after the heading are the annotation; a
    # flush section label resumes the species treatment; any other line is a
    # key lead and is dropped. Indents shift by a column or two between
    # pages (F. oleifolia var. valida, pdf 122), hence the tolerance.
    head_ind <- NA_integer_
    resume <- FALSE
    for (i in seq_len(nrow(l))) {
      if (!is.na(l$hdr[i])) {
        head_ind <- if (l$hdr[i] == "infra") -1L else NA_integer_
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
      if (!resume && l$indent[i] == 0 && str_detect(l$t[i], "^(Distribution|Ecology|Uses?|Notes)\\. ")) {
        resume <- TRUE
      }
      if (head_ind < 0) {
        head_ind <- l$indent[i]
      }
      if (resume) {
        l$block[i] <- sp_of(l$block[i])
        l$rank[i] <- "species"
      } else if (l$indent[i] < head_ind - 2) {
        l$rank[i] <- "drop"
      }
    }
    l
  }
)
single_infra <- c(
  "Artocarpus melinoxylus subsp. brevipedunculatus",
  "Ficus racemosa var. elongata",
  "Ficus scortechinii var. lanceata",
  "Ficus tinctoria subsp. gibbosa",
  "Ficus virens var. glabella"
)

# Labelled paragraphs keep their section; an unlabelled paragraph after a
# label is a note, not a continuation of Ecology or Distribution.
paras <- paras |>
  mutate(
    section = case_when(
      # annotations in a key to varieties (kept unmapped, as in
      # data/Kochummen_1995), including their short descriptions
      rank %in% c("subspecies", "variety") & !block %in% single_infra ~
        "Infraspecific annotation",
      label_len == 0 &
        !section %in% c("Description", "Nomenclature", "Unlabelled note") ~
        "Note",
      TRUE ~ section
    )
  )
page_at <- flora_page_at
