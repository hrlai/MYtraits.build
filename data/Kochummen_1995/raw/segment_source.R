# Segment the Burseraceae treatment (K.M. Kochummen 1995, Tree Flora of Sabah
# and Sarawak vol. 1: 45--100). Run from the repository root.
#
# Source: inst/extdata/TFSS/Burseraceae.pdf, the 2007 electronic version of the
# 1995 volume (L.G. Saw & B. Aslina). Text pages carry embedded fonts and no
# page images, so the text layer is what renders; figure pages (pdf 22, 32,
# 39, 40, 44, 53, 58, 60) are images without a text layer. No scan of the 1995
# print was available to check the retyped text against.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

genera <- c(
  "Canarium",
  "Dacryodes",
  "Garuga",
  "Haplolobus",
  "Protium",
  "Santiria",
  "Scutinanthe",
  "Triomma"
)
gen_re <- paste(genera, collapse = "|")

lines <- flora_read_pdf_lines(
  "inst/extdata/TFSS/Burseraceae.pdf",
  first_line_re = "^BURSERACEAE$",
  last_pdf_page = 62,
  running_head_re = "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 1 \\(1995\\))?|BURSERACEAE \\(KOCHUMMEN\\))$",
  caption_re = "(?!x)x", # captions are inside the figure images
  side_caption_re = "(?!x)x"
) |>
  # pp. 45-74 use the minus sign for ranges, pp. 72-100 the en dash
  mutate(t = str_replace_all(t, "−", "–"))

# The electronic version repeats the last two lines of p. 62 (C.
# pseudodecumanum Distribution) at the top of p. 63. Drop a line at the top of
# a page that repeats one of the last lines of the previous page.
dup <- lines |>
  filter(!drop, t != "") |>
  group_by(pdf_page) |>
  mutate(k = row_number(), n = n()) |>
  ungroup()
tail_text <- dup |> filter(k > n - 4) |> select(pdf_page, t)
dup_lines <- dup |>
  filter(k <= 4) |>
  semi_join(
    tail_text |> mutate(pdf_page = pdf_page + 1L),
    by = c("pdf_page", "t")
  ) |>
  pull(line)
lines <- lines |> mutate(drop = drop | line %in% dup_lines)

sp_of <- \(b) str_remove(b, " (forma|subsp\\.|var\\.) [a-z-]+$")
rank_of <- \(x) {
  case_when(
    str_detect(x, "^forma ") ~ "forma",
    str_detect(x, "\\bvar\\. [a-z]+$|^var\\.") ~ "variety",
    TRUE ~ "subspecies"
  )
}
headers <- list(
  list(
    name = "family",
    re = "^BURSERACEAE$",
    rank = "family",
    block = \(x, sp) "Burseraceae"
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
  # the one infraspecific taxon of C. asperum in the flora region
  list(
    name = "infra_single",
    re = "^subsp\\. asperum var\\. asperum$",
    rank = "variety",
    block = \(x, sp) paste(sp, x)
  ),
  list(
    name = "species",
    # numbered, or one of the four unnumbered headings of genera with one
    # taxon in the region (synonym lines also start with a binomial)
    re = paste0(
      "^\\d{1,2}\\. (",
      gen_re,
      ") [a-z-]+ (\\(|[A-Z])|^(Garuga floribunda Decne|Protium connarifolium \\(Perkins\\)|",
      "Scutinanthe brunnea Thwaites|Triomma malaccensis Hook)"
    ),
    rank = \(x) if (str_detect(x, " var\\. ")) "variety" else "species",
    block = \(x, sp) {
      str_extract(x, paste0("(", gen_re, ") [a-z-]+( Decne\\. var\\. [a-z]+)?")) |>
        str_remove(" Decne\\.")
    },
    sets_species = TRUE
  ),
  # infraspecific annotations inside a "Key to forms / subspecies / varieties"
  list(
    name = "infra",
    re = "^(forma|subsp\\.|var\\.) [a-z-]+",
    rank = rank_of,
    block = \(x, sp) paste(sp, str_extract(x, "^(forma|subsp\\.|var\\.) [a-z-]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy|Uses|Timber|Vernacular names?)\\. ",
  desc_start_re = "^(Small|Medium|Big|Large|Very|Emergent|Deciduous|Dioecious|Tree|Trees|Treelet|Shrubs?)\\b",
  skip_ranks = c("key", "drop"),
  status_ranks = character(),
  # an infraspecific heading counts only when indented (inside a key)
  cancel_header = \(l) l$hdr %in% "infra" & l$indent < 3,
  post_blocks = \(l) {
    # Inside an annotation block, lines indented at least as much as the
    # first line after the heading are the annotation. A flush section label
    # ("Ecology.") resumes the species treatment; any other line is a key lead
    # and is dropped.
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
      if (
        !resume &&
          l$indent[i] == 0 &&
          str_detect(l$t[i], "^(Distribution|Ecology|Uses)\\. ")
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
# The only infraspecific taxon in the flora region: its description is the
# local description of the species (TFSS guide), kept at infraspecific rank.
single_infra <- c(
  "Canarium asperum subsp. asperum var. asperum",
  "Garuga floribunda var. floribunda"
)

# Labelled paragraphs keep their section; an unlabelled paragraph after a
# label is a note, not a continuation of Ecology or Distribution.
paras <- paras |>
  mutate(
    section = case_when(
      rank %in% c("forma", "subspecies", "variety") &
        section != "Description" &
        !block %in% single_infra ~
        "Infraspecific annotation",
      label_len == 0 &
        !section %in% c("Description", "Nomenclature", "Unlabelled note") ~
        "Note",
      TRUE ~ section
    )
  )
page_at <- flora_page_at
