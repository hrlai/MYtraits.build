# Segment the Chrysobalanaceae treatment (Ghillean T. Prance 1995, Tree Flora
# of Sabah and Sarawak vol. 1: 155--179). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Chrysobalanaceae.pdf, the 2007
# electronic version of the 1995 volume (L.G. Saw & B. Aslina), born-digital
# text. PDF pages 7--31 are printed pages 155--179 (printed = pdf + 148). The
# six figure plates (pdf 11, 14, 16, 18, 22, 28) are images whose only text is
# the printed page number; flora_layer_flags() lists them as low_text.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

pdf_file <- "inst/extdata/myflora/pdf/TFSS/Chrysobalanaceae.pdf"
genera <- c(
  "Atuna", "Kostermanthus", "Licania", "Maranthes", "Parastemon", "Parinari"
)
gen_re <- paste(genera, collapse = "|")
plate_pages <- c(11, 14, 16, 18, 22, 28)

lines <- flora_read_pdf_lines(
  pdf_file,
  first_line_re = "^CHRYSOBALANACEAE$",
  last_pdf_page = 31,
  running_head_re = paste0(
    "^(TREE FLORA OF SABAH AND SARAWAK( VOL\\. 1 \\(1995\\))?|",
    "CHRYSOBALANACEAE \\(PRANCE\\))$"
  ),
  # captions are inside the figure images
  caption_re = "(?!x)x",
  side_caption_re = "(?!x)x",
  drop_pages = plate_pages
) |>
  # Parastemon pages use the minus sign for ranges and in compound words
  # ("elliptic−oblong"), and "⎯" as the dash in vernacular names
  mutate(
    t = str_replace_all(t, "(?<=[a-z])−(?=[a-z])", "-") |>
      str_replace_all("−", "–") |>
      str_replace_all("⎯", "—")
  )

sp_of <- \(b) str_remove(b, " subsp\\. [a-z-]+$")
headers <- list(
  list(
    name = "family",
    re = "^CHRYSOBALANACEAE$",
    rank = "family",
    block = \(x, sp) "Chrysobalanaceae"
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
    # numbered, or one of the three unnumbered headings of genera with one
    # species in the region
    re = paste0(
      "^\\d\\. (", gen_re, ") [a-z-]+ (\\(|[A-Z])|",
      "^(Kostermanthus heteropetalus|Licania splendens|Maranthes corymbosa) [(A-Z]"
    ),
    rank = "species",
    block = \(x, sp) str_extract(x, paste0("(", gen_re, ") [a-z-]+")),
    sets_species = TRUE
  ),
  # subspecies annotations inside a "Key to subspecies" (indented)
  list(
    name = "infra",
    re = "^subsp\\. [a-z-]+",
    rank = "subspecies",
    block = \(x, sp) paste(sp, str_extract(x, "^subsp\\. [a-z-]+"))
  )
)

paras <- flora_segment(
  lines,
  headers,
  sec_re = "^(Distribution|Ecology|Taxonomy|Uses|Vernacular names?)\\. ",
  desc_start_re = "^(Small|Medium-sized|Large|Trees?)\\b",
  skip_ranks = c("key", "drop"),
  status_ranks = character(),
  cancel_header = \(l) l$hdr %in% "infra" & l$indent < 3,
  post_blocks = \(l) {
    # Inside a subspecies block, indented lines are its annotation (synonyms,
    # range, habitat; one runs over a page turn at a smaller indent). A flush
    # section label ("Uses.", "Distribution.") resumes the species treatment;
    # any other flush line is a key lead and is dropped.
    in_infra <- FALSE
    resume <- FALSE
    for (i in seq_len(nrow(l))) {
      if (!is.na(l$hdr[i])) {
        in_infra <- l$hdr[i] == "infra"
        resume <- FALSE
        next
      }
      if (!in_infra || l$t[i] == "" || l$drop[i]) {
        if (in_infra && resume) {
          l$block[i] <- sp_of(l$block[i])
          l$rank[i] <- "species"
        }
        next
      }
      if (!resume && l$indent[i] == 0 &&
        str_detect(l$t[i], "^(Distribution|Ecology|Uses|Taxonomy)\\. ")) {
        resume <- TRUE
      }
      if (resume) {
        l$block[i] <- sp_of(l$block[i])
        l$rank[i] <- "species"
      } else if (l$indent[i] == 0) {
        l$rank[i] <- "drop"
      }
    }
    l
  }
)

paras <- paras |>
  mutate(
    section = if_else(
      rank == "subspecies", "Infraspecific annotation", section
    )
  )
page_at <- flora_page_at
