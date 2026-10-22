# Lim_2010: segment the Flora of Peninsular Malaysia Araucariaceae treatment
# (C.L. Lim 2010, FPM Series II vol. 1: 61--66) into taxon blocks and sections.
# Source: raw/Araucariaceae_transcription.txt, transcribed from page images of
# inst/extdata/FPM/Araucariaceae.pdf. The PDF's OCR text layer is not used
# (unknown OCR engine). One paragraph per line; "@page <pdf> <printed>" lines
# mark page turns. Adapted from data/Coode_2018/raw/segment_source.R. Produces
# `paras`: one row per paragraph with block (taxon), rank, status, section and
# per-line page lookups. Sourced by build_trait_data.R; run from the repository
# root.

library(tidyverse)

tx <- read_lines("data/Lim_2010/raw/Araucariaceae_transcription.txt")
tx <- tx[!str_detect(tx, "^#")]
is_page <- str_detect(tx, "^@page ")
pg_id <- cumsum(is_page)
pg_tab <- str_match(tx[is_page], "^@page (\\d+) (\\d+)$")
lines <- tibble(
  pdf_page = as.integer(pg_tab[pg_id, 2]),
  printed_page = as.integer(pg_tab[pg_id, 3]),
  t = str_squish(tx)
)[!is_page, ] |>
  mutate(line = row_number(), indent = 0L, drop = FALSE)

no_match <- "(?!x)x"
hdr_re <- c(
  family = "^ARAUCARIACEAE$",
  genus = "^\\d\\. AGATHIS ",
  group = no_match,
  key = "^Key to",
  species = "^\\d{1,2}\\. Agathis [a-z]+",
  infra = no_match,
  form = no_match,
  formp = no_match,
  section = no_match,
  oddsp = no_match
)
lines$hdr <- map_chr(lines$t, \(x) {
  h <- names(hdr_re)[str_detect(x, hdr_re)]
  if (length(h)) h[1] else NA_character_
})
lines <- lines |> mutate(hdr = if_else(drop, NA, hdr))

# Assign each line to a block: family, genus, key, species.
cur <- NA_character_
rk <- NA_character_
block <- rank <- character(nrow(lines))
for (i in seq_len(nrow(lines))) {
  h <- lines$hdr[i]
  x <- lines$t[i]
  if (!is.na(h)) {
    if (h == "family") {
      cur <- "Araucariaceae"
      rk <- "family"
    } else if (h == "genus") {
      cur <- str_to_title(str_extract(x, "[A-Z]{5,}"))
      rk <- "genus"
    } else if (h == "key") {
      cur <- paste("KEY", x)
      rk <- "key"
    } else if (h == "species") {
      cur <- str_extract(x, "Agathis [a-z]+")
      rk <- "species"
    }
  }
  block[i] <- cur
  rank[i] <- rk
}
lines <- lines |> mutate(block = block, rank = rank, status = "accepted")

form_context <- tibble(block = character(), form_context = character())

sec_re <- "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|Vernacular names?|Phenology)\\. "
desc_start_re <- "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark|Bole)\\b"

# Paragraph breaks: a section label; a line that starts a description right
# after a citation; or a blank line after a sentence end (page turns inside a
# sentence don't break).
paras <- lines |>
  filter(!drop, is.na(hdr), !rank %in% c("key", "section")) |>
  group_by(block) |>
  mutate(
    prev_text = zoo::na.locf(
      lag(if_else(t == "", NA_character_, t)),
      na.rm = FALSE
    ),
    after_blank = lag(t, default = "") == "",
    desc_line = str_detect(t, desc_start_re) &
      coalesce(str_detect(prev_text, "\\.$|\\)\\.?$|^\\(.*\\)$|\\d$"), TRUE),
    new = row_number() == 1 |
      str_detect(t, sec_re) |
      desc_line |
      (after_blank & coalesce(str_detect(prev_text, "[.)]$"), TRUE))
  ) |>
  filter(t != "") |>
  mutate(par = cumsum(new)) |>
  group_by(block, rank, status, par) |>
  summarise(
    # Lines ending "letter-" join without a space (hard hyphens at line ends).
    sep = list(c(if_else(str_detect(head(t, -1), "[a-z]-$"), "", " "), "")),
    text = paste0(t, unlist(sep), collapse = ""),
    line_start = list(cumsum(c(1, head(nchar(t) + nchar(unlist(sep)), -1)))),
    line_pdf_page = list(pdf_page),
    line_printed_page = list(printed_page),
    .groups = "drop"
  ) |>
  select(-sep) |>
  mutate(
    section = str_match(text, sec_re)[, 2],
    label_len = coalesce(nchar(str_extract(text, sec_re)), 0L),
    text = str_remove(text, sec_re),
    pdf_page = map_int(line_pdf_page, 1),
    printed_page = map_int(line_printed_page, 1)
  )

# Unlabelled paragraphs: before any section label and starting with a habit or
# organ word -> Description; before that -> Nomenclature (author, etymology,
# citations, types); after a label -> continuation of that section.
paras <- paras |>
  group_by(block) |>
  mutate(
    seen_label = cumsum(!is.na(section)) > 0,
    in_desc = is.na(section) &
      !seen_label &
      cumsum(is.na(section) & str_detect(text, desc_start_re)) > 0 &
      (str_detect(text, desc_start_re) |
        !str_detect(lag(text, default = ""), "\\.$")),
    section = case_when(
      !is.na(section) ~ section,
      in_desc ~ "Description",
      !seen_label &
        cumsum(is.na(section) & str_detect(text, desc_start_re)) ==
          0 ~ "Nomenclature",
      !seen_label ~ "Unlabelled note",
      TRUE ~ NA_character_
    )
  ) |>
  fill(section) |>
  ungroup() |>
  select(-seen_label, -in_desc) |>
  left_join(form_context, by = "block")

# Pages (pdf, printed) at a character position within one paragraph row.
page_at <- function(par_row, pos) {
  k <- max(which(par_row$line_start[[1]] <= pos + par_row$label_len))
  c(
    pdf = par_row$line_pdf_page[[1]][k],
    printed = par_row$line_printed_page[[1]][k]
  )
}
