# Segment a FPM / TFSS family treatment into paragraphs tagged with block
# (taxon, group, family, genus), rank, status, section and page. This is the
# step that took most of the time in Coode_2018; the code was then copied to
# Lim_2010 with the heading patterns swapped out. Only the heading
# configuration differs between families, so it is a parameter here.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
#      paras <- flora_segment(lines, headers, ...)
# `lines` comes from flora_read_pdf_lines() (born-digital PDFs) or
# flora_read_transcription() (hand-transcribed pages) in flora_source.R.
# Run from the repository root. Tested to reproduce the `paras` tables of
# data/Coode_2018 and data/Lim_2010 exactly.

suppressPackageStartupMessages(library(tidyverse))
# The project .Rprofile attaches traits.build (and so dplyr) before stats, so
# stats::filter and stats::lag would otherwise mask the dplyr verbs.
filter <- dplyr::filter
lag <- dplyr::lag

# One heading spec per heading type, in priority order (first match wins):
#   list(
#     name   = "species",                      # label, free text
#     re     = "^\\d{1,2}\\. Elaeocarpus [a-z]+",
#     rank   = "species",                      # string, or function(x) -> string
#     block  = \(x, sp) str_extract(x, "Elaeocarpus [a-z]+"),  # block label
#     sets_species = TRUE,                     # block becomes `sp` for later infra taxa
#     status = NULL,                           # string/function(x): sets a persistent status
#     reset_status = FALSE                     # TRUE on genus headings
#   )
# `status_ranks` are the ranks that carry the persistent status ("Excluded
# Species" sections etc.); every other rank is "accepted".
flora_segment <- function(
  lines,
  headers,
  sec_re = "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|Vernacular names?|Phenology)\\. ",
  desc_start_re = "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark)\\b",
  form_label_re = NULL,
  skip_ranks = c("key", "section"),
  status_ranks = c(
    "species",
    "subspecies",
    "variety",
    "informal_form",
    "section"
  ),
  cancel_header = NULL,
  ignore_header = NULL,
  post_blocks = NULL
) {
  hdr_re <- map_chr(headers, "re")
  lines$hdr <- map_chr(lines$t, \(x) {
    h <- which(str_detect(x, hdr_re))
    if (length(h)) headers[[h[1]]]$name else NA_character_
  })
  lines <- lines |> mutate(hdr = if_else(drop, NA, hdr))
  if (!is.null(cancel_header)) {
    lines <- lines |> mutate(hdr = if_else(cancel_header(lines), NA, hdr))
  }
  spec <- set_names(headers, map_chr(headers, "name"))

  sp_now <- NA_character_
  cur <- NA_character_
  rk <- NA_character_
  status_now <- "accepted"
  block <- rank <- status <- character(nrow(lines))
  for (i in seq_len(nrow(lines))) {
    h <- lines$hdr[i]
    x <- lines$t[i]
    skip <- !is.na(h) && !is.null(ignore_header) && ignore_header(h, x, rk)
    if (!is.na(h) && !skip) {
      s <- spec[[h]]
      cur <- s$block(x, sp_now)
      rk <- if (is.function(s$rank)) s$rank(x) else s$rank
      if (isTRUE(s$sets_species)) {
        sp_now <- cur
      }
      if (isTRUE(s$reset_status)) {
        status_now <- "accepted"
      }
      if (!is.null(s$status)) {
        status_now <- if (is.function(s$status)) s$status(x) else s$status
      }
    }
    block[i] <- cur
    rank[i] <- rk
    status[i] <- if (rk %in% status_ranks) status_now else "accepted"
  }
  lines <- lines |> mutate(block = block, rank = rank, status = status)
  if (!is.null(post_blocks)) {
    lines <- post_blocks(lines)
  }

  # Form labels ("Typical form") head a description. Keep them as block-level
  # context and blank the label line so the description starts a new paragraph.
  form_context <- tibble(block = character(), form_context = character())
  if (!is.null(form_label_re)) {
    form_context <- lines |>
      filter(str_detect(t, form_label_re)) |>
      distinct(block, form_context = t)
    lines <- lines |> mutate(t = if_else(str_detect(t, form_label_re), "", t))
  }

  # Paragraph breaks: a section label; a line that starts a description right
  # after a citation; or a blank line after a sentence end (a page turn inside a
  # sentence does not break).
  paras <- lines |>
    filter(!drop, is.na(hdr), !rank %in% skip_ranks) |>
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
      # Line offsets and pages are kept so each clause traces to its own page.
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

  # Unlabelled paragraphs: before any section label and starting with a habit
  # or organ word -> Description; before that -> Nomenclature (author,
  # etymology, citations, types); after a label -> continuation of that section.
  paras |>
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
}

# Pages (pdf, printed) at a character position within one paragraph row, so a
# clause can be traced to its own page when a paragraph spans a page turn.
flora_page_at <- function(par_row, pos) {
  k <- max(which(par_row$line_start[[1]] <= pos + par_row$label_len))
  c(
    pdf = par_row$line_pdf_page[[1]][k],
    printed = par_row$line_printed_page[[1]][k]
  )
}
