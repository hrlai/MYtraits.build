# Coode_2018: segment the Flora of Peninsular Malaysia Elaeocarpaceae treatment
# (Coode 2018, FPM Series II vol. 7: 33--126) into taxon blocks and sections.
# Source: inst/extdata/FPM/Elaeocarpaceae.pdf. Born-digital text layer read with
# pdftools (no OCR). Produces `paras`: one row per paragraph with block (taxon or
# group), rank, status, section and per-line page lookups. Sourced by
# build_trait_data.R; run from the repository root.

library(tidyverse)

pdf_file <- "inst/extdata/FPM/Elaeocarpaceae.pdf"
pg <- pdftools::pdf_text(pdf_file)

lines <- map2_dfr(pg, seq_along(pg), \(t, i) {
  tibble(pdf_page = i, text = str_split_1(t, "\n"))
}) |>
  mutate(
    line = row_number(),
    indent = nchar(str_extract(text, "^\\s*")),
    t = str_squish(text)
  ) |>
  group_by(pdf_page) |>
  mutate(
    printed_page = suppressWarnings(as.integer(last(t[str_detect(
      t,
      "^\\d{1,3}$"
    )])))
  ) |>
  ungroup()

# Treatment runs from the family heading to the end of Sloanea sigun (pdf p. 100).
# pdf pp. 101--105 are plates; their captions add nothing to the descriptions.
first <- which(lines$t == "ELAEOCARPACEAE")[1]
lines <- lines |> filter(line >= first, pdf_page <= 100)

# Map and figure captions run until the next blank line.
caption <- str_detect(lines$t, "^(Map \\d+\\.|Figure \\d+\\.)")
for (i in which(caption)) {
  j <- i + 1
  while (j <= nrow(lines) && lines$t[j] != "") {
    caption[j] <- TRUE
    j <- j + 1
  }
}
lines <- lines |>
  mutate(
    drop = caption |
      str_detect(
        t,
        "^(SEED PLANTS VOL\\. 7 \\(2018\\)|ELAEOCARPACEAE \\(COODE\\))$"
      ) |
      str_detect(t, "^\\d{1,3}$") |
      # caption fragments printed beside maps, e.g. "subsp. cuspidatus () and E. kunstleri ()."
      (indent > 30 & str_detect(t, "^(subsp|var)\\. [a-z]+.*\\.$|\\(\\)"))
  )

hdr_re <- c(
  family = "^ELAEOCARPACEAE$",
  genus = "^\\d\\. (ELAEOCARPUS|SLOANEA) L\\.$",
  group = "^(Acronodia|Coilopetalum|Elaeocarpus|Ganitrus|Monocera|Polystachyus) Group$",
  key = "^Key to",
  species = "^\\d{1,2}\\. (Elaeocarpus|Sloanea) [a-z]+",
  infra = "^(\\d{1,2}[a-c]\\. )?(subsp|var)\\. [a-z]+",
  form = "^\\d[a-c]\\. E\\. angustifolius \u2013",
  formp = "^Ovalis form$",
  section = "^(Incompletely Known Species|Excluded Species)$",
  oddsp = "^Elaeocarpus (macrocarpus|euneurus|serratus) "
)
lines$hdr <- map_chr(lines$t, \(x) {
  h <- names(hdr_re)[str_detect(x, hdr_re)]
  if (length(h)) h[1] else NA_character_
})
lines <- lines |>
  mutate(hdr = if_else(drop | (hdr %in% "infra" & indent > 15), NA, hdr))

# Assign each line to a block. Blocks: family, genus, infrageneric group, key,
# species, infraspecific taxon, informal form, special section.
sp_now <- NA_character_
cur <- NA_character_
rk <- NA_character_
status_now <- "accepted"
block <- rank <- status <- character(nrow(lines))
for (i in seq_len(nrow(lines))) {
  h <- lines$hdr[i]
  x <- lines$t[i]
  if (!is.na(h)) {
    if (h == "infra" && rk == "key" && !str_detect(x, "^\\d{1,2}[a-c]\\. |Map \\d")) {
      # epithet wrapped onto its own line inside a key; not a heading
    } else if (h == "family") {
      cur <- "Elaeocarpaceae"
      rk <- "family"
    } else if (h == "genus") {
      cur <- str_to_title(str_extract(x, "[A-Z]{5,}"))
      rk <- "genus"
      status_now <- "accepted"
    } else if (h == "group") {
      cur <- paste("Elaeocarpus", x)
      rk <- "infrageneric_group"
    } else if (h == "key") {
      cur <- paste("KEY", x)
      rk <- "key"
    } else if (h == "species") {
      sp_now <- str_extract(x, "(Elaeocarpus|Sloanea) (sp\\. nov\\.|[a-z]+)")
      cur <- sp_now
      rk <- "species"
    } else if (h == "infra") {
      r <- str_extract(x, "subsp|var")
      cur <- paste0(
        sp_now,
        " ",
        r,
        ". ",
        str_match(x, "(subsp|var)\\. ([a-z]+)")[, 3]
      )
      rk <- if (r == "subsp") "subspecies" else "variety"
    } else if (h == "form") {
      cur <- paste0(
        sp_now,
        " (",
        str_extract(x, "(?<=\u2013 ).*?form|intermediate"),
        ")"
      )
      rk <- "informal_form"
    } else if (h == "formp") {
      cur <- "Elaeocarpus petiolatus (ovalis form)"
      rk <- "informal_form"
    } else if (h == "section") {
      status_now <- if (str_detect(x, "Excluded")) {
        "excluded"
      } else {
        "incompletely_known"
      }
      cur <- paste("SECTION", x)
      rk <- "section"
    } else if (h == "oddsp") {
      cur <- str_extract(x, "Elaeocarpus [a-z]+")
      rk <- "species"
    }
  }
  block[i] <- cur
  rank[i] <- rk
  status[i] <- if (
    rk %in% c("species", "subspecies", "variety", "informal_form", "section")
  ) {
    status_now
  } else {
    "accepted"
  }
}
lines <- lines |> mutate(block = block, rank = rank, status = status)
lines$status[str_detect(lines$block, "sp\\. nov")] <- "unnamed"

# Form labels ("Typical form", "Western form of subsp. apiculatus") head a
# description. Keep them as block-level context and replace the label line with
# a blank line, so the description starts a new paragraph.
form_label_re <- "^(Typical form|Western form of subsp\\. apiculatus)$"
form_context <- lines |>
  filter(str_detect(t, form_label_re)) |>
  distinct(block, form_context = t)
lines <- lines |> mutate(t = if_else(str_detect(t, form_label_re), "", t))

sec_re <- "^(Distribution|Conservation status|Ecology|Taxonomy|Notes?|Uses|Vernacular names?|Phenology)\\. "
desc_start_re <- "^(Small|Medium|Big|Large|Tree|Trees|Shrubs?|Treelet|Twigs?|Leaves|Leaf(?=:)|Flowers?|Petals|Sepals|Fruits?|Bark)\\b"

# Paragraph breaks: a section label; a line that starts a
# description right after a citation (often no blank line between); or a blank
# line after a sentence end (page turns inside a sentence don't break).
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
    # Lines ending "letter-" join without a space ("non-" + "resinous"): the
    # source has hard hyphens at line ends. Line offsets and pages are kept so
    # each clause can be traced to its own page.
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
# organ word -> Description (several may follow each other, e.g. group text);
# before that -> Nomenclature (etymology, citations, types); after a label ->
# continuation of that section.
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
