# Source-text helpers for FPM / TFSS treatments: decide whether a PDF text layer
# can be trusted, render pages for hand transcription, read a transcription, and
# check a transcription against the PDF's text layer.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
# Run from the repository root. Sources live in inst/extdata/myflora/pdf/FPM and
# inst/extdata/myflora/pdf/TFSS.

suppressPackageStartupMessages(library(tidyverse))
# The project .Rprofile attaches traits.build (and so dplyr) before stats, so
# stats::filter and stats::lag would otherwise mask the dplyr verbs.
filter <- dplyr::filter
lag <- dplyr::lag

# `text_layer` comes from assets/myflora_metadata.csv (built by
# inst/extdata/myflora/build_metadata.py from page images and text): scanned_ocr
# means page scans carrying an OCR layer; NA means the PDF is not listed, so
# inspect a few page images. Producer metadata does not tell the two apart:
# Acrobat Pro DC wrote both. Most TFSS authors typed "x" for the multiplication
# sign, so n_x_times is not an OCR sign. bad_glyphs counts characters the layer
# could not map (U+FFFD, private use), common with unembedded fonts, plus the
# dot operator U+22C5 that a Symbol font gave for the multiplication sign
# (Adema_1996).
.flora_bad_glyph_re <- "[\ufffd\ue000-\uf8ff\u22c5]"

flora_pdf_check <- function(file) {
  info <- pdftools::pdf_info(file)
  txt <- pdftools::pdf_text(file)
  all <- paste(txt, collapse = "\n")
  meta <- read_csv(
    ".github/skills/extract-traits-from-floras-my/assets/myflora_metadata.csv",
    show_col_types = FALSE
  )
  key <- str_extract(normalizePath(file, "/"), "(FPM|TFSS)/[^/]+$")
  tibble(
    file = basename(file),
    text_layer = meta$text_layer[match(paste0("pdf/", key), meta$filename)],
    pages = info$pages,
    creator = info$keys$Creator %||% NA_character_,
    producer = info$keys$Producer %||% NA_character_,
    chars_per_page = round(mean(nchar(txt))),
    n_endash = str_count(all, "\u2013"),
    n_times = str_count(all, "\u00d7"),
    n_x_times = str_count(all, "\\d\\s?x\\s?\\d"),
    n_bad_glyphs = str_count(all, .flora_bad_glyph_re),
    # "(0.5}-1", "(--6)", "fig.I", "lJy"
    n_ocr_suspects = str_count(
      all,
      "\\d[}\\]]|\\(--\\d|\\bfig\\.[Il]|\\b[lI]J[a-z]\\b"
    )
  )
}

# Born-digital PDFs: the pages in `pages` worth an image look. Flags pages with
# little text (a plate, or text set as a picture), no stand-alone printed page
# number or an offset unlike the treatment's usual one, or unmapped glyphs.
# Returns flagged pages only; none means no image check is needed for these
# reasons.
flora_layer_flags <- function(file, pages = NULL, min_chars = 500) {
  txt <- pdftools::pdf_text(file)
  pages <- pages %||% seq_along(txt)
  d <- tibble(pdf_page = pages, text = txt[pages]) |>
    mutate(
      chars = nchar(str_remove_all(text, "\\s")),
      printed_page = map_int(text, \(t) {
        l <- str_squish(str_split_1(t, "\n"))
        n <- l[str_detect(l, "^\\d{1,3}$")]
        if (length(n)) as.integer(last(n)) else NA_integer_
      }),
      offset = printed_page - pdf_page,
      bad_glyphs = str_count(text, .flora_bad_glyph_re)
    )
  usual <- if (all(is.na(d$offset))) NA_integer_ else
    as.integer(names(which.max(table(d$offset))))
  d |>
    mutate(
      low_text = chars < min_chars,
      odd_page_number = coalesce(offset != usual, TRUE)
    ) |>
    filter(low_text | odd_page_number | bad_glyphs > 0) |>
    select(pdf_page, printed_page, chars, bad_glyphs, low_text, odd_page_number)
}

# Render pdf pages to PNG, each split into top and bottom halves (about one read
# per half page when transcribing). `outdir` must be outside the tracked tree
# (for example a scratch folder); nothing is deleted.
flora_render_pages <- function(file, pages, outdir, dpi = 220) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  full <- file.path(outdir, sprintf("p%02d_full.png", pages))
  # pdftools expands this template once for each requested PDF page.
  full_template <- file.path(outdir, "p%02d_full.%s")
  pdftools::pdf_convert(
    file,
    pages = pages,
    dpi = dpi,
    filenames = full_template,
    verbose = FALSE
  )
  for (f in full) {
    img <- magick::image_read(f)
    h <- magick::image_info(img)$height
    w <- magick::image_info(img)$width
    magick::image_write(
      magick::image_crop(
        img,
        magick::geometry_area(w, ceiling(h / 2) + 40, 0, 0)
      ),
      sub("_full", "_top", f)
    )
    magick::image_write(
      magick::image_crop(
        img,
        magick::geometry_area(w, ceiling(h / 2) + 40, 0, floor(h / 2) - 40)
      ),
      sub("_full", "_bottom", f)
    )
  }
  invisible(sub("_full", "_top", full))
}

# Transcription format (raw/<Family>_transcription.txt): "#" comment lines, one
# paragraph per line, blank lines kept, "@page <pdf page> <printed page>" at
# every page turn. Verbatim text with true en dashes and x signs. Correct
# image-checked headings and page markers here before segmentation; record
# source repairs in raw/source_fixes.csv and apply them to parsed text.
flora_read_transcription <- function(file) {
  tx <- read_lines(file)
  tx <- tx[!str_detect(tx, "^#")]
  is_page <- str_detect(tx, "^@page ")
  pg_id <- cumsum(is_page)
  pg_tab <- str_match(tx[is_page], "^@page (\\d+) (\\d+)$")
  tibble(
    pdf_page = as.integer(pg_tab[pg_id, 2]),
    printed_page = as.integer(pg_tab[pg_id, 3]),
    t = str_squish(tx)
  )[!is_page, ] |>
    mutate(line = row_number(), indent = 0L, drop = FALSE)
}

# Born-digital PDFs only. Returns one row per text line with pdf page, printed
# page (last stand-alone number on the page) and a `drop` flag for running
# heads, bare page numbers, map / figure captions (to the next blank line) and
# caption fragments printed beside maps. `drop_pages` are PDF page numbers to
# drop whole: figure plates whose only text is a running head (sometimes
# garbled) and stray characters.
flora_read_pdf_lines <- function(
  file,
  first_line_re,
  last_pdf_page = Inf,
  running_head_re = NULL,
  caption_re = "^(Map \\d+\\.|Figure \\d+\\.)",
  side_caption_re = "^(subsp|var)\\. [a-z]+.*\\.$|\\(\\)",
  side_indent = 30,
  drop_pages = NULL
) {
  pg <- pdftools::pdf_text(file)
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
  first <- which(str_detect(lines$t, first_line_re))[1]
  lines <- lines |> filter(line >= first, pdf_page <= last_pdf_page)
  caption <- str_detect(lines$t, caption_re)
  for (i in which(caption)) {
    j <- i + 1
    while (j <= nrow(lines) && lines$t[j] != "") {
      caption[j] <- TRUE
      j <- j + 1
    }
  }
  lines |>
    mutate(
      drop = caption |
        (!is.null(running_head_re) &
          str_detect(t, running_head_re %||% "(?!x)x")) |
        str_detect(t, "^\\d{1,3}$") |
        pdf_page %in% drop_pages |
        (indent > side_indent & str_detect(t, side_caption_re))
    ) |>
    select(-text)
}

# Compare measurements (default) or all numbers per PDF page in a transcription
# with the PDF text layer. `measurements_only = TRUE` ignores citations, page
# numbers and running heads, so it is appropriate for partial transcriptions.
# Use `FALSE` only when the transcription includes the complete page text.
# Read every difference from the page image; this is a review aid, not a pass/fail
# test. Returns only differing values or measurement strings.
flora_compare_numbers <- function(
  transcription_file,
  pdf_file,
  measurements_only = TRUE
) {
  tr <- flora_read_transcription(transcription_file) |>
    group_by(pdf_page) |>
    summarise(text = paste(t, collapse = " "), .groups = "drop")
  layer <- tibble(
    pdf_page = seq_along(pdftools::pdf_text(pdf_file)),
    text = pdftools::pdf_text(pdf_file)
  )
  values <- \(d, nm) {
    d |>
      filter(pdf_page %in% tr$pdf_page) |>
      mutate(
        value = if (measurements_only) {
          str_extract_all(
            text,
            "\\d+(?:\\.\\d+)?(?:\\s?[–-]\\s?\\d+(?:\\.\\d+)?)?(?:\\s?[×x]\\s?\\d+(?:\\.\\d+)?(?:\\s?[–-]\\s?\\d+(?:\\.\\d+)?)?)?\\s?(?:mm|cm|m)\\b"
          )
        } else {
          str_extract_all(text, "\\d+(?:\\.\\d+)?")
        }
      ) |>
      select(pdf_page, value) |>
      unnest(value) |>
      count(pdf_page, value, name = nm)
  }
  full_join(
    values(tr, "n_transcription"),
    values(layer, "n_layer"),
    by = c("pdf_page", "value")
  ) |>
    mutate(across(starts_with("n_"), \(x) coalesce(x, 0L))) |>
    filter(n_transcription != n_layer) |>
    arrange(pdf_page, value)
}
