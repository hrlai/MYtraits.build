# Source-text helpers for FPM / TFSS treatments: decide whether a PDF text layer
# can be trusted, render pages for hand transcription, read a transcription, and
# check a transcription against the PDF's text layer.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
# Run from the repository root. Sources live in inst/extdata/FPM and
# inst/extdata/TFSS.

suppressPackageStartupMessages(library(tidyverse))

# Advisory only: producer metadata plus counts of typical OCR slips. In this
# project Elaeocarpaceae (InDesign) was born-digital; the FPM vol. 1 PDFs
# (Araucariaceae, Pentaphylacaceae) were re-saved in Acrobat Pro DC with an OCR
# layer of unknown origin. Nothing here proves a layer is clean: a TFSS PDF
# must be checked by eye before it is parsed.
flora_pdf_check <- function(file) {
  info <- pdftools::pdf_info(file)
  txt <- pdftools::pdf_text(file)
  all <- paste(txt, collapse = "\n")
  tibble(
    file = basename(file),
    pages = info$pages,
    creator = info$keys$Creator %||% NA_character_,
    producer = info$keys$Producer %||% NA_character_,
    chars_per_page = round(mean(nchar(txt))),
    n_endash = str_count(all, "\u2013"),
    n_times = str_count(all, "\u00d7"),
    # "x" for the multiplication sign, "(0.5}-1", "(--6)", "fig.I", "lJy"
    n_ocr_suspects = str_count(
      all,
      "\\d\\s?x\\s?\\d|\\d[}\\]]|\\(--\\d|\\bfig\\.[Il]|\\b[lI]J[a-z]\\b"
    ),
    layer_suspect = !str_detect(
      coalesce(info$keys$Creator, ""),
      "InDesign|Word|LaTeX"
    )
  )
}

# Render pdf pages to PNG, each split into top and bottom halves (about one read
# per half page when transcribing). `outdir` must be outside the tracked tree
# (for example a scratch folder); nothing is deleted.
flora_render_pages <- function(file, pages, outdir, dpi = 220) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  full <- file.path(outdir, sprintf("p%02d_full.png", pages))
  pdftools::pdf_convert(
    file,
    pages = pages,
    dpi = dpi,
    filenames = full,
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
# every page turn. Verbatim text with true en dashes and x signs. Printed slips
# stay as printed; repairs go in raw/source_fixes.csv.
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
# caption fragments printed beside maps.
flora_read_pdf_lines <- function(
  file,
  first_line_re,
  last_pdf_page = Inf,
  running_head_re = NULL,
  caption_re = "^(Map \\d+\\.|Figure \\d+\\.)",
  side_caption_re = "^(subsp|var)\\. [a-z]+.*\\.$|\\(\\)",
  side_indent = 30
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
        (indent > side_indent & str_detect(t, side_caption_re))
    ) |>
    select(-text)
}

# Compare the multiset of numbers per pdf page in a transcription with the PDF
# text layer. Differences are page numbers, running heads and layer misreads
# unless a measurement differs; read every other difference from the page image.
# Returns only differing numbers.
flora_compare_numbers <- function(transcription_file, pdf_file) {
  tr <- flora_read_transcription(transcription_file) |>
    group_by(pdf_page) |>
    summarise(text = paste(t, collapse = " "), .groups = "drop")
  layer <- tibble(
    pdf_page = seq_along(pdftools::pdf_text(pdf_file)),
    text = pdftools::pdf_text(pdf_file)
  )
  nums <- \(d, nm) {
    d |>
      filter(pdf_page %in% tr$pdf_page) |>
      mutate(number = str_extract_all(text, "\\d+(?:\\.\\d+)?")) |>
      select(pdf_page, number) |>
      unnest(number) |>
      count(pdf_page, number, name = nm)
  }
  full_join(
    nums(tr, "n_transcription"),
    nums(layer, "n_layer"),
    by = c("pdf_page", "number")
  ) |>
    mutate(across(starts_with("n_"), \(x) coalesce(x, 0L))) |>
    filter(n_transcription != n_layer) |>
    arrange(pdf_page, as.numeric(number))
}
