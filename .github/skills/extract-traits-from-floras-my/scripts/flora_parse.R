# Text and number helpers shared by every Flora of Peninsular Malaysia (FPM) /
# Tree Flora of Sabah and Sarawak (TFSS) family parser. Taken from the code that
# was copied unchanged between data/Coode_2018 and data/Lim_2010, plus the
# generic fixes the notes asked for. Family-specific parsing (organ detection,
# clause rules, colour scoping) stays in the family's raw/build_trait_data.R.
#
# Use: source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")
# Run from the repository root.

suppressPackageStartupMessages(library(tidyverse))

# ---- source fixes and normalisation -------------------------------------------

# Per-family source repairs live in raw/source_fixes.csv
# (pattern, replacement, page, reason). Apply once to description paragraphs,
# using the printed page to scope each fix. Unmatched patterns are reported.
flora_apply_fixes <- function(x, fixes, page = NULL) {
  if (is.null(fixes) || !nrow(fixes)) {
    return(x)
  }
  if (!is.null(page) && length(page) != length(x)) {
    stop("`page` must have the same length as `x`.")
  }
  if (anyNA(fixes$pattern) || anyNA(fixes$replacement)) {
    stop("Source fixes require non-missing patterns and replacements.")
  }
  for (i in seq_len(nrow(fixes))) {
    applicable <- if (
      is.null(page) || is.na(fixes$page[i]) || fixes$page[i] == ""
    ) {
      rep(TRUE, length(x))
    } else {
      !is.na(page) & page == fixes$page[i]
    }
    hit <- applicable & !is.na(x) & str_detect(x, fixes$pattern[i])
    if (!any(hit, na.rm = TRUE)) {
      warning(
        "Source fix matched nothing: ",
        fixes$pattern[i],
        " (p. ",
        fixes$page[i],
        ")"
      )
    }
    x[!is.na(hit) & hit] <- str_replace_all(
      x[!is.na(hit) & hit],
      fixes$pattern[i],
      fixes$replacement[i]
    )
  }
  x
}

# Generic clean-up only: soft hyphens, "elliptic- obovate", spacing around en
# dashes between digits, "14 by 7" -> "14 x 7". Keep true en dashes and x signs.
flora_norm <- function(x) {
  x |>
    str_replace_all(
      "\\b(narrowly|broadly) to (narrowly|broadly) ([a-z]+)",
      "\\1 \\3 to \\2 \\3"
    ) |>
    str_replace_all("\u00ad", "") |>
    str_replace_all("\u2013 ", "\u2013") |>
    str_replace_all("(?<=\\d)\\s?\u2013\\s?(?=\\d)", "\u2013") |>
    str_replace_all(
      "\\) by \\(|(?<=\\d) by (?=[\\d(])",
      \(m) if (str_detect(m, "^\\)")) ") \u00d7 (" else " \u00d7 "
    ) |>
    str_squish()
}

# Final step for every character column written to a CSV. Writes ranges as
# a--b so spreadsheets do not read dates, including parenthetical extremes:
# "(7\u2013)9" -> "(7--)9", "(\u20136)" -> "(--6)". Coode_2018 left "(\u20137)" as an en
# dash while converting "4\u20136"; this treats both the same way.
flora_dashes <- function(x) {
  str_replace_all(x, "(?<=[0-9)(])\\s?\u2013\\s?(?=[0-9()])", "--")
}

# trait_value_raw convention: x for the multiplication sign, +/- for the
# plus-minus sign.
flora_ascii_raw <- function(x) {
  x |>
    str_replace_all("\u00d7", "x") |>
    str_replace_all("\u00b1", "+/-")
}

# ---- sentences, pieces, parentheses -----------------------------------------------

# Sentence split protecting abbreviations ("c. 2", "E. glaber", "e.g.").
flora_split_sentences <- function(x) {
  str_split(
    x,
    "(?<!\\bc|\\b[A-Z]|e\\.g|\\bsp|subsp|var|Fig|\\bca)\\.\\s+(?=[A-Z])"
  )[[1]]
}

# Split on commas and semicolons that are outside parentheses. Splitting inside
# them lost "5--6 x c. 2 mm" in "(possibly more, petals strongly incurved)".
flora_split_pieces <- function(x, sep = c(",", ";")) {
  chars <- strsplit(x, "")[[1]]
  depth <- 0L
  start <- 1L
  out <- character()
  for (i in seq_along(chars)) {
    if (chars[i] == "(") {
      depth <- depth + 1L
    } else if (chars[i] == ")") {
      depth <- max(0L, depth - 1L)
    } else if (depth == 0L && chars[i] %in% sep) {
      out <- c(out, substr(x, start, i - 1L))
      start <- i + 1L
    }
  }
  out <- c(out, substr(x, start, nchar(x)))
  str_squish(out)[nzchar(str_squish(out))]
}

# Remove parenthetical asides that are not measurement extremes, e.g.
# "(including acumen)", "(possibly more, petals strongly incurved)". Asides are
# kept in attr(, "asides") so a hedge can still go in `notes`.
flora_drop_asides <- function(x) {
  re <- "\\((?![\u2013\\d]|c\\.)[^)]*\\)"
  asides <- str_extract_all(x, re)[[1]]
  out <- str_squish(str_remove_all(x, re))
  attr(out, "asides") <- asides
  out
}

flora_qual_words <- c(
  "usually",
  "often",
  "sometimes",
  "rarely",
  "occasionally",
  "mostly",
  "generally",
  "frequently",
  "less often",
  "very rarely"
)
flora_qual_re <- paste0("\\b(", paste(flora_qual_words, collapse = "|"), ")\\b")
flora_hedge_re <- "\\b(probably|perhaps|apparently|possibly|presumably)\\b"

# ---- numbers ------------------------------------------------------------------------

.flora_num <- "\\d+(?:\\.\\d+)?"
# (a-)b-c(-d) with optional "c." / "to" and an optional unit
.flora_range_re <- paste0(
  "(?:\\((",
  .flora_num,
  ")\u2013\\)\\s?)?", # 1 extreme min
  "(c\\.\\s?|to\\s)?", # 2 c. / to
  "(",
  .flora_num,
  ")", # 3 low
  "(?:\u2013(",
  .flora_num,
  "))?", # 4 high
  "(?:\\s?\\(\u2013(",
  .flora_num,
  ")\\))?", # 5 extreme max
  "\\s?(mm|cm|m)?\\b" # 6 unit
)

flora_parse_range <- function(s) {
  m <- str_match(s, .flora_range_re)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  tibble(
    xmin = as.numeric(m[, 2]),
    approx = m[, 3],
    lo = as.numeric(m[, 4]),
    hi = as.numeric(m[, 5]),
    xmax = as.numeric(m[, 6]),
    unit = m[, 7]
  )
}

# Rows for one numeric value. Typical range -> minimum + maximum; single or
# "c." value -> mean; "to X" -> maximum. Parenthetical extremes go on
# commonness_qualifier = extreme_range rows, and the typical rows become
# `usually` (the convention used by all three FPM datasets).
flora_num_rows <- function(r, unit = r$unit) {
  out <- list()
  ap <- coalesce(str_trim(r$approx), "")
  if (ap == "c.") {
    out[[1]] <- tibble(value = r$lo, value_type = "mean", cq = NA_character_)
  } else if (ap == "to") {
    out[[1]] <- tibble(value = r$lo, value_type = "maximum", cq = NA_character_)
  } else if (is.na(r$hi)) {
    out[[1]] <- tibble(value = r$lo, value_type = "mean", cq = NA_character_)
  } else {
    out[[1]] <- tibble(
      value = c(r$lo, r$hi),
      value_type = c("minimum", "maximum"),
      cq = NA_character_
    )
  }
  if (!is.na(r$xmin) || !is.na(r$xmax)) {
    out[[1]]$cq <- "usually"
    if (!is.na(r$xmin)) {
      out[[2]] <- tibble(
        value = r$xmin,
        value_type = "minimum",
        cq = "extreme_range"
      )
    }
    if (!is.na(r$xmax)) {
      out[[3]] <- tibble(
        value = r$xmax,
        value_type = "maximum",
        cq = "extreme_range"
      )
    }
  }
  bind_rows(out) |> mutate(unit = unit)
}

# "a-b x c-d cm": returns list(len =, wid =). The first dimension is length;
# record that assumption in a metadata question. The unit comes from the end.
flora_parse_dims <- function(s) {
  s2 <- flora_drop_asides(s)
  m <- str_match(
    s2,
    "(\\(?[\\d.\u2013()c ]+?)\\s?\u00d7\\s?(\\(?[\\d.\u2013()c ]+?)\\s?(mm|cm|m)\\b"
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  a <- flora_parse_range(paste0(str_squish(m[, 2]), " ", m[, 4]))
  b <- flora_parse_range(paste0(str_squish(m[, 3]), " ", m[, 4]))
  if (is.null(a) || is.null(b)) {
    return(NULL)
  }
  list(len = flora_num_rows(a, m[, 4]), wid = flora_num_rows(b, m[, 4]))
}

# ---- categorical dictionary ------------------------------------------------------------

# Dictionary CSV columns: trait, term_regex, level, source, note. Row order is
# priority within a trait. Filter the seed by family before loading it.
flora_load_dict <- function(file) {
  d <- read_csv(file, col_types = cols(.default = "c"), na = character())
  split(d, factor(d$trait, levels = unique(d$trait))) |>
    map(\(x) set_names(x$level, x$term_regex))
}

# Score one clause for one trait. Alternatives split on commas and on
# "or/and/to <commonness word>". Values keep text order. Any commonness word
# moves that chunk to its own row; the unqualified chunk then becomes `usually`.
# A chunk holding a hedge word is skipped. Returns one row per commonness
# qualifier: cq, trait_value (source terms), trait_value_clean (levels), trait.
flora_score_cat <- function(txt, trait, dict) {
  d <- dict[[trait]]
  chunks <- unlist(str_split(
    txt,
    paste0(
      ",\\s*|\\s(?=(or|and|to) (",
      paste(flora_qual_words, collapse = "|"),
      ")\\b)"
    )
  ))
  out <- list()
  for (ci in seq_along(chunks)) {
    ch <- chunks[ci]
    if (str_detect(ch, flora_hedge_re)) {
      next
    }
    q <- str_match(ch, flora_qual_re)[, 2]
    work <- ch
    for (k in seq_along(d)) {
      repeat {
        loc <- str_locate(work, regex(names(d)[k], ignore_case = TRUE))
        if (is.na(loc[1, 1])) {
          break
        }
        out[[length(out) + 1]] <- tibble(
          ci = ci,
          pos = loc[1, 1],
          term = str_sub(work, loc[1, 1], loc[1, 2]),
          level = d[[k]],
          cq = q
        )
        str_sub(work, loc[1, 1], loc[1, 2]) <- strrep(
          "#",
          loc[1, 2] - loc[1, 1] + 1
        )
      }
    }
  }
  if (!length(out)) {
    return(NULL)
  }
  bind_rows(out) |>
    arrange(ci, pos) |>
    mutate(
      term = str_replace_all(str_to_lower(term), " ", "_"),
      cq = if (any(!is.na(cq))) coalesce(cq, "usually") else cq
    ) |>
    group_by(cq) |>
    summarise(
      trait_value = paste(unique(term), collapse = " "),
      trait_value_clean = paste(unique(level[level != ""]), collapse = " "),
      first = min(ci * 1000 + pos),
      .groups = "drop"
    ) |>
    arrange(cq != "usually" | is.na(cq), first) |>
    mutate(trait = trait) |>
    select(-first)
}
