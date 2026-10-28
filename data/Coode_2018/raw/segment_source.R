# Segment the born-digital Elaeocarpaceae treatment; run from repository root.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")

lines <- flora_read_pdf_lines(
  "inst/extdata/myflora/pdf/FPM/Elaeocarpaceae.pdf",
  first_line_re = "^ELAEOCARPACEAE$",
  last_pdf_page = 100,
  running_head_re = "^(SEED PLANTS VOL\\. 7 \\(2018\\)|ELAEOCARPACEAE \\(COODE\\))$"
)
epithet <- \(x) str_match(x, "(subsp|var)\\. ([a-z]+)")[, 3]
headers <- list(
  list(name = "family", re = "^ELAEOCARPACEAE$", rank = "family",
       block = \(x, sp) "Elaeocarpaceae"),
  list(name = "genus", re = "^\\d\\. (ELAEOCARPUS|SLOANEA) L\\.$", rank = "genus",
       block = \(x, sp) str_to_title(str_extract(x, "[A-Z]{5,}")), reset_status = TRUE),
  list(name = "group", re = "^(Acronodia|Coilopetalum|Elaeocarpus|Ganitrus|Monocera|Polystachyus) Group$",
       rank = "infrageneric_group", block = \(x, sp) paste("Elaeocarpus", x)),
  list(name = "key", re = "^Key to", rank = "key", block = \(x, sp) paste("KEY", x)),
  list(name = "species", re = "^\\d{1,2}\\. (Elaeocarpus|Sloanea) [a-z]+", rank = "species",
       block = \(x, sp) str_extract(x, "(Elaeocarpus|Sloanea) (sp\\. nov\\.|[a-z]+)"),
       sets_species = TRUE),
  list(name = "infra", re = "^(\\d{1,2}[a-c]\\. )?(subsp|var)\\. [a-z]+",
       rank = \(x) if (str_extract(x, "subsp|var") == "subsp") "subspecies" else "variety",
       block = \(x, sp) paste0(sp, " ", str_extract(x, "subsp|var"), ". ", epithet(x))),
  list(name = "form", re = "^\\d[a-c]\\. E\\. angustifolius –", rank = "informal_form",
       block = \(x, sp) paste0(sp, " (", str_extract(x, "(?<=– ).*?form|intermediate"), ")")),
  list(name = "formp", re = "^Ovalis form$", rank = "informal_form",
       block = \(x, sp) "Elaeocarpus petiolatus (ovalis form)"),
  list(name = "section", re = "^(Incompletely Known Species|Excluded Species)$",
       rank = "section", block = \(x, sp) paste("SECTION", x),
       status = \(x) if (str_detect(x, "Excluded")) "excluded" else "incompletely_known"),
  list(name = "oddsp", re = "^Elaeocarpus (macrocarpus|euneurus|serratus) ",
       rank = "species", block = \(x, sp) str_extract(x, "Elaeocarpus [a-z]+"))
)
paras <- flora_segment(
  lines, headers,
  form_label_re = "^(Typical form|Western form of subsp\\. apiculatus)$",
  cancel_header = \(l) l$hdr %in% "infra" & l$indent > 15,
  ignore_header = \(h, x, rk) {
    h == "infra" && identical(rk, "key") &&
      !str_detect(x, "^\\d{1,2}[a-c]\\. |Map \\d")
  },
  post_blocks = \(l) {
    l$status[str_detect(l$block, "sp\\. nov")] <- "unnamed"
    l
  }
)
page_at <- flora_page_at
