# Build raw/trait_data.csv for Wong (2010), Selaginellaceae, Flora of
# Peninsular Malaysia Series I vol. 1: 49--86. Run from the repository root.
#
# Born-digital PDF: rows come from the PDF text layer (segment_source.R).
# Descriptions are one paragraph of ";"-separated organ clauses ("Plant ...;
# branch systems ...; stem leaves ...; branch system lateral leaves ...;
# axillary leaves ...; median leaves ...; strobilus ..."). Each clause is split
# into comma pieces outside parentheses; a piece is scored when a rule below or
# the dictionary seed maps it, and kept verbatim as an unmapped row otherwise.
source("data/Wong_2010/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Wong_2010"
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

# Dictionary seed. Hyphenated shapes score both levels (SKILL.md step 4,
# "Compound colours and shapes score every word"), so drop the seed rows that
# block two-shape compounds ("ovate-lanceolate", "ovate-oblong",
# "ovate-elliptic") for this family.
seed <- read_csv(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv",
  col_types = cols(.default = "c"),
  na = character()
) |>
  filter(
    !(trait == "leaf_shape" &
      level == "" &
      str_detect(term_regex, "ovate-lanceolate|ovate-oblong"))
  )
# "appressed" in the seed means appressed hairs (Kochummen_1995); here it is
# leaf posture ("appressed to the stem surface"), so block it first.
seed <- bind_rows(
  tibble(
    trait = c("leaf_hairs_adult_leaves", "stem_hairs"),
    term_regex = "\\bappressed\\b",
    level = "",
    source = "Wong_2010",
    note = "leaf posture, not hairs"
  ),
  seed
)
dict_file <- tempfile(fileext = ".csv")
write_csv(seed, dict_file)
dict <- flora_load_dict(dict_file)

taxa <- paras |>
  filter(rank %in% c("species", "variety")) |>
  distinct(block, rank) |>
  mutate(
    species = str_remove(block, " var\\. [a-z]+$"),
    has_description = block %in%
      filter(paras, section == "Description")$block
  )

# ---- row builder --------------------------------------------------------------

mk <- function(
  p,
  trait,
  raw,
  value,
  clean = value,
  vt = "mode",
  unit = NA_character_,
  em = NA_character_,
  cq = NA_character_,
  notes = NA_character_,
  section = "Description"
) {
  tibble(
    block = p$block,
    rank = p$rank,
    trait = trait,
    trait_value_raw = raw,
    trait_value = as.character(value),
    trait_value_clean = as.character(clean),
    value_type = vt,
    unit = unit,
    entity_measured = em,
    cq = cq,
    notes = notes,
    pdf_page = p$pdf_page,
    printed_page = p$printed_page,
    source_section = section
  )
}
un <- function(p, raw, what, section = "Description") {
  mk(
    p,
    NA_character_,
    raw,
    NA_character_,
    notes = paste0("UNMAPPED: ", what),
    section = section
  )
}

# Pages of a clause: the page where it starts within its paragraph.
at_page <- function(p, clause) {
  # Clauses come from flora_norm() text; locate on their first words.
  pos <- str_locate(str_squish(p$text), fixed(str_sub(clause, 1, 25)))[1, 1]
  if (is.na(pos)) {
    return(p)
  }
  pg <- flora_page_at(p, pos)
  p$pdf_page <- pg[["pdf"]]
  p$printed_page <- pg[["printed"]]
  p
}

# "(occasionally)" and "(more often)" are commonness words set in parentheses;
# keep them as words so flora_score_cat() sees them, then drop other asides.
prep <- function(x) {
  x |>
    str_replace_all(
      "\\((occasionally|rarely|often|usually|sometimes)\\)",
      "\\1"
    ) |>
    flora_drop_asides() |>
    as.character()
}

score <- function(p, piece, trait_set, em, notes = NA_character_) {
  txt <- prep(piece)
  rows <- map(trait_set, \(tr) flora_score_cat(txt, tr, dict)) |>
    compact() |>
    bind_rows()
  if (!nrow(rows)) {
    return(NULL)
  }
  rows <- rows |> filter(trait_value_clean != "")
  if (!nrow(rows)) {
    return(NULL)
  }
  rows |>
    pmap(\(cq, trait_value, trait_value_clean, trait) {
      mk(p, trait, piece, trait_value, trait_value_clean, em = em, cq = cq,
         notes = notes)
    }) |>
    bind_rows()
}

leaf_traits <- c(
  "leaf_shape",
  "leaf_base_shape",
  "leaf_apex_shape",
  "leaf_margin",
  "leaf_hairs_adult_leaves",
  "leaf_surface_colour"
)

# ---- plant clause ---------------------------------------------------------------

height_re <- "(?:to |up to |hardly |below |reaching )?(?:about )?(?:\\(\\d+(?:\\.\\d+)?–\\))?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? cm (?:tall|high)"

height_rows <- function(p, piece) {
  h <- str_extract(piece, height_re)
  if (is.na(h)) {
    return(NULL)
  }
  cq <- str_match(piece, "\\b(often|typically|usually)\\b")[, 2]
  note <- c(
    if (str_detect(h, "about")) "Printed 'about': approximate value.",
    if (str_detect(h, "^(hardly|below) ")) {
      "Printed 'hardly' / 'below' scored as a maximum."
    },
    if (str_detect(h, "^(up )?to \\D*\\d+–")) {
      "Printed 'to a--b' scored as minimum a and maximum b."
    },
    if (str_detect(piece, "or more")) "Printed 'or more'."
  )
  h2 <- h |>
    str_remove("about |up |reaching ") |>
    str_replace("^(hardly|below) ", "to ")
  r <- flora_parse_range(h2)
  flora_num_rows(r, "cm") |>
    rename(cq_range = cq) |>
    pmap(\(value, value_type, cq_range, unit) {
      mk(p, "plant_height", piece, value, value, value_type, unit,
         cq = coalesce(cq_range, cq),
         notes = if (length(note)) paste(note, collapse = " ") else NA)
    }) |>
    bind_rows()
}

plant_piece <- function(p, piece, nxt) {
  out <- list()
  lw <- str_to_lower(piece)
  cq <- str_match(lw, flora_qual_re)[, 2]
  hab <- c(
    if (str_detect(lw, "\\bcreeping\\b") & !str_detect(lw, "connections")) {
      "creeping"
    },
    if (str_detect(lw, "stiffly erect")) "erect",
    if (str_detect(lw, "\\bsuberect\\b")) "suberect",
    if (str_detect(lw, "stoloniferous")) "stoloniferous",
    if (str_detect(lw, "compact tufts")) "tufted"
  )
  if (length(hab)) {
    out$hab <- mk(
      p,
      "stem_growth_habit",
      piece,
      paste(hab, collapse = " "),
      paste(recode(hab, suberect = "decumbent"), collapse = " "),
      cq = cq,
      notes = if ("suberect" %in% hab) {
        "suberect ('arched ascending', key couplet 2) mapped to decumbent (Wong_2010 question 2)."
      } else {
        NA
      }
    )
  }
  if (str_detect(lw, "scrambling over")) {
    out$climb <- mk(p, "plant_climbing_mechanism", piece, "scrambling",
                    cq = cq)
  }
  if (str_detect(lw, "much branched")) {
    out$br <- mk(p, "stem_branching_form", piece, "much_branched",
                 "much-branched")
  }
  if (str_detect(lw, "sparsely branched|very little branched")) {
    out$br <- mk(p, "stem_branching_form", piece, "sparsely_branched",
                 "sparsely-branched")
  }
  out$h <- height_rows(p, piece)
  # "stems glabrous", "stems hairy", "stems short-hairy on ..."; cuprea
  # "with glabrous, ascending stems" splits at the comma.
  if (
    str_detect(lw, "^stems ") |
      (lw == "with glabrous" && str_detect(nxt, "^ascending stems"))
  ) {
    out$st <- score(p, piece, "stem_hairs", NA_character_)
  }
  # "the whole plant often very pale yellowish green", "living leafy parts
  # variously dark green to ...": colour of the leafy shoots.
  if (str_detect(lw, "leafy parts|whole plant|living parts")) {
    out$col <- score(
      p,
      piece,
      "leaf_surface_colour",
      "leafy shoots",
      "Colour of the living leafy shoots (Wong_2010 question 5)."
    )
  }
  bind_rows(out)
}

# ---- one description ---------------------------------------------------------------

organ_of <- function(cl, prev) {
  case_when(
    str_detect(cl, "^(Plant|shoots|stems)\\b") ~ "plant",
    str_detect(cl, "^branch system lateral leaves") ~ "lateral leaves",
    str_detect(cl, "^branch systems?\\b") ~ "branch systems",
    str_detect(cl, "^stem leaves") ~ "stem leaves",
    str_detect(cl, "^axillary leaves along (the )?main stem") ~
      "axillary leaves (main stem)",
    str_detect(cl, "^axillary leaves") ~ "axillary leaves",
    str_detect(cl, "^median leaves") ~ "median leaves",
    str_detect(cl, "^strobil") ~ "strobilus",
    str_detect(cl, "^all leaves") ~ "all leaves",
    .default = prev
  )
}

character_of <- function(piece) {
  case_when(
    str_detect(piece, "rhizophore") ~ "rhizophores",
    str_detect(piece, "exauriculate|not auriculate") ~
      "auricles absent (negative phrase)",
    str_detect(piece, "overlapping") ~ "overlap with branches",
    str_detect(piece, "^(Plant )?(small|wiry|tiny)$|leaves tiny") ~ "stature",
    str_detect(piece, "clumped|broad spread|tangles") ~ "habit",
    str_detect(piece, "subcordate|subauriculate|subpeltate") ~
      "base shape (sub- forms, not scored)",
    str_detect(piece, "^strobilus (single|\\d)") ~ "strobilus count",
    str_detect(piece, "arista") ~ "arista length",
    str_detect(piece, "sporophyll|symmetry|tetragonous|cylindric") ~
      "strobilus and sporophyll form",
    str_detect(piece, "lateral axis|lateral axes|fork|pseudopinnate|flabellate|dichotom|branch") ~
      "branch system construction",
    str_detect(piece, "margin") ~ "upper margin form",
    str_detect(piece, "appressed|patent|ascending|inserted|recurved") ~
      "posture",
    str_detect(piece, "arranged|rows|spaced") ~ "arrangement",
    str_detect(piece, "ciliat|ciliol|ciliate") ~ "ciliate margin",
    str_detect(piece, "high|tall|long|thick|across|mm|cm") ~ "size",
    str_detect(piece, "colour|tint|green|red|maroon|purple|coppery|iridescen|pink") ~
      "colour",
    str_detect(piece, "glabrous|hairy") ~ "hairs",
    .default = "other"
  )
}

tail_re <- "\\s(?:or \\(more often\\) |often )?with (?:basally |slightly )?distended upper margin.*$|\\s(?:or \\(more often\\) )?with (?:upper margin|arista|ciliate base).*$"

parse_description <- function(p) {
  text <- flora_norm(p$text)
  clauses <- flora_split_pieces(text, ";")
  out <- list()
  organ <- "plant"
  for (cl in clauses) {
    organ <- organ_of(cl, organ)
    pc <- at_page(p, cl)
    if (organ == "stem leaves") {
      # leaf_heterogeneity: "stem leaves dimorphic, distinguishable as larger
      # ... lateral leaves and smaller ... median leaves" or, for the erect
      # species, monomorphic basal leaves below dimorphic branch leaves.
      out[[length(out) + 1]] <- mk(
        pc,
        "leaf_heterogeneity",
        cl,
        if (str_detect(cl, "monomorphic")) {
          "monomorphic basal and dimorphic lateral and median leaves"
        } else {
          "dimorphic"
        },
        "anisophyllous",
        notes = "Lateral and median leaves (heterophylly) scored as anisophyllous (Wong_2010 question 3)."
      )
      lat <- str_match(cl, "([a-z-]+) lateral leaves")[, 2]
      med <- str_match(cl, "([a-z-]+) (?:ciliolate )?median leaves")[, 2]
      if (!is.na(lat) && !str_detect(lat, "into")) {
        out[[length(out) + 1]] <- score(pc, lat, "leaf_shape",
                                        "stem lateral leaves")
      }
      if (!is.na(med) && !str_detect(med, "and")) {
        out[[length(out) + 1]] <- score(pc, med, "leaf_shape",
                                        "stem median leaves")
      }
      if (str_detect(cl, "monomorphic")) {
        pieces <- flora_split_pieces(cl, ",")
        for (pz in pieces[-1]) {
          # "with ciliate usually incurved auricles": "usually" qualifies
          # incurved, not the auricles.
          r <- score(pc, str_remove(pz, "usually (?=incurved)"), leaf_traits,
                     "basal stem leaves")
          out[[length(out) + 1]] <- r %||%
            un(pc, pz, paste("basal stem leaves |", character_of(pz)))
        }
      }
      out[[length(out) + 1]] <- un(
        pc,
        cl,
        "stem leaves | posture and arrangement (clause reviewed; shapes and heterophylly scored separately)"
      )
      next
    }
    # Keep the adjective run "slender, pale, tough rhizophores" in one piece.
    pieces <- str_replace_all(cl, "slender, pale, tough", "slender\u00a7 pale\u00a7 tough") |>
      flora_split_pieces(",") |>
      str_replace_all("\u00a7", ",")
    for (k in seq_along(pieces)) {
      pz <- pieces[k]
      r <- NULL
      if (organ == "plant") {
        r <- plant_piece(pc, pz, c(pieces, "")[k + 1])
      } else if (str_detect(organ, "leaves")) {
        pz0 <- if (k == 1) {
          str_remove(pz, "^(branch system lateral |axillary |median |all )?leaves( along (the )?main stem)? ?")
        } else {
          pz
        }
        em <- if (str_detect(pz, "on (the )?main stem")) {
          "axillary leaves (main stem)"
        } else {
          organ
        }
        if (str_detect(pz, "\\bacross\\b")) {
          a <- flora_parse_range(str_remove(pz0, "about "))
          r <- flora_num_rows(a, "mm") |>
            pmap(\(value, value_type, cq, unit) {
              mk(pc, "leaf_width", pz, value, value, value_type, unit,
                 em = "axillary leaves (main stem)",
                 notes = "Width 'across' of a near-orbicular axillary leaf scored as leaf_width (Wong_2010 question 6).")
            }) |>
            bind_rows()
        } else if (
          str_detect(pz, "below|surfaces below") &&
            str_detect(pz, "green|silvery")
        ) {
          r <- score(pc, pz0, "leaf_surface_colour", em,
                     "Lower (ventral) surface (Wong_2010 question 5).")
        } else if (!str_detect(pz, "drying|iridescence|inserted")) {
          # Trailing "with ... distended upper margin" or "with arista ..."
          # carries its own commonness words ("arista often 2/3"); score
          # without it and keep it as an unmapped row.
          rest <- str_extract(pz0, tail_re)
          r <- score(
            pc,
            str_remove(pz0, tail_re),
            setdiff(leaf_traits, "leaf_surface_colour"),
            em,
            if (str_detect(pz, "more often")) {
              "The more common alternative '(more often) with arista' has no level."
            } else {
              NA_character_
            }
          )
          if (!is.null(r) && !is.na(rest)) {
            r <- bind_rows(
              r,
              un(pc, str_trim(rest), paste(organ, "|", character_of(rest)))
            )
          }
        }
      } else if (organ == "branch systems" && str_detect(pz, "green|silvery")) {
        r <- score(pc, pz, "leaf_surface_colour", "leafy branch systems",
                   "Lower (ventral) side of the leafy branch systems (Wong_2010 question 5).")
      }
      out[[length(out) + 1]] <- if (!is.null(r) && nrow(r)) {
        r
      } else {
        un(pc, pz, paste(organ, "|", character_of(pz)))
      }
    }
  }
  bind_rows(out)
}

desc <- paras |> filter(section == "Description", rank %in% c("species", "variety"))
# Two varieties give only a differential sentence; handled by hand below.
differential <- c(
  "Selaginella roxburghii var. strigosa",
  "Selaginella selangorensis var. ciliata"
)
desc_parsed <- desc |> filter(!block %in% differential)
own <- map(seq_len(nrow(desc_parsed)), \(i) parse_description(desc_parsed[i, ])) |>
  bind_rows()

d_str <- desc |> filter(block == differential[1])
d_cil <- desc |> filter(block == differential[2])
own <- bind_rows(
  own,
  mk(d_str, "leaf_hairs_adult_leaves", d_str$text, "short-hairy", "hairy",
     em = "lateral leaves",
     notes = "Upper surface. Differential description; other values of the typical variety not inherited (Wong_2010 question 7)."),
  mk(d_cil, "stem_growth_habit", d_cil$text, "stoloniferous",
     notes = "Differential description; other values of the typical variety not inherited (Wong_2010 question 7)."),
  un(d_cil, "basally ciliate lateral leaves", "lateral leaves | ciliate margin")
)

# ---- genus description and other genus sections ---------------------------------

gp <- paras |> filter(block == "Selaginella", section == "Description")
gp_tax <- paras |> filter(block == "Selaginella", section == "Taxonomy")
genus_scored <- bind_rows(
  mk(gp, "woodiness", "Plants herbaceous, ... with only primary growth (no vascular cambium)",
     "herbaceous"),
  mk(gp, "leaf_type", "Leaves scale-like, microphyllous", "scale-like", "scale",
     notes = "Microphylls described as scale-like (Wong_2010 question 4)."),
  mk(gp, "leaf_compoundness", "simple (undivided)", "simple"),
  mk(gp, "plant_growth_substrate",
     "creeping to (sub) erect terrestrial plants several centimetres high, epiphytes and scrambling-climbing forms",
     "terrestrial epiphytes", "terrestrial epiphyte",
     notes = "Genus worldwide; no Peninsular Malaysian species is described as an epiphyte."),
  at_page(gp_tax, "Among the extant Lycopsida") |>
    mk("plant_growth_form",
       "Among the extant Lycopsida, the Selaginellaceae are distinguished from the Lycopodiaceae by ligulate microphylls and the heterosporous condition",
       "Lycopsida", "lycophyte", section = "Taxonomy",
       notes = "Growth form from the taxonomic placement in the Taxonomy paragraph (Wong_2010 question 1).")
)
genus_desc_unmapped <- flora_split_sentences(flora_norm(gp$text)) |>
  map(\(s) flora_split_pieces(s, ";")) |>
  unlist() |>
  map(\(cl) un(at_page(gp, cl), cl, paste("genus |", character_of(cl)))) |>
  bind_rows()

# ---- other sections: unmapped, one row per sentence ------------------------------

sec_paras <- paras |>
  filter(
    section %in% c("Distribution", "Ecology", "Uses", "Taxonomy", "Notes",
                   "Conservation status", "Chromosome numbers"),
    rank %in% c("genus", "species", "variety")
  )
sec_unmapped <- map(seq_len(nrow(sec_paras)), \(i) {
    p <- sec_paras[i, ]
    flora_split_sentences(flora_norm(p$text)) |>
      map(\(s) un(at_page(p, s), s,
                  paste(str_to_lower(p$section), "| text"),
                  section = p$section)) |>
      bind_rows()
  }) |>
  bind_rows()

# Manual rows from reviewed Ecology / Notes paragraphs (same columns as
# trait_data.csv).
manual <- read_csv(
  "data/Wong_2010/raw/manual_rows.csv",
  col_types = cols(.default = "c"),
  na = ""
)

# ---- copy-down and assembly --------------------------------------------------------

members <- taxa |>
  transmute(block, genus = "Selaginella", group = NA_character_, species, rank)
copied <- flora_copy_down(
  higher_rows = genus_scored,
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
if (nrow(rejected)) print(rejected)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  genus_scored |> mutate(rank_scored_from = NA_character_),
  copied,
  genus_desc_unmapped |> mutate(rank_scored_from = NA_character_),
  sec_unmapped |> mutate(rank_scored_from = NA_character_)
) |>
  transmute(
    taxon_name = block,
    taxon_rank = if_else(block == "Selaginella", "genus",
                         taxa$rank[match(block, taxa$block)]),
    rank_scored_from,
    commonness_qualifier = cq,
    population_region = "Peninsular Malaysia",
    form = NA_character_,
    entity_measured,
    trait,
    trait_value_raw,
    trait_value,
    trait_value_clean,
    value_type,
    unit,
    source_section,
    pdf_page = as.character(pdf_page),
    printed_page = as.character(printed_page),
    notes,
    source_id = NA_character_
  ) |>
  bind_rows(manual) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits) |>
  arrange(taxon_name, is.na(trait), trait)

write_csv(out, "data/Wong_2010/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/Wong_2010/raw/unmapped_terms.csv",
  na = ""
)

flags <- taxa |>
  left_join(
    out |> filter(!is.na(trait)) |> count(taxon_name, name = "n_mapped_rows"),
    by = c("block" = "taxon_name")
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank = rank,
    status = "accepted",
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      !has_description ~
        "No species description: the varieties carry the descriptions; only genus copy-down rows.",
      block %in% differential ~
        "Differential description only; typical-variety values not inherited (Wong_2010 question 7).",
      TRUE ~ ""
    )
  ) |>
  bind_rows(tibble(
    taxon_name = c("Selaginella tamariscina", "Selaginella siamensis"),
    taxon_rank = "species",
    status = c("not native", "misapplied name"),
    n_mapped_rows = 0L,
    flag = c(
      "Genus Uses: not native in Peninsular Malaysia, introduced as an aquarium plant; not scored.",
      "Genus Taxonomy: S. siamensis in Parris & Latiff (1997) is S. frondosa; not scored."
    )
  ))
write_csv(flags, "data/Wong_2010/raw/taxon_flags.csv", na = "")
