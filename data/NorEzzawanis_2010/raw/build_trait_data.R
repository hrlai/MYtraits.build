# Build raw/trait_data.csv for Nor-Ezzawanis (2010), Cibotiaceae, Flora of
# Peninsular Malaysia Series I vol. 1: 119--122. Run from the repository root.
#
# Born-digital PDF: rows come from the PDF text layer (segment_source.R).
# Monogeneric family with no family description; one genus (Cibotium) and one
# species (C. barometz). Rows are scored by hand but keyed to the comma pieces
# of each description (Jaman_2020 pattern): every Description sentence is
# split into ";" clauses and "," pieces outside parentheses; a piece named in
# the tables below is scored, and every other piece is kept verbatim as an
# unmapped row (consecutive unscored pieces of one clause form one row). Fern
# mappings follow SKILL.md step 4 "Ferns". Hedged statements are not scored.
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.
source("data/NorEzzawanis_2010/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

traits <- yaml::read_yaml("config/traits.yml")$traits$elements
PM <- "Peninsular Malaysia"
sp <- "Cibotium barometz"

paras <- paras |> mutate(text_n = flora_norm(text))
stopifnot(
  identical(
    distinct(filter(paras, status == "accepted"), block, rank)$rank,
    c("family", "genus", "species")
  )
)

# ---- pieces ---------------------------------------------------------------------

page_of <- function(p, fragment) {
  pos <- str_locate(p$text_n, fixed(str_sub(fragment, 1, 30)))[1, 1]
  pg <- flora_page_at(p, coalesce(pos, 1L))
  list(pdf = pg[["pdf"]], printed = pg[["printed"]])
}

# flora_split_sentences() also splits at abbreviations inside citations
# ("(Holttum, Rev. Fl. Malaya 2 (1968) 112; ...)"); mask full stops inside
# parentheses first so each cited sentence stays whole.
split_sentences <- function(x) {
  chars <- strsplit(x, "")[[1]]
  depth <- cumsum(chars == "(") - cumsum(chars == ")") + (chars == ")")
  chars[chars == "." & depth > 0] <- "\u00a7"
  flora_split_sentences(paste(chars, collapse = "")) |>
    str_replace_all("\u00a7", ".")
}

desc_paras <- paras |> filter(section == "Description")
pieces <- seq_len(nrow(desc_paras)) |>
  map(\(r) {
    p <- desc_paras[r, ]
    s <- split_sentences(p$text_n)
    map(seq_along(s), \(i) {
      cl <- flora_split_pieces(s[i], ";")
      map(seq_along(cl), \(j) {
        pc <- str_remove(flora_split_pieces(cl[j], ","), "\\.$")
        pg <- page_of(p, cl[j])
        tibble(
          block = p$block,
          rank = p$rank,
          sent = i,
          clause = j,
          piece_i = seq_along(pc),
          piece = pc,
          sentence_head = flora_split_pieces(s[i], ",")[1],
          pdf_page = pg$pdf,
          printed_page = pg$printed
        )
      }) |>
        bind_rows()
    }) |>
      bind_rows()
  }) |>
  bind_rows() |>
  mutate(pid = row_number())

# ---- scored pieces ----------------------------------------------------------------

# k(): categorical row. num(): numeric rows parsed from `txt` (default the
# piece). lw(): length and width of "a x b", each parsed from its own text so
# "to" keeps its meaning. First dimension is length (Jaman_2020 question 6).
k <- function(piece, trait, value, clean = value, em = NA, notes = NA) {
  tibble(
    piece = list(piece),
    trait,
    trait_value = value,
    trait_value_clean = clean,
    value_type = "mode",
    unit = NA_character_,
    cq = NA_character_,
    entity_measured = em,
    notes
  )
}
num <- function(piece, trait, txt = piece, em = NA, unit = NA, notes = NA) {
  r <- flora_num_rows(flora_parse_range(txt))
  stopifnot(!is.null(r), nrow(r) > 0)
  if (!is.na(unit)) r$unit <- unit
  stopifnot(!anyNA(r$unit))
  tibble(
    piece = list(piece),
    trait,
    trait_value = as.character(r$value),
    trait_value_clean = as.character(r$value),
    value_type = r$value_type,
    unit = r$unit,
    cq = r$cq,
    entity_measured = em,
    notes
  )
}
lw <- function(piece, len, wid, em, notes = NA) {
  bind_rows(
    num(piece, "leaflet_length", len, em = em, notes = notes),
    num(piece, "leaflet_width", wid, em = em, notes = notes)
  )
}

n_st <- "Stipe length scored as petiole length (SKILL.md step 4, Ferns)."
n_la <- "Lamina scored as leaf; excludes the stipe (SKILL.md step 4, Ferns)."
n_lobe <- "Pinnule lobes cut almost to the vein scored as segments (leaflet traits; Jaman_2020 question 3)."
n_hair <- "Hairs (not scales) on the costa, costules, costulets or veins scored as leaf hairs (Jaman_2020 question 2)."
n_tri <- "'tripinnatifid' (bipinnate with pinnatifid pinnules) has no level; scored as bipinnate, the nearest level, since the description gives pinnae and stalked or sessile pinnules (NorEzzawanis_2010 question 1)."
tri_rows <- function(piece, notes = n_tri) {
  bind_rows(
    k(piece, "leaf_lamina_division", "tripinnatifid", "bipinnate",
      notes = notes),
    k(piece, "leaf_compoundness", "tripinnatifid", "compound")
  )
}
n_else <- "The '(elsewhere ...)' aside is outside the region (Regions rule) and not scored."

scored <- list(
  "Cibotium" = bind_rows(
    k("Large to very large terrestrial ferns", "plant_growth_form", "fern"),
    k("Large to very large terrestrial ferns", "plant_growth_substrate",
      "terrestrial"),
    k("bearing spirally arranged fronds", "leaf_arrangement",
      "spirally arranged", "spiral"),
    tri_rows("deeply tripinnatifid (elsewhere from bipinnatifid, bipinnate, tripinnatifid to tripinnate)",
             notes = paste(n_tri, n_else))
  ),
  "Cibotium barometz" = bind_rows(
    k("Large terrestrial fern", "plant_growth_form", "fern",
      notes = "Not a tree fern: no trunk (see Notes), so fern, not fern palmoid (SKILL.md step 4, Ferns)."),
    k("Large terrestrial fern", "plant_growth_substrate", "terrestrial"),
    num("80–225 cm long", "petiole_length", em = "stipe", notes = n_st),
    k("Laminae mid-green above", "leaf_surface_colour", "mid-green", "green",
      em = "lamina", notes = "Upper surface."),
    k("glaucous beneath", "leaf_glaucousness", "glaucous", em = "lamina",
      notes = "Lower surface."),
    k("lower surface of veins and veinlets of mature plants more or less densely covered with pale brown appressed hairs",
      "leaf_hairs_adult_leaves", "hairs", "hairy",
      em = "pinna axes and veins",
      notes = paste(n_hair, "'more or less' qualifies the density, not the presence of hairs.")),
    k("ovate (in outline)@1", "leaf_shape", "ovate", em = "lamina"),
    lw("110–220 × 120–170 cm", "110–220 cm", "120–170 cm", em = "lamina",
       notes = n_la) |>
      mutate(trait = str_replace(trait, "^leaflet", "leaf")),
    tri_rows("deeply tripinnatifid"),
    num("pinnae pairs 12–22", "leaflet_count", txt = "12–22 mm",
        unit = "pairs", em = "pinnae",
        notes = "Pinna pairs (unit pairs, doubled by config/unit_conversions.csv; SKILL.md step 4, Ferns)."),
    k("ovate (in outline)@2", "leaf_shape", "ovate", em = "pinnae"),
    lw("largest pinnae at or near base to c. 94 × 33 cm", "to 94 cm",
       "to 33 cm", em = "largest pinnae",
       notes = "Printed 'to c.': scored as a maximum."),
    lw("the largest pinnules 5–20 × 3–4 mm", "5–20 mm", "3–4 mm",
       em = "largest pinnules",
       notes = "Values as printed; see NorEzzawanis_2010 question 3."),
    k("margin shallowly toothed and more deeply so towards apex",
      "leaf_margin", "toothed", em = "pinnule lobes",
      notes = paste(n_lobe, "The margin follows 'lobes' in the same clause (NorEzzawanis_2010 question 2)."))
  )
)

# Attach each scored row to its piece(s); a name must match exactly one piece
# of that taxon's description ("text@2": the second piece with this text).
scored_rows <- imap(scored, \(rows, b) {
  pb <- pieces |> filter(block == b)
  rows |>
    mutate(pid = map(piece, \(ps) {
      map_int(ps, \(x) {
        occ <- as.integer(str_match(x, "@(\\d)$")[, 2])
        x <- str_remove(x, "@\\d$")
        hit <- pb$pid[pb$piece == x]
        if (!is.na(occ)) hit <- hit[occ]
        if (length(hit) != 1) {
          stop(b, ": piece '", x, "' matched ", length(hit), " pieces")
        }
        hit
      })
    }))
}) |>
  bind_rows()

own_desc <- scored_rows |>
  mutate(
    p1 = map_int(pid, 1),
    trait_value_raw = map_chr(piece, \(x) {
      paste(str_remove(x, "@\\d$"), collapse = ", ")
    })
  ) |>
  left_join(
    pieces |> select(p1 = pid, block, rank, pdf_page, printed_page),
    by = "p1"
  ) |>
  mutate(source_section = "Description") |>
  select(-piece, -pid, -p1)

# ---- unmapped description pieces ------------------------------------------------------

organ_re <- c(
  "^Large" = "plant",
  "^(Rhizomes|the apex|young parts)" = "rhizome",
  "^(Stipes|base densely|softly|glabrescent|1–2 cm|reddish)" = "stipe",
  "^(Laminae|laminae|when dry|young plants|lower surface)" = "lamina",
  "^(pinnae|arranged|largest pinnae)" = "pinnae",
  "^(the largest pinnules|the proximal|the distal|all pinnules)" = "pinnules",
  "^(lobes|2–5 mm|apex)" = "pinnule lobes",
  "^(each lobe|veins|simple or forked)" = "veins",
  "^(Sori|sori|at the ends|terminal|protected)" = "sori",
  "^(indusia|the inner indusium|outer|inner|cup-shaped|entire|pale green|stiff)" = "indusia",
  "^(the two indusia)" = "indusia",
  "^(sporangi|longitudinal)" = "sporangia",
  "^(spores)" = "spores",
  "^(Gametophyte|above ground|green|broadly)" = "gametophyte"
)
organ_of <- function(x) {
  hit <- which(str_detect(x, names(organ_re)))
  if (length(hit)) organ_re[[hit[1]]] else NA_character_
}

desc_unmapped <- pieces |>
  filter(!pid %in% unlist(scored_rows$pid)) |>
  group_by(block, rank, sent, clause) |>
  mutate(run = cumsum(c(1, diff(piece_i) != 1))) |>
  group_by(block, rank, sent, clause, run) |>
  summarise(
    trait_value_raw = paste(piece, collapse = ", "),
    first_piece = first(piece),
    head = first(sentence_head),
    pdf_page = first(pdf_page),
    printed_page = first(printed_page),
    .groups = "drop"
  ) |>
  mutate(
    organ = coalesce(
      map_chr(first_piece, organ_of),
      map_chr(head, organ_of),
      "plant"
    ),
    notes = flora_unmapped_note(organ, "description"),
    source_section = "Description"
  ) |>
  select(block, rank, trait_value_raw, pdf_page, printed_page, notes,
         source_section)

# The "(elsewhere ...)" aside of a scored genus piece is kept as its own
# unmapped row (SKILL.md step 4, Parentheses).
aside_unmapped <- own_desc |>
  filter(str_detect(trait_value_raw, "\\(elsewhere")) |>
  distinct(block, rank, trait_value_raw, pdf_page, printed_page) |>
  mutate(
    trait_value_raw = str_extract(trait_value_raw, "\\(elsewhere[^)]*\\)"),
    notes = flora_unmapped_note("lamina", "division", "outside the region"),
    source_section = "Description"
  )

# ---- other sections ----------------------------------------------------------------

# One unmapped row per sentence of every labelled section (Wong_2010
# pattern). Nomenclature and vernacular names are not trait text; the figure
# specimen is in raw/NorEzzawanis_2010_vouchers.csv. Sentences reporting
# another work's observation carry its source_id.
cited <- c(
  "^The golden hairs on the rhizome" = "Praptosuwiryo_2003",
  "^Holttum \\(1963\\) stated" = "Holttum_1963",
  "^Lowry \\(Malay" = "Lowry_1970",
  "staunch bleeding wounds \\(Praptosuwiryo" = "Praptosuwiryo_2003",
  "^Bidin \\(Paku-pakis" = "Bidin_1985",
  "Backer & Posthumus" = "Backer_1939",
  "^However, none of the wild plants" = "Holttum_1968",
  "woolly lamb \\(Holttum, 1968\\)" = "Holttum_1968"
)
sec_paras <- paras |>
  filter(
    status == "accepted",
    !section %in% c("Nomenclature", "Description", "Vernacular names",
                    "English name")
  )
sec_unmapped <- map(seq_len(nrow(sec_paras)), \(r) {
  p <- sec_paras[r, ]
  s <- split_sentences(p$text_n)
  map(s, \(x) {
    pg <- page_of(p, x)
    hit <- which(str_detect(x, names(cited)))
    tibble(
      block = p$block,
      rank = p$rank,
      trait_value_raw = x,
      pdf_page = pg$pdf,
      printed_page = pg$printed,
      notes = flora_unmapped_note("section", "text", str_to_lower(p$section)),
      source_section = p$section,
      source_id = if (length(hit)) cited[[hit[1]]] else NA_character_
    )
  }) |>
    bind_rows()
}) |>
  bind_rows()
stopifnot(sum(!is.na(sec_unmapped$source_id)) == length(cited))

# ---- copy-down and assembly -----------------------------------------------------------

higher <- own_desc |> filter(rank == "genus")
own <- own_desc |> filter(rank == "species")
members <- tibble(
  block = sp,
  genus = "Cibotium",
  group = NA_character_,
  species = sp,
  rank = "species"
)
# Monogeneric family with no family description: the genus is the only
# higher rank (SKILL.md step 5).
copied <- flora_copy_down(higher, own, members)
rejected <- attr(copied, "rejected")
if (nrow(rejected)) print(rejected)

out <- bind_rows(
  own_desc |> mutate(rank_scored_from = NA_character_),
  copied,
  desc_unmapped,
  aside_unmapped,
  sec_unmapped
) |>
  transmute(
    taxon_name = block,
    taxon_rank = case_when(
      block == "Cibotiaceae" ~ "family",
      block == "Cibotium" ~ "genus",
      .default = "species"
    ),
    rank_scored_from,
    commonness_qualifier = cq,
    # FPM species descriptions describe the species in Peninsular Malaysia;
    # genus values and copied rows have no region.
    population_region = if_else(
      taxon_name == sp & is.na(rank_scored_from),
      PM,
      NA_character_
    ),
    form = NA_character_,
    entity_measured,
    trait,
    trait_value_raw,
    trait_value,
    trait_value_clean,
    value_type = coalesce(value_type, "mode"),
    unit,
    source_section,
    pdf_page = as.character(pdf_page),
    printed_page = as.character(printed_page),
    notes,
    source_id
  ) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits) |>
  arrange(taxon_name, is.na(trait), trait)

stopifnot(all(out$trait[!is.na(out$trait)] %in% names(traits)))
write_csv(out, "data/NorEzzawanis_2010/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/NorEzzawanis_2010/raw/unmapped_terms.csv",
  na = ""
)

n_mapped <- out |>
  filter(!is.na(trait)) |>
  count(taxon_name, name = "n_mapped_rows")
tibble(
  taxon_name = c("Cibotiaceae", "Cibotium", sp),
  taxon_rank = c("family", "genus", "species"),
  status = "accepted"
) |>
  left_join(n_mapped, by = "taxon_name") |>
  mutate(
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      taxon_name == "Cibotiaceae" ~
        "Monogeneric family with no family description; not scored.",
      taxon_name == sp ~
        "Only Malaysian species of Cibotium; name retained as printed.",
      .default = ""
    )
  ) |>
  write_csv("data/NorEzzawanis_2010/raw/taxon_flags.csv", na = "")
