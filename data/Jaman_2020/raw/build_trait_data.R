# Build raw/trait_data.csv for Jaman (2020), Cyatheaceae, Flora of Peninsular
# Malaysia Series I vol. 3: 77--107. Run from the repository root.
#
# Born-digital PDF: rows come from the PDF text layer (segment_source.R). One
# genus (Cyathea) with 21 species and one excluded species. Descriptions are
# short organ paragraphs (Trunk, Stipes, Laminae, Sori, "Scales and hairs:"),
# so rows are scored by hand but keyed to the comma pieces of each description
# (Cheek_2012 pattern): every Description sentence is split into ";" clauses
# and "," pieces outside parentheses; a piece named in the tables below is
# scored, and every other piece is kept verbatim as an unmapped row
# (consecutive unscored pieces of one clause form one row). Fern mappings
# follow SKILL.md step 4 "Ferns". Hedged statements are not scored. Writes
# trait_data.csv, unmapped_terms.csv and taxon_flags.csv.
source("data/Jaman_2020/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Jaman_2020"
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
PM <- "Peninsular Malaysia"

fixes <- read_csv(
  "data/Jaman_2020/raw/source_fixes.csv",
  col_types = cols(.default = "c"),
  na = ""
)
paras <- paras |>
  mutate(
    text_n = if_else(
      section == "Description",
      flora_norm(flora_apply_fixes(text, fixes, as.character(printed_page))),
      flora_norm(text)
    )
  )

species <- paras |>
  filter(rank == "species", status == "accepted") |>
  distinct(block) |>
  pull()
stopifnot(length(species) == 21)

# ---- pieces ---------------------------------------------------------------------

page_of <- function(p, fragment) {
  pos <- str_locate(p$text_n, fixed(str_sub(fragment, 1, 30)))[1, 1]
  pg <- flora_page_at(p, coalesce(pos, 1L))
  list(pdf = pg[["pdf"]], printed = pg[["printed"]])
}

desc_paras <- paras |>
  filter(section == "Description", status == "accepted",
         rank %in% c("family", "genus", "species"))
pieces <- seq_len(nrow(desc_paras)) |>
  map(\(r) {
    p <- desc_paras[r, ]
    s <- flora_split_sentences(p$text_n)
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
          clause_head = pc[1],
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
# piece); `vt` overrides the value type of a single value ("at least 30 cm" ->
# minimum, "largest 70 cm" -> maximum). lw(): length and width of "a x b",
# each parsed from its own text so "to" and "c." keep their meaning
# (flora_parse_dims() drops a leading "to"). First dimension is length
# (Jaman_2020 question 6).
k <- function(piece, trait, value, clean = value, q = NA, em = NA, notes = NA) {
  tibble(
    piece = list(piece),
    trait,
    trait_value = value,
    trait_value_clean = clean,
    value_type = "mode",
    unit = NA_character_,
    cq = q,
    entity_measured = em,
    notes
  )
}
num <- function(piece, trait, txt = piece, em = NA, q = NA, vt = NA,
                unit = NA, notes = NA) {
  r <- flora_num_rows(flora_parse_range(txt))
  stopifnot(!is.null(r), nrow(r) > 0)
  if (!is.na(vt)) {
    stopifnot(nrow(r) == 1)
    r$value_type <- vt
  }
  if (!is.na(unit)) r$unit <- unit
  stopifnot(!anyNA(r$unit))
  tibble(
    piece = list(piece),
    trait,
    trait_value = as.character(r$value),
    trait_value_clean = as.character(r$value),
    value_type = r$value_type,
    unit = r$unit,
    cq = coalesce(r$cq, q),
    entity_measured = em,
    notes
  )
}
lw <- function(piece, len, wid, em, q = NA, vt = NA, notes = NA,
               traits = c("leaflet_length", "leaflet_width")) {
  bind_rows(
    num(piece, traits[1], len, em = em, q = q, vt = vt, notes = notes),
    num(piece, traits[2], wid, em = em, q = q, vt = vt, notes = notes)
  )
}

# Notes used on many rows.
n_tr <- "Trunk height of a tree fern scored as plant_height (SKILL.md step 4, Ferns)."
n_st <- "Stipe length scored as petiole length (SKILL.md step 4, Ferns)."
n_la <- "Lamina length scored as leaf length; excludes the stipe (SKILL.md step 4, Ferns)."
n_dia <- "Trunk diameter scored as stem_diameter (Jaman_2020 question 1)."
n_to_c <- "Printed 'to c.': scored as a maximum."
n_ab <- "Printed 'about' or 'c.': approximate."
n_lg <- "Size of the largest or longest organ scored as a maximum (Jaman_2020 question 5)."
n_lobe <- "Pinnule lobes cut almost to the costule scored as segments (leaflet traits; Jaman_2020 question 3)."
n_hair <- "Hairs (not scales) on the costa, costules, costulets or veins scored as leaf hairs (Jaman_2020 question 2)."
n_uneq <- "'unequal' base scored as oblique."
tr <- "trunk"
sti <- "stipe"
lam <- "lamina"
pa <- "pinnae"
mp <- "middle pinnae"
pi <- "pinnules"
pmp <- "pinnules of middle pinnae"
lp <- "largest pinnules"
seg <- "lamina segments"
lob <- "pinnule lobes"
axes <- "pinna axes and veins"

hairy <- function(piece) {
  k(piece, "leaf_hairs_adult_leaves", "hairs", "hairy", em = axes,
    notes = n_hair)
}
crenate <- function(piece, em = seg) {
  k(piece, "leaf_margin", "crenate", "toothed_crenate", em = em)
}
bip <- function(piece, value = "bipinnate", clean = value) {
  bind_rows(
    k(piece, "leaf_lamina_division", value, clean),
    k(piece, "leaf_compoundness", value, "compound")
  )
}

scored <- list(
  "Cyatheaceae" = bind_rows(
    k("Scaly tree ferns", "plant_growth_form", "tree_ferns", "fern palmoid",
      notes = "Tree fern scored as fern palmoid (SKILL.md step 4, Ferns)."),
    k("laminae 1–3-pinnate (rarely simple, not in our area)",
      "leaf_lamina_division", "1–3-pinnate",
      "pinnately_compound bipinnate tripinnate",
      notes = "'1–3-pinnate' expanded to the three levels; 'rarely simple' is outside the region (Regions rule) and not scored."),
    k("laminae 1–3-pinnate (rarely simple, not in our area)",
      "leaf_compoundness", "pinnate", "compound",
      notes = "'rarely simple' is outside the region (Regions rule) and not scored.")
  ),
  "Cyathea alleniae" = bind_rows(
    num("Trunk to 4.5 m", "plant_height", em = tr, notes = n_tr),
    num("15 cm in diameter", "stem_diameter", em = tr, notes = n_dia),
    num("Stipes at least 30 cm", "petiole_length", txt = "30 cm", em = sti,
        vt = "minimum", notes = paste(n_st, "Printed 'at least'.")),
    num("Laminae: pinnae to 70 cm long", "leaflet_length", em = pa),
    k("short-acuminate", "leaf_apex_shape", "short-acuminate", "acuminate",
      em = pi),
    lw("largest 14–15 × 3 cm", "14–15 cm", "3 cm", em = lp,
       notes = "'largest' follows 'pinnules' in the same clause."),
    crenate("lamina-segments deeply crenate"),
    hairy(c("distal half at least bearing crisped hairs on lower surface",
            "costules bearing some hairs as costa on lower surface"))
  ),
  "Cyathea alternans" = bind_rows(
    num("Trunk 1.5(–2) m tall", "plant_height", em = tr, notes = n_tr),
    num("c. 8 fronds in crown", "leaf_count", txt = "c. 8 mm",
        unit = "{count}",
        notes = "Fronds in the crown counted as leaves (Jaman_2020 question 1)."),
    num("36–66 cm", "petiole_length", em = sti, notes = n_st),
    bip("Laminae bipinnate"),
    num("(1–)2.1–2.5 m long", "leaf_length", em = lam, notes = n_la),
    num("the segments or pinnules about 7–8 mm wide", "leaflet_width",
        txt = "7–8 mm", em = "pinnules or segments", notes = n_ab),
    k("apex rounded or in the largest fronds sometimes acute",
      "leaf_apex_shape", "rounded", q = "usually", em = "pinnules or segments"),
    k("apex rounded or in the largest fronds sometimes acute",
      "leaf_apex_shape", "acute", q = "sometimes", em = "pinnules or segments",
      notes = "Acute in the largest fronds."),
    hairy("Scales and hairs: on costules and veins beneath are a few hairs and also narrow to ovate acuminate usually pale scales with setose edges")
  ),
  "Cyathea borneensis" = bind_rows(
    num("Trunk 1.2–2(–4) m", "plant_height", em = tr, notes = n_tr),
    num("8–10 cm diameter", "stem_diameter", em = tr, notes = n_dia),
    num("Stipes 20–60(–100) cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae to 250 cm long", "leaf_length", em = lam, notes = n_la),
    num("longest pinnae about 55 cm long", "leaflet_length", txt = "55 cm",
        em = "longest pinnae", notes = n_ab),
    num("18 cm wide", "leaflet_width", em = "longest pinnae", notes = n_ab),
    k("caudately acuminate at apex", "leaf_apex_shape", "caudately_acuminate",
      "acuminate", em = "longest pinnae"),
    hairy("sparsely hairy with pale crisped hairs and bearing very sparse pale brown scales"),
    num("pinnules more than 25 pairs", "leaflet_count", txt = "25 mm",
        unit = "pairs", vt = "minimum", em = "pinnules per pinna",
        notes = "Printed 'more than 25 pairs': scored as a minimum of 25 pairs."),
    lw("to 9 × 1.5 cm", "to 9 cm", "to 1.5 cm", em = pi),
    k("lanceolate", "leaf_shape", "lanceolate", em = pi),
    k("gradually narrowing towards acuminate apex", "leaf_apex_shape",
      "acuminate", em = pi),
    k("truncate at base", "leaf_base_shape", "truncate", em = pi),
    k("falcate", "leaf_shape", "falcate", em = lob, notes = n_lobe),
    k("round at apex", "leaf_apex_shape", "round", "rounded", em = lob,
      notes = n_lobe),
    lw("8–10 × 3–4 mm", "8–10 mm", "3–4 mm", em = lob, notes = n_lobe)
  ),
  "Cyathea bunnemeijeri" = bind_rows(
    num("Trunk c. 3 m tall", "plant_height", em = tr, notes = n_tr),
    num("at least 30 cm long", "petiole_length", txt = "30 cm", em = sti,
        vt = "minimum", notes = paste(n_st, "Printed 'at least'.")),
    lw("pinnules to 70 × 16 mm", "to 70 mm", "to 16 mm", em = pi),
    k("falcate drying very dark", "leaf_shape", "falcate", em = seg),
    crenate("edges crenate")
  ),
  "Cyathea contaminans" = bind_rows(
    num("3–6(–10) m or more and much thickened by adventitious roots at base",
        "plant_height", txt = "3–6(–10) m", em = tr,
        notes = paste(n_tr, "Printed 'or more' after the extreme.")),
    num("Stipes 60–155 cm long", "petiole_length", em = sti, notes = n_st),
    bip("Laminae bipinnate"),
    num("230–350 cm long", "leaf_length", em = lam, notes = n_la),
    num("largest 70 cm", "leaflet_length", txt = "70 cm", em = pa,
        vt = "maximum", notes = n_lg),
    lw("pinnules to c. 150 × 30 mm", "to 150 mm", "to 30 mm", em = pi,
       notes = n_to_c),
    k("often falcate", "leaf_shape", "falcate", q = "often", em = seg),
    k("glaucous beneath", "leaf_glaucousness", "glaucous", em = seg,
      notes = "Lower surface."),
    hairy("a few hairs normally present towards apex of pinnules on both costules and costulets")
  ),
  "Cyathea excavata" = bind_rows(
    num("Trunk 1.5–3 m tall", "plant_height", em = tr, notes = n_tr),
    num("40–70 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae (1.7–)2.3–2.7(–3.4) m long", "leaf_length", em = lam,
        notes = n_la),
    k("glabrous on both surfaces", "leaf_hairs_adult_leaves", "glabrous",
      em = lam),
    lw("middle pinnae about 50–60 × 16 cm", "50–60 cm", "16 cm", em = mp,
       notes = n_ab),
    lw("pinnules of middle pinnae about 8–10(–13) × 1.8–2.2(–2.7) cm",
       "8–10(–13) cm", "1.8–2.2(–2.7) cm", em = pmp, notes = n_ab),
    k("base unequally truncate", "leaf_base_shape", "unequally truncate",
      "truncate oblique", em = pi, notes = n_uneq),
    num("about 2.5 mm wide", "leaflet_width", txt = "2.5 mm", em = lob,
        notes = paste(n_lobe, n_ab)),
    k("rounded at the apex", "leaf_apex_shape", "rounded", em = lob,
      notes = n_lobe),
    k("edges entire", "leaf_margin", "entire", em = lob, notes = n_lobe),
    hairy("on veins many conspicuous very small hairs (bases of former scales)")
  ),
  "Cyathea gigantea" = bind_rows(
    num("Trunk up to a meter tall (outside our area to 5 m tall)",
        "plant_height", txt = "to 1 m", em = tr,
        notes = paste(n_tr, "'up to a meter' read as a maximum of 1 m; the height outside the region is not scored (Regions rule).")),
    lw("middle pinnae to c. 45 × 18 cm", "to 45 cm", "to 18 cm", em = mp,
       notes = n_to_c),
    lw("pinnules of middle pinnae to c. 12 × 2 cm", "to 12 cm", "to 2 cm",
       em = pmp, notes = n_to_c),
    k("cuneate", "leaf_base_shape", "cuneate", em = pi,
      notes = "'the base slightly unequal' not scored as oblique ('slightly')."),
    k("apex acuminate", "leaf_apex_shape", "acuminate", em = pi)
  ),
  "Cyathea glabra" = bind_rows(
    num("Trunk 1–3 m tall", "plant_height", em = tr, notes = n_tr),
    num("12–20 cm diameter", "stem_diameter", em = tr, notes = n_dia),
    num("Stipes 85–115 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae 110–200 cm long", "leaf_length", em = lam, notes = n_la),
    lw("middle pinnae to 55 × 20 cm", "to 55 cm", "to 20 cm", em = mp),
    lw("more often c. 45 × 15 cm", "c. 45 cm", "c. 15 cm", em = mp,
       q = "more often"),
    lw("pinnules of middle pinnae to 12 × 2 cm", "to 12 cm", "to 2 cm",
       em = pmp),
    lw("more often c. 9 × 1.5 cm and occasionally smaller", "c. 9 cm",
       "c. 1.5 cm", em = pmp, q = "more often"),
    k(c("unequal at base", "broadly rounded or subtruncate"),
      "leaf_base_shape", "unequal rounded", "oblique rounded", em = pi,
      notes = paste(n_uneq, "'subtruncate' (near-truncate) not scored."))
  ),
  "Cyathea hymenodes" = bind_rows(
    num("Trunk (0.8–)1.7–2(–3) m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 22–90 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae (70–)128–220 cm long", "leaf_length", em = lam, notes = n_la),
    num("longest 50 cm long", "leaflet_length", txt = "50 cm",
        em = "longest pinnae", vt = "maximum", notes = n_lg),
    lw("70–100 × 12–20 mm", "70–100 mm", "12–20 mm", em = lp),
    crenate("edges crenate"),
    hairy(c("Scales and hairs: short crisped hairs more-or-less abundant on distal part of lower surface of costa",
            "upper surface of costules always hairy"))
  ),
  "Cyathea incisoserrata" = bind_rows(
    num("Trunk 1.5–3.5 m tall", "plant_height", em = tr, notes = n_tr),
    num("12 cm diameter including leaf bases", "stem_diameter", em = tr,
        notes = paste(n_dia, "Includes the leaf bases.")),
    num("Stipes 50–140 cm", "petiole_length", em = sti, notes = n_st),
    num("Laminae 150–350 cm long", "leaf_length", em = lam, notes = n_la),
    lw("pinnules commonly to 85 × 25 mm", "to 85 mm", "to 25 mm", em = pi,
       q = "commonly"),
    lw("sometimes to 130 × 35 mm", "to 130 mm", "to 35 mm", em = pi,
       q = "sometimes"),
    k("acuminate", "leaf_apex_shape", "acuminate", em = pi),
    k("strongly serrate", "leaf_margin", "serrate", "toothed_serrate", em = seg)
  ),
  "Cyathea latebrosa" = bind_rows(
    num("Trunk 1.3–3 m", "plant_height", em = tr, notes = n_tr),
    num("Stipes 50–170 cm", "petiole_length", em = sti, notes = n_st),
    num("Laminae 100–350 cm", "leaf_length", em = lam, notes = n_la),
    lw("middle pinnae to c. 75 × 25 cm", "to 75 cm", "to 25 cm", em = mp,
       notes = n_to_c),
    lw("pinnules of middle pinnae to c. 11 × 1.8 cm", "to 11 cm", "to 1.8 cm",
       em = pmp, notes = n_to_c),
    k("base broadly cuneate", "leaf_base_shape", "cuneate", em = pi),
    k("acuminate", "leaf_apex_shape", "acuminate", em = pi,
      notes = "The alternative 'or sometimes more shortly pointed' has no level."),
    num("lobes 2–3 mm wide", "leaflet_width", em = lob, notes = n_lobe),
    k("moderately falcate", "leaf_shape", "falcate", em = lob, notes = n_lobe),
    k("the apex rounded", "leaf_apex_shape", "rounded", em = lob,
      notes = n_lobe),
    k("edges crenulate to sharply toothed", "leaf_margin", "crenulate toothed",
      "toothed_crenate toothed", em = lob, notes = n_lobe)
  ),
  "Cyathea lurida" = bind_rows(
    num("0.7–1.3(–3) m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 85–200 cm", "petiole_length", em = sti, notes = n_st),
    num("Laminae 170–280 cm", "leaf_length", em = lam, notes = n_la),
    num("pinnae commonly to 50 cm long", "leaflet_length", em = pa,
        q = "commonly"),
    num("rarely to 75 cm", "leaflet_length", em = pa, q = "rarely"),
    lw("largest sterile pinnules 75–100 × 15–25 mm", "75–100 mm", "15–25 mm",
       em = "largest sterile pinnules"),
    lw("60–90 × 6–12(–17) mm wide", "60–90 mm", "6–12(–17) mm",
       em = "fertile pinnules")
  ),
  "Cyathea moluccana" = bind_rows(
    num("c. 50 cm tall", "plant_height", em = tr,
        notes = paste(n_tr, "'often short' not scored.")),
    num("Stipes 30–90 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae 95–200 cm long", "leaf_length", em = lam, notes = n_la),
    bip("simply pinnate", "simply_pinnate", "pinnately_compound"),
    k("pinnae entire", "leaf_margin", "entire", em = pa,
      notes = "The later clause adds 'toothed towards the apex' (kept unmapped)."),
    lw("pinnae sessile or on stalks to about 4 mm long (longest median pinnae occasionally to 7 mm and lowest pinnae to 15 mm) 12–30 × 2–4 cm",
       "12–30 cm", "2–4 cm", em = pa),
    k("base rather unequally cuneate or truncate", "leaf_base_shape",
      "cuneate truncate", em = pa,
      notes = "'rather unequally' not scored as oblique."),
    k("apex acuminate", "leaf_apex_shape", "acuminate", em = pa)
  ),
  "Cyathea obscura" = bind_rows(
    num("Trunk 1.2–2.5 m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 45–95 cm long", "petiole_length", em = sti, notes = n_st),
    k("Laminae usually bipinnatifid", "leaf_lamina_division", "bipinnatifid",
      q = "usually"),
    num("125–310 cm long", "leaf_length", em = lam, notes = n_la),
    lw("middle pinnae to c. 55 × 15 cm", "to 55 cm", "to 15 cm", em = mp,
       notes = n_to_c),
    lw("pinnules of middle pinnae to c. 8 × 1.5 cm", "to 8 cm", "to 1.5 cm",
       em = pmp, notes = n_to_c),
    k("base unequal and more-or-less truncate", "leaf_base_shape", "unequal",
      "oblique", em = pi,
      notes = paste(n_uneq, "'more-or-less truncate' is hedged and not scored.")),
    k("apex acute", "leaf_apex_shape", "acute", em = pi),
    hairy("mid-vein densely covered with hairs")
  ),
  "Cyathea oosora" = bind_rows(
    num("Stipes 50 cm or more", "petiole_length", txt = "50 cm", em = sti,
        vt = "minimum", notes = paste(n_st, "Printed 'or more'.")),
    num("longest pinnae to 60 cm long", "leaflet_length", em = "longest pinnae"),
    lw("largest pinnules c. 90 × 20 mm", "c. 90 mm", "c. 20 mm", em = lp),
    k("acuminate", "leaf_apex_shape", "acuminate", em = pi),
    crenate("edges crenate")
  ),
  "Cyathea polypoda" = bind_rows(
    num("Trunk to 3 m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes to 80 cm long", "petiole_length", em = sti, notes = n_st),
    bip("Laminae bipinnate"),
    k("shiny deep green when living", "leaf_surface_colour", "deep_green",
      "dark_green", em = lam),
    num("pinnae to 60 cm long", "leaflet_length", em = pa),
    lw("pinnules commonly c. 85 × 20 mm", "c. 85 mm", "c. 20 mm", em = pi,
       q = "commonly"),
    lw("sometimes to 110 × 27 mm", "to 110 mm", "to 27 mm", em = pi,
       q = "sometimes"),
    k("the base truncate and slightly unequal", "leaf_base_shape", "truncate",
      em = pi, notes = "'slightly unequal' not scored as oblique."),
    num("lobes 4–5 mm wide", "leaflet_width", em = lob, notes = n_lobe),
    k("apex rounded", "leaf_apex_shape", "rounded", em = lob, notes = n_lobe),
    k("margin faintly toothed", "leaf_margin", "toothed", em = lob,
      notes = n_lobe)
  ),
  "Cyathea recommutata" = bind_rows(
    k("Small and slender tree fern", "plant_growth_form", "tree_fern",
      "fern palmoid",
      notes = "Tree fern scored as fern palmoid (SKILL.md step 4, Ferns)."),
    num("trunk 1–3 m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 50–77 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae 110–195 cm long", "leaf_length", em = lam, notes = n_la),
    lw("base of stipe bearing a few or several reduced pinnae 1.5–7.5 × 1–2 cm",
       "1.5–7.5 cm", "1–2 cm", em = "reduced basal pinnae", q = "usually",
       notes = "Reduced pinnae at the stipe base (Jaman_2020 question 4)."),
    num("largest to 10 cm long", "leaflet_length", em = "reduced basal pinnae",
        q = "extreme_range",
        notes = "Reduced pinnae at the stipe base (Jaman_2020 question 4). 'largest to 10 cm' beyond the typical 1.5--7.5 cm scored as an extreme."),
    lw("middle pinnae to c. 50 × 15 cm", "to 50 cm", "to 15 cm", em = mp,
       notes = n_to_c),
    lw("fertile pinnules contracted", "40–80 mm", "6–12 mm",
       em = "fertile pinnules",
       notes = "Dimensions in the next piece '40–80 × 6–12 mm'.") |>
      mutate(piece = list(c("fertile pinnules contracted", "40–80 × 6–12 mm"))),
    k("base truncate to cordate", "leaf_base_shape", "truncate cordate",
      em = "fertile pinnules"),
    k("apex acuminate", "leaf_apex_shape", "acuminate", em = "fertile pinnules"),
    lw("sterile pinnules to 9 × 1.8 cm", "to 9 cm", "to 1.8 cm",
       em = "sterile pinnules")
  ),
  "Cyathea squamulata" = bind_rows(
    num("Trunk 1.2–2 m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 45–80 cm long", "petiole_length", em = sti, notes = n_st),
    k("Laminae bipinnatifid to bipinnate", "leaf_lamina_division",
      "bipinnatifid bipinnate"),
    num("largest pinnae to 50 cm long", "leaflet_length", em = "largest pinnae"),
    lw("middle pinnae c. 50 × 15 cm", "c. 50 cm", "c. 15 cm", em = mp),
    lw("pinnules of middle pinnae to c. 11 × 2 cm (usually c. 8 × 1.6 cm)",
       "to 11 cm", "to 2 cm", em = pmp, notes = n_to_c),
    lw("pinnules of middle pinnae to c. 11 × 2 cm (usually c. 8 × 1.6 cm)",
       "c. 8 cm", "c. 1.6 cm", em = pmp, q = "usually",
       notes = "From the aside '(usually c. 8 × 1.6 cm)'."),
    k("base unequal truncate", "leaf_base_shape", "unequal truncate",
      "oblique truncate", em = pi, notes = n_uneq),
    k("apex rather shortly acute", "leaf_apex_shape", "acute", q = "usually",
      em = pi),
    k("occasionally acuminate", "leaf_apex_shape", "acuminate",
      q = "occasionally", em = pi),
    hairy("a few stout hairs present on upper surface of costulets")
  ),
  "Cyathea sumatrana" = bind_rows(
    num("Trunk to c. 1 m", "plant_height", txt = "to 1 m", em = tr,
        notes = paste(n_tr, n_to_c)),
    num("Stipes 20–60 cm long", "petiole_length", em = sti, notes = n_st),
    num("Laminae: longest pinnae 60 cm long", "leaflet_length", txt = "60 cm",
        em = "longest pinnae", vt = "maximum", notes = n_lg),
    lw("8–11 × 2 cm", "8–11 cm", "2 cm", em = "lowest pinnae",
       notes = "Lowest pinnae, reduced (Jaman_2020 question 4)."),
    lw("pinnules to 100 × 25 mm", "to 100 mm", "to 25 mm", em = pi),
    num("lobe 3–4 mm wide", "leaflet_width", em = lob, notes = n_lobe),
    crenate("crenate"),
    hairy(c("Scales and hairs: lower surface of costa covered throughout with short flexuous brown hairs",
            "upper surface of costules bearing dark antrorse hairs throughout"))
  ),
  "Cyathea trichodesma" = bind_rows(
    num("1–3 m tall", "plant_height", em = tr, notes = n_tr),
    num("(20–)60–100 cm", "petiole_length", em = sti, notes = n_st),
    bip("Laminae bipinnate"),
    num("230–300 cm long", "leaf_length", em = lam, notes = n_la),
    num("largest pinnae usually to c. 60 cm", "leaflet_length", txt = "to 60 cm",
        em = "largest pinnae", q = "usually", notes = n_to_c),
    num("occasionally to 85 cm", "leaflet_length", em = "largest pinnae",
        q = "occasionally"),
    lw("pinnules commonly to 90 × 15 mm", "to 90 mm", "to 15 mm", em = pi,
       q = "commonly"),
    lw("largest seen 125 × 25 mm", "125 mm", "25 mm", em = pi, vt = "maximum",
       notes = paste(n_lg, "Printed 'largest seen'.")),
    k("shortly acuminate", "leaf_apex_shape", "acuminate", em = pi),
    hairy(c("Scales and hairs: upper surface of costulets and veins with hairs present",
            "costae", "costules", "costulets and veins on lower surface bearing many pale spreading hairs 1–2 mm long"))
  ),
  "Cyathea tripinnata" = bind_rows(
    num("Trunk 3–5 m tall", "plant_height", em = tr, notes = n_tr),
    num("Stipes 40–110 cm long", "petiole_length", em = sti, notes = n_st),
    bip("Laminae tripinnate", "tripinnate"),
    num("c. 265 cm long", "leaf_length", em = lam, notes = n_la),
    num("lowest pinnae 20–30 cm long", "leaflet_length", em = "lowest pinnae"),
    num("middle pinnae 60–75 cm", "leaflet_length", em = mp),
    lw("pinnules usually 100 × 18 mm", "100 mm", "18 mm", em = pi,
       q = "usually"),
    lw("sometimes to 140 × 25 mm", "to 140 mm", "to 25 mm", em = pi,
       q = "sometimes"),
    k("apex shortly acuminate", "leaf_apex_shape", "acuminate", em = pi),
    lw("lamina segments 6–9(–15) × 2(–3.5) mm", "6–9(–15) mm", "2(–3.5) mm",
       em = seg),
    crenate("crenate edges")
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

# Organ of a run of unscored pieces: from its first piece, else its clause and
# sentence heads; sub-organs of the lamina (pinnae, pinnules, lobes, veins,
# axes) are named so the summary shows which parts lack traits.
organ_re <- c(
  "^(Scales and hairs|Scales and hairs:)" = "scales and hairs",
  "^(Trunk|trunk|old fronds|rachises of old fronds|hidden by|small branches|several smaller crowns|with a V-shaped)" = "trunk",
  "^Small and slender" = "plant",
  "^(Stipes?|base of stipe|basal scales|base densely)" = "stipe",
  "^(pneumathodes|Pneumathodes)" = "pneumathodes",
  "^(main rachis|rachis|rachises|Rachis|Laminae: rachises)" = "rachis",
  "^(costa|costae|Costa|lower surface of costa|upper surface of costa)" = "costa",
  "^(costules|costular|scales on costule|scales at base|scales near|on costules)" = "costules",
  "^(costulets|costulet|scales on costulets|scales on the costulets|bullate scales|thinner similar|sub-bullate)" = "costulets",
  "^(veins|Veins|on veins)" = "veins",
  "^(lobes?|lobe basal|midrib strongly)" = "pinnule lobes",
  "^(lamina-segments|lamina segments|segments|segment|lowest segment)" = "lamina segments",
  "^(Laminae: pinnae|pinnae|lower pinnae|lowest pinnae|middle pinnae|median|longest pinnae|largest pinnae|fertile pinnae|distal part of pinnae|the reduced pinnae|base of stipe bearing|Fronds of branch-crowns)" = "pinnae",
  "^(pinnules|largest|lowest pinnule|fertile pinnules|sterile pinnules|stalks of lower)" = "pinnules",
  "^(Laminae|laminae|Fronds|simply pinnate|thinly|thin but)" = "lamina",
  "^(Sori|sori)" = "sori",
  "^(indusi|without indusi)" = "indusia",
  "^(receptacles?)" = "receptacle",
  "^(paraphyses|dark paraphyses)" = "paraphyses",
  "^(Sporangia)" = "sporangia",
  "^(Spores)" = "spores",
  "^(Gametophytes)" = "gametophyte",
  "^(Stems|Scales present)" = "stem and scales",
  "^(scales|linear scales|largest scales|Scales)" = "scales"
)
organ_of <- function(x) {
  hit <- which(str_detect(x, names(organ_re)))
  if (length(hit)) organ_re[[hit[1]]] else NA_character_
}
character_of <- function(x) {
  ch <- c(
    if (str_detect(x, "\\d")) "size or number",
    if (str_detect(x, "\\b(green|brown|black|dark|pale|purplish|ferruginous|golden|glaucous|white|colour)\\b")) "colour",
    if (str_detect(x, "hairs?|hairy|glabr|setae|setose|ciliat")) "indumentum",
    if (str_detect(x, "scal|bullate|flabell|setifer")) "scales",
    if (str_detect(x, "spin|warty|rough|thorny|smooth")) "surface or spines",
    if (str_detect(x, "lobed|free|stalk|sessile|pinnate|dimorph|reduced")) "division or attachment",
    if (str_detect(x, "late|shaped|falcate|ovate|oblique|crenate|serrate|toothed|entire|apex|base")) "shape or margin"
  )
  if (length(ch)) paste(ch, collapse = ", ") else "form"
}

desc_unmapped <- pieces |>
  filter(!pid %in% unlist(scored_rows$pid)) |>
  group_by(block, rank, sent, clause) |>
  mutate(run = cumsum(c(1, diff(piece_i) != 1))) |>
  group_by(block, rank, sent, clause, run) |>
  summarise(
    trait_value_raw = paste(piece, collapse = ", "),
    first_piece = first(piece),
    chead = first(clause_head),
    head = first(sentence_head),
    pdf_page = first(pdf_page),
    printed_page = first(printed_page),
    .groups = "drop"
  ) |>
  mutate(
    organ = coalesce(
      map_chr(first_piece, organ_of),
      map_chr(chead, organ_of),
      map_chr(head, organ_of),
      "plant"
    ),
    notes = paste0(
      "UNMAPPED: ",
      organ,
      " | description clause | ",
      map_chr(trait_value_raw, character_of)
    ),
    source_section = "Description"
  ) |>
  select(block, rank, trait_value_raw, pdf_page, printed_page, notes,
         source_section)

# ---- other sections ----------------------------------------------------------------

# One unmapped row per sentence of every labelled section of the family, genus
# and species (Wong_2010 pattern). Nomenclature and vernacular names are not
# trait text; types are in raw/Jaman_2020_vouchers.csv.
sec_paras <- paras |>
  filter(
    rank %in% c("family", "genus", "species"),
    status == "accepted",
    !section %in% c("Nomenclature", "Description", "Vernacular names",
                    "Vernacular name", "English name")
  )
sec_unmapped <- map(seq_len(nrow(sec_paras)), \(r) {
  p <- sec_paras[r, ]
  s <- flora_split_sentences(p$text_n)
  map(s, \(x) {
    pg <- page_of(p, x)
    tibble(
      block = p$block,
      rank = p$rank,
      trait_value_raw = x,
      pdf_page = pg$pdf,
      printed_page = pg$printed,
      notes = paste0("UNMAPPED: ", str_to_lower(p$section), " | text"),
      source_section = p$section
    )
  }) |>
    bind_rows()
}) |>
  bind_rows()

# Scored statements from later sections. The sentence stays an unmapped row
# too, since its other clauses are not scored.
sec <- function(taxon, section, fragment, trait, value, clean = value,
                em = NA, q = NA, notes = NA) {
  p <- paras |> filter(block == taxon, .data$section == .env$section)
  stopifnot(nrow(p) == 1, str_detect(p$text_n, fixed(fragment)))
  pg <- page_of(p, fragment)
  tibble(
    block = taxon,
    rank = "species",
    trait,
    trait_value = value,
    trait_value_clean = clean,
    value_type = "mode",
    unit = NA_character_,
    cq = q,
    entity_measured = em,
    notes,
    trait_value_raw = fragment,
    pdf_page = pg$pdf,
    printed_page = pg$printed,
    source_section = section
  )
}
sec_scored <- bind_rows(
  sec("Cyathea trichodesma", "Notes",
      "readily distinguished by its soft and hairy olive-green fronds",
      "leaf_surface_colour", "olive-green", "green_olive", em = "frond",
      notes = "From the Notes paragraph.")
)

# ---- copy-down and assembly -----------------------------------------------------------

desc_all <- bind_rows(own_desc, sec_scored)
higher <- desc_all |> filter(rank %in% c("family", "genus"))
own <- desc_all |> filter(rank == "species")

members <- tibble(
  block = species,
  genus = "Cyathea",
  group = NA_character_,
  species = species,
  rank = "species"
)
# "Scaly tree ferns" is one term mapped to two levels (fern palmoid)
# (Jaman_2020 question 7).
copied <- flora_copy_down(
  higher |> mutate(single_term = trait == "plant_growth_form"),
  own,
  members
)
rejected <- attr(copied, "rejected")
if (nrow(rejected)) print(rejected)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  higher |> mutate(rank_scored_from = NA_character_),
  copied,
  desc_unmapped,
  sec_unmapped
) |>
  transmute(
    taxon_name = block,
    taxon_rank = case_when(
      block == "Cyatheaceae" ~ "family",
      block == "Cyathea" ~ "genus",
      .default = "species"
    ),
    rank_scored_from,
    commonness_qualifier = cq,
    # FPM species descriptions describe the species in Peninsular Malaysia;
    # family and genus values and copied rows have no region.
    population_region = if_else(
      taxon_name %in% species & is.na(rank_scored_from),
      PM,
      NA_character_
    ),
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
  flora_merge_duplicate_keys() |>
  flora_finalise(traits) |>
  arrange(taxon_name, is.na(trait), trait)

stopifnot(all(out$trait[!is.na(out$trait)] %in% names(traits)))
write_csv(out, "data/Jaman_2020/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/Jaman_2020/raw/unmapped_terms.csv",
  na = ""
)

n_mapped <- out |>
  filter(!is.na(trait)) |>
  count(taxon_name, name = "n_mapped_rows")
tibble(
  taxon_name = c("Cyatheaceae", "Cyathea", species, "Cyathea podophylla"),
  taxon_rank = c("family", "genus", rep("species", length(species) + 1)),
  status = c("accepted", "accepted", rep("accepted", length(species)),
             "excluded")
) |>
  left_join(n_mapped, by = "taxon_name") |>
  mutate(
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      taxon_name == "Cyathea" ~
        "Only genus; description holds no unqualified mapped value (all genus clauses unmapped).",
      taxon_name == "Cyathea podophylla" ~
        "Excluded: Peninsular Malaysian record (Iwatsuki 1995) assumed an error; no specimens seen. Not scored.",
      taxon_name == "Cyathea oosora" ~
        "Trunk not seen; one Peninsular Malaysian collection (P. Tioman, 1974).",
      taxon_name == "Cyathea alleniae" ~
        "Endemic; known only from the type collection. Branch-crown frond sizes kept unmapped.",
      taxon_name == "Cyathea bunnemeijeri" ~
        "Known in Peninsular Malaysia from one specimen (Kadim & Noor K 581).",
      .default = ""
    )
  ) |>
  write_csv("data/Jaman_2020/raw/taxon_flags.csv", na = "")
