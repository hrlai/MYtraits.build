# Build raw/trait_data.csv for Cheek & Jebb (2012), Nepenthaceae, Flora of
# Peninsular Malaysia Series II vol. 3: 249--275. Run from the repository root.
#
# Born-digital PDF: rows come from the PDF text layer (segment_source.R). The
# family is monogeneric with no family description ("Monogeneric family."), so
# the genus description holds the higher-rank values (SKILL.md step 5).
#
# Ten species, so rows are scored by hand (flora_hand() style) but keyed to the
# comma pieces of each description: every Description sentence is split into
# ";" clauses and "," pieces outside parentheses; a piece named in the tables
# below is scored, and every other piece is kept verbatim as an unmapped row
# (consecutive unscored pieces of one clause form one row). Most of each
# description is the pitcher, which has no trait in config/traits.yml.
# Hedged statements are not scored. Writes trait_data.csv, unmapped_terms.csv
# and taxon_flags.csv.
source("data/Cheek_2012/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Cheek_2012"
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
PM <- "Peninsular Malaysia"

fixes <- read_csv(
  "data/Cheek_2012/raw/source_fixes.csv",
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
hybrids <- paras |>
  filter(status == "hybrid") |>
  distinct(block) |>
  pull()

# ---- pieces ---------------------------------------------------------------------

# Page of a text fragment: where it starts within its paragraph. Positions in
# the normalised text differ from the segmented text by a few characters at
# most, which only matters at a page turn.
page_of <- function(p, fragment) {
  pos <- str_locate(p$text_n, fixed(str_sub(fragment, 1, 30)))[1, 1]
  pg <- flora_page_at(p, coalesce(pos, 1L))
  list(pdf = pg[["pdf"]], printed = pg[["printed"]])
}

desc_paras <- paras |> filter(section == "Description", status == "accepted")
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

# ---- scored pieces ------------------------------------------------------------------

# k(): categorical row; `piece` may name several pieces of one statement
# ("green", "brown or red"), joined in trait_value_raw.
# num(): numeric rows parsed from the piece (or from `txt` when the printed text
# needs a reading, e.g. "to at least 6 m"; counts are parsed from "4 mm" and
# their unit then set to {count}); `dim` = 1 (length) or 2 (width) of
# an "a x b" pair (first dimension is length; Cheek_2012 question 3).
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
num <- function(piece, trait, txt = piece, dim = NA, em = NA, q = NA,
              notes = NA) {
  r <- if (is.na(dim)) {
    flora_num_rows(flora_parse_range(txt))
  } else {
    flora_parse_dims(txt)[[c("len", "wid")[dim]]]
  }
  stopifnot(!is.null(r), nrow(r) > 0, !anyNA(r$unit))
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

# Notes used on many rows.
tep <- "Tepal length scored as sepal length (Nepenthes flowers have one whorl of tepals and no petals; Cheek_2012 question 4)."
valve <- "Capsule valve length scored as fruit length (Cheek_2012 question 4)."
climb_h <- "Height of a climbing plant (Cheek_2012 question 2)."
seedtex <- "Texture of the central seed body only (Cheek_2012 question 6)."
rl <- "rosette leaf"
cl <- "climbing-stem leaf"
mi <- "male inflorescence"
mf <- "male flower"

habit <- function(piece, h, substrate = "Terrestrial") {
  bind_rows(
    k(piece, "plant_growth_form", "climber"),
    k(piece, "plant_growth_substrate", str_to_lower(substrate), "terrestrial"),
    num(piece, "plant_height_climbing_plant", txt = h, notes = climb_h)
  )
}

scored <- list(
  "Nepenthes" = bind_rows(
    k("Carnivorous", "plant_alternative_energy_and_nutrient_acquisition_strategy", "carnivorous"),
    k("dioecious", "sex_type", "dioecious"),
    k("woody climbers", "plant_growth_form", "woody_climbers", "climber_woody",
      q = "usually"),
    k("or sometimes a sub-shrub (N. mirabilis)", "plant_growth_form",
      "sub-shrub", "subshrub", q = "sometimes"),
    k("terrestrial or epiphytic", "plant_growth_substrate",
      "terrestrial epiphytic", "terrestrial epiphyte"),
    k("Leaves spirally arranged", "leaf_arrangement", "spirally_arranged", "spiral"),
    k("simple", "leaf_compoundness", "simple"),
    k("Inflorescences terminal", "inflorescence_type", "terminal"),
    k("a paniculoid thyrse or raceme", "inflorescence_type",
      "paniculoid_thyrse raceme", "panicle raceme",
      notes = "Paniculoid thyrse scored as panicle (Cheek_2012 question 7)."),
    k("locules opening by longitudinal slits", "flower_anther_dehiscence",
      "longitudinal_slits", "longitudinal_slit"),
    k("extrorse", "flower_anther_orientation", "extrorse"),
    k("gynoecium lacking", "flower_structural_sex_type", "unisexual",
      notes = "Male flowers lack a gynoecium and female flowers an androecium."),
    k("androecium lacking", "flower_structural_sex_type", "unisexual",
      notes = "Male flowers lack a gynoecium and female flowers an androecium."),
    num("a single whorl of 4 tepals (also interpreted as two whorls of two)",
      "flower_perianth_parts_count", txt = "4 mm",
      notes = "Tepals counted as perianth parts.") |>
      mutate(unit = "{count}"),
    k("free or basally united", "flower_perianth_fusion_categorical",
      "free basally_united", "perianth_parts_unfused perianth_parts_fused_at_base"),
    k("nectariferous", "flower_pollinator_reward", "nectariferous", "nectar",
      notes = "Nectariferous tepals; the Reproductive biology paragraph adds that sessile glands cover the adaxial tepal surface with nectar."),
    k(c("green", "brown or red"), "flower_colour", "green brown red",
      "green red_brown", notes = "Tepal colour (female flowers; male perianth 'as in female') scored as flower colour."),
    k("ovary superior", "flower_ovary_position", "superior"),
    num("4-carpellate", "flower_structural_carpels_count", txt = "4 mm") |>
      mutate(unit = "{count}"),
    k("placentation lamellar", "flower_gynoecium_placentation", "lamellar",
      "laminar", notes = "Lamellar placentation scored as laminar (Cheek_2012 question 7)."),
    k("Fruit a loculicidally dehiscent capsule of 4 valves containing 50–500 seeds",
      "fruit_type", "capsule"),
    k("Fruit a loculicidally dehiscent capsule of 4 valves containing 50–500 seeds",
      "fruit_dehiscence", "dehiscent"),
    k("Seeds fusiform", "seed_shape", "fusiform"),
    num("3–25 mm long", "seed_length",
      notes = "Seed length includes the long basal and apical appendages.")
  ),
  "Nepenthes albomarginata" = bind_rows(
    habit("Terrestrial climber 2(–10) m tall", "2(–10) m"),
    k("narrowly oblanceolate to spathulate", "leaf_shape",
      "narrowly_oblanceolate spathulate"),
    k("margins entire", "leaf_margin", "entire"),
    num("basal rosette leaves 20–36 × 2–3.5 cm", "leaf_length", dim = 1, em = rl),
    num("basal rosette leaves 20–36 × 2–3.5 cm", "leaf_width", dim = 2, em = rl),
    num("leaves on climbing stems 7–17 × 1.2–2 cm", "leaf_length", dim = 1, em = cl),
    num("leaves on climbing stems 7–17 × 1.2–2 cm", "leaf_width", dim = 2, em = cl),
    k("apex acute", "leaf_apex_shape", "acute", em = cl),
    num("Male inflorescences 11–45 cm long", "inflorescence_length", em = mi),
    num("tepals 1.75–2.5 × 1.2–1.5 mm", "flower_sepal_length", dim = 1, em = mf,
      notes = tep),
    num("Fruits: valves 25–29 × 2.2–2 mm", "fruit_length", dim = 1, notes = valve),
    num("Seeds 10–12 mm long", "seed_length")
  ),
  "Nepenthes ampullaria" = bind_rows(
    habit("Terrestrial climber to 15 m tall", "to 15 m"),
    k("laminas lanceolate to spathulate", "leaf_shape", "lanceolate spathulate"),
    k("margins entire", "leaf_margin", "entire"),
    num("rosette leaves 2–5 × c. 0.5 cm", "leaf_length", dim = 1, em = rl),
    num("rosette leaves 2–5 × c. 0.5 cm", "leaf_width", dim = 2, em = rl),
    num("climbing leaves c. 25 × 6 cm", "leaf_length", txt = "c. 25 cm", em = cl),
    num("climbing leaves c. 25 × 6 cm", "leaf_width", txt = "c. 6 cm", em = cl,
      notes = "'c.' read as applying to both dimensions."),
    k("base attenuate", "leaf_base_shape", "attenuate"),
    k("apex acute", "leaf_apex_shape", "acute", q = "usually"),
    k("rarely acuminate", "leaf_apex_shape", "acuminate", q = "rarely"),
    k("Male inflorescences paniculate to 40 cm long", "inflorescence_type",
      "paniculate", "panicle", em = mi),
    num("Male inflorescences paniculate to 40 cm long", "inflorescence_length",
      txt = "to 40 cm", em = mi),
    k("tepals green to yellow broadly elliptic", "flower_colour",
      "green yellow", "green yellow_orange", em = mf,
      notes = "Tepal colour scored as flower colour."),
    num("4–5 × 3–5 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("Fruits 25–30 mm long", "fruit_length"),
    num("Seeds 12–15 mm long", "seed_length")
  ),
  "Nepenthes benstonei" = bind_rows(
    habit("Terrestrial climber to 10 m tall", "to 10 m"),
    k("sparse on stem", "stem_hairs", "hairs", "hairy",
      notes = "Simple white hairs to 1 mm long, sparse on the stem."),
    k("Leaves in life dull green with a white-blue waxy (glaucous) sheen",
      "leaf_glaucousness", "glaucous"),
    k("broadly linear-lanceolate to slightly spathulate", "leaf_shape",
      "linear-lanceolate spathulate", "linear lanceolate spathulate",
      notes = "Hyphenated shape scored as both levels; 'broadly' and 'slightly' not scored."),
    num("lower leaf laminas 25–30(–60) × 4–5(–9) cm", "leaf_length", dim = 1,
      em = "lower leaf"),
    num("lower leaf laminas 25–30(–60) × 4–5(–9) cm", "leaf_width", dim = 2,
      em = "lower leaf"),
    k("upper surface with white hairs to 1 mm long", "leaf_hairs_adult_leaves",
      "hairs", "hairy", em = "lower leaf upper surface"),
    num("upper laminas to 20 × 4.5 cm", "leaf_length", txt = "to 20 cm",
      em = "upper leaf"),
    num("upper laminas to 20 × 4.5 cm", "leaf_width", txt = "to 4.5 cm",
      em = "upper leaf"),
    k("to a broad amplexicaul sheath clasping the stem for half to three-quarters of its circumference",
      "leaf_base_shape", "sheath", "sheathing",
      notes = "Amplexicaul sheath scored as sheathing (Cheek_2012 question 8)."),
    k("margins entire", "leaf_margin", "entire"),
    k("apex rounded to acute@1", "leaf_apex_shape", "rounded acute"),
    num("male inflorescences to 50 cm long", "inflorescence_length", em = mi),
    k("tepals green", "flower_colour", "green", em = mf,
      notes = "Tepal colour scored as flower colour."),
    num("c. 4 × 3.5 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("Fruits c. 16 mm long", "fruit_length"),
    num("Seeds 7–10 mm long", "seed_length")
  ),
  "Nepenthes gracilis" = bind_rows(
    habit("Terrestrial climber 2(–5) m tall", "2(–5) m"),
    k("Indumentum absent from stems and upper surface of leaves", "stem_hairs",
      "absent", "glabrous"),
    k("Indumentum absent from stems and upper surface of leaves",
      "leaf_hairs_adult_leaves", "absent", "glabrous", em = "upper surface"),
    k("laminas of the climbing stems narrowly lanceolate", "leaf_shape",
      "narrowly_lanceolate", em = cl),
    num("(6.5–)12–19 × (1–)1.5–2.8(–3.7) cm", "leaf_length", dim = 1, em = cl),
    num("(6.5–)12–19 × (1–)1.5–2.8(–3.7) cm", "leaf_width", dim = 2, em = cl),
    k("apex acute", "leaf_apex_shape", "acute", q = "usually", em = cl),
    k("rarely acuminate", "leaf_apex_shape", "acuminate", q = "rarely", em = cl),
    num("basal rosette leaves to 3 cm long", "leaf_length", em = rl),
    num("Male inflorescences (9.5–)15–22(–30) cm long", "inflorescence_length",
      em = mi),
    num("2.5–5 × 1.5–2.5 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("Seeds c. 15 mm long", "seed_length"),
    k("papillate at the centre", "seed_surface_texture", "papillate", "bumpy",
      notes = seedtex)
  ),
  "Nepenthes gracillima" = bind_rows(
    k("Terrestrial climber", "plant_growth_form", "climber"),
    k("Terrestrial climber", "plant_growth_substrate", "terrestrial"),
    num("1–5 m tall", "plant_height_climbing_plant", notes = climb_h),
    k("laminas of climbing stems lanceolate to oblanceolate", "leaf_shape",
      "lanceolate oblanceolate", em = cl),
    num("5–10(–16) × 1–1.5(–2) cm", "leaf_length", dim = 1, em = cl),
    num("5–10(–16) × 1–1.5(–2) cm", "leaf_width", dim = 2, em = cl),
    k("glabrous", "leaf_hairs_adult_leaves", "glabrous"),
    k("base cuneate", "leaf_base_shape", "cuneate", em = cl),
    k("slightly auriculate", "leaf_base_shape", "auriculate", em = cl,
      notes = "'slightly' not scored."),
    k("margin entire", "leaf_margin", "entire", em = cl),
    k("apex acute", "leaf_apex_shape", "acute", em = cl),
    k("rosette leaves oblanceolate", "leaf_shape", "oblanceolate", em = rl),
    num("4–7× 1–1.5 cm", "leaf_length", dim = 1, em = rl),
    num("4–7× 1–1.5 cm", "leaf_width", dim = 2, em = rl),
    num("Male inflorescences 23–27 cm long", "inflorescence_length", em = mi),
    num("3.5–4 × 2 mm", "flower_sepal_length", dim = 1, em = mf,
      notes = paste(tep, "Unit added in source_fixes.csv (Cheek_2012 question 5).")),
    num("Fruits: valves c. 18 × 3 mm", "fruit_length", dim = 1, notes = valve),
    num("Seeds 9–10 mm long", "seed_length"),
    k("central body smooth", "seed_surface_texture", "smooth", notes = seedtex)
  ),
  "Nepenthes macfarlanei" = bind_rows(
    k("Terrestrial", "plant_growth_substrate", "terrestrial", q = "usually"),
    k("sometimes epiphytic or climber to 3 m tall", "plant_growth_substrate",
      "epiphytic", "epiphyte", q = "sometimes"),
    k("sometimes epiphytic or climber to 3 m tall", "plant_growth_form",
      "climber", q = "sometimes"),
    num("sometimes epiphytic or climber to 3 m tall",
      "plant_height_climbing_plant", txt = "to 3 m", notes = climb_h),
    k("laminas oblong or narrowly elliptic-oblanceolate", "leaf_shape",
      "oblong narrowly_elliptic-oblanceolate",
      "oblong narrowly_elliptical oblanceolate",
      notes = "Hyphenated shape scored as both levels; 'narrowly' read with the first."),
    k("base cuneate", "leaf_base_shape", "cuneate"),
    k("auriculate", "leaf_base_shape", "auriculate"),
    k("margin entire", "leaf_margin", "entire"),
    k("apex rounded to acute", "leaf_apex_shape", "rounded acute"),
    num("c. 15.5–35 × 1.8–4 cm", "leaf_length", txt = "15.5–35 × 1.8–4 cm",
      dim = 1, em = "short-stem leaf", notes = "Printed 'c.': approximate range."),
    num("c. 15.5–35 × 1.8–4 cm", "leaf_width", txt = "15.5–35 × 1.8–4 cm",
      dim = 2, em = "short-stem leaf", notes = "Printed 'c.': approximate range."),
    num("4–10.5(–11.9) × 1.5–2.3(–2.7) cm", "leaf_length", dim = 1, em = cl),
    num("4–10.5(–11.9) × 1.5–2.3(–2.7) cm", "leaf_width", dim = 2, em = cl),
    num("Male inflorescences 14–26 cm long", "inflorescence_length", em = mi),
    num("c. 4 × 2.5 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("Fruits: valves 14.5–20 mm long", "fruit_length", notes = valve),
    num("Seeds 10–12 mm long", "seed_length"),
    k("tuberculate at centre", "seed_surface_texture", "tuberculate", "bumpy",
      notes = seedtex)
  ),
  "Nepenthes mirabilis" = bind_rows(
    k("Terrestrial sub-shrub or climber to 10 m tall", "plant_growth_form",
      "sub-shrub climber", "subshrub climber"),
    k("Terrestrial sub-shrub or climber to 10 m tall", "plant_growth_substrate",
      "terrestrial"),
    num("Terrestrial sub-shrub or climber to 10 m tall",
      "plant_height_climbing_plant", txt = "to 10 m", notes = climb_h),
    k("laminas lanceolate or oblong", "leaf_shape", "lanceolate oblong",
      em = "rosette and short-stem leaf"),
    k("apex acuminate", "leaf_apex_shape", "acuminate",
      em = "rosette and short-stem leaf"),
    num("3–20 cm long", "petiole_length", em = cl),
    k(c("obovate", "oblong or lanceolate"), "leaf_shape",
      "obovate oblong lanceolate", em = cl),
    num("(15–)20–40 × 4–10 cm", "leaf_length", dim = 1, em = cl),
    num("(15–)20–40 × 4–10 cm", "leaf_width", dim = 2, em = cl),
    k("margin entire", "leaf_margin", "entire", em = cl),
    k("apex acute", "leaf_apex_shape", "acute", em = cl),
    num("Male inflorescences 24–41(–50) cm long", "inflorescence_length", em = mi),
    num("(2–)4–4.5(–6) × (2–)3.5–4(–4.5) mm", "flower_sepal_length", dim = 1,
      em = mf, notes = tep),
    num("Fruits: valves 18–20 mm long", "fruit_length", notes = valve),
    num("Seeds 15–16 mm long", "seed_length")
  ),
  "Nepenthes rafflesiana" = bind_rows(
    habit("Terrestrial climber 2–6 m tall", "2–6 m"),
    num("4–17 cm long", "petiole_length"),
    k("of short stems narrowly oblong", "leaf_shape", "narrowly_oblong",
      em = "short-stem leaf"),
    num("(12–)17–37(–45) × (3–)4–7(–11) cm", "leaf_length", dim = 1,
      em = "short-stem leaf"),
    num("(12–)17–37(–45) × (3–)4–7(–11) cm", "leaf_width", dim = 2,
      em = "short-stem leaf"),
    k("base abruptly acute or attenuate", "leaf_base_shape", "acute attenuate",
      em = "short-stem leaf", notes = "'abruptly' not scored."),
    k("margin entire", "leaf_margin", "entire", em = "short-stem leaf"),
    k("apex acute or slightly acuminate", "leaf_apex_shape", "acute acuminate",
      em = "short-stem leaf", notes = "'slightly' not scored."),
    num("10–20(–30) × (3–)3.5–5.5(–7) cm", "leaf_length", dim = 1, em = cl),
    num("10–20(–30) × (3–)3.5–5.5(–7) cm", "leaf_width", dim = 2, em = cl),
    num("Male inflorescences (12–)18.5–49 cm long", "inflorescence_length",
      em = mi),
    k("tepals red or brown-red", "flower_colour", "red brown-red", "red_brown",
      em = mf, notes = "Tepal colour scored as flower colour."),
    num("(5–)6–7 × 5 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("valves (16–)28–40 × (0.5–)0.6–10 mm", "fruit_length", dim = 1,
      notes = valve),
    num("Seeds (12–)17–18 × 0.2–0.4 mm", "seed_length", dim = 1),
    num("Seeds (12–)17–18 × 0.2–0.4 mm", "seed_width", dim = 2)
  ),
  "Nepenthes ramispina" = bind_rows(
    habit("Terrestrial climber to 5 m tall", "to 5 m"),
    k("Indumentum on stems sparingly tomentose", "stem_hairs", "tomentose",
      "hairy"),
    k("laminas obovate-oblong", "leaf_shape", "obovate-oblong", "obovate oblong",
      notes = "Hyphenated shape scored as both levels."),
    num("to 16 × 3.1 cm", "leaf_length", txt = "to 16 cm"),
    num("to 16 × 3.1 cm", "leaf_width", txt = "to 3.1 cm"),
    k("glabrous", "leaf_hairs_adult_leaves", "glabrous"),
    k("auriculate", "leaf_base_shape", "auriculate"),
    k("margin entire", "leaf_margin", "entire"),
    k("apex acute to rounded", "leaf_apex_shape", "acute rounded"),
    num("Male inflorescences 20–36 cm long", "inflorescence_length", em = mi),
    num("c. 4.5 × 2 mm", "flower_sepal_length", dim = 1, em = mf, notes = tep),
    num("Fruits: valves c. 20 × 3 mm", "fruit_length", dim = 1, notes = valve),
    num("Seeds c. 12 mm long", "seed_length"),
    k("central body tuberculate", "seed_surface_texture", "tuberculate", "bumpy",
      notes = seedtex)
  ),
  "Nepenthes sanguinea" = bind_rows(
    habit("Terrestrial climber to at least 6 m tall", "to 6 m") |>
      mutate(notes = if_else(
        trait == "plant_height_climbing_plant",
        paste(climb_h, "Printed 'to at least 6 m': scored as a maximum of 6 m."),
        notes
      )),
    k("thinnest and long-spathulate", "leaf_shape", "long-spathulate",
      "spathulate", em = "lower climbing-stem leaf"),
    num("to 20 × 5.2 cm", "leaf_length", txt = "to 20 cm",
      em = "lower climbing-stem leaf"),
    num("to 20 × 5.2 cm", "leaf_width", txt = "to 5.2 cm",
      em = "lower climbing-stem leaf"),
    k("glabrous", "leaf_hairs_adult_leaves", "glabrous"),
    k("base spathulate or cuneate", "leaf_base_shape", "cuneate",
      em = "lower climbing-stem leaf",
      notes = "'spathulate' base has no leaf_base_shape level; not scored."),
    k("auriculate", "leaf_base_shape", "auriculate", q = "usually",
      em = "lower climbing-stem leaf",
      notes = "'rarely not auriculate' is a negative phrase; not scored."),
    k("apex obtuse to slightly acuminate", "leaf_apex_shape", "obtuse acuminate",
      em = "lower climbing-stem leaf", notes = "'slightly' not scored."),
    k("oblong", "leaf_shape", "oblong", em = "upper climbing-stem leaf"),
    num("10–15.5 × 2.1–3.4 cm", "leaf_length", dim = 1,
      em = "upper climbing-stem leaf"),
    num("10–15.5 × 2.1–3.4 cm", "leaf_width", dim = 2,
      em = "upper climbing-stem leaf"),
    k("leaf laminas of the basal rosettes mostly oblanceolate-spathulate",
      "leaf_shape", "oblanceolate-spathulate", "oblanceolate spathulate",
      q = "mostly", em = rl, notes = "Hyphenated shape scored as both levels."),
    num("7–8.5 × c. 2.5 cm", "leaf_length", dim = 1, em = rl),
    num("7–8.5 × c. 2.5 cm", "leaf_width", dim = 2, em = rl),
    num("Male inflorescences 35–65 cm long", "inflorescence_length", em = mi),
    num("c. 3 × 1.5(–2) mm long", "flower_sepal_length", txt = "c. 3 mm",
      em = mf, notes = tep),
    num("Fruits: valves 20–25 mm long", "fruit_length", notes = valve),
    num("Seeds 8–12 mm long", "seed_length"),
    k("rugose in centre", "seed_surface_texture", "rugose", "rough",
      notes = seedtex)
  )
)

# Attach each scored row to its piece(s); a name must match exactly one piece
# of that taxon's description.
scored_rows <- imap(scored, \(rows, b) {
  pb <- pieces |> filter(block == b)
  rows |>
    mutate(pid = map(piece, \(ps) {
      map_int(ps, \(x) {
        # "text@2": the second piece with this text (repeated wording).
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

# ---- unmapped description pieces -----------------------------------------------------

organ_re <- c(
  "^(Lower pitchers|lower pitchers)" = "lower pitcher",
  "^(Upper pitchers|upper pitchers)" = "upper pitcher",
  "^rosette pitchers" = "rosette pitcher",
  "^(mouth|some pitchers with the mouth)" = "pitcher mouth",
  "^peristome" = "peristome",
  "^(lid|underside)" = "pitcher lid",
  "^nectar glands" = "lid nectar glands",
  "^spur" = "pitcher spur",
  "^(with (two|2) (fringed )?(wings|ridges)|wings|the fringe|fringe)" = "pitcher wings",
  "^(Pitchers|pitchers?|exposed pitchers|inner surface)" = "pitcher",
  "^Indumentum" = "indumentum",
  "^(Stems|Climbing stems|climbing stems|stem)" = "stem",
  "^internodes" = "stem internodes",
  "^(Buds|axillary buds)" = "bud",
  "^Stipules" = "stipules",
  "^(longitudinal veins|pennate veins)" = "leaf venation",
  "^(Leaves|leaves|laminas|basal rosette|rosette leaves|climbing leaves|lower leaf|upper laminas|leaf laminas|petioles?)" = "leaf",
  "^(Male inflorescences|male inflorescences|Inflorescences|female inflorescence|peduncles?|partial|bracts?|Infructescence)" = "inflorescence",
  "^(Male flowers|Female flowers|tepals|androphore|anthers?|stamens|filaments|ovary|stigmas|placentation|ovules|locules)" = "flower",
  "^Fruits?" = "fruit",
  "^(Seeds|testa|endosperm|embryo|cotyledons)" = "seed",
  "^(Terrestrial|Carnivorous)" = "plant"
)
organ_of <- function(x) {
  hit <- which(str_detect(x, names(organ_re)))
  if (length(hit)) organ_re[[hit[1]]] else NA_character_
}
character_of <- function(x) {
  ch <- c(
    if (str_detect(x, "\\d")) "size or number",
    if (str_detect(x, "\\b(green|red|white|yellow|brown|purple|maroon|cream|coppery|black|ivory|pink|glaucous|colour)\\b")) "colour",
    if (str_detect(x, "hairs|glabr|tomentose|pubescent|velvety|indumentum|Indumentum|glands")) "indumentum or glands",
    if (str_detect(x, "late|form|ovoid|cylindric|globose|elliptic|ovate|orbicular|shaped|oblong|angular|terete|flattened|hipped|ventricose|spathulate")) "shape"
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
    # Character hints go in the detail field so the summary stays one row per
    # organ.
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

# ---- other sections ------------------------------------------------------------------

# One unmapped row per sentence of every labelled section of the genus,
# species and hybrids (Wong_2010 pattern); hybrid Descriptions are comparisons
# with the parents and are kept the same way (Cheek_2012 question 1).
sec_paras <- paras |>
  filter(
    rank %in% c("genus", "species"),
    status %in% c("accepted", "hybrid"),
    !section %in% c("Nomenclature", "Vernacular names", "Vernacular name"),
    !(section == "Description" & status == "accepted")
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
      notes = paste0(
        "UNMAPPED: ",
        if (p$status == "hybrid" && p$section == "Description") {
          "hybrid | comparison with the parents"
        } else {
          paste(str_to_lower(p$section), "| text")
        }
      ),
      source_section = p$section
    )
  }) |>
    bind_rows()
}) |>
  bind_rows()

# Scored statements from the genus Morphology and Reproductive biology
# paragraphs and from species Ecology. `taxon` is the taxon the statement is
# about; it can be a species named inside a genus paragraph.
sec <- function(taxon, from, section, fragment, trait, value, clean = value,
                q = NA, notes = NA, source_id = NA) {
  p <- paras |> filter(block == from, .data$section == .env$section)
  stopifnot(nrow(p) == 1, str_detect(p$text_n, fixed(fragment)))
  pg <- page_of(p, fragment)
  tibble(
    block = taxon,
    rank = if (taxon == "Nepenthes") "genus" else "species",
    trait,
    trait_value = value,
    trait_value_clean = clean,
    value_type = "mode",
    unit = NA_character_,
    cq = q,
    entity_measured = NA_character_,
    notes,
    trait_value_raw = fragment,
    pdf_page = pg$pdf,
    printed_page = pg$printed,
    source_section = section,
    source_id
  )
}
RB <- "Reproductive biology"
sec_scored <- bind_rows(
  sec("Nepenthes", "Nepenthes", "Morphology",
      "finally become lianas with climbing stems and even longer internodes, the stems being supported by the twining leaf tendrils",
      "plant_climbing_mechanism", "tendrils"),
  sec("Nepenthes", "Nepenthes", RB,
      "Being dioecious, all Nepenthes species are obligate outbreeders",
      "breeding_system", "obligate_outbreeders", "allogamy",
      notes = "Obligate outbreeding scored as allogamy."),
  sec("Nepenthes", "Nepenthes", RB,
      "At maturity, the capsule valves open slightly allowing the light seeds with air-filled, thread-like appendages to be blown away by gusts of wind",
      "dispersal_syndrome", "blown_away_by_gusts_of_wind", "anemochory"),
  sec("Nepenthes gracilis", "Nepenthes", RB,
      "In N. gracilis pollen is shed between 1600 and 1800 h when both the male and female flowers emit a heavy foetid smell (Lim & Prakash, 1973)",
      "flower_scent_production", "heavy_foetid_smell", "scent_produced",
      notes = "From the genus Reproductive biology paragraph, citing Lim & Prakash (1973).",
      source_id = "Lim_1973"),
  sec("Nepenthes macfarlanei", "Nepenthes", RB,
      "For N. macfarlanei, Chua (2000) observed flower odour as slightly musky or foetid",
      "flower_scent_production", "slightly_musky_or_foetid", "scent_produced",
      notes = "From the genus Reproductive biology paragraph, citing Chua (2000).",
      source_id = "Chua_2000"),
  sec("Nepenthes macfarlanei", "Nepenthes", RB,
      "She suggested muscid flies as the pollinators because they occasionally visited the flowers",
      "flower_visitor", "muscid_flies", "fly",
      notes = "Visits observed (Chua 2000); the suggested pollinator role is hedged and not scored.",
      source_id = "Chua_2000"),
  sec("Nepenthes ampullaria", "Nepenthes", RB,
      "Clarke (2001) recorded that N. ampullaria flowers produce a ‘foul musky smell’ in daytime",
      "flower_scent_production", "foul_musky_smell", "scent_produced",
      notes = "From the genus Reproductive biology paragraph, citing Clarke (2001); the suggested carrion-fly pollination is hedged and not scored.",
      source_id = "Clarke_2001"),
  sec("Nepenthes mirabilis", "Nepenthes", RB,
      "noted that inflorescences of N. mirabilis smelled of mice droppings",
      "flower_scent_production", "smelled_of_mice_droppings", "scent_produced",
      notes = "From the genus Reproductive biology paragraph, citing Cheek & Jebb (2001), Flora Malesiana.",
      source_id = "Cheek_2001"),
  sec("Nepenthes sanguinea", "Nepenthes sanguinea", "Ecology",
      "sometimes epiphytic on branches and tree trunks covered by a thick layer of moss",
      "plant_growth_substrate", "epiphytic", "epiphyte", q = "sometimes")
)

# ---- copy-down and assembly ------------------------------------------------------------

desc_all <- bind_rows(own_desc, sec_scored)
genus_scored <- desc_all |> filter(rank == "genus")
own <- desc_all |> filter(rank == "species")

# Members are the ten accepted species; hybrids and excluded species are not
# members (Cheek_2012 question 1).
members <- tibble(
  block = species,
  genus = "Nepenthes",
  group = NA_character_,
  species = species,
  rank = "species"
)
copied <- flora_copy_down(genus_scored, own, members)
rejected <- attr(copied, "rejected")
if (nrow(rejected)) print(rejected)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  genus_scored |> mutate(rank_scored_from = NA_character_),
  copied,
  desc_unmapped,
  sec_unmapped
) |>
  transmute(
    taxon_name = block,
    taxon_rank = if_else(block == "Nepenthes", "genus", "species"),
    rank_scored_from,
    commonness_qualifier = cq,
    # FPM species descriptions describe the species in Peninsular Malaysia;
    # genus values and statements cited from other works have no region.
    population_region = if_else(
      taxon_name %in% species & is.na(rank_scored_from) & is.na(source_id) &
        source_section %in% c("Description", "Ecology", "Distribution", "Note",
                              "Uses", "Conservation status"),
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
    source_id
  ) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits) |>
  arrange(taxon_name, is.na(trait), trait)

stopifnot(all(out$trait[!is.na(out$trait)] %in% names(traits)))
write_csv(out, "data/Cheek_2012/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/Cheek_2012/raw/unmapped_terms.csv",
  na = ""
)

n_mapped <- out |>
  filter(!is.na(trait)) |>
  count(taxon_name, name = "n_mapped_rows")
tibble(
  taxon_name = c("Nepenthes", species, hybrids,
                 "Nepenthes alata", "Nepenthes reinwardtiana"),
  taxon_rank = c("genus", rep("species", length(species) + length(hybrids) + 2)),
  status = c("accepted", rep("accepted", length(species)),
             rep("named hybrid", length(hybrids)), "excluded", "excluded")
) |>
  left_join(n_mapped, by = "taxon_name") |>
  mutate(
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      taxon_name == "Nepenthes" ~
        "Monogeneric family with no family description; genus values copied to the ten species.",
      status == "named hybrid" ~
        "Named nothospecies; comparative description kept unmapped, no copy-down (Cheek_2012 question 1).",
      taxon_name == "Nepenthes alata" ~
        "Excluded: Peninsular Malaysian records are misidentified N. gracillima; restricted to the Philippines. Not scored.",
      taxon_name == "Nepenthes reinwardtiana" ~
        "Excluded: Peninsular Malaysian specimens are N. gracilis. Not scored.",
      taxon_name == "Nepenthes benstonei" ~
        "Endemic to NE Peninsular Malaysia; the only species assessed as Endangered.",
      TRUE ~ ""
    )
  ) |>
  write_csv("data/Cheek_2012/raw/taxon_flags.csv", na = "")
