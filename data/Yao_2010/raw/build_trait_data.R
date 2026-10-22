# Hand-transcribed extraction of Pentaphylacaceae (T.L. Yao 2010, FPM vol. 1: 161-164).
# Source text: raw/Pentaphylacaceae_transcription.txt. Writes trait_data.csv,
# unmapped_terms.csv, taxon_flags.csv. Mapped and unmapped records share one file;
# unmapped rows have an empty `trait` and notes starting "UNMAPPED".

library(dplyr)

tx <- "Pentaphylax euryoides"
traits <- yaml::read_yaml("config/traits.yml")$traits$elements |> names()

# pdf/printed page by source section
pg <- list(
  genus1 = c(8, 161),
  genus2 = c(9, 162),
  gen_dist = c(9, 162),
  sp1 = c(9, 162),
  sp2 = c(11, 164),
  eco = c(11, 164)
)

row <- function(
  trait,
  raw,
  value = "",
  clean = "",
  vt = "mode",
  unit = "",
  q = "",
  em = "",
  rank = "",
  sec = "Description",
  p = "sp1",
  region = "",
  notes = ""
) {
  tibble(
    taxon_name = tx,
    taxon_rank = "species",
    entity_type = "species",
    rank_scored_from = rank,
    commonness_qualifier = q,
    population_region = region,
    form = "",
    entity_measured = em,
    trait = trait,
    trait_value_raw = raw,
    trait_value = value,
    trait_value_clean = clean,
    value_type = vt,
    unit = unit,
    source_section = paste0(sec, ", p.", pg[[p]][2]),
    pdf_page = pg[[p]][1],
    printed_page = pg[[p]][2],
    notes = notes
  )
}
# Unmapped clause: no trait, no level
un <- function(raw, what, ...) {
  row("", raw, notes = paste0("UNMAPPED: ", what), ...)
}
# Typical (a-)b(-c): mean on `usually`, extremes on `extreme_range`
tri <- function(trait, raw, lo, mid, hi, unit, ...) {
  bind_rows(
    row(trait, raw, mid, mid, "mean", unit, "usually", ...),
    row(trait, raw, lo, lo, "minimum", unit, "extreme_range", ...),
    row(trait, raw, hi, hi, "maximum", unit, "extreme_range", ...)
  )
}
rng <- function(trait, raw, lo, hi, unit, ...) {
  bind_rows(
    row(trait, raw, lo, lo, "minimum", unit, ...),
    row(trait, raw, hi, hi, "maximum", unit, ...)
  )
}

dd <- function(x) gsub("\u2013", "--", x, fixed = TRUE)
G1 <- "Genus description"
G2 <- "Genus description"

d <- bind_rows(
  # ---- Genus description (copied down; species silent) ----
  row(
    "plant_canopy_form",
    "Shrubs or trees with dense crowns.",
    "dense crowns",
    "dense-crowned",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "leaf_arrangement",
    "Leaves simple, spirally arranged, pinnately veined.",
    "spirally arranged",
    "spiral",
    rank = "genus",
    sec = G1,
    p = "genus1",
    notes = "Family Taxonomy paragraph (p.161) also says 'spirally arranged leaves'. leaf_phyllotaxis has no 'spiral' level; not scored."
  ),
  row(
    "leaf_compoundness",
    "Leaves simple, spirally arranged, pinnately veined.",
    "simple",
    "simple",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "inflorescence_type",
    "Inflorescences axillary, spike-like, often developing into foliate twigs, or with only solitary flowers along foliate twigs",
    "axillary, spike-like; solitary flowers",
    "axillary spike solitary",
    rank = "genus",
    sec = G1,
    p = "genus1",
    notes = "'spike-like' mapped to spike. Species description does not state inflorescence type."
  ),
  row(
    "flower_structural_sex_type",
    "Flowers bisexual, actinomorphic, 5-merous",
    "bisexual",
    "bisexual",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "flower_perianth_symmetry",
    "Flowers bisexual, actinomorphic, 5-merous",
    "actinomorphic",
    "actinomorphic_general",
    rank = "genus",
    sec = G1,
    p = "genus1",
    notes = "Level actinomorphic_general chosen as nearest; check."
  ),
  row(
    "flower_perianth_merism",
    "Flowers bisexual, actinomorphic, 5-merous",
    "5",
    "5",
    "mean",
    "{count}",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "flower_fertile_stamens_count",
    "stamens 5, alternating with and shorter than petals",
    "5",
    "5",
    "mean",
    "{count}",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "flower_anther_orientation",
    "anther 2-locular, introrse, basifixed, opening by terminal pores",
    "introrse",
    "introrse",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  row(
    "flower_anther_attachment",
    "anther 2-locular, introrse, basifixed, opening by terminal pores",
    "basifixed",
    "basifixed",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  row(
    "flower_anther_dehiscence",
    "opening by terminal pores",
    "terminal pores",
    "poricidal",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  row(
    "flower_ovary_position",
    "ovary superior, 5-locular, ovules 2 per locule, pendulous on axile placentas.",
    "superior",
    "superior",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  row(
    "flower_gynoecium_placentation",
    "ovary superior, 5-locular, ovules 2 per locule, pendulous on axile placentas.",
    "axile",
    "axile",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  row(
    "flower_structural_carpels_count",
    "ovary superior, 5-locular",
    "5",
    "5",
    "mean",
    "{count}",
    rank = "genus",
    sec = G2,
    p = "genus2",
    notes = "Carpel number inferred from 5-locular ovary (Coode_2018 convention); not stated as carpels."
  ),
  row(
    "fruit_type",
    "Fruit a woody capsule, splitting loculicidally in the upper half or down to the base into 5 valves",
    "capsule",
    "capsule",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  row(
    "fruit_dehiscence",
    "Fruit a woody capsule, splitting loculicidally in the upper half or down to the base into 5 valves",
    "splitting loculicidally",
    "dehiscent",
    rank = "genus",
    sec = G2,
    p = "genus2",
    notes = "Species Ecology also says 'dehiscent fruits'."
  ),

  # ---- Genus clauses with no mapped trait ----
  un(
    "Without stipules.",
    "stipules absent (negative phrase, not scored)",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    "Leaves simple, spirally arranged, pinnately veined.",
    "venation: pinnately veined",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    dd(
      "Inflorescences axillary, spike-like, often developing into foliate twigs, or with only solitary flowers along foliate twigs; bracteate, bracteoles 1–2, persistent."
    ),
    "bracts/bracteoles presence, count (1--2), persistence",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    "pedicel short or absent",
    "pedicel length (qualitative)",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    "sepals 5, imbricate; petals 5, imbricate, adnate with the stamens at the base",
    "sepal and petal count, aestivation, adnation to stamens",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    "filaments broadest at the base, narrowing towards the anther, anther 2-locular",
    "filament shape; anther locule count",
    rank = "genus",
    sec = G1,
    p = "genus1"
  ),
  un(
    "ovules 2 per locule, pendulous on axile placentas",
    "ovule count per locule; ovule orientation",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  un(
    "splitting loculicidally in the upper half or down to the base into 5 valves detaching from a persistent central column, each valve bearing a woody septum down the centre",
    "capsule valve count (5), columella, septum, wood texture",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  un(
    "Seeds laterally flattened, with a membranous wing at one end; embryo horseshoe-shaped, endosperm scarce.",
    "seed flattening; wing texture; embryo shape; endosperm amount",
    rank = "genus",
    sec = G2,
    p = "genus2"
  ),
  un(
    "Monogeneric family.",
    "family circumscription; no trait",
    rank = "family",
    sec = "Family description",
    p = "genus1"
  ),
  un(
    "Engler (1897) described Pentaphylacaceae as a family distinct from Ternstroemiaceae based on its spirally arranged leaves and five anthers with terminal pores opening with small lids",
    "taxonomic argument; repeats genus characters (leaf arrangement, anther count/dehiscence); small lids on pores",
    rank = "family",
    sec = "Family Taxonomy",
    p = "genus1"
  ),
  un(
    "A genus with one widely distributed species, extending from SE China (including Hainan and Hong Kong, where it is a dominant species in montane evergreen forest), to Vietnam, N and C Sumatra, and Peninsular Malaysia.",
    "global distribution",
    rank = "genus",
    sec = "Genus Distribution",
    p = "gen_dist",
    region = ""
  ),
  un(
    "It is sometimes cultivated as an ornamental.",
    "use: ornamental (qualified 'sometimes')",
    rank = "genus",
    sec = "Genus Uses",
    p = "gen_dist"
  ),
  un(
    "Pentaphylax is an aluminium accumulator and its leaves dry yellowish green due to their high aluminium content (Chenery, Kew Bull. (1948) 176).",
    "aluminium accumulator (qualitative; cf. leaf_Al_per_dry_mass, no value given); dried leaf colour",
    rank = "genus",
    sec = "Genus Note",
    p = "gen_dist"
  ),

  # ---- Species description: mapped ----
  row(
    "plant_growth_form",
    "Shrub or small to medium-sized tree",
    "Shrub or small to medium-sized tree",
    "shrub tree"
  ),
  tri(
    "plant_height",
    dd("Shrub or small to medium-sized tree, (2–)8(–18) m tall"),
    "2",
    "8",
    "18",
    "m"
  ),
  row(
    "plant_diameter_breast_height",
    "bole smooth, columnar, to 10 cm diameter",
    "10",
    "10",
    "maximum",
    "cm",
    em = "bole",
    notes = "Bole diameter; height of measurement not stated."
  ),
  row(
    "bark_texture",
    "Bark dark brown to black, smooth or rugose, with lenticels, cracking and flaking-off in small papery scales",
    "smooth or rugose; flaking-off in small papery scales",
    "smooth rough flaky",
    notes = "rugose (wrinkled) mapped to rough; 'flaking-off in small papery scales' mapped to flaky; check."
  ),
  row(
    "bark_colour",
    "Bark dark brown to black",
    "dark brown to black",
    "brown_dark black"
  ),
  row(
    "bark_colour",
    "inner bark yellow to reddish brown",
    "yellow to reddish brown",
    "yellow brown",
    em = "inner bark",
    notes = "reddish brown mapped to brown; check."
  ),
  row(
    "stem_hairs",
    "Twigs black when dry, ridged lengthwise, sparsely minutely pubescent",
    "sparsely minutely pubescent",
    "hairy",
    em = "twigs"
  ),
  tri(
    "petiole_length",
    dd("petioles slender, (1–)5(–10) mm long"),
    "1",
    "5",
    "10",
    "mm"
  ),
  rng(
    "leaf_length",
    dd(
      "laminas lanceolate to elliptic, rarely ovate or falcate, 3.3–5(–8.1) x 1.2–2.8 cm"
    ),
    "3.3",
    "5",
    "cm",
    q = "usually"
  ),
  row(
    "leaf_length",
    "laminas lanceolate to elliptic, rarely ovate or falcate, 3.3--5(--8.1) x 1.2--2.8 cm",
    "8.1",
    "8.1",
    "maximum",
    "cm",
    "extreme_range"
  ),
  rng(
    "leaf_width",
    "laminas lanceolate to elliptic, rarely ovate or falcate, 3.3--5(--8.1) x 1.2--2.8 cm",
    "1.2",
    "2.8",
    "cm"
  ),
  row(
    "leaf_shape",
    "laminas lanceolate to elliptic",
    "lanceolate to elliptic",
    "lanceolate elliptical",
    q = "usually"
  ),
  row(
    "leaf_shape",
    "rarely ovate or falcate",
    "ovate or falcate",
    "ovate falcate",
    q = "rarely"
  ),
  row(
    "leaf_surface_reflectivity",
    "glossy above",
    "glossy",
    "shiny",
    em = "adaxial surface"
  ),
  row(
    "leaf_hairs_adult_leaves",
    "glossy above, sparsely minutely pubescent, subglabrous beneath",
    "sparsely minutely pubescent",
    "hairy",
    em = "adaxial surface",
    notes = "Source sentence is ambiguous: 'glossy above, sparsely minutely pubescent, subglabrous beneath'. Read as pubescent above."
  ),
  row(
    "leaf_hairs_adult_leaves",
    "subglabrous beneath",
    "subglabrous",
    "glabrous",
    q = "usually",
    em = "abaxial surface",
    notes = "subglabrous mapped to glabrous."
  ),
  row(
    "leaf_hairs_adult_leaves",
    "rarely sparsely, or minutely pubescent so near the base",
    "sparsely, or minutely pubescent",
    "hairy",
    q = "rarely",
    em = "abaxial surface",
    notes = "'so near the base' (OCR/print) read as 'only near the base'."
  ),
  row(
    "leaf_base_shape",
    "base cuneate to slightly rounded",
    "cuneate to slightly rounded",
    "cuneate rounded"
  ),
  row(
    "leaf_margin_posture",
    "margin slightly revolute and sometimes minutely ciliate",
    "slightly revolute",
    "revolute"
  ),
  row(
    "leaf_apex_shape",
    "apex acute, sometimes acuminate, tip minutely retuse, rarely minutely apiculate",
    "acute",
    "acute",
    q = "usually"
  ),
  row(
    "leaf_apex_shape",
    "apex acute, sometimes acuminate",
    "acuminate",
    "acuminate",
    q = "sometimes"
  ),
  row(
    "leaf_apex_shape",
    "rarely minutely apiculate",
    "apiculate",
    "apiculate",
    q = "rarely"
  ),
  tri(
    "inflorescence_length",
    dd("Inflorescences (1–)2.3(–4) cm long"),
    "1",
    "2.3",
    "4",
    "cm"
  ),
  row(
    "flower_bract_length",
    "bracts and bracteoles deltoid, keeled, c. 1 mm long",
    "1",
    "1",
    "mean",
    "mm",
    notes = "Bracts and bracteoles together."
  ),
  row(
    "pedicel_length",
    "pedicels to 1 mm",
    "1",
    "1",
    "maximum",
    "mm",
    p = "sp1"
  ),
  row(
    "bud_width",
    "buds light green to green, globular, to 2 mm diameter",
    "2",
    "2",
    "maximum",
    "mm",
    p = "sp1"
  ),
  row(
    "flower_colour",
    "petals white to pale green, sometimes tinged reddish",
    "white to pale green",
    "white_cream green",
    em = "petals",
    notes = "Tinge ('sometimes tinged reddish') not scored."
  ),
  row(
    "flower_sepal_length",
    dd("sepals deltoid, (0.5)–1 x c. 1 mm"),
    "1",
    "1",
    "maximum",
    "mm",
    "usually",
    notes = "'(0.5)--1': length read as 1 mm with extreme 0.5; first dimension taken as length."
  ),
  row(
    "flower_sepal_length",
    "sepals deltoid, (0.5)--1 x c. 1 mm",
    "0.5",
    "0.5",
    "minimum",
    "mm",
    "extreme_range"
  ),
  rng(
    "flower_petal_length",
    "petals ... obovate to oblong, c. 2--3 x (0.5)--1.5 mm",
    "2",
    "3",
    "mm",
    q = "usually",
    notes = "c."
  ),
  rng("flower_stamen_length", dd("stamens 1--2 mm long"), "1", "2", "mm"),
  row(
    "flower_style_length",
    "style cylindric, c. 1 mm long",
    "1",
    "1",
    "mean",
    "mm",
    p = "sp2"
  ),
  row(
    "fruit_colour",
    "Fruits green, sometimes with longitudinal light green lines, ripening brown to black",
    "ripening brown to black",
    "brown black",
    p = "sp2",
    notes = "Ripe colour only; unripe green not scored."
  ),
  tri(
    "fruit_length",
    dd("Fruits ... (2–)4(–6) mm long"),
    "2",
    "4",
    "6",
    "mm",
    p = "sp2"
  ),
  tri(
    "fruit_width",
    dd("(4–)6(–8) mm diameter"),
    "4",
    "6",
    "8",
    "mm",
    p = "sp2",
    notes = "Fruit 'diameter' mapped to fruit_width."
  ),
  row(
    "fruit_surface_hairs",
    "glabrous, style persistent",
    "glabrous",
    "glabrous",
    p = "sp2"
  ),
  row(
    "dispersal_appendage",
    "Seeds 1--2 per locule, wing linear-oblong, 5--6 mm long, 1--2 mm wide.",
    "wing",
    "wings",
    p = "sp2"
  ),
  row(
    "flowering_time",
    "It flowers more-or-less continuously throughout the year.",
    "more-or-less continuously throughout the year",
    "yyyyyyyyyyyy",
    sec = "Ecology",
    p = "eco",
    region = "Peninsular Malaysia",
    notes = "Hedged ('more-or-less'); all months scored y. Check."
  ),
  row(
    "dispersal_syndrome",
    "The dehiscent fruits with winged seeds suggest wind dispersal.",
    "wind dispersal",
    "anemochory",
    sec = "Ecology",
    p = "eco",
    region = "Peninsular Malaysia",
    notes = "Hedged ('suggest wind dispersal'); scored anemochory on user decision, see metadata question."
  ),

  # ---- Species description: unmapped ----
  un(
    "beginning to flower at 60 cm",
    "size at first flowering (height 60 cm); not plant_height",
    p = "sp1"
  ),
  un("bole smooth, columnar", "bole surface and shape", p = "sp1"),
  un(
    "without buttresses",
    "buttresses absent (negative phrase; stem_base_form has no 'absent' level)",
    p = "sp1"
  ),
  un(
    "Sapwood pale yellow to yellow and pink-tinged.",
    "sapwood colour",
    p = "sp1"
  ),
  un(
    "Bark ... with lenticels; inner bark ... granular, brittle",
    "lenticels; inner bark texture",
    p = "sp1"
  ),
  un(
    "Twigs black when dry, ridged lengthwise",
    "twig colour and surface",
    p = "sp1"
  ),
  un(
    "Leaves in life with lateral and intercostal veins dark green contrasting pale yellowish green beneath, drying khaki, young leaves red",
    "leaf colour in life/dried/young; ambiguous which part is dark green",
    p = "sp1"
  ),
  un(
    "petioles slender, ... grooved above, sparsely minutely pubescent or glabrous",
    "petiole shape, grooving, hairs",
    p = "sp1"
  ),
  un("chartaceous to thickly coriaceous", "lamina texture", p = "sp1"),
  un(
    "margin ... sometimes minutely ciliate",
    "leaf margin ciliation",
    p = "sp1"
  ),
  un("tip minutely retuse", "apex tip shape", p = "sp1"),
  un(
    "midrib sparsely minutely pubescent, in the lower half grooved above, prominent beneath; lateral veins 7--12 pairs, fine, raised above, prominent beneath; intercostal veins finely reticulate, conspicuous, sometimes faint",
    "midrib hairs/relief; lateral vein count (7--12 pairs); venation",
    p = "sp1"
  ),
  un(
    "rachis ridged, minutely pubescent, rarely both inflorescences and solitary flowers on the same shoot",
    "rachis surface/hairs; inflorescence mixing",
    p = "sp1"
  ),
  un(
    "bracts and bracteoles deltoid, keeled, ... minutely pubescent, margin ciliate",
    "bract shape, keel, hairs, margin",
    p = "sp1"
  ),
  un("buds light green to green, globular", "bud colour and shape", p = "sp1"),
  un(
    "sepals deltoid, ... c. 1 mm, unequal, outside minutely pubescent, inside keeled, glabrous, margin ciliate",
    "sepal shape, width, inequality, hairs, keel, margin",
    p = "sp1"
  ),
  un(
    "petals ... obovate to oblong, ... (0.5)--1.5 mm, glabrous, margin entire, apex rounded to slightly retuse",
    "petal shape, width, hairs, margin, apex",
    p = "sp1"
  ),
  un(
    "sepals deltoid, (0.5)--1 x c. 1 mm",
    "flower_sepal_width (candidate): c. 1 mm; length read as first dimension, see metadata question",
    p = "sp1"
  ),
  un(
    "petals ... obovate to oblong, c. 2--3 x (0.5)--1.5 mm",
    "flower_petal_width (candidate): (0.5)--1.5 mm",
    p = "sp1"
  ),
  un(
    "anthers c. 0.5 mm across, pores opening by a minute beaked lid, attached at the lower end of the pore aperture",
    "anther width (across); pore lid",
    p = "sp2"
  ),
  un(
    "ovary ovoid, c. 0.5 mm long, 0.5 mm diameter",
    "ovary shape and size",
    p = "sp2"
  ),
  un("stigma 5-lobed and star-shaped", "stigma lobes and shape", p = "sp2"),
  un(
    "Fruits green, sometimes with longitudinal light green lines",
    "unripe fruit colour, striping",
    p = "sp2"
  ),
  un(
    "ellipsoid, rounded at both ends, ... style persistent",
    "fruit shape, apex/base, style persistence",
    p = "sp2"
  ),
  un(
    "Seeds 1--2 per locule",
    "seeds per locule (not per plant; seed_count not applicable)",
    p = "sp2"
  ),
  un(
    "wing linear-oblong, 5--6 mm long, 1--2 mm wide",
    "seed wing shape and size (no wing traits; seed_length not scored from wing)",
    p = "sp2"
  ),
  un(
    "in Peninsular Malaysia recorded in Kedah, Perak, Kelantan, Pahang, Selangor, and Johor.",
    "state-level distribution",
    sec = "Distribution",
    p = "sp2",
    region = "Peninsular Malaysia"
  ),
  un(
    "Least concern.",
    "conservation status",
    sec = "Conservation status",
    p = "sp2"
  ),
  un(
    "it grows in lower to upper montane forest between 900--2000 m, mainly on rocky ridge tops.",
    "habitat (forest type, ridge tops) and elevation 900--2000 m; no traits",
    sec = "Ecology",
    p = "eco",
    region = "Peninsular Malaysia"
  )
) |>
  mutate(
    in_traits.yml = ifelse(trait %in% traits, "x", ""),
    form = NULL
  ) |>
  mutate(form = "", .after = population_region) |>
  relocate(in_traits.yml, .after = trait)

stopifnot(all(d$trait[d$trait != ""] %in% traits))

write.csv(d, "data/Yao_2010/raw/trait_data.csv", row.names = FALSE, na = "")

d |>
  filter(trait == "") |>
  transmute(
    organ = sub(";.*", "", sub("^UNMAPPED: ", "", notes)),
    character = sub("^UNMAPPED: ", "", notes),
    n_taxa = 1,
    example_taxon = taxon_name,
    example_text = trait_value_raw,
    printed_page
  ) |>
  write.csv("data/Yao_2010/raw/unmapped_terms.csv", row.names = FALSE)

tibble(
  taxon_name = tx,
  taxon_rank = "species",
  status = "accepted",
  n_rows = nrow(d),
  flag = "Synonyms P. malayana, P. arborea listed under accepted name; type of P. euryoides is Hong Kong (lectotype)"
) |>
  write.csv("data/Yao_2010/raw/taxon_flags.csv", row.names = FALSE)
