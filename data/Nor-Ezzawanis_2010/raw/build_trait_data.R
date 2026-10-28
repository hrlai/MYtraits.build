library(dplyr)

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

tx <- "Cibotium barometz"
h <- flora_hand(
  tx,
  list(genus = c(7, 119), species = c(8, 120), notes = c(10, 122))
)

d <- bind_rows(
  h$row(
    "plant_growth_form",
    "Large terrestrial fern.",
    "fern",
    "fern",
    p = "species"
  ),
  h$row(
    "leaf_arrangement",
    "bearing spirally arranged fronds.",
    "spirally arranged",
    "spiral",
    rank_from = "genus",
    p = "genus"
  ),
  h$row(
    "leaf_compoundness",
    "Laminae ... deeply tripinnatifid.",
    "deeply tripinnatifid",
    "compound",
    p = "species",
    notes = "Fern mapping under review: frond/lamina treated as leaf; tripinnatifid division treated as compound."
  ),
  h$rng(
    "petiole_length",
    "Stipes ... 80–225 cm long",
    80,
    225,
    "cm",
    p = "species",
    notes = "Fern mapping under review: stipe treated as petiole."
  ),
  h$rng(
    "leaf_length",
    "Laminae ... 110–220 × 120–170 cm",
    110,
    220,
    "cm",
    p = "species",
    notes = "Fern mapping under review: lamina treated as leaf; first dimension treated as length."
  ),
  h$rng(
    "leaf_width",
    "Laminae ... 110–220 × 120–170 cm",
    120,
    170,
    "cm",
    p = "species",
    notes = "Fern mapping under review: lamina treated as leaf; second dimension treated as width."
  ),
  h$rng(
    "leaflet_count",
    "pinnae pairs 12–22",
    12,
    22,
    "{count}",
    p = "species",
    em = "pinna pairs",
    notes = "Fern mapping under review: pinna-pair count stored as leaflet_count; this is a count of pairs, not individual pinnae."
  ),
  h$row(
    "leaf_shape",
    "Laminae ... ovate (in outline)",
    "ovate",
    "ovate",
    p = "species"
  ),
  h$row(
    "leaf_margin",
    "margin shallowly toothed and more deeply so towards apex",
    "toothed",
    "toothed",
    p = "species",
    em = "pinnules",
    notes = "Fern mapping under review: pinnule margin treated as leaf margin."
  ),
  h$row(
    "leaf_hairs_adult_leaves",
    "lower surface of veins and veinlets of mature plants more or less densely covered with pale brown appressed hairs",
    "pale brown appressed hairs",
    "hairy",
    p = "species",
    em = "abaxial veins and veinlets",
    notes = "Fern mapping under review: mature lamina hairs treated as adult-leaf hairs."
  ),
  h$row(
    "leaf_surface_colour",
    "Laminae mid-green above",
    "mid-green",
    "green",
    p = "species",
    em = "adaxial surface"
  ),
  h$row(
    "leaflet_length",
    "largest pinnae at or near base to c. 94 × 33 cm",
    94,
    94,
    "maximum",
    "cm",
    p = "species",
    em = "largest pinnae",
    notes = "Fern mapping under review: pinnae treated as leaflets; first dimension treated as length."
  ),
  h$row(
    "leaflet_width",
    "largest pinnae at or near base to c. 94 × 33 cm",
    33,
    33,
    "maximum",
    "cm",
    p = "species",
    em = "largest pinnae",
    notes = "Fern mapping under review: pinnae treated as leaflets; second dimension treated as width."
  ),
  h$rng(
    "leaflet_length",
    "the largest pinnules 5–20 × 3–4 mm",
    5,
    20,
    "mm",
    p = "species",
    em = "largest pinnules",
    notes = "Fern mapping under review: pinnules treated as leaflets; first dimension treated as length."
  ),
  h$rng(
    "leaflet_width",
    "the largest pinnules 5–20 × 3–4 mm",
    3,
    4,
    "mm",
    p = "species",
    em = "largest pinnules",
    notes = "Fern mapping under review: pinnules treated as leaflets; second dimension treated as width."
  ),
  h$un(
    "Rhizomes prostrate, 77–100 cm long, 10–32 cm diameter, young parts with dense brown shining hairs, to 5 cm long.",
    "rhizome | position, dimensions and hairs",
    p = "species"
  ),
  h$un(
    "Stipes green when young, reddish brown when mature, 80–225 cm long, 1–2 cm diameter, base densely hairy for 1/5 to 2/5 of stipe length, softly short-hairy when young, glabrescent when old.",
    "stipe | colour, diameter and hairs",
    p = "species"
  ),
  h$un(
    "Laminae ... glaucous beneath, young plants covered all over by soft, pale brown hairs, when dry papery.",
    "lamina | abaxial colour, juvenile hairs and texture",
    p = "species"
  ),
  h$un(
    "pinnae pairs ... arranged more or less alternately, ovate (in outline)",
    "pinnae | arrangement and shape",
    p = "species"
  ),
  h$un(
    "the proximal pinnules with short stalk 1–4 mm long, the distal pinnules sessile, all pinnules lobed almost to the vein, lobes slightly oblique, 2–5 mm apart, ... apex broadly pointed",
    "pinnules | stalk, lobing, lobe position and apex",
    p = "species"
  ),
  h$un(
    "Sori 1–10 pairs on each pinnule ... indusia 0.5–0.8 × 0.5–0.8 mm, pale green, stiff, outer indusium round, inner elongating ... oblong.",
    "sori and indusia | count, position, dimensions, colour, texture and shape",
    p = "species"
  ),
  h$un(
    "Distribution. NE India, S China, and Taiwan, Ryukyu Islands, Sumatra, Peninsular Malaysia, Java, Borneo (Kalimantan and Sarawak), Maluku and New Guinea. In Peninsular Malaysia, all states except Perlis, Negeri Sembilan, Melaka and Terengganu.",
    "distribution | geographic range",
    p = "species",
    sec = "Distribution",
    region = "Peninsular Malaysia"
  ),
  h$un(
    "Conservation status. Least Concern.",
    "conservation status | IUCN category",
    p = "species",
    sec = "Conservation status",
    region = "Peninsular Malaysia"
  ),
  h$un(
    "Ecology. On slopes and stream banks in lowland rain forest from 60 m altitude to lower montane forest up to 1670 m or in rocky places on quartzite ridges, sandstone and granite cliffs (but not on calcareous soils), and on open, bare earth banks and paths recently cut through forest.",
    "ecology | habitat, substrate and altitude",
    p = "species",
    sec = "Ecology",
    region = "Peninsular Malaysia"
  ),
  h$un(
    "Holttum (1963) stated that the species probably spreads by invading landslides.",
    "dispersal | hedged mechanism",
    p = "species",
    sec = "Ecology",
    region = "Peninsular Malaysia",
    source_id = "Holttum_1963"
  ),
  h$un(
    "Uses. Lowry ... recorded trade of golden chicken plants ...",
    "uses | trade and ornamental use",
    p = "notes",
    sec = "Uses",
    region = "Peninsular Malaysia",
    source_id = "Lowry_1970"
  ),
  h$un(
    "In Asia, including Peninsular Malaysia, the hairs on the rhizome have long been used to staunch bleeding wounds ... young leaves are eaten as ulam ...",
    "uses | medicinal and food use",
    p = "notes",
    sec = "Uses",
    region = "Peninsular Malaysia",
    source_id = "Praptosuwiryo_2003"
  ),
  h$un(
    "It is not certain whether this species is completely trunkless ... specimen label ... reported the plant had a trunk ‘four feet tall’ ...",
    "stem | uncertain trunk presence and non-Malaysian height",
    p = "notes",
    sec = "Notes",
    region = "Peninsular Malaysia"
  )
) |>
  flora_finalise(yaml::read_yaml("config/traits.yml")$traits$elements)

write.csv(
  d,
  "data/Nor-Ezzawanis_2010/raw/trait_data.csv",
  row.names = FALSE,
  na = ""
)
write.csv(
  flora_unmapped_summary(d),
  "data/Nor-Ezzawanis_2010/raw/unmapped_terms.csv",
  row.names = FALSE,
  na = ""
)
