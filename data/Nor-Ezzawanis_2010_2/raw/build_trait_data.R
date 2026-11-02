# Hand extraction of Casuarinaceae (A.T. Nor-Ezzawanis 2010, Flora of Peninsular
# Malaysia Series II vol. 1: 87--91). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Casuarinaceae.pdf, a scan with an Acrobat
# OCR layer (scanned_ocr). Every clause below was checked against page images
# (220 dpi) and is copied from raw/Casuarinaceae_transcription.txt. PDF page =
# printed page - 79 (pp. 87--91 on PDF 8--12; PDF 10 is Figure 1).
#
# One family, one genus, one species, so rows are written by hand with
# flora_hand() (Yao_2010 / Berhaman_1995 pattern). Family and genus rows stay at
# their own rank; values are copied down by hand under the flora_copy_down()
# rules (unqualified, single level, the species is silent, most specific rank
# wins). No hedged statement was found, so none is scored. The family Taxonomy
# paragraph (relationships to other families) and the vernacular names are not
# recorded. Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(87:91, \(p) c(p - 79L, p)), paste0("p", 87:91))
PM <- "Peninsular Malaysia"

# Row helpers for one taxon with default rank_scored_from, region and section.
mk <- function(taxon, rank, from = "", region = "", sec = "Description") {
  h <- flora_hand(taxon, pg, rank)
  map(h, \(f) {
    \(...) {
      a <- list(...)
      a$rank_from <- a$rank_from %||% from
      a$region <- a$region %||% region
      a$sec <- a$sec %||% sec
      do.call(f, a)
    }
  })
}

# ---- Family -----------------------------------------------------------------
f <- mk("Casuarinaceae", "family", "family", sec = "Family description")
FE <- "Family Ecology"
T1 <- "Trees (elsewhere shrubs, e.g., Allocasuarina and Gymnostoma), dioecious or less commonly monoecious."
TW <- "Twigs of two kinds: brown stout twigs, green only at the tips, soon becoming woody and developing into branches; and green needle-like twigs with pale swollen joints, borne on the woody twigs, branched or unbranched, deciduous, not thickened nor becoming woody, internodes round and finely grooved or angled."
LV <- "Leaves arranged in whorls of 4–20, reduced to minute teeth in a sheath surrounding the nodes of the green needle-like twigs, apex rounded-acute to narrowly acute."
MI <- "Male inflorescences usually terminal on the green needle-like twigs, simple or compound, erect, spike-like, flowers arranged in whorls, subtended by bracts."
FI <- "Female inflorescence a globose or ovoid head, terminal or subterminal on both brown and green twigs, flowers arranged in whorls, each subtended by a bract."
MF <- "Male flowers: bracteoles 2, small, scale-like, perianth absent; stamen 1, filaments long with small, basifixed anthers."
FF <- "Female flowers: bracteoles 2, perianth absent; ovary small with two long stigmatic arms."
IN <- "Infructescence a globose or ellipsoid cone-like structure comprising alternate whorls of woody bracts and bracteole-pairs with each of the bracteole-pair tightly enclosing one fruit; at maturity the woody bracteole-pairs gaping open to release the fruit."
FR <- "Fruit a samaroid nut with a pair of membranous wings."
SD <- "Seed one per fruit, without endosperm."
E1 <- "The flowers are wind-pollinated."
E2 <- "The flat, light, winged fruits are wind-dispersed."
family <- bind_rows(
  f$row(
    "plant_growth_form", T1, "trees", "tree", p = "p87",
    notes = "'elsewhere shrubs' refers to genera outside Peninsular Malaysia; not scored."
  ),
  f$row("sex_type", T1, "dioecious", "dioecious", q = "usually", p = "p87"),
  f$row("sex_type", T1, "monoecious", "monoecious", q = "less commonly", p = "p87"),
  f$un(TW, "twigs | dimorphic woody and green needle-like twigs, branching, shedding, internodes", p = "p87"),
  f$row(
    "leaf_phyllotaxis", LV, "whorls", "whorled", p = "p87",
    notes = "Scale leaves on the twig nodes."
  ),
  f$row(
    "leaf_type", LV, "reduced_to_minute_teeth", "scale", p = "p87",
    notes = "'reduced to minute teeth in a sheath' scored as scale leaves."
  ),
  f$un(LV, "leaves | number per whorl (4--20)", p = "p87"),
  f$un(LV, "leaves | apex rounded-acute to narrowly acute", p = "p87"),
  f$row(
    "inflorescence_type", MI, "terminal", "terminal", q = "usually",
    em = "male inflorescences", p = "p87"
  ),
  f$row(
    "inflorescence_type", MI, "spike-like", "spike", em = "male inflorescences",
    p = "p87", notes = "'spike-like' scored as spike."
  ),
  f$un(MI, "male inflorescences | simple or compound, erect, flowers whorled, bracts", p = "p87"),
  f$row("inflorescence_type", FI, "head", "head", em = "female inflorescences", p = "p87"),
  f$un(FI, "female inflorescences | shape globose or ovoid", p = "p87"),
  f$un(FI, "female inflorescences | position terminal or subterminal on brown and green twigs", p = "p87"),
  f$un(MF, "male flowers | bracteoles 2, scale-like", p = "p87"),
  f$un(MF, "male flowers | perianth absent (negative phrase)", p = "p87"),
  f$row(
    "flower_fertile_stamens_count", MF, "1", "1", "mean", "{count}",
    em = "male flowers", p = "p87"
  ),
  f$un(MF, "male flowers | filaments long", p = "p87"),
  f$row(
    "flower_anther_attachment", MF, "basifixed", "basifixed", em = "male flowers",
    p = "p87"
  ),
  f$un(FF, "female flowers | bracteoles 2", p = "p87"),
  f$un(FF, "female flowers | perianth absent (negative phrase)", p = "p87"),
  f$un(FF, "female flowers | ovary small, two long stigmatic arms", p = "p87"),
  f$un(IN, "infructescence | cone-like, globose or ellipsoid, woody bracts and bracteole-pairs", p = "p87"),
  f$row(
    "fruit_type", FR, "samaroid_nut", "samara", p = "p87",
    notes = "'samaroid nut' scored as samara."
  ),
  f$row("dispersal_appendage", FR, "pair_of_membranous_wings", "wings", p = "p87"),
  f$row("seeds_per_fruit", SD, "1", "1", "mean", "{count}/{count}", p = "p87"),
  f$un(SD, "seed | endosperm absent (negative phrase)", p = "p87"),
  f$un(
    "A southern hemisphere family with 4 genera and 95 species, largely Australian, distributed to the SW Pacific and throughout Malesia to SE Asia (Mabberley, (2008). In Peninsular Malaysia, one genus, Casuarina, with one native species, C. equisetifolia. Casuarina junghuhniana Miq., Gymnostoma nobile (Whitmore) L.A.S.Johnson and G. sumatranum (Jungh. ex de Vriese) L.A.S.Johnson are planted as ornamentals (Boo et al., 1001 Gard. Pl. Sing. (2006) 545).",
    "distribution | range and planted species", sec = "Family Distribution", p = "p87",
    source_id = "Mabberley_2008"
  ),
  f$row("pollination_syndrome", E1, "wind-pollinated", "wind", sec = FE, p = "p87"),
  f$row("dispersal_syndrome", E2, "wind-dispersed", "anemochory", sec = FE, p = "p87"),
  f$row("dispersers", E2, "wind-dispersed", "wind", sec = FE, p = "p87"),
  f$row("dispersal_unit", E2, "fruits", "fruit", sec = FE, p = "p87"),
  f$un(
    "At anthesis, the filaments elongate exposing the anthers, and the stigmatic arms also project out from the subtending bracteoles.",
    "flowers | anthesis", sec = FE, p = "p87"
  )
)

# ---- Genus (statements in the family Ecology paragraph that name Casuarina,
# and the genus treatment) ---------------------------------------------------
g <- mk("Casuarina", "genus", "genus", sec = "Genus description")
NF <- "Root nodules, containing symbiotic nitrogen-fixing bacteria, enable Casuarina species to grow in such nutrient deficient habitats as sandy beaches (Corner, 1988)."
GT <- "Trees, bole straight, cylindrical; crown initially conical, then spreading with age."
GW <- "Green twigs usually unbranched; internodes terete, 5–15 mm long, furrows deep."
GL <- "Scale leaves 5–18 per node, apex narrowly acute."
GM <- "Male inflorescences simple spikes with very short internodes, each node with many male flowers."
GFF <- "Female flowers: ovary superior, initially 2-locular but later becoming 1-locular by abortion, ovules 2 per locule but only one developing, style short with two elongated stigmatic arms."
GN <- "Samaroid nut glabrous, dull grey or pale yellow-brown."
genus <- bind_rows(
  g$un(
    "Casuarina is confined to sandy beaches in Peninsular Malaysia.",
    "habitat | sandy beaches", sec = FE, p = "p87", region = PM
  ),
  g$row(
    "nitrogen_fixing", NF, "root_nodules_with_nitrogen-fixing_bacteria",
    "nitrogen_fixer", sec = FE, p = "p87", source_id = "Corner_1988",
    notes = "Statement about Casuarina species, printed in the family Ecology paragraph and cited from Corner (1988)."
  ),
  g$row("plant_growth_form", GT, "trees", "tree", p = "p88"),
  g$un(GT, "bole | straight, cylindrical", p = "p88"),
  g$row(
    "plant_canopy_form", GT, "spreading", "spreading", p = "p88",
    notes = "Crown of older trees; 'initially conical' (young trees) has no level and is not scored."
  ),
  g$un(GW, "green twigs | branching, internode shape, length and furrows", p = "p88"),
  g$un(GL, "scale leaves | number per node (5--18)", p = "p88"),
  g$row(
    "leaf_apex_shape", GL, "narrowly_acute", "acute", p = "p88",
    notes = "'narrowly acute' scored as acute."
  ),
  g$row(
    "inflorescence_type", GM, "spikes", "spike", em = "male inflorescences",
    p = "p88"
  ),
  g$un(GM, "male inflorescences | internodes very short, many flowers per node", p = "p88"),
  g$un(
    "Female inflorescences lateral on persistent woody twigs.",
    "female inflorescences | position lateral on woody twigs", p = "p88"
  ),
  g$un(
    "Male flowers: at anthesis with filaments elongating and exposing the anthers.",
    "male flowers | anthesis", p = "p88"
  ),
  g$row(
    "flower_ovary_position", GFF, "superior", "superior", em = "female flowers",
    p = "p88"
  ),
  g$un(GFF, "female flowers | locules 2 becoming 1, ovules 2 per locule, one developing", p = "p88"),
  g$un(GFF, "female flowers | style short, two elongated stigmatic arms", p = "p88"),
  g$un(
    "Cone-like infructescence with thin bracts in exposed part; bracteoles protruding.",
    "infructescence | bracts thin, bracteoles protruding", p = "p88"
  ),
  g$row(
    "fruit_type", GN, "samaroid_nut", "samara", p = "p88",
    notes = "'samaroid nut' scored as samara."
  ),
  g$row("fruit_surface_hairs", GN, "glabrous", "glabrous", p = "p88"),
  g$row(
    "fruit_colour", GN, "dull_grey pale_yellow-brown", "grey brown", p = "p88",
    notes = "'pale yellow-brown' scored as brown."
  ),
  g$un(
    "Seventeen species in SE Asia, Malesia, Melanesia, Polynesia, New Caledonia, and Australia (Wilson & Johnson, 1989).",
    "distribution | range", sec = "Genus Distribution", p = "p88",
    source_id = "Wilson_1989"
  ),
  g$un(
    "On infertile, sandy or acid soils; coastal or in mountains.",
    "habitat | soils, coastal or montane", sec = "Genus Ecology", p = "p88"
  )
)

# ---- Casuarina equisetifolia -------------------------------------------------
s <- mk("Casuarina equisetifolia", "species", region = PM)
H <- "Small to very big tree, 5–50(–70) m tall, 27–100(–200) cm diameter, usually dioecious, rarely monoecious;"
BT <- "sometimes with buttresses to 76 cm high;"
CR <- "crown symmetrical, sometimes asymmetrical, lowest branches of large trees usually arising at c. 3 m above the ground."
BK <- "Bark usually greyish brown to grey, sometimes pale red or black, finely scaly-fissured to irregularly coarsely scaly-fissured, easily broken; outer bark brown;"
IB <- "inner bark usually pinkish, sometimes pale brown to red, fibrous, soft."
WT <- "Woody twigs terete, reddish brown to almost black when dry, hairy or glabrous at base, hairy towards the point of attachment of the green twigs, 2–3 mm thick."
GTW <- "Green twigs usually unbranched, terete, usually drying greenish yellow, sometimes reddish brown, to 34 cm long; nodes usually drying yellow, sometimes pale brown, internodes to 1.3 cm long, 0.5–1 mm thick, furrows hairy to glabrous."
SL <- "Scale leaves (6–)7(–8) per node, broadest at base, apex narrowly acute; on woody twigs to 4 mm long, base hairy, apex recurved; on green twigs 0.5–1 mm long, base glabrous, apex erect."
SMI <- "Male inflorescences simple elongated spikes to 9 cm long."
SFI <- "Female inflorescences ellipsoid, to 6 mm long, 4 mm diameter."
SMF <- "Male flowers: bracts 0.8–1 mm long, transparent with white hairs; stamens with filaments 1.2–3 mm long, translucent in life, drying mid-brown, anthers broadly oblong, c. 0.5 mm long, golden brown in life, drying mid-brown."
SFF <- "Female flowers with bracts 0.8–1 mm long, creamy white, with white hairs on margin; ovary white, c. 0.5–1 × 0.2 mm, stigmatic arms red, 1.5–5 mm long."
SIN <- "Infructescences: globose or ovoid-cylindric, to 2 cm long, 1.5 cm diameter; peduncles 2–20 mm long; bracts obovate, apex acute, c. 4.5 × 2.5 mm; bracteoles obovate, c. 4 × 2 mm, apex acute, outside with short, white hairs."
SN <- "Samaroid nut with the wing pair connate, broadly elliptic, transparent, pale yellow-brown, c. 3.5 cm long, pointed at apex."
sp <- bind_rows(
  s$row("plant_growth_form", H, "tree", "tree", p = "p88"),
  s$row("plant_height", H, "5", "5", "minimum", "m", "usually", p = "p88"),
  s$row("plant_height", H, "50", "50", "maximum", "m", "usually", p = "p88"),
  s$row("plant_height", H, "70", "70", "maximum", "m", "extreme_range", p = "p88"),
  s$row(
    "plant_diameter_breast_height", H, "27", "27", "minimum", "cm", "usually", p = "p88",
    notes = "Stem 'diameter'; height of measurement not stated."
  ),
  s$row(
    "plant_diameter_breast_height", H, "100", "100", "maximum", "cm", "usually", p = "p88",
    notes = "Stem 'diameter'; height of measurement not stated."
  ),
  s$row(
    "plant_diameter_breast_height", H, "200", "200", "maximum", "cm", "extreme_range",
    p = "p88", notes = "Stem 'diameter'; height of measurement not stated."
  ),
  s$row("sex_type", H, "dioecious", "dioecious", q = "usually", p = "p88"),
  s$row("sex_type", H, "monoecious", "monoecious", q = "rarely", p = "p88"),
  s$row("stem_base_form", BT, "buttresses", "buttressed", q = "sometimes", p = "p88"),
  s$un(BT, "buttresses | height to 76 cm", p = "p88"),
  s$un(CR, "crown | symmetrical, sometimes asymmetrical; height of lowest branches", p = "p88"),
  s$row(
    "bark_colour", BK, "greyish_brown grey", "brown grey", q = "usually", p = "p88",
    notes = "'greyish brown' scored as brown."
  ),
  s$row("bark_colour", BK, "pale_red black", "red black", q = "sometimes", p = "p88"),
  s$row(
    "bark_texture", BK, "scaly-fissured", "furrowed", p = "p88",
    notes = "'finely scaly-fissured to irregularly coarsely scaly-fissured' scored as furrowed; scaly has no level."
  ),
  s$un(BK, "bark | easily broken", p = "p88"),
  s$row("bark_colour", BK, "brown", "brown", em = "outer bark", p = "p88"),
  s$row("bark_colour", IB, "pinkish", "pink", q = "usually", em = "inner bark", p = "p88"),
  s$row(
    "bark_colour", IB, "pale_brown red", "brown_light red", q = "sometimes",
    em = "inner bark", p = "p88"
  ),
  s$un(IB, "inner bark | fibrous, soft", p = "p88"),
  s$un(
    "Sapwood yellowish to yellow, hard.",
    "sapwood | colour and hardness", p = "p88"
  ),
  s$un(WT, "woody twigs | shape, colour, hairs, thickness (2--3 mm)", p = "p88"),
  s$un(GTW, "green twigs | branching, shape, colour, length (to 34 cm)", p = "p88"),
  s$un(GTW, "green twigs | nodes colour, internode length (to 1.3 cm) and thickness, furrow hairs", p = "p88"),
  s$row(
    "leaf_type", SL, "scale_leaves", "scale", p = "p88",
    notes = "Leaves are scale leaves at the twig nodes; the green twigs are the photosynthetic organs."
  ),
  s$un(SL, "scale leaves | number per node (6--)7(--8)", p = "p88"),
  s$row(
    "leaf_apex_shape", SL, "narrowly_acute", "acute", p = "p90",
    notes = "'narrowly acute' scored as acute. Apex recurved on woody twigs, erect on green twigs (not scored)."
  ),
  s$un(SL, "scale leaves | broadest at base; base hairy or glabrous; apex recurved or erect by twig type", p = "p90"),
  s$row(
    "leaf_length", SL, "4", "4", "maximum", "mm", em = "scale leaves on woody twigs",
    p = "p90"
  ),
  s$row(
    "leaf_length", SL, "0.5", "0.5", "minimum", "mm", em = "scale leaves on green twigs",
    p = "p90"
  ),
  s$row(
    "leaf_length", SL, "1", "1", "maximum", "mm", em = "scale leaves on green twigs",
    p = "p90"
  ),
  s$row("inflorescence_type", SMI, "spikes", "spike", em = "male inflorescences", p = "p90"),
  s$row(
    "inflorescence_shape", SMI, "elongated", "elongated", em = "male inflorescences",
    p = "p90"
  ),
  s$row(
    "inflorescence_length", SMI, "9", "9", "maximum", "cm", em = "male inflorescences",
    p = "p90"
  ),
  s$un(SFI, "female inflorescences | shape ellipsoid", p = "p90"),
  s$row(
    "inflorescence_length", SFI, "6", "6", "maximum", "mm", em = "female inflorescences",
    p = "p90"
  ),
  s$row(
    "inflorescence_diameter", SFI, "4", "4", "maximum", "mm", em = "female inflorescences",
    p = "p90"
  ),
  s$row(
    "flower_bract_length", SMF, "0.8", "0.8", "minimum", "mm", em = "male flowers",
    p = "p90"
  ),
  s$row(
    "flower_bract_length", SMF, "1", "1", "maximum", "mm", em = "male flowers",
    p = "p90"
  ),
  s$un(SMF, "male flowers | bracts transparent with white hairs", p = "p90"),
  s$row(
    "flower_filament_length", SMF, "1.2", "1.2", "minimum", "mm", em = "male flowers",
    p = "p90"
  ),
  s$row(
    "flower_filament_length", SMF, "3", "3", "maximum", "mm", em = "male flowers",
    p = "p90"
  ),
  s$un(SMF, "male flowers | filaments translucent in life, drying mid-brown", p = "p90"),
  s$un(SMF, "male flowers | anthers broadly oblong", p = "p90"),
  s$row(
    "flower_anther_length", SMF, "0.5", "0.5", "mean", "mm", em = "male flowers",
    p = "p90"
  ),
  s$row(
    "flower_anther_colour", SMF, "golden_brown", "brown", em = "male flowers",
    p = "p90", notes = "Colour in life; 'golden brown' scored as brown. Dry anthers mid-brown (not scored)."
  ),
  s$row(
    "flower_bract_length", SFF, "0.8", "0.8", "minimum", "mm", em = "female flowers",
    p = "p90"
  ),
  s$row(
    "flower_bract_length", SFF, "1", "1", "maximum", "mm", em = "female flowers",
    p = "p90"
  ),
  s$un(SFF, "female flowers | bracts creamy white, margin with white hairs", p = "p90"),
  s$un(SFF, "female flowers | ovary white, c. 0.5--1 x 0.2 mm", p = "p90"),
  s$un(SFF, "female flowers | stigmatic arms red", p = "p90"),
  s$row(
    "flower_stigma_length", SFF, "1.5", "1.5", "minimum", "mm", em = "female flowers",
    p = "p90", notes = "Length of the stigmatic arms."
  ),
  s$row(
    "flower_stigma_length", SFF, "5", "5", "maximum", "mm", em = "female flowers",
    p = "p90", notes = "Length of the stigmatic arms."
  ),
  s$un(SIN, "infructescence | shape globose or ovoid-cylindric, to 2 cm long, 1.5 cm diameter", p = "p90"),
  s$row(
    "peduncle_length", SIN, "2", "2", "minimum", "mm", em = "infructescences", p = "p90"
  ),
  s$row(
    "peduncle_length", SIN, "20", "20", "maximum", "mm", em = "infructescences", p = "p90"
  ),
  s$un(SIN, "infructescence | bracts and bracteoles shape, size, apex and hairs", p = "p90"),
  s$row(
    "fruit_type", SN, "samaroid_nut", "samara", p = "p90",
    notes = "'samaroid nut' scored as samara."
  ),
  s$row("dispersal_appendage", SN, "wing_pair", "wings", p = "p90"),
  s$row(
    "fruit_colour", SN, "pale_yellow-brown", "brown", p = "p90",
    notes = "'pale yellow-brown' scored as brown; the clause may describe the wing ('transparent')."
  ),
  s$un(SN, "fruit | broadly elliptic, transparent, pointed at apex", p = "p90"),
  # Suspect value (TFSS vol. 2 gives the samara as 0.6--0.8 cm): not scored.
  s$un(
    SN, "fruit | length c. 3.5 cm, suspect | candidate fruit_length, not scored (metadata question 1)",
    p = "p90"
  ),
  s$un(
    "Widespread from Myanmar and Vietnam, throughout Malesia to N and NE Australia, and thence to Melanesia, and Polynesia. Although commonly planted as an ornamental tree, this species is native on sandy coasts in Kedah (Langkawi), Penang (Muka Head), Perak (Teluk Batik), Selangor (Jugra FR), Negeri Sembilan (Port Dickson), Melaka (Pulau Besar) and is widespread on the East Coast from Terengganu to Johor.",
    "distribution | range and native localities", sec = "Distribution", p = "p90"
  ),
  s$un("Least concern.", "conservation status | Least concern", sec = "Conservation status", p = "p90"),
  s$un(
    "Casuarina equisetifolia occurs naturally along tropical and subtropical sandy coastlines. In Peninsular Malaysia it occurs wild as a light-demanding pioneer tree on sandy coasts, on open sand above the high-water mark where it forms a strip of forest (Corner, 1988). The fallen twigs prevent self regeneration, and often seedlings of Syzygium grande (Wight) Walp. become established in the shade of the Casuarina trees, and eventually transform the vegetation into S. grande forest with a narrow strip of Casuarina on the seaward side.",
    "habitat | sandy coasts, light-demanding pioneer, regeneration", sec = "Ecology",
    p = "p90", source_id = "Corner_1988"
  ),
  s$un(
    "Mabberley (2008) reported that it produces the 'best firewood in the world'. In Peninsular Malaysia, it is commonly planted as an ornamental tree and is also used for hedges and topiary because it is easily pruned. Being a pioneer seashore tree, it is important as a windbreak for coastal resorts.",
    "uses | firewood, ornamental, hedges, windbreak", sec = "Uses", p = "p91",
    source_id = "Mabberley_2008"
  )
)

# ---- Copy-down -----------------------------------------------------------------
# Family and genus values copied to the species where its description is silent.
# Every value is a single, unqualified level (checked by hand); the species
# itself scores plant_growth_form, sex_type, leaf_type, leaf_apex_shape, male
# inflorescence_type, fruit_type, fruit_colour and dispersal_appendage, so those
# are not copied. Numbers (stamen count, seeds per fruit) are never copied.
higher <- bind_rows(family, genus)
copy <- tribble(
  ~from, ~trait, ~entity_measured,
  "Casuarinaceae", "leaf_phyllotaxis", "",
  "Casuarinaceae", "inflorescence_type", "female inflorescences",
  "Casuarinaceae", "flower_anther_attachment", "male flowers",
  "Casuarinaceae", "pollination_syndrome", "",
  "Casuarinaceae", "dispersal_syndrome", "",
  "Casuarinaceae", "dispersers", "",
  "Casuarinaceae", "dispersal_unit", "",
  "Casuarina", "nitrogen_fixing", "",
  "Casuarina", "plant_canopy_form", "",
  "Casuarina", "flower_ovary_position", "female flowers",
  "Casuarina", "fruit_surface_hairs", ""
)
copied <- copy |>
  inner_join(
    higher,
    by = c(from = "taxon_name", "trait", "entity_measured")
  ) |>
  mutate(
    notes = str_squish(paste0("Copied from the ", from, " description. ", notes)),
    taxon_name = "Casuarina equisetifolia",
    taxon_rank = "species"
  ) |>
  select(-from)

# Copy-down checks: each source row is unqualified and single-level, and the
# species does not already score the trait for that entity.
stopifnot(
  nrow(copied) == nrow(copy),
  all(copied$commonness_qualifier == ""),
  all(copied$value_type == "mode"),
  !any(str_detect(copied$trait_value_clean, " ")),
  nrow(semi_join(copied, sp, by = c("trait", "entity_measured"))) == 0
)

d <- bind_rows(higher, sp, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Nor-Ezzawanis_2010_2/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(d),
  "data/Nor-Ezzawanis_2010_2/raw/unmapped_terms.csv",
  na = ""
)

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Casuarinaceae", "family", "accepted", "Family description; one genus and one native species in Peninsular Malaysia.",
  "Casuarina", "genus", "accepted", "",
  "Casuarina equisetifolia", "species", "accepted", "Only accepted species heading.",
  "Casuarina junghuhniana", "species", "planted only", "Named in the family Distribution as planted ornamental; not treated, not scored.",
  "Gymnostoma nobile", "species", "planted only", "Named in the family Distribution as planted ornamental; not treated, not scored.",
  "Gymnostoma sumatranum", "species", "planted only", "Named in the family Distribution as planted ornamental; not treated, not scored."
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Nor-Ezzawanis_2010_2/raw/taxon_flags.csv", na = "")
