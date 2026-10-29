# Hand extraction of Casuarinaceae (Runi S. Pungga 1996, Tree Flora of Sabah
# and Sarawak vol. 2: 119--127). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Casuarinaceae.pdf, the 2007 electronic
# version of the 1996 volume (L.G. Saw & B. Aslina); born-digital
# (assets/myflora_metadata.csv), so clauses are read from the PDF text layer.
# PDF page = printed page - 112. Figure pages (pdf 10, 12, 14 = printed 122,
# 124, 126) are plates; their captions give vouchers only. PDF 14 and 15 were
# image-checked (garbled caption; Symbol-font multiplication sign).
#
# Small family (3 genera, 4 species), so rows are written by hand with
# flora_hand() (Yao_2010 / Berhaman_1995 pattern). Source repairs are in
# raw/source_fixes.csv and are already applied to the verbatim clauses below.
# Higher-rank values are copied down by hand under the flora_copy_down() rules
# (unqualified, single level, every member that scores the trait agrees, most
# specific rank wins). Writes trait_data.csv, unmapped_terms.csv and
# taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(119:127, \(p) c(p - 112L, p)), paste0("p", 119:127))
SS <- "Sabah and Sarawak"
MI <- "male inflorescence"
FI <- "female inflorescence"
MF <- "male flower"
FF <- "female flower"
SA <- "samara"
CL <- "cladode"

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

# ---- Family ---------------------------------------------------------------
f <- mk("Casuarinaceae", "family", "family", sec = "Family description")
family <- bind_rows(
  f$row(
    "sex_type",
    "Dioecious or less commonly monoecious trees or shrubs",
    "dioecious",
    "dioecious",
    q = "usually",
    p = "p119"
  ),
  f$row(
    "sex_type",
    "Dioecious or less commonly monoecious trees or shrubs",
    "monoecious",
    "monoecious",
    q = "less often",
    p = "p119",
    notes = "'less commonly' recorded as 'less often'."
  ),
  f$row(
    "plant_growth_form",
    "Dioecious or less commonly monoecious trees or shrubs",
    "trees shrubs",
    "tree shrub",
    p = "p119"
  ),
  f$un("bole straight, cylindrical", "bole | form", p = "p119"),
  f$row(
    "stem_base_form",
    "bole straight, cylindrical, sometimes fluted; short buttresses sometimes present",
    "fluted buttresses",
    "fluted buttressed",
    q = "sometimes",
    p = "p119"
  ),
  f$un("short buttresses", "buttresses | size", p = "p119"),
  f$row(
    "bark_colour",
    "Bark greyish to reddish brown",
    "greyish_to_reddish_brown",
    "grey red brown",
    p = "p119",
    notes = "Read as 'greyish brown to reddish brown'; every colour word scored."
  ),
  f$row(
    "bark_texture",
    "rough, fissured and shedding off into narrow, elongate flakes",
    "rough fissured flakes",
    "rough furrowed flaky",
    p = "p119"
  ),
  f$un(
    "Twigs of two kinds, woody and persistent, and non-woody and caducous; the latter modified into green needle-shaped structures, branched or unbranched, superficially resembling those of pine trees",
    "needle-shaped twigs | dimorphic twigs, deciduous branchlets, branching",
    p = "p119"
  ),
  f$row(
    "plant_photosynthetic_organ",
    "the latter modified into green needle-shaped structures",
    "green_needle-shaped_structures",
    "cladode",
    p = "p119",
    notes = "Needle-shaped twigs scored as cladode, as in the AusTraits flora datasets."
  ),
  f$un(
    "internodes rounded or quadrangular in cross-section, usually with longitudinal grooves or furrows, nodes swollen",
    "needle-shaped twigs | internode cross-section, furrows, nodes",
    p = "p119"
  ),
  f$row(
    "leaf_phyllotaxis",
    "Leaves in whorls of 4--18",
    "in whorls",
    "whorled",
    p = "p119"
  ),
  f$un("Leaves in whorls of 4--18", "leaf | number per whorl", p = "p119"),
  f$row("leaf_type", "Leaves ... tiny, scale-like", "scale-like", "scale", p = "p119"),
  f$row(
    "leaf_apex_shape",
    "apex rounded-acute to narrowly acute",
    "rounded-acute narrowly_acute",
    "rounded acute",
    p = "p119"
  ),
  f$row(
    "inflorescence_type",
    "Male inflorescences usually terminal",
    "terminal",
    "terminal",
    q = "usually",
    em = MI,
    p = "p119"
  ),
  f$un(
    "Male inflorescences ... simple or compound, erect, spike-like structures",
    "male inflorescence | branching, orientation, spike-like",
    p = "p119"
  ),
  f$row(
    "flower_perianth_parts_count",
    "each male flower of one or two scale-like perianth lobes (falling at anthesis)",
    "1--2",
    "1",
    "minimum",
    "{count}",
    em = MF,
    p = "p119"
  ),
  f$row(
    "flower_perianth_parts_count",
    "each male flower of one or two scale-like perianth lobes (falling at anthesis)",
    "1--2",
    "2",
    "maximum",
    "{count}",
    em = MF,
    p = "p119"
  ),
  f$un(
    "scale-like perianth lobes (falling at anthesis)",
    "male perianth | form, caducous",
    p = "p119"
  ),
  f$row(
    "flower_fertile_stamens_count",
    "a single stamen with basifixed anther",
    "1",
    "1",
    "mean",
    "{count}",
    em = MF,
    p = "p119"
  ),
  f$row(
    "flower_anther_attachment",
    "a single stamen with basifixed anther",
    "basifixed",
    "basifixed",
    em = MF,
    p = "p119"
  ),
  f$row(
    "inflorescence_shape",
    "Female inflorescence a globose cone",
    "globose",
    "spherical",
    em = FI,
    p = "p119"
  ),
  f$un(
    "terminal or subterminal on woody persistent twigs or on green needle-shaped twigs",
    "female inflorescence | position (terminal or subterminal)",
    p = "p119"
  ),
  f$row(
    "flower_structural_carpels_count",
    "each female flower of one-carpellate ovary",
    "1",
    "1",
    "mean",
    "{count}",
    em = FF,
    p = "p119"
  ),
  f$un(
    "with 2-branched, divergent stigmas, subtended by a bract and two bracteoles",
    "female flower | stigma branches, bract and bracteoles",
    p = "p119"
  ),
  f$un(
    "Fruit cone (infructescence) a woody structure developed from a whole female inflorescence, with several alternating woody bracts, each subtending a pair of woody bracteoles, bracteole-pairs gaping open at maturity",
    "fruit cone | infructescence type (compound fruit), bracts, bracteoles",
    p = "p119"
  ),
  f$row(
    "fruit_type",
    "Fruitlet a samara with membranous wing",
    "samara",
    "samara",
    p = "p119"
  ),
  f$row(
    "dispersal_appendage",
    "Fruitlet a samara with membranous wing",
    "membranous wing",
    "wings",
    p = "p119",
    notes = "Wing of the fruitlet (samara), not of the seed."
  ),
  f$row(
    "seeds_per_fruit",
    "Seed solitary in each samara",
    "solitary",
    "1",
    "mean",
    "{count}/{count}",
    p = "p119"
  ),
  f$un(
    "often with more than one embryo, each with two large cotyledons",
    "seed | polyembryony, cotyledon number and size",
    p = "p119"
  ),
  f$un(
    "Four genera with about 100 species; from SE Asia to Australia and Polynesia. Three genera with 4 species in Sabah and Sarawak.",
    "distribution | range, species number",
    sec = "Family Distribution",
    p = "p119"
  ),
  f$un(
    "Often growing gregariously in sandy beach vegetation (Casuarina equisetifolia) and in kerangas, hill, and montane forest on ultramafic soils and limestone outcrops (Ceuthostoma and Gymnostoma spp.), from sea level to 2000 m.",
    "ecology | habitat, gregariousness, soil, altitude",
    sec = "Family Ecology",
    p = "p119"
  ),
  f$un(
    "In the past, timbers of Casuarina equisetifolia were used for shingles and other construction work, but they are now of little commercial importance. The timber is hard and heavy, excellent for firewood and for making charcoal and matches.",
    "uses | timber; wood hardness and weight",
    sec = "Family Uses",
    p = "p119"
  ),
  f$un(
    "Johnson ... reclassified Casuarina s.l. into 4 smaller genera ... based on the number of scale-leaves per node, branching pattern, the cross-sectional outline and position and arrangement of stomata in the grooves of the needle-shaped twigs, the structure of the infructescence, the form of the bracts and bracteoles, and chromosome numbers. ... it is difficult to distinguish Ceuthostoma from Gymnostoma using only sterile specimens and without the help of a scanning electron microscope.",
    "taxonomy | generic delimitation",
    sec = "Family Taxonomy",
    p = "p119"
  )
)

# ---- 1. Casuarina ---------------------------------------------------------
g <- mk("Casuarina", "genus", "genus", sec = "Genus description")
casuarina_g <- bind_rows(
  g$row(
    "sex_type",
    "Dioecious or monoecious trees",
    "dioecious monoecious",
    "dioecious monoecious",
    p = "p120"
  ),
  g$row("plant_growth_form", "Dioecious or monoecious trees", "trees", "tree", p = "p120"),
  g$un("bole cylindrical, straight", "bole | form", p = "p120"),
  g$un(
    "crown narrow, branches ascending, with a rather untidy appearance",
    "crown | shape, branch angle",
    p = "p120"
  ),
  g$row(
    "bark_colour",
    "Bark greyish to reddish brown",
    "greyish_to_reddish_brown",
    "grey red brown",
    p = "p120",
    notes = "Read as 'greyish brown to reddish brown'; every colour word scored."
  ),
  g$row(
    "bark_texture",
    "ridged, fissured and peeling off into oblong flakes",
    "fissured flakes",
    "furrowed flaky",
    p = "p120"
  ),
  g$un("ridged ... peeling off", "bark | ridged, peeling", p = "p120"),
  g$un(
    "Needle-shaped twigs mostly unbranched; internodes ... rounded in cross-section, furrows of internodes deep and closed",
    "needle-shaped twigs | branching, cross-section, furrows",
    p = "p120"
  ),
  g$rng(
    "leaf_length",
    "internodes 0.5--1.5 cm long",
    0.5,
    1.5,
    "cm",
    em = CL,
    p = "p120"
  ),
  g$row("leaf_type", "Scale-leaves 5--18 per node", "scale-leaves", "scale", p = "p120"),
  g$un("Scale-leaves 5--18 per node", "scale-leaf | number per node", p = "p120"),
  g$row(
    "leaf_apex_shape",
    "Scale-leaves ... narrowly acute",
    "narrowly acute",
    "acute",
    p = "p120"
  ),
  g$row(
    "inflorescence_type",
    "Male inflorescences simple, elongated spikes, at the end of long needle-shaped twigs",
    "spikes at_the_end",
    "spike terminal",
    em = MI,
    p = "p120",
    notes = "'at the end of' read as terminal."
  ),
  g$un(
    "Female inflorescences lateral on woody persistent twigs",
    "female inflorescence | position (lateral)",
    p = "p120"
  ),
  g$un(
    "Fruit cones globose or ovoid-cylindrical; bracts thin at the exposed portions, distally not expanded; bracteoles protruding slightly from the cone surface, not greatly thickened",
    "fruit cone | shape, bracts, bracteoles",
    p = "p120"
  ),
  g$row("fruit_type", "Samaras glabrous", "samaras", "samara", p = "p120"),
  g$row(
    "fruit_surface_hairs",
    "Samaras glabrous",
    "glabrous",
    "glabrous",
    em = SA,
    p = "p120"
  ),
  g$row(
    "fruit_colour",
    "Samaras ... dull grey or pale yellow-brown",
    "grey pale_yellow-brown",
    "grey yellow brown",
    em = SA,
    p = "p120"
  ),
  g$un("dull", "samara | surface lustre", p = "p120"),
  g$un(
    "About 17 species; from the Bay of Bengal to Malesia, Melanesia, Polynesia, New Caledonia and Australia. Only C. equisetifolia is found in Borneo (Sabah, Sarawak, Brunei and Kalimantan).",
    "distribution | range, species number",
    sec = "Genus Distribution",
    p = "p120"
  ),
  g$un(
    "Found on infertile, sandy or acidic soils along the coasts (C. equisetifolia) and elsewhere (other species) on mountains.",
    "ecology | soil, habitat",
    sec = "Genus Ecology",
    p = "p121"
  )
)

s <- mk("Casuarina equisetifolia", "species", region = SS)
casuarina_equisetifolia <- bind_rows(
  s$row(
    "sex_type",
    "Monoecious trees to 40 m tall, 100 cm diameter",
    "monoecious",
    "monoecious",
    p = "p121"
  ),
  s$row(
    "plant_growth_form",
    "Monoecious trees to 40 m tall, 100 cm diameter",
    "trees",
    "tree",
    p = "p121"
  ),
  s$row(
    "plant_height",
    "Monoecious trees to 40 m tall, 100 cm diameter",
    40,
    40,
    "maximum",
    "m",
    p = "p121"
  ),
  s$row(
    "plant_diameter_breast_height",
    "Monoecious trees to 40 m tall, 100 cm diameter",
    100,
    100,
    "maximum",
    "cm",
    p = "p121",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$un("bole straight, cylindrical", "bole | form", p = "p121"),
  s$row(
    "stem_base_form",
    "usually fluted towards the base",
    "fluted",
    "fluted",
    q = "usually",
    p = "p121"
  ),
  s$un(
    "crown conical when young, becoming untidy at maturity",
    "crown | shape, ontogenetic change",
    p = "p121"
  ),
  s$row(
    "bark_colour",
    "Bark greyish brown",
    "greyish_brown",
    "grey brown",
    p = "p121",
    notes = "Every colour word scored, as in the AusTraits flora datasets."
  ),
  s$row(
    "bark_texture",
    "ridged, fissured, and flaking into oblong pieces",
    "fissured flaking",
    "furrowed flaky",
    p = "p121"
  ),
  s$un("ridged", "bark | ridged", p = "p121"),
  s$un(
    "Needle-shaped twigs to 30 cm long, mostly unbranched, greenish yellow, green stripes distinct",
    "needle-shaped twigs | length, branching, colour",
    p = "p121"
  ),
  s$rng(
    "leaf_length",
    "internodes 0.5--1.2 cm long",
    0.5,
    1.2,
    "cm",
    em = CL,
    p = "p121"
  ),
  s$rng(
    "leaf_width",
    "internodes ... 0.5--1 mm thick",
    0.5,
    1,
    "mm",
    em = CL,
    p = "p121"
  ),
  s$un(
    "furrows usually densely hairy",
    "needle-shaped twigs | furrow hairs",
    p = "p121"
  ),
  s$row(
    "leaf_type",
    "Scale-leaves (7--)8(--9) per node",
    "scale-leaves",
    "scale",
    p = "p121"
  ),
  s$un(
    "Scale-leaves (7--)8(--9) per node",
    "scale-leaf | number per node",
    p = "p121"
  ),
  s$row(
    "leaf_apex_shape",
    "apex narrowly acute",
    "narrowly acute",
    "acute",
    p = "p121"
  ),
  s$row(
    "inflorescence_type",
    "Male inflorescences simple elongated spikes, terminal on needle-shaped twigs",
    "spikes terminal",
    "spike terminal",
    em = MI,
    p = "p121"
  ),
  s$row(
    "flower_anther_attachment",
    "stamens with basifixed anthers",
    "basifixed",
    "basifixed",
    em = MF,
    p = "p121"
  ),
  s$un(
    "Female inflorescences on lateral woody persistent twigs",
    "female inflorescence | position (lateral)",
    p = "p121"
  ),
  s$row(
    "inflorescence_length",
    "Female inflorescences on lateral woody persistent twigs, c. 0.5 cm long",
    0.5,
    0.5,
    "mean",
    "cm",
    em = FI,
    p = "p121",
    notes = "Length read as that of the inflorescence, not the twigs (see questions)."
  ),
  s$un("style red", "style | colour", p = "p121"),
  s$row(
    "flower_style_length",
    "style red, 0.3--0.5 cm long",
    0.3,
    0.3,
    "minimum",
    "cm",
    em = FF,
    p = "p121"
  ),
  s$row(
    "flower_style_length",
    "style red, 0.3--0.5 cm long",
    0.5,
    0.5,
    "maximum",
    "cm",
    em = FF,
    p = "p121"
  ),
  s$un(
    "Fruit cones ovoid-cylindrical, c. 2.5 x 2.2 cm, sparsely hairy; peduncles 0.3--1.3 cm long",
    "fruit cone | shape, size, hairs, peduncle length",
    p = "p121"
  ),
  s$un(
    "bracts woody but rather thin in the exposed portion; bracteoles acute, slightly protruding, more or less of equal size",
    "fruit cone | bracts, bracteoles",
    p = "p121"
  ),
  s$row(
    "fruit_type",
    "Samara 0.6--0.8 cm long",
    "samara",
    "samara",
    p = "p121"
  ),
  s$rng(
    "fruit_length",
    "Samara 0.6--0.8 cm long",
    0.6,
    0.8,
    "cm",
    em = SA,
    p = "p121"
  ),
  s$un(
    "As for the genus; in Sabah and Sarawak, the species is found only along sandy beaches.",
    "distribution | range, coastal habitat",
    sec = "Distribution",
    p = "p121"
  ),
  s$un(
    "Naturally common in sandy soils near the sea but also grows rather well when planted far inland.",
    "ecology | soil, habitat, cultivation",
    sec = "Ecology",
    p = "p121"
  ),
  s$un(
    "The wood is suitable for light construction, piling and fuel. In Peninsular Malaysia, the Malays use a decoction of the twigs as a lotion to soothe swelling. The powdered bark is used to treat pimples. The species is protected in Sarawak due to its importance in controlling coastal erosion. The tree is frequently planted as a wind-break along major roads.",
    "uses | timber, medicine (Peninsular Malaysia, outside scope), protection, planting",
    sec = "Uses",
    p = "p121"
  )
)

# ---- 2. Ceuthostoma -------------------------------------------------------
g <- mk("Ceuthostoma", "genus", "genus", sec = "Genus description")
ceuthostoma_g <- bind_rows(
  g$row("sex_type", "Dioecious trees", "dioecious", "dioecious", p = "p121"),
  g$row("plant_growth_form", "Dioecious trees", "trees", "tree", p = "p121"),
  g$un(
    "Needle-shaped twigs 4-ribbed, furrows deep and almost closed, concealing the rows of stomata, mostly branched",
    "needle-shaped twigs | ribs, furrows, stomata, branching",
    p = "p121"
  ),
  g$row("leaf_type", "Leaf-scales 4 per node", "leaf-scales", "scale", p = "p121"),
  g$un("Leaf-scales 4 per node", "scale-leaf | number per node", p = "p121"),
  g$row(
    "inflorescence_type",
    "Female flowers in globose, terminal cone-like structures",
    "terminal",
    "terminal",
    em = FI,
    p = "p121"
  ),
  g$row(
    "inflorescence_shape",
    "Female flowers in globose, terminal cone-like structures",
    "globose",
    "spherical",
    em = FI,
    p = "p121"
  ),
  g$un(
    "Fruit cones longer than broad; bracts somewhat expanded distally; bracteoles strongly protruding and inserted at an oblique angle with the cone axis, not strongly woody, convex dorsally, with prominent dorsal eccentric rib, neither ridged nor with protuberances",
    "fruit cone | shape, bracts, bracteoles",
    p = "p121"
  ),
  g$row("fruit_type", "Samara dull pale brown", "samara", "samara", p = "p121"),
  g$row(
    "fruit_colour",
    "Samara dull pale brown",
    "pale_brown",
    "brown",
    em = SA,
    p = "p121"
  ),
  g$un("dull", "samara | surface lustre", p = "p121"),
  g$un(
    "Two species; Palawan Is., Borneo, Maluku, and New Guinea. Only C. terminale is found in Borneo (Sabah).",
    "distribution | range, species number",
    sec = "Genus Distribution",
    p = "p123"
  ),
  g$un(
    "In hill and lower montane forests on ultramafic soils, to c. 1500 m.",
    "ecology | habitat, soil, altitude",
    sec = "Genus Ecology",
    p = "p123"
  )
)

s <- mk("Ceuthostoma terminale", "species", region = SS)
ceuthostoma_terminale <- bind_rows(
  s$row(
    "plant_growth_form",
    "Tree to 30 m tall, 30 cm diameter",
    "tree",
    "tree",
    p = "p123"
  ),
  s$row(
    "plant_height",
    "Tree to 30 m tall, 30 cm diameter",
    30,
    30,
    "maximum",
    "m",
    p = "p123"
  ),
  s$row(
    "plant_diameter_breast_height",
    "Tree to 30 m tall, 30 cm diameter",
    30,
    30,
    "maximum",
    "cm",
    p = "p123",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$row(
    "bark_texture",
    "Bark rough, scaly or flaky",
    "rough flaky",
    "rough flaky",
    p = "p123"
  ),
  s$un(
    "scaly ... peeling off into long broad strips",
    "bark | scaly, peeling in strips",
    p = "p123"
  ),
  s$un(
    "Needle-shaped twigs rounded-quadrangular in cross-section, little-branched, longest unbranched ones exceeding 10 cm long",
    "needle-shaped twigs | cross-section, branching, length",
    p = "p123"
  ),
  s$rng(
    "leaf_length",
    "internodes 0.5--1 cm long",
    0.5,
    1,
    "cm",
    em = CL,
    p = "p123"
  ),
  s$un(
    "occasionally set with white hairs on the furrows",
    "needle-shaped twigs | furrow hairs",
    p = "p123"
  ),
  s$row(
    "leaf_type",
    "Scale-leaves narrowly deltoid, 0.7--1 mm long",
    "scale-leaves",
    "scale",
    p = "p123"
  ),
  s$un(
    "Scale-leaves narrowly deltoid, 0.7--1 mm long",
    "scale-leaf | shape, length",
    p = "p123"
  ),
  s$row(
    "inflorescence_type",
    "Male flowers in simple terminal spikes",
    "terminal spikes",
    "terminal spike",
    em = MI,
    p = "p123"
  ),
  s$row(
    "inflorescence_type",
    "Female inflorescences cone-like, terminal on moderately elongated needle-shaped twigs",
    "terminal",
    "terminal",
    em = FI,
    p = "p123"
  ),
  s$un(
    "Female inflorescences cone-like ... on moderately elongated needle-shaped twigs",
    "female inflorescence | form, bearing twig",
    p = "p123"
  ),
  s$un(
    "Fruit cones c. 1 cm across, with 1--4 whorls of fertile fruitlets",
    "fruit cone | size, fertile fruitlet whorls",
    p = "p123"
  ),
  s$un(
    "bracts woody, 2--2.5 mm high and broad, not markedly ridged; bracteoles strongly protruding, to 5--7 mm long, with a prominent dorsal eccentric rib, tips sharply acute",
    "fruit cone | bract and bracteole size and form",
    p = "p123"
  ),
  s$row(
    "fruit_type",
    "Samaras c. 6 mm long, with transparent wing",
    "samaras",
    "samara",
    p = "p123"
  ),
  s$row(
    "fruit_length",
    "Samaras c. 6 mm long",
    6,
    6,
    "mean",
    "mm",
    em = SA,
    p = "p123"
  ),
  s$row(
    "dispersal_appendage",
    "Samaras ... with transparent wing",
    "wing",
    "wings",
    p = "p123",
    notes = "Wing of the fruitlet (samara), not of the seed."
  ),
  s$un("transparent wing", "samara wing | transparency", p = "p123"),
  s$un(
    "Borneo, Philippines (Palawan Is.), Maluku (Halmahera), and Irian Jaya. In Borneo, so far known from Mt. Kinabalu and Mt. Tawai in Sabah.",
    "distribution | range, localities",
    sec = "Distribution",
    p = "p123"
  ),
  s$un(
    "In primary and secondary forests on ultramafic soils, at 100--1500 m.",
    "ecology | habitat, soil, altitude",
    sec = "Ecology",
    p = "p123"
  )
)

# ---- 3. Gymnostoma --------------------------------------------------------
g <- mk("Gymnostoma", "genus", "genus", sec = "Genus description")
gymnostoma_g <- bind_rows(
  g$row(
    "plant_growth_form",
    "Trees or tall shrubs, dioecious or monoecious",
    "trees shrubs",
    "tree shrub",
    p = "p123"
  ),
  g$un("tall shrubs", "shrub | stature", p = "p123"),
  g$row(
    "sex_type",
    "Trees or tall shrubs, dioecious or monoecious",
    "dioecious monoecious",
    "dioecious monoecious",
    p = "p123"
  ),
  g$un(
    "Needle-shaped twigs mostly branched, internodes 4-ribbed, furrows shallow and open, exposing the stomata, quadrangular in cross-section, ridges rounded to somewhat sharp-edged",
    "needle-shaped twigs | branching, ribs, furrows, stomata, cross-section",
    p = "p123"
  ),
  g$row(
    "leaf_phyllotaxis",
    "Scale-leaves in whorls of 4",
    "in whorls",
    "whorled",
    p = "p123"
  ),
  g$row("leaf_type", "Scale-leaves in whorls of 4", "scale-leaves", "scale", p = "p123"),
  g$un("Scale-leaves in whorls of 4", "scale-leaf | number per whorl", p = "p123"),
  g$row(
    "leaf_apex_shape",
    "apex rounded-acute to narrowly acute",
    "rounded-acute narrowly_acute",
    "rounded acute",
    p = "p123"
  ),
  g$row(
    "inflorescence_type",
    "Male flowers in simple or compound spikes at the ends of needle-shaped twigs",
    "spikes at_the_ends",
    "spike terminal",
    em = MI,
    p = "p123",
    notes = "'at the ends of' read as terminal; 'simple or compound' unmapped."
  ),
  g$un(
    "simple or compound spikes",
    "male inflorescence | branching",
    p = "p123"
  ),
  g$row(
    "inflorescence_shape",
    "Female flowers in terminal or subterminal globose cone-like structures",
    "globose",
    "spherical",
    em = FI,
    p = "p123"
  ),
  g$un(
    "Female flowers in terminal or subterminal globose cone-like structures on woody persistent twigs or on elongated branches of needle-shaped twigs",
    "female inflorescence | position (terminal or subterminal)",
    p = "p123"
  ),
  g$un(
    "Fruit cones globose or cylindrical; bracts woody, much expanded distally, with the abaxial surface below the apex broader than high; bracteoles prominently protruding and inserted perpendicularly to the fruit cone axis, convex, dorsally neither split nor furnished with protuberances",
    "fruit cone | shape, bracts, bracteoles",
    p = "p123"
  ),
  g$row("fruit_type", "Samaras grey to pale brown", "samaras", "samara", p = "p123"),
  g$row(
    "fruit_colour",
    "Samaras grey to pale brown",
    "grey pale_brown",
    "grey brown",
    em = SA,
    p = "p123"
  ),
  g$un(
    "About 17 species; Malesia, Solomons Is., tropical NE Australia, Fiji, and New Caledonia; 2 species in Sabah and Sarawak.",
    "distribution | range, species number",
    sec = "Genus Distribution",
    p = "p125"
  ),
  g$un(
    "In open and closed lowland to montane habitats on shallow soils, stream banks, and peaty or sandy substrates. Often found in shrubby plant communities on ultramafic soils.",
    "ecology | habitat, soil",
    sec = "Genus Ecology",
    p = "p125"
  ),
  g$un(
    "The timber is used mainly as firewood and for piling work.",
    "uses | timber",
    sec = "Genus Uses",
    p = "p125"
  )
)

s <- mk("Gymnostoma nobile", "species", region = SS)
gymnostoma_nobile <- bind_rows(
  s$row(
    "plant_growth_form",
    "Tree to 40 m tall, 50 cm diameter",
    "tree",
    "tree",
    p = "p125"
  ),
  s$row(
    "plant_height",
    "Tree to 40 m tall, 50 cm diameter",
    40,
    40,
    "maximum",
    "m",
    p = "p125"
  ),
  s$row(
    "plant_diameter_breast_height",
    "Tree to 40 m tall, 50 cm diameter",
    50,
    50,
    "maximum",
    "cm",
    p = "p125",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$un("bole cylindrical", "bole | form", p = "p125"),
  s$row(
    "stem_base_form",
    "short buttresses often present",
    "buttresses",
    "buttressed",
    q = "often",
    p = "p125"
  ),
  s$un("short buttresses", "buttresses | size", p = "p125"),
  s$un(
    "crown bushy conical, becoming umbrella-shaped",
    "crown | shape, ontogenetic change",
    p = "p125"
  ),
  s$row(
    "bark_colour",
    "Bark greyish to reddish brown",
    "greyish_to_reddish_brown",
    "grey red brown",
    p = "p125",
    notes = "Read as 'greyish brown to reddish brown'; every colour word scored."
  ),
  s$row(
    "bark_texture",
    "fissured and scaly",
    "fissured",
    "furrowed",
    p = "p125"
  ),
  s$un("scaly", "bark | scaly", p = "p125"),
  s$un(
    "Wood hard, inner part reddish brown, occasionally with black streaks and often with a wavy figure.",
    "wood | hardness, heartwood colour, streaks, figure",
    p = "p125"
  ),
  s$un(
    "Needle-shaped twigs with a few lateral branches, the longest unbranched ones mostly exceeding 10 cm long",
    "needle-shaped twigs | branching, length",
    p = "p125"
  ),
  s$rng(
    "leaf_width",
    "Needle-shaped twigs ... 1--1.5 mm thick",
    1,
    1.5,
    "mm",
    em = CL,
    p = "p125"
  ),
  s$rng(
    "leaf_length",
    "internodes 0.5--1 cm long",
    0.5,
    1,
    "cm",
    em = CL,
    p = "p125"
  ),
  s$row(
    "leaf_type",
    "Scale-leaves with rounded-acute apex",
    "scale-leaves",
    "scale",
    p = "p125"
  ),
  s$row(
    "leaf_apex_shape",
    "Scale-leaves with rounded-acute apex",
    "rounded-acute",
    "rounded acute",
    p = "p125"
  ),
  s$un(
    "Male flowers in compound, cone-like structures",
    "male inflorescence | compound cone-like",
    p = "p125"
  ),
  s$row(
    "inflorescence_type",
    "Female flowers in terminal or subterminal capitate inflorescences",
    "capitate",
    "head",
    em = FI,
    p = "p125"
  ),
  s$un(
    "Female flowers in terminal or subterminal capitate inflorescences",
    "female inflorescence | position (terminal or subterminal)",
    p = "p125"
  ),
  s$un(
    "Fruit cones globose, c. 2.2 cm across, usually 1 or more cones terminal or subterminal on lateral, woody persistent twigs or needle-shaped twigs",
    "fruit cone | shape, size, number, position",
    p = "p125"
  ),
  s$un(
    "each cone with 20--30 fertile fruitlets, and whorls of sterile, undeveloped fruitlets at its upper and basal parts; bracteoles slightly projecting, rounded-acute at the tips",
    "fruit cone | fertile fruitlet number, sterile fruitlets, bracteoles",
    p = "p125"
  ),
  s$un(
    "Palawan Is., Borneo (Sabah, Sarawak, Brunei, and Kalimantan).",
    "distribution | range",
    sec = "Distribution",
    p = "p125"
  ),
  s$un(
    "Lowland and hill kerangas forest to 1000 m, on sandstone and ultramafic rocks. Occasional in forest on impoverished sandy infertile soils in coastal areas and near the central zone of peat swamp forests.",
    "ecology | habitat, soil, altitude",
    sec = "Ecology",
    p = "p125"
  ),
  s$un(
    "Commonly planted at roadsides and in gardens. Its wood is useful for construction and fuel.",
    "uses | planting, timber",
    sec = "Uses",
    p = "p125"
  )
)

s <- mk("Gymnostoma sumatranum", "species", region = SS)
gymnostoma_sumatranum <- bind_rows(
  s$row(
    "plant_growth_form",
    "Tree to 20 m tall, 60 cm diameter",
    "tree",
    "tree",
    p = "p127"
  ),
  s$row(
    "plant_height",
    "Tree to 20 m tall, 60 cm diameter",
    20,
    20,
    "maximum",
    "m",
    p = "p127"
  ),
  s$row(
    "plant_diameter_breast_height",
    "Tree to 20 m tall, 60 cm diameter",
    60,
    60,
    "maximum",
    "cm",
    p = "p127",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$un("bole cylindrical, often twisted", "bole | form, twisted", p = "p127"),
  s$row(
    "stem_base_form",
    "often twisted or fluted",
    "fluted",
    "fluted",
    q = "often",
    p = "p127"
  ),
  s$row(
    "stem_base_form",
    "sometimes with short buttresses",
    "buttresses",
    "buttressed",
    q = "sometimes",
    p = "p127"
  ),
  s$un("short buttresses", "buttresses | size", p = "p127"),
  s$un(
    "crown spreading with its main branches ascending and topped by softly bushy needle-shaped twigs",
    "crown | shape, branch angle",
    p = "p127"
  ),
  s$row(
    "bark_colour",
    "Bark greyish brown",
    "greyish_brown",
    "grey brown",
    p = "p127",
    notes = "Every colour word scored, as in the AusTraits flora datasets."
  ),
  s$un(
    "sometimes marked with white patches",
    "bark | patches",
    p = "p127"
  ),
  s$row(
    "bark_texture",
    "rough and fissured",
    "rough fissured",
    "rough furrowed",
    p = "p127"
  ),
  s$row(
    "leaf_width",
    "Needle-shaped twigs c. 0.5 mm thick",
    0.5,
    0.5,
    "mean",
    "mm",
    em = CL,
    p = "p127"
  ),
  s$un(
    "with many short lateral branches, bushy, the longest unbranched ones seldom exceeding 10 cm long",
    "needle-shaped twigs | branching, length",
    p = "p127"
  ),
  s$rng(
    "leaf_length",
    "internodes short, 0.2--0.3 cm long",
    0.2,
    0.3,
    "cm",
    em = CL,
    p = "p127"
  ),
  s$row(
    "leaf_type",
    "Scale-leaves with rounded-acute apex",
    "scale-leaves",
    "scale",
    p = "p127"
  ),
  s$row(
    "leaf_apex_shape",
    "Scale-leaves with rounded-acute apex",
    "rounded-acute",
    "rounded acute",
    p = "p127"
  ),
  s$row(
    "inflorescence_type",
    "Male flowers in simple, terminal spikes",
    "terminal spikes",
    "terminal spike",
    em = MI,
    p = "p127"
  ),
  s$row(
    "flower_anther_attachment",
    "anthers basifixed",
    "basifixed",
    "basifixed",
    em = MF,
    p = "p127"
  ),
  s$row(
    "inflorescence_type",
    "Female flowers in terminal capitate structures",
    "terminal capitate",
    "terminal head",
    em = FI,
    p = "p127"
  ),
  s$un(
    "Fruit cones ellipsoid, c. 2.5 x 4.5 cm, usually solitary on the apex of woody persistent twigs, each with 20--35 fruitlets",
    "fruit cone | shape, size, number, position, fruitlet number",
    p = "p127"
  ),
  s$un(
    "bracteoles strongly protruding, with acute tips",
    "fruit cone | bracteoles",
    p = "p127"
  ),
  s$un(
    "Sumatra and Borneo. In Sabah, the species is known from the Tawau Hills, Batanga near Sipitang, Mt. Tawai and Melinau River, and Mt. Silam, whereas in Sarawak it occurs in many localities.",
    "distribution | range, localities",
    sec = "Distribution",
    p = "p127"
  ),
  s$un(
    "Apparently confined to hill, ridge and lower montane forests on ultramafic soils and in kerapah vegetation on sandstone, shale or acidic soils, at 600--1800 m. Grows well when planted in fertile soil.",
    "ecology | habitat, soil, altitude (hedged: apparently)",
    sec = "Ecology",
    p = "p127"
  ),
  s$un(
    "Attractive ornamental tree suitable for road sides and gardens.",
    "uses | ornamental",
    sec = "Uses",
    p = "p127"
  )
)

# ---- Copy-down ------------------------------------------------------------
# Family and genus values copied to member species silent on the trait (and
# entity measured). Every value below is a single, unqualified level; members
# that score the trait themselves all agree (checked by hand):
#   - not copied: sex_type (family qualified; Casuarina and Gymnostoma give two
#     levels), plant_growth_form, bark_texture and fruit_colour where two
#     levels, numbers (perianth parts, stamens, carpels, seeds per fruit),
#     male inflorescence position (family 'usually').
#   - not copied: family bark_colour (several levels), so C. terminale has no
#     bark colour.
#   - family plant_photosynthetic_organ (cladode) to all four species.
#   - family leaf_phyllotaxis to Casuarina and Ceuthostoma species; Gymnostoma
#     genus gives the same value to its species (most specific rank wins).
#   - family inflorescence_shape (female) to C. equisetifolia; the two other
#     genera give the same value to their species.
#   - family flower_anther_attachment (male flower) to the two members silent
#     on it; C. equisetifolia and G. sumatranum score basifixed themselves.
#   - family dispersal_appendage to the three members silent on it.
own <- bind_rows(
  casuarina_equisetifolia,
  ceuthostoma_terminale,
  gymnostoma_nobile,
  gymnostoma_sumatranum
)
higher <- bind_rows(family, casuarina_g, ceuthostoma_g, gymnostoma_g)
copy <- tribble(
  ~from, ~trait, ~em, ~to,
  "Casuarinaceae", "plant_photosynthetic_organ", "", "Casuarina equisetifolia",
  "Casuarinaceae", "plant_photosynthetic_organ", "", "Ceuthostoma terminale",
  "Casuarinaceae", "plant_photosynthetic_organ", "", "Gymnostoma nobile",
  "Casuarinaceae", "plant_photosynthetic_organ", "", "Gymnostoma sumatranum",
  "Casuarinaceae", "leaf_phyllotaxis", "", "Casuarina equisetifolia",
  "Casuarinaceae", "leaf_phyllotaxis", "", "Ceuthostoma terminale",
  "Casuarinaceae", "inflorescence_shape", FI, "Casuarina equisetifolia",
  "Casuarinaceae", "flower_anther_attachment", MF, "Ceuthostoma terminale",
  "Casuarinaceae", "flower_anther_attachment", MF, "Gymnostoma nobile",
  "Casuarinaceae", "dispersal_appendage", "", "Casuarina equisetifolia",
  "Casuarinaceae", "dispersal_appendage", "", "Gymnostoma nobile",
  "Casuarinaceae", "dispersal_appendage", "", "Gymnostoma sumatranum",
  "Casuarina", "fruit_surface_hairs", SA, "Casuarina equisetifolia",
  "Ceuthostoma", "sex_type", "", "Ceuthostoma terminale",
  "Ceuthostoma", "inflorescence_shape", FI, "Ceuthostoma terminale",
  "Ceuthostoma", "fruit_colour", SA, "Ceuthostoma terminale",
  "Gymnostoma", "leaf_phyllotaxis", "", "Gymnostoma nobile",
  "Gymnostoma", "leaf_phyllotaxis", "", "Gymnostoma sumatranum",
  "Gymnostoma", "inflorescence_shape", FI, "Gymnostoma nobile",
  "Gymnostoma", "inflorescence_shape", FI, "Gymnostoma sumatranum",
  "Gymnostoma", "fruit_type", "", "Gymnostoma nobile",
  "Gymnostoma", "fruit_type", "", "Gymnostoma sumatranum"
)

copied <- copy |>
  inner_join(
    higher,
    by = c(from = "taxon_name", "trait", em = "entity_measured"),
    relationship = "many-to-many"
  ) |>
  mutate(
    notes = paste0("Copied from the ", from, " description."),
    taxon_rank = "species",
    entity_measured = em
  ) |>
  select(-from, -em) |>
  rename(taxon_name = to)

# Copy-down checks: each source row is unqualified and single-level, and no
# target already scores the trait for that entity.
stopifnot(
  nrow(copied) == nrow(copy),
  all(copied$commonness_qualifier == ""),
  !any(str_detect(copied$trait_value_clean, " ")),
  nrow(semi_join(
    copied,
    own,
    by = c("taxon_name", "trait", "entity_measured")
  )) ==
    0
)

d <- bind_rows(higher, own, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Pungga_1996/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(d),
  "data/Pungga_1996/raw/unmapped_terms.csv",
  na = ""
)

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Casuarinaceae", "family", "accepted", "",
  "Casuarina", "genus", "accepted", "About 17 species; only C. equisetifolia in Borneo.",
  "Casuarina equisetifolia", "species", "accepted", "",
  "Ceuthostoma", "genus", "accepted", "Two species; only C. terminale in Borneo.",
  "Ceuthostoma terminale", "species", "accepted", "",
  "Gymnostoma", "genus", "accepted", "",
  "Gymnostoma nobile", "species", "accepted", "",
  "Gymnostoma sumatranum", "species", "accepted", ""
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Pungga_1996/raw/taxon_flags.csv", na = "")
