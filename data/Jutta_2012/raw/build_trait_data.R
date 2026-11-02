# Hand extraction of Cycadaceae (M. Jutta 2012, Flora of Peninsular Malaysia
# Series II vol. 3: 149--160). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Cycadaceae.pdf, born-digital text layer
# read with pdftools; flora_layer_flags() found no bad glyphs. PDF page =
# printed page - 142 (treatment pdf 7--18). pdf 10 (p. 152, Fig. 1) and pdf 12
# (p. 154, Fig. 2) are figures, used for vouchers only; the plates (pdf 20--22,
# pp. 346--348) are photographs with photographer credits only. No source
# repairs were made; the probable typo in the C. clivicola pinna size is left
# unscored (metadata question).
#
# Monogeneric family with no family description ("Monogeneric family."), so
# the genus description is the higher rank (Campbell-Gasis_1996 rule); the
# family Taxonomy paragraph is kept as one unmapped row. Four species; C.
# clivicola is represented locally by subsp. clivicola only, so its
# description is scored at subspecies rank (single infraspecific taxon rule).
# Rows are written by hand with flora_hand(). Genus values are copied down by
# hand under the flora_copy_down() rules (unqualified, single level, every
# member that scores the trait agrees). Hedged statements are not scored.
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(149:160, \(p) c(p - 142L, p)), paste0("p", 149:160))
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
cnt <- "{count}"
PN <- "pinnae"
UP <- "upper surface"
LO <- "lower surface"
SEED <- "seed including sarcotesta"
diam <- "Diameter scored as width."
ldim <- "Total leaf length (petiole and rachis), printed after the petiole clause."
flush <- "Hairs on the young leaves in flush, shed as the leaves uncurl."
rec <- "Recurved scored as revolute (AusTraits ABRS_2023 precedent)."

# ---- Family ---------------------------------------------------------------
f <- mk("Cycadaceae", "family", "family", sec = "Family Taxonomy")
family <- bind_rows(
  f$un(
    "Monogeneric family. Taxonomy. Cycadaceae ... is characterised by the megasporophylls that are not arranged in distinct cones. Vegetatively, Cycadaceae is easily distinguished from Zamiaceae by the leaves having pinnae with a midrib, but lacking lateral veins. ... They have massive apical buds measuring 3 mm across ... the largest sperm in the plant kingdom measuring c. 300 um long and have some of the largest ovules among plants.",
    "taxonomy | family diagnosis, fossil history, apical bud, sperm and ovule size (no family description)",
    p = "p149"
  )
)

# ---- Genus ------------------------------------------------------------------
g <- mk("Cycas", "genus", "genus", sec = "Genus description")
GE <- "Genus Ecology"
genus <- bind_rows(
  g$row("sex_type", "Dioecious", "dioecious", "dioecious", p = "p149"),
  g$row(
    "plant_growth_form",
    "shrubby or aborescent, palm-like trees",
    "palm-like trees",
    "palmoid",
    p = "p149",
    notes = "Palm-like trees scored as palmoid (traits.yml: 'trunked cycads')."
  ),
  g$un(
    "shrubby or aborescent, palm-like trees, stems always aerial",
    "plant | stature (shrubby or arborescent); stems aerial",
    p = "p149"
  ),
  g$row(
    "stem_growth_habit",
    "erect to prostrate-pendulous",
    "erect prostrate pendulous",
    "erect prostrate pendulous",
    p = "p149"
  ),
  g$row("plant_height", "to 12 m tall", "12", "12", "maximum", "m", p = "p149"),
  g$un(
    "Roots contractile, especially in seedlings; lateral roots without root hairs",
    "roots | contractile; root hairs absent (negative phrase)",
    p = "p149"
  ),
  g$row(
    "root_system_type",
    "lateral roots ... often modified into above-ground 'coralloid' roots | The symbiotic association with cyanobacteria ... in specialised coralloid roots",
    "coralloid roots",
    "coralloid_roots",
    sec = GE,
    p = "p150",
    notes = "Ecology states coralloid roots without qualifier; 'often' in the description qualifies their above-ground position."
  ),
  g$row(
    "woodiness",
    "Stems stout, cylindric, woody",
    "woody",
    "woody",
    p = "p149"
  ),
  g$un(
    "Stems stout, cylindric, woody with a high proportion of mostly non-woody storage tissue, sometimes enlarged at the base",
    "stem | shape, storage tissue, enlarged base",
    p = "p150"
  ),
  g$row(
    "stem_branching_form",
    "usually unbranched",
    "unbranched",
    "unbranched",
    q = "usually",
    p = "p150"
  ),
  g$un(
    "terminal portion enclosed in leaf bases",
    "stem | apex enclosed in leaf bases",
    p = "p150"
  ),
  g$row(
    "bark_texture",
    "Bark smooth to variously fissured with age",
    "smooth",
    "smooth",
    p = "p150"
  ),
  g$row(
    "bark_texture",
    "Bark smooth to variously fissured with age",
    "fissured",
    "furrowed",
    em = "old bark",
    p = "p150"
  ),
  g$row(
    "leaf_compoundness",
    "Leaves large, pinnately compound",
    "compound",
    "compound",
    p = "p150"
  ),
  g$row(
    "leaf_lamina_division",
    "Leaves large, pinnately compound",
    "pinnately compound",
    "pinnately_compound",
    p = "p150"
  ),
  g$row(
    "leaf_phyllotaxis",
    "arranged in close whorls, forming a tufted terminal crown",
    "in close whorls",
    "whorled",
    p = "p150",
    notes = "Not copied down pending a curator check (metadata question)."
  ),
  g$un(
    "forming a tufted terminal crown, produced in flushes, new leaves erect, circinate, later straight or arching; leaf bases persistent",
    "leaf | crown, flushing, vernation, posture, persistent bases",
    p = "p150"
  ),
  g$un(
    "petioles swollen towards the base, tomentose, with a lateral series of short, rigid thorn-like processes borne in more-or-less opposite pairs; rachis devoid of thorns",
    "petiole, rachis | swelling, hairs, thorn-like processes (candidate plant_physical_defence_structures)",
    p = "p150"
  ),
  g$row(
    "leaflet_count",
    "pinnae 30 to more than 150",
    "30",
    "30",
    "minimum",
    cnt,
    em = PN,
    p = "p150",
    notes = "Upper bound 'more than 150' is open and not scored."
  ),
  g$un(
    "alternate to opposite, straight to falcate",
    "pinnae | arrangement on rachis, curvature",
    p = "p150"
  ),
  g$row("leaflet_length", "to 50 x 2.5 cm", "50", "50", "maximum", "cm", em = PN, p = "p150"),
  g$row("leaflet_width", "to 50 x 2.5 cm", "2.5", "2.5", "maximum", "cm", em = PN, p = "p150"),
  g$un(
    "basal and terminal ones smaller than median pinnae, smooth, soft to leathery, glabrescent, base decurrent",
    "pinnae | size gradient, texture, glabrescent (not scored), decurrent base",
    p = "p150"
  ),
  g$row("leaf_margin", "margins entire", "entire", "entire", em = PN, p = "p150"),
  g$row(
    "leaf_apex_shape",
    "apex acute-acuminate",
    "acute acuminate",
    "acute acuminate",
    em = PN,
    p = "p150"
  ),
  g$un(
    "sometimes sharp-tipped, with a prominent midrib, without lateral veins",
    "pinnae | sharp tip; midrib; lateral veins absent",
    p = "p150"
  ),
  g$un(
    "Cataphylls prominent, arising in flushes alternating with the leaves, attenuate to sub-linear, to 18 x 3.5 cm, outer surface tomentose, inner surface glabrous, persistent",
    "cataphylls | shape, size, hairs, persistence (no trait)",
    p = "p150"
  ),
  g$un(
    "Male cones terminal, cylindrical to ovoid-ellipsoid, to 60 cm long, 13 cm diameter at anthesis, sub-sessile or stalked",
    "male cone | position, shape, size, stalk (candidate pollen cone traits)",
    p = "p150"
  ),
  g$un(
    "Microsporophylls many, spirally arranged on a central axis; each composed of a fertile zone on the abaxial surface covered by numerous microsporangia (pollen sacs), apical zone sterile with a stiff, usually up-turned terminal spine; surface matted or pubescent; microsporangia opening by slits",
    "microsporophylls | arrangement, spine, hairs, dehiscence",
    p = "p150"
  ),
  g$un(
    "Megasporophylls not in terminal cones with a median axis, but spirally arranged in a loose or dense grouping around the stem apex, each with a linear fertile stalk and a spreading, flattened sterile apical lamina with entire, dentate or comb-like margins",
    "megasporophylls | arrangement, stalk, lamina margin",
    p = "p150"
  ),
  g$un(
    "ovules about 1 cm diameter with 1--4 on each megasporophyll, attached to the lateral margins of the stalk",
    "ovules | size, number per megasporophyll, attachment",
    p = "p150"
  ),
  g$row(
    "seed_colour",
    "Seeds large, yellow, orange or brown when ripe",
    "yellow orange brown",
    "yellow_orange red_brown",
    p = "p150"
  ),
  g$row(
    "seed_shape",
    "sub-globose to ellipsoid",
    "sub-globose ellipsoid",
    "globoid ellipsoid",
    p = "p150"
  ),
  g$row(
    "dispersal_appendage",
    "sarcotesta (outermost layer of seed-coat) ... when ripe pulpy",
    "sarcotesta",
    "sarcotesta",
    p = "p150"
  ),
  g$un(
    "sarcotesta ... sometimes with slightly waxy cuticle, ... with or without a fibrous layer; sclerotesta (middle layer of seed-coat) woody, with or without a crested apical spine; endotesta (innermost layer of seed-coat) spongy or membranous",
    "seed coat | cuticle, fibrous layer, sclerotesta, endotesta",
    p = "p150"
  ),
  g$un(
    "embryo embedded in a solid or spongy endosperm, straight, suspensor very long, spirally twisted; cotyledons 2",
    "embryo, endosperm | form; cotyledon number",
    p = "p150"
  ),
  g$row(
    "seedling_germination_location",
    "germination epigeal",
    "epigeal",
    "epigeal",
    p = "p150"
  ),
  g$un(
    "About 90 species ... in E Africa and Madagascar, and in Asia from India to Japan through Malesia to Australia and Polynesia ... In Peninsular Malaysia, represented by 4 species.",
    "distribution",
    sec = "Genus Distribution",
    p = "p150"
  ),
  g$un(
    "Cycas species occupy diverse habitats ranging from coastal habitats to interior hills, on well-drained, often nutrient-deficient substrates or elsewhere in savannah where some species are fire resistant. In arid regions, many are deciduous.",
    "habitat; fire resistance and deciduousness outside the region",
    sec = GE,
    p = "p150"
  ),
  g$row(
    "life_history",
    "They are long-lived, slow-growing, with low recruitment and population turnover.",
    "long-lived",
    "perennial",
    sec = GE,
    p = "p150"
  ),
  g$un(
    "slow-growing, with low recruitment and population turnover",
    "growth rate; recruitment",
    sec = GE,
    p = "p150"
  ),
  g$row(
    "nitrogen_fixing",
    "The symbiotic association with cyanobacteria (nitrogen-fixing, photosynthetic 'blue-green' algae) in specialised coralloid roots ... with the cyanobacteria providing the plant with nitrates in return for carbohydrates.",
    "symbiotic nitrogen-fixing cyanobacteria",
    "nitrogen_fixer",
    sec = GE,
    p = "p150",
    notes = "Cyanobacterial symbiosis (not rhizobial) scored as nitrogen_fixer."
  ),
  g$row(
    "pollination_vector_possible",
    "Pollination is by weevils (Family Curculionidae).",
    "weevils",
    "beetle",
    sec = GE,
    p = "p150",
    notes = "Weevils scored as beetle."
  ),
  g$un(
    "Male and female plants in (pre-)anthesis produce a faint to strong odour that may be associated with attracting pollinators.",
    "cones | odour (candidate flower_scent_production; cones are not flowers; link to pollinators hedged 'may')",
    sec = GE,
    p = "p150"
  ),
  g$row(
    "dispersers",
    "Seeds are dispersed by animals or, in species having seeds with a spongy endotesta, also by water.",
    "animals",
    "animals",
    sec = GE,
    p = "p150"
  ),
  g$row(
    "dispersal_syndrome",
    "Seeds are dispersed by animals",
    "dispersed by animals",
    "zoochory",
    sec = GE,
    p = "p150"
  ),
  g$row(
    "dispersal_unit",
    "Seeds are dispersed by animals",
    "seeds",
    "seed",
    sec = GE,
    p = "p150"
  ),
  g$un(
    "in species having seeds with a spongy endotesta, also by water",
    "dispersal | water, conditional on spongy endotesta (candidate hydrochory for C. edentata)",
    sec = GE,
    p = "p150"
  ),
  g$un(
    "All parts of the plant are toxic (hepato- and neuro-toxic as well as carcinogenic). Plants ... are heavily predated by caterpillars of the butterfly Chilades pandava pandava Horsfield ...; successive infestations can seriously weaken affected plants.",
    "toxicity; herbivores",
    sec = GE,
    p = "p150"
  ),
  g$un(
    "Cycads are highly sought after for the ornamental plant trade ... Malaysian species are used as food and in traditional medicine applications.",
    "uses; toxins (BMAA, cycasin)",
    sec = "Genus Uses",
    p = "p151"
  ),
  g$un(
    "The separation of the genus into Cycas and Epicycas ... is untenable ... Cycas clivicola subsp. clivicola and C. cantafolia belong to sect. Indosinenses that is distinguished by closed-type female 'cones'. Section Cycas is represented by C. edentata (subsect. Rumphiae K.D.Hill) and C. macrocarpa (subsect. Cycas) which have open-type female 'cones'.",
    "taxonomy | sections; closed or open female 'cones' (candidate trait)",
    sec = "Genus Taxonomy",
    p = "p151"
  )
)

# ---- Species ----------------------------------------------------------------
# Shared unmapped wording for organs with no trait.
cone_un <- function(s, raw, p) s$un(raw, "male cone | number, stalk, shape, size, colour (candidate pollen cone traits)", p = p)
micro_un <- function(s, raw, p) s$un(raw, "microsporophylls | shape, size, hairs, colour, spine, fertile zone", p = p)
mega_un <- function(s, raw, p) s$un(raw, "megasporophylls | 'cone' type, size, hairs, colour, lamina, spines, ovule number", p = p)
cata_un <- function(s, raw, p) s$un(raw, "cataphylls | shape, size, hairs (no trait)", p = p)

# 1. Cycas cantafolia
s <- mk("Cycas cantafolia", "species", region = PM)
c_cantafolia <- bind_rows(
  s$row(
    "stem_growth_habit",
    "Stems erect, often prostrate-pendulous",
    "erect",
    "erect",
    q = "usually",
    p = "p153"
  ),
  s$row(
    "stem_growth_habit",
    "Stems erect, often prostrate-pendulous",
    "prostrate-pendulous",
    "prostrate pendulous",
    q = "often",
    p = "p153"
  ),
  s$un(
    "to 5.5 m or more tall",
    "plant | height, open upper bound ('or more'; candidate plant_height maximum)",
    p = "p153"
  ),
  s$un(
    "diameter at narrowest point to 25 cm",
    "stem | diameter at narrowest point (not at base)",
    p = "p153"
  ),
  s$un(
    "usually with an enlarged bulbous base, also in short-stemmed individuals",
    "stem | bulbous base",
    p = "p153"
  ),
  s$row(
    "stem_diameter",
    "usually with an enlarged bulbous base ... to 64 cm diameter at the widest point",
    "64",
    "64",
    "maximum",
    "cm",
    em = "bulbous stem base",
    p = "p153",
    notes = "Diameter of the bulbous base at its widest point."
  ),
  s$un(
    "in the distal portion with distinct annular rings",
    "stem | annular rings",
    p = "p153"
  ),
  s$row("bark_colour", "Bark light grey", "light grey", "grey", p = "p153"),
  s$row(
    "bark_texture",
    "with age becoming corky with deep polygonal fissuring",
    "corky fissuring",
    "corky furrowed",
    em = "old bark",
    p = "p153"
  ),
  s$un(
    "Leaves to 50 or more, generally flat in profile (not keeled, opposing pinnae inserted at 180 degrees on rachis), glabrescent, strongly discolourous",
    "leaf | number per crown (open bound; candidate leaf_count), profile, glabrescent (not scored), discolourous",
    p = "p153"
  ),
  s$row(
    "leaf_surface_colour",
    "dark green and semi-glossy above",
    "dark green",
    "dark_green",
    em = UP,
    p = "p153"
  ),
  s$un(
    "semi-glossy above",
    "lamina | 'semi-glossy' (intermediate reflectivity)",
    p = "p153"
  ),
  s$row(
    "leaf_hairs_immature_leaves",
    "in flush covered by pale whitish hairs that are shed as the leaves uncurl",
    "hairs",
    "hairy",
    p = "p153",
    notes = flush
  ),
  s$rng("petiole_length", "petioles 45--50 cm long", "45", "50", "cm", p = "p153"),
  s$un(
    "glabrous, spinescent to 60% of length",
    "petiole | hairs, spines",
    p = "p153"
  ),
  s$row(
    "leaf_length",
    "to 200 cm long",
    "200",
    "200",
    "maximum",
    "cm",
    p = "p153",
    notes = ldim
  ),
  s$un(
    "rachis terminated either by a spine, 23 mm long, or by paired pinnae; pinnae to 165 pairs",
    "rachis, pinnae | rachis apex; number of pinna pairs (candidate leaflet_count)",
    p = "p153"
  ),
  s$row(
    "leaflet_length",
    "basal pinnae to 130 mm long",
    "130",
    "130",
    "maximum",
    "mm",
    em = "basal pinnae",
    p = "p153"
  ),
  s$un(
    "not gradually diminishing in size or becoming spinescent",
    "pinnae | basal size gradient",
    p = "p153"
  ),
  s$row(
    "leaf_shape",
    "linear-lanceolate",
    "linear-lanceolate",
    "linear lanceolate",
    em = PN,
    p = "p153"
  ),
  s$un(
    "widest below the middle, straight to slightly falcate",
    "pinnae | widest point, curvature",
    p = "p153"
  ),
  s$rng("leaflet_length", "220--275 x 8 mm", "220", "275", "mm", em = PN, p = "p153"),
  s$row("leaflet_width", "220--275 x 8 mm", "8", "8", "mean", "mm", em = PN, p = "p153"),
  s$un(
    "inserted at 50--60 degrees to rachis, decurrent for 2--3 mm, narrowed to 2--3 mm at base",
    "pinnae | insertion angle, decurrent base",
    p = "p153"
  ),
  s$row(
    "leaf_margin_posture",
    "margins slightly recurved",
    "recurved",
    "revolute",
    em = PN,
    p = "p153",
    notes = rec
  ),
  s$row("leaf_apex_shape", "apex acute, spinescent", "acute", "acute", em = PN, p = "p153"),
  s$un(
    "spinescent; midrib raised above and below in dry material, light green",
    "pinnae | spinescent apex; midrib relief, colour",
    p = "p153"
  ),
  cata_un(
    s,
    "Cataphylls narrowly triangular, 88--112 x 15 mm, pilose, in the upper half the hairs extending beyond the margin and appearing shaggy",
    "p153"
  ),
  cone_un(
    s,
    "Male cones usually solitary, sessile or shortly stalked, ovoid to narrowly ellipsoid, 33 cm long, 13 cm diameter (mature size, pre-anthesis measurement), golden yellow or orange",
    "p153"
  ),
  micro_un(
    s,
    "Microsporophylls: spathulate, 40 x 13--15 mm, glabrous, pale yellow, apical spine soft, sharply upturned at c. 70 degrees, 3 mm long; fertile zone 30 mm long, firm, not dorsiventrally thickened; sterile apex 10 mm long, flat",
    "p153"
  ),
  mega_un(
    s,
    "Megasporophylls forming a closed-type 'cone', 18--22 x 6--6.5 cm, erect, concave, closely spaced with laminas overlapping each other and forming a low cabbage-like dome enclosing the apical meristem, pale grey to light-brown, tomentose, lamina elliptic, 60--65 x 40 mm, shallowly pectinate, lateral spines 25--30, soft, 10--18 mm long, 3 mm wide at base; apical spine distinct from lateral spines, 36--50 mm long, 6 mm wide at base, usually with one or two shorter lateral processes; ovules 1--4, glabrous",
    "p153"
  ),
  s$row("seed_colour", "Seeds ripening yellow", "yellow", "yellow_orange", p = "p153"),
  s$row(
    "seed_shape",
    "subglobose, laterally flattened",
    "subglobose laterally flattened",
    "globoid_flattened",
    p = "p153",
    notes = "Subglobose scored as globoid."
  ),
  s$rng(
    "seed_length",
    "including the sarcotesta 35--39 mm long, 35--37 mm wide, 25--30 mm thick",
    "35",
    "39",
    "mm",
    em = SEED,
    p = "p153"
  ),
  s$rng(
    "seed_width",
    "including the sarcotesta 35--39 mm long, 35--37 mm wide, 25--30 mm thick",
    "35",
    "37",
    "mm",
    em = SEED,
    p = "p153"
  ),
  s$rng(
    "seed_height",
    "including the sarcotesta 35--39 mm long, 35--37 mm wide, 25--30 mm thick",
    "25",
    "30",
    "mm",
    em = SEED,
    p = "p153",
    notes = "Thickness scored as seed height (shortest axis)."
  ),
  s$un(
    "sarcotesta yellow, not waxy, 2--4 mm thick, fibrous layer present; sclerotesta smooth; spongy endotesta absent",
    "seed coat | sarcotesta colour, wax, thickness, fibrous layer; sclerotesta; endotesta",
    p = "p153"
  ),
  s$un(
    "Endemic in Peninsular Malaysia, known only from the type locality, a chain of low-elevation hills west of the southern tip of the G. Ledang massif.",
    "distribution",
    sec = "Distribution",
    p = "p153"
  ),
  s$un(
    "Critically Endangered A3c, B2ab(iii), C1. The species has a very narrow localised distribution ... its entire habitat is threatened because the area has been designated for quarrying activities ...",
    "conservation status",
    sec = "Conservation status",
    p = "p153"
  ),
  s$row(
    "plant_growth_substrate",
    "It usually roots directly in crevices and cracks in the bare granite rock.",
    "roots directly in rock crevices",
    "lithophyte",
    q = "usually",
    sec = "Ecology",
    p = "p155"
  ),
  s$un(
    "Cycas cantafolia favours precipitous granite cliffs at 200--500 m altitude.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p155"
  )
)

# 2. Cycas clivicola: species heading with Distribution and Taxonomy only.
s <- mk("Cycas clivicola", "species", region = "")
c_clivicola <- bind_rows(
  s$un(
    "Limestone outcrops in southern Thailand and northern Peninsular Malaysia.",
    "distribution",
    sec = "Distribution",
    p = "p155"
  ),
  s$un(
    "Hill (1999) described two subspecies ... Hill (1999) distinguished subsp. lutea from subsp. clivicola on the basis of its yellow, smooth stems and smaller male cones with larger microsporophylls. However, field observations in Peninsular Malaysia have recorded all these characters, to a various extent, in populations at Lenggong, Perak, thus casting doubt on the validity of Hill's subspecies.",
    "taxonomy | subspecies boundaries; stem colour and smoothness, male cone size",
    sec = "Taxonomy",
    p = "p155"
  )
)

# 2a. subsp. clivicola: the only infraspecific taxon in the region, so its
# description is the local description of the species (scored at subspecies).
s <- mk("Cycas clivicola subsp. clivicola", "subspecies", region = PM)
c_clivicola_cl <- bind_rows(
  s$row(
    "stem_growth_habit",
    "Stems erect, often prostrate-pendulous to pendulous",
    "erect",
    "erect",
    q = "usually",
    p = "p156"
  ),
  s$row(
    "stem_growth_habit",
    "Stems erect, often prostrate-pendulous to pendulous",
    "prostrate-pendulous pendulous",
    "prostrate pendulous",
    q = "often",
    p = "p156"
  ),
  s$row("plant_height", "to 7 m tall", "7", "7", "maximum", "m", p = "p156"),
  s$row(
    "stem_diameter",
    "to 60 cm diameter at base",
    "60",
    "60",
    "maximum",
    "cm",
    p = "p156"
  ),
  s$row(
    "stem_branching_form",
    "occasionally branching",
    "branching",
    "branched",
    q = "occasionally",
    p = "p156"
  ),
  s$un(
    "base often conspicuously enlarged, bulbous",
    "stem | bulbous base",
    p = "p156"
  ),
  s$row("bark_texture", "Bark smooth", "smooth", "smooth", p = "p156"),
  s$row("bark_colour", "light grey", "light grey", "grey", p = "p156"),
  s$row(
    "bark_texture",
    "with age deeply fissured, often in a honeycomb-like pattern",
    "deeply fissured",
    "furrowed",
    em = "old bark",
    p = "p156"
  ),
  s$un(
    "Leaves up to 50 or more, generally flat in profile (not keeled, opposing pinnae inserted at 180 degrees on rachis), glabrescent, discolourous",
    "leaf | number per crown (open bound; candidate leaf_count), profile, glabrescent (not scored), discolourous",
    p = "p156"
  ),
  s$row(
    "leaf_hairs_immature_leaves",
    "in flush covered by woolly, pale yellow hairs that are shed as the leaves uncurl",
    "woolly hairs",
    "hairy",
    p = "p156",
    notes = flush
  ),
  s$rng("petiole_length", "petioles 20--60 cm long", "20", "60", "cm", p = "p156"),
  s$un(
    "spinescent usually only in the upper half, spines soft-tipped, to 2 mm long, base sparsely tomentose",
    "petiole | spines, hairs",
    p = "p156"
  ),
  s$row(
    "leaf_length",
    "to 200 cm long, but usually much shorter",
    "200",
    "200",
    "maximum",
    "cm",
    p = "p156",
    notes = ldim
  ),
  s$un("but usually much shorter", "leaf | typical length (not quantified)", p = "p156"),
  s$un(
    "rachis 40--140 cm long; pinnae 30--100 pairs",
    "rachis, pinnae | rachis length (no trait); number of pinna pairs (candidate leaflet_count)",
    p = "p156"
  ),
  s$row(
    "leaf_shape",
    "linear/lanceolate or slightly falcate",
    "linear lanceolate falcate",
    "linear lanceolate falcate",
    em = PN,
    p = "p156"
  ),
  s$un(
    "(except in seedlings where they are elliptic), widest just above the base",
    "pinnae | seedling shape; widest point",
    p = "p156"
  ),
  s$un(
    "to 27 x 12 mm",
    "pinnae | size (probable source typo; not scored)",
    p = "p156"
  ),
  s$un(
    "in sub-opposite pairs, inserted on the rachis at 45--80 degrees, stiffly coriaceous",
    "pinnae | arrangement, insertion angle, texture",
    p = "p156"
  ),
  s$row(
    "leaf_surface_colour",
    "shiny to glaucous green above",
    "green",
    "green",
    em = UP,
    p = "p156"
  ),
  s$un(
    "shiny to glaucous green above (especially in younger leaves), paler beneath",
    "lamina | shiny or glaucous (alternatives on one surface, age-dependent); paler beneath",
    p = "p156"
  ),
  s$un("base abruptly tapering", "pinnae | base (candidate leaf_base_shape attenuate)", p = "p156"),
  s$row(
    "leaf_margin_posture",
    "margins flat to slightly recurved",
    "flat recurved",
    "flat revolute",
    em = PN,
    p = "p156",
    notes = rec
  ),
  s$row("leaf_apex_shape", "apex acute, sharp", "acute", "acute", em = PN, p = "p156"),
  s$un(
    "sharp; midrib prominent above and below in live material, in dry material only prominent below, light green",
    "pinnae | sharp apex; midrib relief, colour",
    p = "p156"
  ),
  cata_un(s, "Cataphylls narrowly triangular, pilose, margins entire, to 105 x 15 mm, widest at base", "p156"),
  cone_un(
    s,
    "Male cones often in pairs; sub-sessile to shortly stalked, cone narrowly ellipsoid, at anthesis to 65 cm long, 17.5 cm diameter, pale yellow",
    "p156"
  ),
  micro_un(
    s,
    "Microsporophylls spathulate, to 45 x 22 mm, glabrous, pale yellow, with a terminal spine to 10 mm long, rigid, sharp, upturned at an acute angle; fertile zone firm, slightly dorsi-ventrally thickened, the distal portion splitting into two halves leaving a V-shaped free portion; pollen sacs closely arranged covering three quarters of the surface; sterile apical portion 2 mm long",
    "p156"
  ),
  mega_un(
    s,
    "Megasporophylls forming a closed-type 'cone', to 15 x 5--6 cm, erect, concave, overlapping and forming a cabbage-like head, soft yellow-brown silky throughout, each bearing 1--4 ovules; lamina broadly rhomboid to orbicular, margins deeply pectinate with 30--40 lateral spines, to 30 mm long including the glabrous tips; terminal spine to 7 cm long, attenuate, with 2--4 similarly shaped lateral processes; ovules glabrous",
    "p156"
  ),
  s$row("seed_colour", "Seeds ripening yellow", "yellow", "yellow_orange", p = "p156"),
  s$row(
    "seed_shape",
    "ovoid, laterally flattened",
    "ovoid laterally flattened",
    "ovoid_flattened",
    p = "p156"
  ),
  s$row(
    "seed_length",
    "including the sarcotesta to 40 mm long, 32 mm diameter",
    "40",
    "40",
    "maximum",
    "mm",
    em = SEED,
    p = "p156"
  ),
  s$row(
    "seed_width",
    "including the sarcotesta to 40 mm long, 32 mm diameter",
    "32",
    "32",
    "maximum",
    "mm",
    em = SEED,
    p = "p156",
    notes = diam
  ),
  s$row(
    "seed_surface_texture",
    "sarcotesta smooth",
    "smooth",
    "smooth",
    p = "p156"
  ),
  s$un(
    "cuticle slightly waxy; sarcotesta ... 3 mm thick, with a fibrous layer; sclerotesta without an apical spine, base distinctly stalked; endotesta not spongy",
    "seed coat | wax, sarcotesta thickness, fibrous layer; sclerotesta; endotesta",
    p = "p156"
  ),
  s$un(
    "In Southern Thailand (Yala and Trang Province) and in Peninsular Malaysia it is recorded from Perlis, Kedah (including Langkawi and nearby islands) and Perak as far south as Tambun. Due to commercial collecting, its distribution is now largely restricted to inaccessible and protected areas.",
    "distribution",
    sec = "Distribution",
    p = "p156"
  ),
  s$un(
    "Endangered A4cd. It is endangered by on-going, active commercial harvesting in its natural habitats.",
    "conservation status",
    sec = "Conservation status",
    p = "p156"
  ),
  s$row(
    "plant_growth_substrate",
    "Restricted to inland or offshore limestone outcrops, ... growing directly on rock or wedged into crevices and cracks",
    "growing directly on rock or wedged into crevices and cracks",
    "lithophyte",
    sec = "Ecology",
    p = "p156"
  ),
  s$row(
    "plant_tolerance_calcicole",
    "Restricted to inland or offshore limestone outcrops",
    "restricted to limestone",
    "calcicole",
    sec = "Ecology",
    p = "p156",
    notes = "Restricted to limestone scored as calcicole."
  ),
  s$un(
    "favouring south-facing cliff faces, ... in moderate shade to full sun.",
    "habitat; light (no categorical light trait)",
    sec = "Ecology",
    p = "p156"
  ),
  s$un(
    "Burkill ... reported the species traded as an ornamental plant since the early 20th century ... Harvesting and trade continue today ...",
    "uses",
    sec = "Uses",
    p = "p156"
  ),
  s$un(
    "One specimen (Curtis 2427) assigned to Epicyas elongata by de Laubenfels & Adema (1998) rightly belongs here.",
    "taxonomy | specimen identity",
    sec = "Note",
    p = "p157"
  )
)

# 3. Cycas edentata
s <- mk("Cycas edentata", "species", region = PM)
c_edentata <- bind_rows(
  s$row(
    "stem_growth_habit",
    "Stems erect, sometimes prostrate-pendulous to pendulous",
    "erect",
    "erect",
    q = "usually",
    p = "p157"
  ),
  s$row(
    "stem_growth_habit",
    "Stems erect, sometimes prostrate-pendulous to pendulous",
    "prostrate-pendulous pendulous",
    "prostrate pendulous",
    q = "sometimes",
    p = "p157"
  ),
  s$row("plant_height", "to 7 m tall", "7", "7", "maximum", "m", p = "p157"),
  s$un(
    "to 40 cm diameter",
    "stem | diameter, position not stated (candidate stem_diameter)",
    p = "p157"
  ),
  s$row(
    "stem_branching_form",
    "occasionally branching",
    "branching",
    "branched",
    q = "occasionally",
    p = "p157"
  ),
  s$un("not conspicuously swollen at base", "stem | base not swollen", p = "p157"),
  s$row("bark_texture", "Bark smooth", "smooth", "smooth", p = "p157"),
  s$row(
    "bark_colour",
    "generally light grey",
    "light grey",
    "grey",
    q = "generally",
    p = "p157"
  ),
  s$row(
    "bark_texture",
    "with age shallowly fissured",
    "shallowly fissured",
    "furrowed",
    em = "old bark",
    p = "p157"
  ),
  s$un("Leaves arching", "leaf | posture (arching)", p = "p157"),
  s$row(
    "leaf_hairs_immature_leaves",
    "when young densely covered in tomentose whitish hairs that are shed as the leaves uncurl",
    "tomentose hairs",
    "hairy",
    p = "p157",
    notes = flush
  ),
  s$row("leaf_length", "to 250 cm long", "250", "250", "maximum", "cm", p = "p157"),
  s$rng("petiole_length", "petioles 20--75 cm long", "20", "75", "cm", p = "p157"),
  s$un(
    "base sparsely tomentose, margin in the upper part or throughout with spines, spines to 3 mm long, soft-tipped",
    "petiole | hairs, spines",
    p = "p157"
  ),
  s$un(
    "rachis 55--175 cm long; pinnae 25--90 pairs",
    "rachis, pinnae | rachis length (no trait); number of pinna pairs (candidate leaflet_count)",
    p = "p157"
  ),
  s$row(
    "leaf_shape",
    "linear-lanceolate to falcate",
    "linear-lanceolate falcate",
    "linear lanceolate falcate",
    em = PN,
    p = "p157"
  ),
  s$un("widest below the middle", "pinnae | widest point", p = "p157"),
  s$row("leaflet_length", "to 37 x 2.5 cm", "37", "37", "maximum", "cm", em = PN, p = "p157"),
  s$row("leaflet_width", "to 37 x 2.5 cm", "2.5", "2.5", "maximum", "cm", em = PN, p = "p157"),
  s$un(
    "in sub-opposite pairs, inserted on the rachis at c. 50--70 degrees, coriaceous",
    "pinnae | arrangement, insertion angle, texture",
    p = "p157"
  ),
  s$row(
    "leaf_surface_colour",
    "in life glossy dark green above",
    "dark green",
    "dark_green",
    em = UP,
    p = "p157"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "in life glossy dark green above",
    "glossy",
    "shiny",
    em = UP,
    p = "p157"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "paler and not glossy beneath",
    "not glossy",
    "dull",
    em = LO,
    p = "p157",
    notes = "Not glossy scored as dull."
  ),
  s$un("paler ... beneath", "lamina | lower surface paler", p = "p157"),
  s$un("base tapering", "pinnae | base (candidate leaf_base_shape attenuate)", p = "p157"),
  s$row(
    "leaf_margin_posture",
    "margins distinctly, but shallowly inrolled",
    "inrolled",
    "involute",
    em = PN,
    p = "p157",
    notes = "Inrolled scored as involute."
  ),
  s$un(
    "(except in seedlings where they are often undulate)",
    "pinnae | seedling margin posture",
    p = "p157"
  ),
  s$row("leaf_apex_shape", "apex acute, soft", "acute", "acute", em = PN, p = "p157"),
  s$un(
    "soft; midrib prominent on both upper and lower surfaces in live material, in dry material only on upper surface as a broad, low, centrally depressed keel, often yellowish",
    "pinnae | soft apex; midrib relief, colour",
    p = "p157"
  ),
  cata_un(s, "Cataphylls to 180 x 35 mm", "p157"),
  cone_un(
    s,
    "Male cone usually solitary; cone ellipsoid to cylindric, sub-sessile to distinctly stalked (stalk to 10 cm long), at anthesis 40 cm long, 12 cm diameter, pale yellow",
    "p157"
  ),
  micro_un(
    s,
    "Microsporophylls spathulate, to 40 x 22 mm, finely tomentose, in age often becoming glabrous, pale yellow, apical spine to 25 mm long, sharp, upturned at an acute angle; fertile zone firm, slightly thickened; pollen-sacs closely arranged; sterile apical portion to 10 mm long",
    "p157"
  ),
  mega_un(
    s,
    "Megasporophylls forming an open-type 'cone', to 35 cm long, arching outward, when ripe pendulous, densely yellow- to brown-tomentose, often with a greenish tinge, bearing 1--6 ovules; lamina lanceolate, soft, flat, margins entire to more-or-less dentate, but never pectinate, soft, apex acuminate tapering to a 7 cm-long terminal spine",
    "p157"
  ),
  s$row(
    "seed_colour",
    "Seeds ripening reddish-brown",
    "reddish-brown",
    "red_brown",
    p = "p157"
  ),
  s$row(
    "seed_shape",
    "globose-ovoid",
    "globose ovoid",
    "globoid ovoid",
    p = "p157"
  ),
  s$un("slightly laterally flattened", "seed | slight lateral flattening", p = "p157"),
  s$rng(
    "seed_length",
    "including the sarcotesta to 60--80 mm long, 50 mm diameter",
    "60",
    "80",
    "mm",
    em = SEED,
    p = "p157",
    notes = "Printed 'to 60--80 mm'; scored as a range."
  ),
  s$row(
    "seed_width",
    "including the sarcotesta to 60--80 mm long, 50 mm diameter",
    "50",
    "50",
    "mean",
    "mm",
    em = SEED,
    p = "p157",
    notes = diam
  ),
  s$row(
    "seed_surface_texture",
    "cuticle thin, smooth, not waxy",
    "smooth",
    "smooth",
    p = "p157"
  ),
  s$un(
    "cuticle thin, ... not waxy; sarcotesta 5--6 mm thick, without a fibrous layer, base rounded; sclerotesta with or without short, crested spine; endotesta spongy",
    "seed coat | cuticle, sarcotesta thickness, fibrous layer, base; sclerotesta; endotesta",
    p = "p157"
  ),
  s$un(
    "SE Asia: Myanmar, Thailand, Vietnam, Peninsular Malaysia, Singapore (now extinct), Sumatra and Borneo. In Peninsular Malaysia it is reported to have once been widespread along the coast and on offshore islands of the entire peninsula. ... Today distribution is largely restricted to inaccessible and protected areas.",
    "distribution",
    sec = "Distribution",
    p = "p157"
  ),
  s$un("Near Threatened.", "conservation status", sec = "Conservation status", p = "p158"),
  s$un(
    "Restricted to coastal areas, usually on shallow, poor substrates such as sandy beaches or on bare granite rock, in moderate shade to full sun.",
    "habitat; substrate; light",
    sec = "Ecology",
    p = "p158"
  ),
  s$un(
    "Popular as an ornamental ... The young leaves are mucilaginous and consumed as a vegetable ...",
    "uses",
    sec = "Uses",
    p = "p158"
  ),
  s$un(
    "field observations on C. edentata populations in Peninsular Malaysia found both entire to distinctly dentate megasporophyll laminas within the same population. ... seeds from Peninsular Malaysian plants of C. edentata exhibit both crested and smooth seed morphology. ... C. edentata that has distinct stout terminal spines.",
    "taxonomy | megasporophyll margin, seed crest, microsporophyll spine variation",
    sec = "Taxonomy",
    p = "p158"
  )
)

# 4. Cycas macrocarpa
s <- mk("Cycas macrocarpa", "species", region = PM)
c_macrocarpa <- bind_rows(
  s$row(
    "stem_growth_habit",
    "Stems erect, sometimes prostrate-pendulous",
    "erect",
    "erect",
    q = "usually",
    p = "p159"
  ),
  s$row(
    "stem_growth_habit",
    "Stems erect, sometimes prostrate-pendulous",
    "prostrate-pendulous",
    "prostrate pendulous",
    q = "sometimes",
    p = "p159"
  ),
  s$row("plant_height", "to 12 m tall", "12", "12", "maximum", "m", p = "p159"),
  s$un(
    "to 34 cm diameter",
    "stem | diameter, position not stated (candidate stem_diameter)",
    p = "p159"
  ),
  s$row(
    "stem_branching_form",
    "occasionally branching",
    "branching",
    "branched",
    q = "occasionally",
    p = "p159"
  ),
  s$un("not swollen at base", "stem | base not swollen", p = "p159"),
  s$row(
    "bark_colour",
    "Bark glaucous brown-grey",
    "brown-grey",
    "brown grey",
    p = "p159"
  ),
  s$un("Bark glaucous", "bark | glaucous", p = "p159"),
  s$row("bark_texture", "smooth, rarely fissured", "smooth", "smooth", q = "usually", p = "p159"),
  s$row(
    "bark_texture",
    "smooth, rarely fissured",
    "fissured",
    "furrowed",
    q = "rarely",
    p = "p159"
  ),
  s$un("Leaves arching", "leaf | posture (arching)", p = "p159"),
  s$row(
    "leaf_hairs_immature_leaves",
    "in bud densely covered by woolly, brown hairs that are shed as the leaves uncurl",
    "woolly hairs",
    "hairy",
    p = "p159",
    notes = "Hairs on the leaves in bud, shed as the leaves uncurl."
  ),
  s$un(
    "new flush with a distinctive blue-green sheen",
    "young leaves | colour (candidate leaf_surface_colour blue_green for new flush)",
    p = "p159"
  ),
  s$row(
    "leaf_length",
    "total length to 250 cm long",
    "250",
    "250",
    "maximum",
    "cm",
    p = "p159"
  ),
  s$rng("petiole_length", "petioles 25--100 cm long", "25", "100", "cm", p = "p159"),
  s$un(
    "base thickened, sparsely tomentose, always spinescent to base, spines sharp-tipped, to 3 mm long",
    "petiole | base, hairs, spines",
    p = "p159"
  ),
  s$un(
    "rachis 50--150 cm long; pinnae 25--105 pairs",
    "rachis, pinnae | rachis length (no trait); number of pinna pairs (candidate leaflet_count)",
    p = "p159"
  ),
  s$row(
    "leaf_shape",
    "usually linear or linear-elliptic",
    "linear linear-elliptic",
    "linear linear_elliptical",
    q = "usually",
    em = PN,
    p = "p159"
  ),
  s$row("leaflet_length", "to 50 x 2.2 cm", "50", "50", "maximum", "cm", em = PN, p = "p159"),
  s$row("leaflet_width", "to 50 x 2.2 cm", "2.2", "2.2", "maximum", "cm", em = PN, p = "p159"),
  s$un(
    "in sub-opposite pairs, inserted on the rachis at 60--80 degrees, membranous, soft (in exposed locations less so)",
    "pinnae | arrangement, insertion angle, texture",
    p = "p159"
  ),
  s$row(
    "leaf_surface_colour",
    "glossy dark green above",
    "dark green",
    "dark_green",
    em = UP,
    p = "p159"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "glossy dark green above",
    "glossy",
    "shiny",
    em = UP,
    p = "p159"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "paler beneath (and not glossy)",
    "not glossy",
    "dull",
    em = LO,
    p = "p159",
    notes = "Not glossy scored as dull."
  ),
  s$un("paler beneath", "lamina | lower surface paler", p = "p159"),
  s$un(
    "base tapering and twisted",
    "pinnae | base (candidate leaf_base_shape attenuate), twisted",
    p = "p159"
  ),
  s$row(
    "leaf_margin_posture",
    "margins often undulate, not inrolled",
    "undulate",
    "undulate",
    q = "often",
    em = PN,
    p = "p159"
  ),
  s$row(
    "leaf_apex_shape",
    "apex acute, soft, in both live and dry leaves",
    "acute",
    "acute",
    em = PN,
    p = "p159"
  ),
  s$un(
    "soft ...; midrib raised only above, in dry leaves as a narrow sharp keel, light green",
    "pinnae | soft apex; midrib relief, colour",
    p = "p159"
  ),
  cata_un(s, "Cataphylls to 140 x 10 mm", "p159"),
  cone_un(
    s,
    "Male cones usually solitary, ovoid, sub-sessile to shortly stalked, to 25 cm long (pre-anthesis), 12 cm diameter, orange",
    "p159"
  ),
  micro_un(
    s,
    "Microsporophylls spathulate, to 45 x 17 mm, pubescent or felty, orange, with a brittle apical spine inconspicuous to prominent, linear, to 28 mm long, sharply upturned; fertile zone firm, slightly thickened; pollen-sacs closely arranged, sterile apical portion to 2 mm long",
    "p159"
  ),
  mega_un(
    s,
    "Megasporophylls forming an open-type 'cone', to 20 cm long, arching outwards, densely reddish brown pilose, bearing 2--6 ovules; lamina lanceolate, margins pectinate, with 8--10 sharp-tipped spines on each side, terminal spine to 6.5 cm long, attenuate, without lateral processes",
    "p159"
  ),
  s$row(
    "seed_colour",
    "Seeds ripening reddish brown",
    "reddish brown",
    "red_brown",
    p = "p159"
  ),
  s$un(
    "obovoid, ... laterally flattened, base slightly stalked",
    "seed | shape obovoid (no seed_shape level; dictionary blocks it), flattening, stalk",
    p = "p159"
  ),
  s$rng("seed_length", "50--60 mm long, 40 mm diameter", "50", "60", "mm", p = "p159"),
  s$row(
    "seed_width",
    "50--60 mm long, 40 mm diameter",
    "40",
    "40",
    "mean",
    "mm",
    p = "p159",
    notes = diam
  ),
  s$un(
    "cuticle not waxy; sarcotesta 4 mm thick, without a fibrous layer; sclerotesta apex often retuse with a spine-like tip; endotesta not spongy",
    "seed coat | wax, sarcotesta thickness, fibrous layer; sclerotesta; endotesta",
    p = "p159"
  ),
  s$un(
    "Peninsular Thailand ((Narathiwat Province) and SW Thailand (Prachuap Khiri Khan Province) and in Peninsular Malaysia in Perak, Melaka, Kelantan, Terengganu and Pahang.",
    "distribution",
    sec = "Distribution",
    p = "p159"
  ),
  s$un("Vulnerable B1b(iii)+c(iv).", "conservation status", sec = "Conservation status", p = "p159"),
  s$un(
    "In primary lowland forest, on well-drained, sandy clay loam soils, usually on ridges over granite at 300--600 m altitude or occasionally on limestone hills where soil accumulates or directly on rock. Interestingly this species has been observed both as widely scattered populations of few individuals and as high density populations of many closely spaced individuals.",
    "habitat; soil; altitude; population density",
    sec = "Ecology",
    p = "p159"
  ),
  s$un(
    "An ornamental species ... In times past, in Upper Perak its seeds were eaten by Orang Asli ... its stems were used as a medicine in childbirth.",
    "uses",
    sec = "Uses",
    p = "p160"
  )
)

# ---- Copy-down ----------------------------------------------------------------
# Genus values copied to members silent on the trait. Every value below is a
# single, unqualified level, and no member scores the trait itself. Not
# copied: multi-level or qualified genus rows (stem_growth_habit,
# stem_branching_form, bark_texture, leaf_apex_shape, seed_colour,
# seed_shape), numbers, and leaf_phyllotaxis 'whorled' (metadata question).
own <- bind_rows(c_cantafolia, c_clivicola_cl, c_edentata, c_macrocarpa)
members <- c(
  "Cycas cantafolia",
  "Cycas clivicola subsp. clivicola",
  "Cycas edentata",
  "Cycas macrocarpa"
)
copy <- tibble(
  from = "Cycas",
  trait = c(
    "sex_type",
    "plant_growth_form",
    "woodiness",
    "root_system_type",
    "leaf_compoundness",
    "leaf_lamina_division",
    "leaf_margin",
    "dispersal_appendage",
    "seedling_germination_location",
    "life_history",
    "nitrogen_fixing",
    "pollination_vector_possible",
    "dispersers",
    "dispersal_syndrome",
    "dispersal_unit"
  ),
  to = list(members)
) |>
  unnest(to)

copied <- copy |>
  inner_join(genus, by = c(from = "taxon_name", "trait"), relationship = "many-to-many") |>
  mutate(
    notes = str_squish(paste("Copied from the", from, "description.", notes)),
    taxon_rank = if_else(str_detect(to, "subsp[.]"), "subspecies", "species")
  ) |>
  select(-from) |>
  rename(taxon_name = to)

# Copy-down checks: each source row is unqualified and single-level, and no
# target already scores the trait.
stopifnot(
  nrow(copied) == nrow(copy),
  all(copied$commonness_qualifier == ""),
  !any(str_detect(copied$trait_value_clean, " ")),
  nrow(semi_join(copied, own, by = c("taxon_name", "trait"))) == 0
)

d <- bind_rows(family, genus, c_clivicola, own, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Jutta_2012/raw/trait_data.csv", na = "")
write_csv(flora_unmapped_summary(d), "data/Jutta_2012/raw/unmapped_terms.csv", na = "")

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Cycadaceae", "family", "accepted", "Monogeneric; no family description (Taxonomy paragraph only).",
  "Cycas", "genus", "accepted", "",
  "Cycas cantafolia", "species", "accepted", "Described by the author in 2010; known only from the type locality.",
  "Cycas clivicola", "species", "accepted", "Species heading with Distribution and Taxonomy only; described locally by subsp. clivicola. Author doubts Hill's subspecies.",
  "Cycas clivicola subsp. clivicola", "subspecies", "accepted", "Only infraspecific taxon in the region; its description is scored at subspecies rank.",
  "Cycas edentata", "species", "accepted", "Includes C. litoralis K.D.Hill as a synonym; separation from C. rumphii questioned in Taxonomy.",
  "Cycas macrocarpa", "species", "accepted", ""
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Jutta_2012/raw/taxon_flags.csv", na = "")
