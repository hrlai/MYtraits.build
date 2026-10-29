# Hand extraction of Nymphaeaceae (R. Kiew 2015, Flora of Peninsular Malaysia
# Series II vol. 5: 207--219). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Nymphaeaceae.pdf, born-digital (InDesign)
# text layer read with pdftools; flora_layer_flags() found no bad glyphs on the
# treatment pages. PDF page = printed page - 199 (treatment pdf 8--20); pdf 16
# (p. 215, Fig. 1) and the plates (pdf 24--25, pp. 302--303) are figures, used
# for vouchers only. No source repairs were needed.
#
# Small family (2 genera, 5 species), so rows are written by hand with
# flora_hand() (Berhaman_1995 pattern). Higher-rank values are copied down by
# hand under the flora_copy_down() rules (unqualified, single level, every
# member that scores the trait agrees, most specific rank wins). Hedged
# statements (suggest, possibly, appears, may) are not scored.
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(207:219, \(p) c(p - 199L, p)), paste0("p", 207:219))
PM <- "Peninsular Malaysia"
WS <- "Williamson_1994"

# Row helpers for one taxon with default rank_scored_from, region and section.
mk <- function(taxon, rank, from = "", region = "", sec = "Description") {
  h <- flora_hand(taxon, pg, rank)
  h <- map(h, \(f) {
    \(...) {
      a <- list(...)
      a$rank_from <- a$rank_from %||% from
      a$region <- a$region %||% region
      a$sec <- a$sec %||% sec
      do.call(f, a)
    }
  })
  h
}
cnt <- "{count}"
dry <- "Measured on dry material ('when dry'); diameter scored as width."
diam <- "Diameter scored as width."
loc <- "Ovary locules scored as carpels (one locule per carpel assumed)."
lobe <- "Corolla lobe length scored as petal length (petals connate into a tube in Barclaya)."

# ---- Family ---------------------------------------------------------------
f <- mk("Nymphaeaceae", "family", "family", sec = "Family description")
FE <- "Family Ecology"
family <- bind_rows(
  f$row(
    "plant_growth_form",
    "Aquatic perennial herbs rooted in substrate",
    "herbs",
    "herb",
    p = "p207"
  ),
  f$row(
    "life_history",
    "Aquatic perennial herbs rooted in substrate",
    "perennial",
    "perennial",
    p = "p207"
  ),
  f$row(
    "plant_growth_substrate",
    "Aquatic perennial herbs rooted in substrate",
    "aquatic rooted in substrate",
    "aquatic_rooted",
    q = "usually",
    p = "p207"
  ),
  f$row(
    "plant_growth_substrate",
    "rarely marsh plants (Barclaya rotundifolia)",
    "marsh plants",
    "semiaquatic",
    q = "rarely",
    p = "p207",
    notes = "Marsh plant scored as semiaquatic."
  ),
  f$row(
    "stem_growth_habit",
    "with long horizontal rhizomes, sometimes tuberous or with a short erect stem",
    "rhizomes",
    "rhizomatous",
    q = "usually",
    p = "p207"
  ),
  f$un(
    "with long horizontal rhizomes, sometimes tuberous or with a short erect stem",
    "rhizome | length, orientation; tubers; erect stem",
    p = "p207"
  ),
  f$un("Stipules present or lacking", "stipules | presence", p = "p207"),
  f$row(
    "leaf_arrangement",
    "Leaves spirally arranged",
    "spirally arranged",
    "spiral",
    p = "p207"
  ),
  f$un(
    "floating, submerged or rarely emergent (B. rotundifolia)",
    "leaf | position in water (candidate trait)",
    p = "p207"
  ),
  f$un(
    "with or without thorns on the petiole and lower lamina surface; petioles long",
    "petiole, lamina | thorns; petiole length",
    p = "p207"
  ),
  f$row("leaf_compoundness", "lamina simple", "simple", "simple", p = "p207"),
  f$un(
    "peltate or attached at lamina base",
    "lamina | petiole attachment (peltate or basal)",
    p = "p207"
  ),
  f$row(
    "leaf_shape",
    "ovate to orbicular or rarely ribbon-like (B. longifolia)",
    "ovate orbicular",
    "ovate orbicular",
    q = "usually",
    p = "p207"
  ),
  f$row(
    "leaf_shape",
    "ovate to orbicular or rarely ribbon-like (B. longifolia)",
    "ribbon-like",
    "strap-shaped",
    q = "rarely",
    p = "p207",
    notes = "Ribbon-like scored as strap-shaped."
  ),
  f$row(
    "leaf_base_shape",
    "base cordate or sagittate",
    "cordate sagittate",
    "cordate sagittate",
    p = "p207"
  ),
  f$row(
    "leaf_margin",
    "margin entire or dentate or upturned (Victoria spp.)",
    "entire dentate",
    "entire toothed_dentate",
    p = "p207"
  ),
  f$un(
    "or upturned (Victoria spp.)",
    "leaf margin | upturned (genus not in the flora region)",
    p = "p207"
  ),
  f$un("Flowers often large and showy", "flower | size, showiness", p = "p207"),
  f$row(
    "flower_perianth_symmetry",
    "Flowers often large and showy, actinomorphic",
    "actinomorphic",
    "actinomorphic_general",
    p = "p207"
  ),
  f$row(
    "flower_structural_sex_type",
    "actinomorphic, bisexual",
    "bisexual",
    "bisexual",
    p = "p207"
  ),
  f$row(
    "inflorescence_type",
    "bisexual, solitary",
    "solitary",
    "solitary",
    p = "p207"
  ),
  f$un(
    "held above the water surface on long pedicels",
    "flower | position above water; pedicel length",
    p = "p207"
  ),
  f$row(
    "flower_perianth_phyllotaxis",
    "organs spirally arranged",
    "spirally arranged",
    "spiral",
    p = "p207",
    notes = "Not copied down: Barclaya petals are connate into a tube with lobes 'in several series inserted at different levels'."
  ),
  f$row(
    "flower_androecium_structural_phyllotaxis",
    "organs spirally arranged",
    "spirally arranged",
    "spiral",
    p = "p207",
    notes = "Not copied down: Barclaya stamens are epipetalous within the corolla tube."
  ),
  f$un(
    "sepals 4--9, green or coloured (or petaloid in Nuphar)",
    "sepals | number, colour",
    p = "p207"
  ),
  f$un(
    "petals 3 to many (or absent in Ondinea)",
    "petals | number",
    p = "p207"
  ),
  f$row(
    "flower_colour",
    "petals ... yellow, pink, red, blue, purple or white",
    "yellow pink red blue purple white",
    "yellow_orange pink red_brown blue_purple white_cream",
    p = "p207"
  ),
  f$row(
    "flower_fertile_stamens_count",
    "stamens 14 to many",
    "14",
    "14",
    "minimum",
    cnt,
    p = "p207"
  ),
  f$un(
    "free, laminar or with a distinct filament",
    "stamens | fusion, laminar or filament",
    p = "p207"
  ),
  f$un(
    "gynoecium inferior (Barclaya) or semi-inferior or superior (Nuphar and Ondinea)",
    "gynoecium | ovary position by genus",
    p = "p207"
  ),
  f$rng(
    "flower_structural_carpels_count",
    "carpels 3--40",
    "3",
    "40",
    cnt,
    p = "p207"
  ),
  f$un("joined or partly free", "carpels | fusion", p = "p207"),
  f$row(
    "flower_gynoecium_placentation",
    "placentation lamellar",
    "lamellar",
    "laminar",
    p = "p207",
    notes = "Lamellar scored as laminar."
  ),
  f$row(
    "fruit_type",
    "Fruit a spongy berry",
    "berry",
    "berry",
    p = "p207"
  ),
  f$un("Fruit a spongy berry", "fruit | spongy texture", p = "p207"),
  f$row(
    "fruit_dehiscence",
    "splitting when the mucilage inside swells or indehiscent",
    "splitting indehiscent",
    "dehiscent indehiscent",
    p = "p207"
  ),
  f$un(
    "Seeds many, arillate (Nymphaea and Ondinea) or aril lacking; perisperm copious, endosperm scanty.",
    "seed | number, aril by genus, perisperm, endosperm",
    p = "p207"
  ),
  f$un(
    "Cosmopolitan in tropical and subtropical regions, with 6 genera and c. 70 species; ... In Peninsular Malaysia, the family is represented by 2 genera (Barclaya and Nymphaea) and 5 species.",
    "distribution and species counts",
    sec = "Family Distribution",
    p = "p207"
  ),
  f$row(
    "plant_growth_substrate",
    "Confined to freshwater in lakes, ponds, rivers and streams, ditches, canals, etc., rooted in mud.",
    "freshwater rooted in mud",
    "aquatic_rooted",
    sec = FE,
    p = "p207",
    notes = "Not copied down: B. rotundifolia is a marsh plant (semiaquatic)."
  ),
  f$un(
    "Confined to freshwater in lakes, ponds, rivers and streams, ditches, canals, etc.",
    "habitat",
    sec = FE,
    p = "p207"
  ),
  f$un(
    "The Victoria waterlilies have the largest leaves ... scented of pineapple.",
    "Victoria leaf and flower size, scent (genus not in the flora region; not scored)",
    sec = FE,
    p = "p207"
  ),
  f$row(
    "pollination_syndrome",
    "Waterlilies are insect pollinated either by bees (day flowering species) or beetles (night flowering species, some of which, e.g. Victoria spp., are thermogenic).",
    "bees beetles",
    "bee beetle",
    sec = FE,
    p = "p207",
    notes = "General statement for waterlilies; not copied down (two levels)."
  ),
  f$un(
    "night flowering species, some of which, e.g. Victoria spp., are thermogenic",
    "flower | anthesis time, thermogenesis",
    sec = FE,
    p = "p207"
  ),
  f$un(
    "The fruit often ripens below the water surface.",
    "fruit | ripening below water",
    sec = FE,
    p = "p207"
  ),
  f$un(
    "Seeds of some species have arils that enclose an air bubble and so the seed floats and are dispersed by water currents (Nymphaea) or the seeds are covered in spines (Barclaya) that suggest they could become attached to the fur or feathers of animals or birds (Mabberley, 2008).",
    "seed dispersal | 'some species' (Nymphaea); Barclaya epizoochory hedged ('suggest ... could'), not scored",
    sec = FE,
    p = "p207",
    source_id = "Mabberley_2008"
  ),
  f$un(
    "The main value of the family is the ornamental waterlilies ... Several species, such as Nymphaea nouchali, yield edible seeds and rhizomes.",
    "uses",
    sec = "Family Uses",
    p = "p208"
  ),
  f$un(
    "Based on similarities in aquatic habit, earlier accounts ... Barclaya differs from the other genera in a number of characters: the gynoecium is completely inferior, the petals are joined into a tube ... ovules are orthotropous ... and the seeds do not have an aril. ...",
    "taxonomy; Barclaya diagnostic characters (ovary inferior, ovules orthotropous)",
    sec = "Family Taxonomy",
    p = "p208"
  )
)

# ---- 1. Barclaya ------------------------------------------------------------
g <- mk("Barclaya", "genus", "genus", sec = "Genus description")
GE <- "Genus Ecology"
barclaya_g <- bind_rows(
  g$row("plant_growth_form", "Perennial herbs", "herbs", "herb", p = "p209"),
  g$row("life_history", "Perennial herbs", "perennial", "perennial", p = "p209"),
  g$row(
    "stem_growth_habit",
    "rhizome fleshy, creeping",
    "rhizome",
    "rhizomatous",
    p = "p209"
  ),
  g$un("rhizome fleshy, creeping", "rhizome | fleshy, creeping", p = "p209"),
  g$row(
    "leaf_arrangement",
    "Leaves in rosettes",
    "in rosettes",
    "rosette",
    p = "p209"
  ),
  g$un(
    "submerged or just above the water surface (B. rotundifolia)",
    "leaf | position in water (candidate trait)",
    p = "p209"
  ),
  g$row(
    "leaf_shape",
    "lamina lanceolate to ovate to orbicular",
    "lanceolate ovate orbicular",
    "lanceolate ovate orbicular",
    p = "p209"
  ),
  g$un(
    "margin entire or undulate",
    "leaf margin | entire or undulate (margin and posture mixed)",
    p = "p209"
  ),
  g$un("venation pinnate", "leaf | venation", p = "p209"),
  g$un(
    "Flowers hypogynous",
    "flower | hypogynous (conflicts with 'gynoecium inferior (Barclaya)', p. 207)",
    p = "p209"
  ),
  g$un("buds narrowly ellipsoid", "bud | shape", p = "p209"),
  g$un(
    "sepals narrowly lanceolate, opening wide, pale yellow or greenish brown, (4--)5, free, strongly keeled elongating into a long apical tip",
    "sepals | shape, colour, number, keel",
    p = "p209"
  ),
  g$row(
    "flower_colour",
    "petals deep pink or purple or red",
    "deep_pink purple red",
    "pink blue_purple red_brown",
    p = "p209"
  ),
  g$un(
    "connate into a corolla tube adnate to the syncarpous ovary, lobes (8--)12(--many), in several series inserted at different levels",
    "corolla | fusion, adnation to ovary, lobe number and series",
    p = "p209"
  ),
  g$un(
    "stamens many, magenta, epipetalous within the corolla tube, pendent, staminodia 10--20, above the stamens",
    "stamens | number, colour, epipetalous, pendent; staminodes",
    p = "p209"
  ),
  g$rng(
    "flower_structural_carpels_count",
    "ovary syncarpous, carpels 8--14",
    "8",
    "14",
    cnt,
    p = "p209"
  ),
  g$un(
    "ovary syncarpous ... carpellary appendages encircling the stigmatic cup",
    "ovary | syncarpy, carpellary appendages",
    p = "p209"
  ),
  g$row("fruit_type", "Berry globose to ellipsoid", "berry", "berry", p = "p209"),
  g$row(
    "fruit_dehiscence",
    "spongy, dehiscing irregularly",
    "dehiscing irregularly",
    "dehiscent",
    p = "p209"
  ),
  g$un(
    "Berry globose to ellipsoid, spongy ... sepals persistent",
    "fruit | shape, texture, persistent sepals",
    p = "p209"
  ),
  g$row(
    "seed_shape",
    "Seeds many, globose or ellipsoid",
    "globose ellipsoid",
    "globoid ellipsoid",
    p = "p209"
  ),
  g$row(
    "seed_width",
    "c. 1 mm diameter",
    "1",
    "1",
    "mean",
    "mm",
    p = "p209",
    notes = diam
  ),
  g$un(
    "Seeds many ... embedded in mucilage or in spongy tissue",
    "seed | number, mucilage",
    p = "p209"
  ),
  g$row(
    "seed_surface_texture",
    "testa with long, soft spines",
    "spines",
    "spiny",
    p = "p209"
  ),
  g$un(
    "without an aril",
    "seed | aril absent (negative phrase; dispersal_appendage not scored)",
    p = "p209"
  ),
  g$un(
    "India (Andaman Is.), Myanmar, Thailand, Sumatra, Peninsular Malaysia, Singapore, Borneo and New Guinea. Three species, all of which are native in Peninsular Malaysia.",
    "distribution",
    sec = "Genus Distribution",
    p = "p209"
  ),
  g$un(
    "In shaded primary lowland forest, in freshwater or peat swamp forest or in waterlogged hollows or in clear, shallow streams on fine clay substrate or on fine gravel, sometimes persisting after disturbance.",
    "habitat; light; substrate; disturbance",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "Only B. rotundifolia holds its leaves above the water surface; in B. longifolia and B. motleyi the leaves are submerged or when exposed by falling water levels lie prostrate on the wet substrate.",
    "leaf | position in water by species (candidate trait)",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "The flowers of all species are held and open above the water surface",
    "flower | position above water",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "The sepals open wide, their pale dull colour contrasts with the maroon or purple corolla.",
    "sepals, corolla | colour contrast",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "After pollination, the sepals and corolla tube persist, while the stalk bends so that the fruit ripens at or below the water surface.",
    "fruit | ripening at or below water; persistent perianth",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "The pericarp disintegrates or ruptures irregularly as the mucilage inside swells and the spongy pericarp tissue enables the fruit to float so it may be dispersed by water currents. The spiny seeds may be further dispersed on the fur or feathers of mammals or birds.",
    "dispersal | hydrochory, epizoochory hedged ('may be'), not scored",
    sec = GE,
    p = "p209"
  ),
  g$row(
    "vegetative_reproduction_ability",
    "Barclaya species also propagate vegetatively by runners or by small tubers.",
    "propagate vegetatively",
    "vegetative",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "by runners or by small tubers",
    "clonal spread | runners, tubers",
    sec = GE,
    p = "p209"
  ),
  g$un(
    "In Continental Europe and N America B. longifolia is traded as an aquarium plant valued for its reddish submerged leaves.",
    "uses",
    sec = "Genus Uses",
    p = "p210"
  ),
  g$un(
    "Several authors ... interpreted the sepals as bracts and the corolla as the perianth. Developmental anatomy confirms the outer organs as sepals (Williamson & Schneider ...).",
    "taxonomy | sepal interpretation",
    sec = "Genus Note",
    p = "p210",
    source_id = WS
  )
)

# 1. Barclaya longifolia
s <- mk("Barclaya longifolia", "species", region = PM)
b_longifolia <- bind_rows(
  s$row(
    "plant_growth_form",
    "Submerged aquatic herb with a branching white rhizome",
    "herb",
    "herb",
    p = "p210"
  ),
  s$row(
    "plant_growth_substrate",
    "Submerged aquatic herb with a branching white rhizome",
    "aquatic",
    "aquatic_rooted",
    p = "p210",
    notes = "Rhizomatous submerged aquatic scored as aquatic_rooted."
  ),
  s$row(
    "stem_growth_habit",
    "Submerged aquatic herb with a branching white rhizome",
    "submerged rhizome",
    "submerged rhizomatous",
    p = "p210"
  ),
  s$un("branching white rhizome", "rhizome | branching, colour", p = "p210"),
  s$rng(
    "petiole_length",
    "petiole flexible, 9--18 cm long",
    "9",
    "18",
    "cm",
    p = "p210"
  ),
  s$rng(
    "petiole_width",
    "1.5--3 mm diameter when dry",
    "1.5",
    "3",
    "mm",
    p = "p210",
    notes = dry
  ),
  s$un(
    "petiole flexible ... glabrous or ferrugineous at base",
    "petiole | flexibility, hairs",
    p = "p210"
  ),
  s$row(
    "leaf_surface_colour",
    "lamina deep green or reddish above",
    "deep_green reddish",
    "dark_green red",
    em = "upper surface",
    p = "p210"
  ),
  s$row(
    "leaf_surface_colour",
    "red-brown beneath",
    "red-brown",
    "brown",
    em = "lower surface",
    p = "p210"
  ),
  s$un("very thin", "lamina | thickness (descriptive)", p = "p210"),
  s$row(
    "leaf_shape",
    "narrowly oblong",
    "narrowly oblong",
    "narrowly_oblong",
    p = "p210"
  ),
  s$rng("leaf_length", "narrowly oblong, 16--24 x 2.5--4 cm", "16", "24", "cm", p = "p210"),
  s$rng("leaf_width", "narrowly oblong, 16--24 x 2.5--4 cm", "2.5", "4", "cm", p = "p210"),
  s$row("leaf_base_shape", "base sagittate", "sagittate", "sagittate", p = "p210"),
  s$un("basal lobes 0.2--0.7 cm long", "lamina | basal lobe length", p = "p210"),
  s$row(
    "leaf_margin_posture",
    "margin undulate",
    "undulate",
    "undulate",
    p = "p210"
  ),
  s$row(
    "leaf_apex_shape",
    "narrowed to an obtuse apex",
    "obtuse",
    "obtuse",
    p = "p210"
  ),
  s$un(
    "veins 11--20 on each side of midrib, fine, scarcely raised above and beneath; intercostal venation obscure",
    "veins | number, relief",
    p = "p210"
  ),
  s$un(
    "pedicel (4--)15--30 cm long, glabrous or with sparse short white hairs",
    "pedicel | length (candidate pedicel_length), hairs",
    p = "p210"
  ),
  s$ext(
    "flower_sepal_length",
    "sepals 5, purplish brown or green, narrowly lanceolate, (20--)25--30 x (2.5--)4--6 mm",
    a = "20",
    b = "25",
    c = "30",
    unit = "mm",
    p = "p210"
  ),
  s$un(
    "sepals 5, purplish brown or green, narrowly lanceolate, ... x (2.5--)4--6 mm, apex long attenuate with an apiculus 2--4 mm long or sometimes truncate, outer surface glabrous to glabrescent and keeled, inner surface glabrous, pink",
    "sepals | number, colour, shape, width (candidate flower_sepal_width), apex, hairs",
    p = "p210"
  ),
  s$un(
    "corolla 20--25 mm long",
    "corolla | length (no corolla length trait)",
    p = "p210"
  ),
  s$row(
    "flower_colour",
    "tube deep purple or deep red",
    "deep_purple deep_red",
    "blue_purple red_brown",
    p = "p210",
    notes = "Corolla tube colour; lobe colours (greenish, magenta, white) kept unmapped."
  ),
  s$row(
    "flower_tube_length",
    "tube deep purple or deep red to 9 mm long",
    "9",
    "9",
    "maximum",
    "mm",
    p = "p210"
  ),
  s$un(
    "lobes 12--15 ... rounded and cucullate, outermost three greenish opening horizontally with a curved tip, middle four larger and magenta, innermost eight lobes reduced and white",
    "corolla lobes | number, shape, colour by series",
    p = "p210"
  ),
  s$rng(
    "flower_petal_length",
    "lobes 12--15, 7--9 x 3--5 mm",
    "7",
    "9",
    "mm",
    p = "p210",
    notes = lobe
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens c. 21",
    "21",
    "21",
    "mean",
    cnt,
    p = "p211"
  ),
  s$row(
    "flower_anther_colour",
    "anthers subsessile, white, oblong",
    "white",
    "white",
    p = "p211"
  ),
  s$row(
    "flower_anther_length",
    "to 1.5 mm long",
    "1.5",
    "1.5",
    "maximum",
    "mm",
    p = "p211"
  ),
  s$un(
    "anthers subsessile ... oblong ... tip slightly apiculate",
    "anthers | subsessile, shape, apex",
    p = "p211"
  ),
  s$un(
    "ovary with stigma ring concave, stigmas c. 9",
    "stigma | ring shape, number",
    p = "p211"
  ),
  s$row(
    "flower_stigma_length",
    "stigmas c. 9, c. 1.3 mm long",
    "1.3",
    "1.3",
    "mean",
    "mm",
    p = "p211"
  ),
  s$row("fruit_type", "Berry globose", "berry", "berry", p = "p211"),
  s$un("Berry globose", "fruit | shape", p = "p211"),
  s$rng(
    "fruit_width",
    "Berry globose, 1.5--2 cm diameter",
    "1.5",
    "2",
    "cm",
    p = "p211",
    notes = diam
  ),
  s$row(
    "fruit_dehiscence",
    "splitting into 2--3 pieces, exposing white jelly and seeds",
    "splitting",
    "dehiscent",
    p = "p211"
  ),
  s$un(
    "splitting into 2--3 pieces, exposing white jelly and seeds",
    "fruit | number of pieces, jelly",
    p = "p211"
  ),
  s$row("seed_shape", "Seeds globose, spiny", "globose", "globoid", p = "p211"),
  s$row("seed_surface_texture", "Seeds globose, spiny", "spiny", "spiny", p = "p211"),
  s$un(
    "India (Andaman Is.), Myanmar (Pegu, Tenasserim), E & S Thailand, Laos, Vietnam and Peninsular Malaysia. In Peninsular Malaysia only in the extreme north in Kedah (Langkawi Is., ...) and Kelantan (Setir, ...).",
    "distribution",
    sec = "Distribution",
    p = "p211"
  ),
  s$un(
    "Global Status--Least Concern. ... Peninsular Malaysia Red List--Endangered B2ab(iii). ...",
    "conservation status",
    sec = "Conservation status",
    p = "p211"
  ),
  s$un(
    "It grows in light shade in small streams with a sandy or fine gravel substrate with a moderately strong current or on the edge of muddy slow streams. Its leaves grow submerged to 30 cm deep.",
    "habitat; light; substrate; water depth",
    sec = "Ecology",
    p = "p211"
  ),
  s$un(
    "In Thailand, its flowers were recorded as smelling of 'decomposing prawns' (P.C.Boyce 903).",
    "flower | scent (outside the flora region; not scored)",
    sec = "Ecology",
    region = "Thailand",
    p = "p211"
  ),
  s$un(
    "In cultivation, it flowers and fruits seasonally in July.",
    "flowering, fruiting | July in cultivation (place not stated; not scored)",
    sec = "Ecology",
    region = "",
    p = "p211"
  ),
  s$row(
    "breeding_system",
    "The flowers are self-pollinating whether produced above or below the water surface",
    "self-pollinating",
    "autogamy",
    sec = "Ecology",
    p = "p211"
  ),
  s$row(
    "breeding_system",
    "although Williamson & Schneider (1994) have reported that B. longifolia is sometimes cleistogamous and self-pollinating with flowers remaining closed and not emerging above the water surface",
    "cleistogamous",
    "cleistogamy",
    q = "sometimes",
    sec = GE,
    region = "",
    p = "p209",
    source_id = WS,
    notes = "From the genus Ecology paragraph; region of the study not stated. Species Ecology (p. 211): submerged flowers are cleistogamous."
  ),
  s$un(
    "and produce many seeds that readily germinate",
    "seed | germination",
    sec = "Ecology",
    p = "p211"
  ),
  s$un(
    "unlike those of B. longifolia that readily germinate submerged",
    "seed | germination under water",
    sec = GE,
    p = "p209"
  ),
  s$un(
    "Traded commercially as 'Orchid Lily', it comes in two colour forms ... In aquaria its leaves can grow to 30--80 x 6.5 cm long. ... In aquaria, it periodically undergoes a dormant period when the leaves die back after which it produces new leaves.",
    "uses; trade colour forms; leaf size and dormancy in aquaria (cultivated, not scored)",
    sec = "Uses",
    region = "",
    p = "p211"
  )
)

# 2. Barclaya motleyi
s <- mk("Barclaya motleyi", "species", region = PM)
b_motleyi <- bind_rows(
  s$row(
    "plant_growth_form",
    "Submerged aquatic herb with a long thin branching rhizome",
    "herb",
    "herb",
    p = "p212"
  ),
  s$row(
    "plant_growth_substrate",
    "Submerged aquatic herb with a long thin branching rhizome, nodes rooting",
    "aquatic",
    "aquatic_rooted",
    p = "p212",
    notes = "Rhizomatous submerged aquatic scored as aquatic_rooted."
  ),
  s$row(
    "stem_growth_habit",
    "Submerged aquatic herb with a long thin branching rhizome",
    "submerged rhizome",
    "submerged rhizomatous",
    p = "p212"
  ),
  s$un(
    "long thin branching rhizome, nodes rooting and producing leaves",
    "rhizome | length, thickness, branching, rooting nodes",
    p = "p212"
  ),
  s$rng(
    "petiole_length",
    "petiole reddish, thin and flexible, 8.5--27.5 cm long",
    "8.5",
    "27.5",
    "cm",
    p = "p212"
  ),
  s$ext(
    "petiole_width",
    "(1--)2--3(--5) mm diameter when dry",
    a = "1",
    b = "2",
    c = "3",
    d = "5",
    unit = "mm",
    p = "p212",
    notes = dry
  ),
  s$un(
    "petiole reddish, thin and flexible ... densely pubescent becoming glabrescent",
    "petiole | colour, flexibility, hairs",
    p = "p212"
  ),
  s$row(
    "leaf_surface_colour",
    "lamina brownish or purplish green or dull green above",
    "brownish_green purplish_green dull_green",
    "green_brown green",
    em = "upper surface",
    p = "p212",
    notes = "Purplish green and dull green scored as green."
  ),
  s$row(
    "leaf_surface_colour",
    "yellowish, reddish or purplish brown or deep magenta beneath",
    "yellowish reddish_brown purplish_brown deep_magenta",
    "yellow brown purple",
    em = "lower surface",
    p = "p212"
  ),
  s$un("thin", "lamina | thickness (descriptive)", p = "p212"),
  s$row(
    "leaf_shape",
    "broadly oval to obovate or ovate-rotund, rarely rotund",
    "broadly_oval obovate ovate-rotund",
    "widely_elliptical obovate widely_ovate",
    q = "usually",
    p = "p212",
    notes = "Broadly oval scored as widely elliptical; ovate-rotund as widely ovate."
  ),
  s$row(
    "leaf_shape",
    "broadly oval to obovate or ovate-rotund, rarely rotund",
    "rotund",
    "orbicular",
    q = "rarely",
    p = "p212"
  ),
  s$rng("leaf_length", "4--10 x 3.5--8 cm", "4", "10", "cm", p = "p212"),
  s$rng("leaf_width", "4--10 x 3.5--8 cm", "3.5", "8", "cm", p = "p212"),
  s$row(
    "leaf_base_shape",
    "base slightly cordate, lobes 0.3--1.5 cm long and lobes far apart or truncate",
    "cordate truncate",
    "cordate truncate",
    p = "p212"
  ),
  s$un(
    "lobes 0.3--1.5 cm long and lobes far apart ... sometimes unequal",
    "lamina | basal lobe length, spacing, symmetry",
    p = "p212"
  ),
  s$row(
    "leaf_margin_posture",
    "margin undulate",
    "undulate",
    "undulate",
    p = "p212"
  ),
  s$row(
    "leaf_apex_shape",
    "apex rounded, sometimes obtusely acute",
    "rounded",
    "rounded",
    q = "usually",
    p = "p212"
  ),
  s$row(
    "leaf_apex_shape",
    "apex rounded, sometimes obtusely acute",
    "obtusely_acute",
    "acute",
    q = "sometimes",
    p = "p212",
    notes = "Obtusely acute scored as acute."
  ),
  s$un(
    "lateral veins 3--5 pairs, sometimes the lowermost two pairs arising together from base of the midrib, plain above, slightly prominent beneath, glabrous to densely pubescent; intercostal veins obscure",
    "veins | number, arrangement, relief, hairs",
    p = "p212"
  ),
  s$un(
    "pedicel (5--)10.5--19 cm long, lengthening to (9.5--)22--26.5 cm in fruit, pubescent, sparsely clothed with short white hairs or glabrous",
    "pedicel | length in flower and fruit (candidate pedicel_length), hairs",
    p = "p212"
  ),
  s$rng(
    "flower_sepal_length",
    "sepals 5, dull purple-brown or pink, narrowly lanceolate 14--50 x 3--5 mm",
    "14",
    "50",
    "mm",
    p = "p212"
  ),
  s$un(
    "sepals 5, dull purple-brown or pink, narrowly lanceolate ... x 3--5 mm, outer surface slightly hairy, slightly keeled, sometimes apex abruptly narrowed into an apiculus 2--4 mm long",
    "sepals | number, colour, shape, width (candidate flower_sepal_width), hairs, apex",
    p = "p212"
  ),
  s$row(
    "flower_colour",
    "corolla tube dark red, purplish or greenish yellow",
    "dark_red purplish greenish_yellow",
    "red_brown blue_purple yellow_orange",
    p = "p212",
    notes = "Corolla tube colour; greenish yellow scored as yellow_orange; lobe colours kept unmapped."
  ),
  s$rng(
    "flower_tube_length",
    "corolla tube ... to 8--9 mm long",
    "8",
    "9",
    "mm",
    p = "p212",
    notes = "'to 8--9' recorded as a range."
  ),
  s$un(
    "c. 4.5 mm wide, with (5--)12--15 lobes ... rounded and hood-like, 3 outermost lobes greenish, middle four larger and pink-magenta, eight innermost lobes reduced and white",
    "corolla | tube width, lobe number, shape, colour by series",
    p = "p212"
  ),
  s$rng(
    "flower_petal_length",
    "with (5--)12--15 lobes, 7--9 x 2--5 mm",
    "7",
    "9",
    "mm",
    p = "p212",
    notes = lobe
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens c. 21",
    "21",
    "21",
    "mean",
    cnt,
    p = "p212"
  ),
  s$row(
    "flower_anther_colour",
    "anthers subsessile, white, tinged pink, oblong",
    "white",
    "white",
    p = "p212",
    notes = "'tinged pink' not scored."
  ),
  s$row(
    "flower_anther_length",
    "to 1.5 mm long",
    "1.5",
    "1.5",
    "maximum",
    "mm",
    p = "p212"
  ),
  s$un(
    "anthers subsessile ... tinged pink, oblong ... tip slightly apiculate",
    "anthers | subsessile, tinge, shape, apex",
    p = "p212"
  ),
  s$row(
    "flower_structural_carpels_count",
    "ovary with concave stigma ring, locules c. 9",
    "9",
    "9",
    "mean",
    cnt,
    p = "p212",
    notes = loc
  ),
  s$row("fruit_type", "Berry globose, pinkish", "berry", "berry", p = "p212"),
  s$row("fruit_colour", "Berry globose, pinkish", "pinkish", "pink", p = "p212"),
  s$rng(
    "fruit_width",
    "1.5--2.2 cm diameter",
    "1.5",
    "2.2",
    "cm",
    p = "p212",
    notes = diam
  ),
  s$row(
    "seed_surface_texture",
    "Seeds spiny, globose, black",
    "spiny",
    "spiny",
    p = "p212"
  ),
  s$row("seed_shape", "Seeds spiny, globose, black", "globose", "globoid", p = "p212"),
  s$row("seed_colour", "Seeds spiny, globose, black", "black", "black", p = "p212"),
  s$un(
    "Peninsular Thailand (Setul), Peninsular Malaysia (Penang, Perak, Melaka, Selangor, Terengganu, Pahang, Johor), Singapore, Borneo (Sarawak and Kalimantan, apparently absent from Sabah) and New Guinea.",
    "distribution",
    sec = "Distribution",
    p = "p212"
  ),
  s$un(
    "Least Concern (Assessed by C.L. Lim).",
    "conservation status",
    sec = "Conservation status",
    p = "p212"
  ),
  s$un(
    "In freshwater or peat swamp forest in deep shade in stagnant or slow-flowing clear streams with fluctuating water levels where its leaves may be submerged in water to c. 65 cm deep or in dry periods lie on the water-logged mud. It sometimes grows together with Cryptocoryne (Araceae) species.",
    "habitat; light; water depth (c. 65 cm here, c. 30 cm in Taxonomy p. 213)",
    sec = "Ecology",
    p = "p212"
  ),
  s$row(
    "flowering_time",
    "In Peninsular Malaysia, it has been collected in flower in February and March and again in November and December",
    "nyynnnnnnnyy",
    "nyynnnnnnnyy",
    sec = "Ecology",
    p = "p212",
    notes = "Months of flowering collections as printed."
  ),
  s$un(
    "suggesting it flowers seasonally",
    "flowering | seasonality (hedged 'suggesting'; not scored)",
    sec = "Ecology",
    p = "p212"
  ),
  s$row(
    "breeding_system",
    "Exclusion experiments also show that B. motleyi flowers are self pollinating (Williamson & Schneider, 1994).",
    "self pollinating",
    "autogamy",
    sec = GE,
    region = "",
    p = "p209",
    source_id = WS,
    notes = "From the genus Ecology paragraph; region of the study not stated."
  ),
  s$row(
    "flower_scent_production",
    "Barclaya motleyi produces a strong stink at night (Abdullah Piee, pers. comm.)",
    "strong stink",
    "scent_produced",
    sec = GE,
    p = "p209",
    notes = "Personal communication cited in the flora."
  ),
  s$un(
    "suggesting beetle pollination for that species. Flower structure also indicates beetle pollination because the apical corolla lobes do not open widely and so would restrict access for flies.",
    "pollination | beetle (hedged and inferred from structure; not scored)",
    sec = GE,
    p = "p209"
  ),
  s$un(
    "In B. motleyi seeds germinate within 48--72 hours on wet mud but do not germinate when submerged (Lok et al., Nature in Singapore 2 (2009) 237)",
    "seed | germination time and conditions (cited study, place not stated, reference not verified; not scored)",
    sec = GE,
    p = "p209"
  ),
  s$un(
    "Barclaya motleyi is distinct from B. rotundifolia in being truly aquatic, its leaves thriving underwater to a depth of c. 30 cm. ... Its leaves are very variable in both size and shape. ... var. kunstleri ... is here reduced to synonomy. The only Barclaya found in Sumatra ... laminas are uniformly smaller (3.5--6.5 x 4--9 cm) ...",
    "taxonomy; water depth; leaf variation; Sumatran and type-sheet leaf sizes (outside the flora region; not scored)",
    sec = "Taxonomy",
    p = "p213"
  )
)

# 3. Barclaya rotundifolia
s <- mk("Barclaya rotundifolia", "species", region = PM)
b_rotundifolia <- bind_rows(
  s$row(
    "life_history",
    "Robust, perennial marsh plant",
    "perennial",
    "perennial",
    p = "p214"
  ),
  s$row(
    "plant_growth_substrate",
    "Robust, perennial marsh plant",
    "marsh plant",
    "semiaquatic",
    p = "p214",
    notes = "Marsh plant scored as semiaquatic."
  ),
  s$row(
    "stem_growth_habit",
    "with branching rhizome, 1--2 cm thick",
    "rhizome",
    "rhizomatous",
    p = "p214"
  ),
  s$un(
    "Robust ... branching rhizome, 1--2 cm thick",
    "plant robustness; rhizome branching, thickness",
    p = "p214"
  ),
  s$un(
    "Indumentum tawny tomentose comprised of long, multicelluar uniseriate hairs densely covering petioles, lower surface of veins, pedicel and sparsely on outer surface of sepals.",
    "indumentum | type, distribution",
    p = "p214"
  ),
  s$row(
    "leaf_arrangement",
    "Leaves in a spreading rosette above the water surface",
    "rosette",
    "rosette",
    p = "p214"
  ),
  s$un(
    "above the water surface",
    "leaf | position in water (candidate trait)",
    p = "p214"
  ),
  s$ext(
    "petiole_length",
    "petioles stout, (6.5--)10--20(--28) cm long",
    a = "6.5",
    b = "10",
    c = "20",
    d = "28",
    unit = "cm",
    p = "p214"
  ),
  s$tri(
    "petiole_width",
    "in life fleshy, (3--)6(--9) mm diameter when dry",
    lo = "3",
    mid = "6",
    hi = "9",
    unit = "mm",
    p = "p214",
    notes = dry
  ),
  s$un("petioles stout ... in life fleshy", "petiole | stoutness, fleshy", p = "p214"),
  s$row(
    "leaf_surface_colour",
    "lamina dark green, glossy, glabrous and slightly bullate above",
    "dark_green",
    "dark_green",
    em = "upper surface",
    p = "p214"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "lamina dark green, glossy, glabrous and slightly bullate above",
    "glossy",
    "shiny",
    em = "upper surface",
    p = "p214"
  ),
  s$row(
    "leaf_hairs_adult_leaves",
    "lamina dark green, glossy, glabrous and slightly bullate above",
    "glabrous",
    "glabrous",
    em = "upper surface",
    p = "p214"
  ),
  s$un("slightly bullate above", "lamina | bullate surface", p = "p214"),
  s$row(
    "leaf_surface_colour",
    "beneath pale green or pale brown",
    "pale_green pale_brown",
    "pale_green brown",
    em = "lower surface",
    p = "p214"
  ),
  s$un("thick", "lamina | thickness (descriptive)", p = "p214"),
  s$row(
    "leaf_shape",
    "orbicular usually wider than long",
    "orbicular",
    "orbicular",
    p = "p214"
  ),
  s$ext(
    "leaf_length",
    "(6--)8--10(--17) x 8--17.5 cm",
    a = "6",
    b = "8",
    c = "10",
    d = "17",
    unit = "cm",
    p = "p214"
  ),
  s$rng("leaf_width", "(6--)8--10(--17) x 8--17.5 cm", "8", "17.5", "cm", p = "p214"),
  s$row("leaf_base_shape", "base deeply cordate", "cordate", "cordate", p = "p214"),
  s$un(
    "basal lobes rounded, to 4.5 cm long, sometimes overlapping",
    "lamina | basal lobe shape, length, overlap",
    p = "p214"
  ),
  s$row("leaf_margin", "margin entire", "entire", "entire", p = "p214"),
  s$row("leaf_apex_shape", "apex rounded", "rounded", "rounded", p = "p214"),
  s$un(
    "lateral veins very prominent beneath, 6--8 pairs, lowest two pairs touching at base of midrib; intercostal venation prominent",
    "veins | relief, number, arrangement",
    p = "p214"
  ),
  s$un(
    "pedicel (3.5--)7--9.5(--13) cm long, brownish, shorter than petioles",
    "pedicel | length (candidate pedicel_length), colour",
    p = "p214"
  ),
  s$rng(
    "flower_sepal_length",
    "sepals (4--)5, pale green to brownish purple, pink towards base, oblong-lanceolate, 30--35 x 3--8 mm",
    "30",
    "35",
    "mm",
    p = "p214"
  ),
  s$un(
    "sepals (4--)5, pale green to brownish purple, pink towards base, oblong-lanceolate, ... x 3--8 mm, outside slightly keeled, keel prolonged into an apiculus 2--4 mm long",
    "sepals | number, colour, shape, width (candidate flower_sepal_width), keel",
    p = "p214"
  ),
  s$row(
    "flower_colour",
    "corolla tube deep violet-pink, dark red, pinkish maroon or brownish",
    "deep_violet-pink dark_red pinkish_maroon brownish",
    "pink red_brown",
    p = "p214",
    notes = "Corolla tube colour; pinkish maroon and brownish scored as red_brown."
  ),
  s$rng(
    "flower_tube_length",
    "corolla tube ... 9--14 mm long",
    "9",
    "14",
    "mm",
    p = "p214"
  ),
  s$rng(
    "flower_petal_length",
    "lobes 12--15, 9--15 x 3--5 mm",
    "9",
    "15",
    "mm",
    p = "p214",
    notes = lobe
  ),
  s$un(
    "lobes 12--15 ... three outermost narrow and oblong, middle four shorter, and eight innermost rounded and cucullate",
    "corolla lobes | number, shape by series",
    p = "p214"
  ),
  s$row(
    "flower_anther_colour",
    "stamens with white oblong anthers",
    "white",
    "white",
    p = "p214"
  ),
  s$row(
    "flower_anther_length",
    "white oblong anthers to 1.5 mm long",
    "1.5",
    "1.5",
    "maximum",
    "mm",
    p = "p214"
  ),
  s$un(
    "ovary dirty light orange, stigmas in a ring of about 9",
    "ovary | colour; stigma number",
    p = "p214"
  ),
  s$row(
    "flower_stigma_length",
    "stigmas in a ring of about 9, c. 1.3 mm long",
    "1.3",
    "1.3",
    "mean",
    "mm",
    p = "p214"
  ),
  s$row("fruit_type", "Berry globose", "berry", "berry", p = "p214"),
  s$rng("fruit_length", "Berry globose, 12--16 mm long", "12", "16", "mm", p = "p214"),
  s$row("fruit_colour", "light green", "light_green", "green", p = "p214"),
  s$un(
    "Berry globose ... crowned by remnant of persistent corolla tube",
    "fruit | shape, persistent corolla",
    p = "p214"
  ),
  s$row("seed_surface_texture", "Seeds spiny, ellipsoid", "spiny", "spiny", p = "p214"),
  s$row("seed_shape", "Seeds spiny, ellipsoid", "ellipsoid", "ellipsoid", p = "p214"),
  s$row(
    "seed_length",
    "c. 2 mm long",
    "2",
    "2",
    "mean",
    "mm",
    p = "p214",
    notes = "Genus description gives seeds c. 1 mm diameter (p. 209)."
  ),
  s$un(
    "Peninsular Malaysia (Kelantan, Terengganu, Pahang and Johor) and Borneo (Brunei, Kalimantan, Sabah and Sarawak).",
    "distribution",
    sec = "Distribution",
    p = "p214"
  ),
  s$un(
    "Least Concern (Assessed by C.L. Lim).",
    "conservation status",
    sec = "Conservation status",
    p = "p214"
  ),
  s$un(
    "On waterlogged fine clay substrate at the edge of freshwater swamp, shallow margins of slow flowing streams or in muddy hollows.",
    "habitat; substrate",
    sec = "Ecology",
    p = "p214"
  ),
  s$row(
    "flowering_time",
    "In Peninsular Malaysia, it has been collected in flower throughout the year",
    "yyyyyyyyyyyy",
    "yyyyyyyyyyyy",
    sec = "Ecology",
    p = "p214",
    notes = "'collected in flower throughout the year'."
  ),
  s$un(
    "but peak flowering appears to occur in February to April and again in August",
    "flowering | peak months (hedged 'appears'; not scored)",
    sec = "Ecology",
    p = "p214"
  ),
  s$un(
    "this species is a marsh plant and not truly aquatic ... Nursery experiments showed that ... B. rotundifolia when submerged produced no new leaves and the old ones gradually rotted away resulting in the death of the plant ... It can therefore withstand only limited periods of submersion. ... B. rotundifolia forming gregarious colonies around the margin of the swamp forest",
    "submergence tolerance; gregarious colonies",
    sec = "Ecology",
    p = "p214"
  ),
  s$un(
    "Williamson & Schneider (1994) suggested flies as a possible pollinator for B. rotundifolia based on observations of flies in the vicinity of the flowers and drowned in fluid in the stigmatic cup.",
    "pollination | flies (hedged 'suggested ... possible'; not scored)",
    sec = GE,
    region = "",
    p = "p209",
    source_id = WS
  ),
  s$un(
    "They reported this species blooms for three days, the flowers opening in the morning ... and closing at dusk.",
    "flower | blooming period of three days (flower or plant unclear); anthesis time",
    sec = GE,
    region = "",
    p = "p209",
    source_id = WS
  ),
  s$row(
    "flower_scent_production",
    "the flowers opening in the morning when they produce a pungent fermenting odour ... Its flowers have also been reported as smelling of rotten meat (Poore 6415).",
    "pungent fermenting odour",
    "scent_produced",
    sec = GE,
    region = "",
    p = "p209",
    source_id = WS,
    notes = "Williamson & Schneider (1994), region not stated; rotten-meat smell from a specimen label (Poore 6415)."
  ),
  s$un(
    "In Borneo, plants do not reach such a large size as those in Peninsular Malaysia, their petioles measuring 6--16.5 cm, lamina 5--9(12) x (5--)7--10 cm, pedicel 5.5--7 cm, and sepals 27--33 x 4--5 mm.",
    "Bornean sizes (outside the flora region; not scored)",
    sec = "Taxonomy",
    region = "Borneo",
    p = "p214"
  ),
  s$un(
    "Although the type specimen was not distributed to SAR ... Ridley (1922) included the Kuala Teku, Pahang, population ... in B. motleyi.",
    "taxonomy",
    sec = "Taxonomy",
    p = "p214"
  )
)

# ---- 2. Nymphaea -----------------------------------------------------------
g <- mk("Nymphaea", "genus", "genus", sec = "Genus description")
nymphaea_g <- bind_rows(
  g$row(
    "plant_growth_form",
    "Herbaceous, rhizomatous perennials",
    "herbaceous",
    "herb",
    p = "p216"
  ),
  g$row(
    "life_history",
    "Herbaceous, rhizomatous perennials",
    "perennials",
    "perennial",
    p = "p216"
  ),
  g$row(
    "stem_growth_habit",
    "Herbaceous, rhizomatous perennials",
    "rhizomatous",
    "rhizomatous",
    p = "p216"
  ),
  g$un(
    "Leaves mostly floating on the water surface",
    "leaf | position in water (candidate trait)",
    p = "p216"
  ),
  g$row(
    "leaf_shape",
    "lamina peltate, orbicular to sagittate",
    "peltate orbicular",
    "peltate orbicular",
    p = "p216",
    notes = "'sagittate' has no leaf_shape level and is not scored."
  ),
  g$un(
    "usually with a deep sinus on one side, usually glabrous above",
    "lamina | sinus; hairs above",
    p = "p216"
  ),
  g$row(
    "leaf_margin",
    "margin entire, sinuate or dentate",
    "entire dentate",
    "entire toothed_dentate",
    p = "p216",
    notes = "'sinuate' has no leaf_margin level and is not scored."
  ),
  g$un(
    "Flowers perigynous",
    "flower | perigynous (no ovary-position level)",
    p = "p216"
  ),
  g$un("sepals free, (3--)4(--5)", "sepals | fusion, number", p = "p216"),
  g$row(
    "flower_colour",
    "petals yellow, pink, red, blue or white",
    "yellow pink red blue white",
    "yellow_orange pink red_brown blue_purple white_cream",
    p = "p216"
  ),
  g$un("free, (6--)8--40(--50)", "petals | fusion, number", p = "p216"),
  g$row(
    "flower_fertile_stamens_count",
    "stamens 10 to several hundred",
    "10",
    "10",
    "minimum",
    cnt,
    p = "p216"
  ),
  g$un(
    "outermost petaloid, innermost with filaments, erect and not attached to the corolla",
    "stamens | petaloid outer stamens, filaments, attachment",
    p = "p216"
  ),
  g$un(
    "ovary syncarpus at base, apocarpous to syncarpous apically",
    "ovary | carpel fusion",
    p = "p216"
  ),
  g$ext(
    "flower_structural_carpels_count",
    "carpels (5--)8--35",
    a = "5",
    b = "8",
    c = "35",
    unit = cnt,
    p = "p216"
  ),
  g$row(
    "flower_style_differentiation",
    "style absent",
    "absent",
    "absent",
    p = "p216"
  ),
  g$un(
    "carpellary appendages encircling the stigmatic cup, ovules numerous",
    "carpellary appendages; ovule number",
    p = "p216"
  ),
  g$row("fruit_type", "Berry globular to ellipsoid", "berry", "berry", p = "p216"),
  g$un(
    "Berry globular to ellipsoid, disintegrating irregularly",
    "fruit | shape; dehiscence ('disintegrating irregularly')",
    p = "p216"
  ),
  g$row("seed_shape", "Seeds numerous, ovoid", "ovoid", "ovoid", p = "p216"),
  g$un(
    "testa smooth, papillose or hairy",
    "seed | testa surface (three alternatives)",
    p = "p216"
  ),
  g$row("dispersal_appendage", "arillate", "arillate", "aril", p = "p216"),
  g$un(
    "Seeds numerous ... embedded in mucilage, endosperm starchy",
    "seed | number, mucilage, endosperm",
    p = "p216"
  ),
  g$un(
    "Cosmopolitan, c. 40 species. In Peninsular Malaysia represented by 2 species, one (N. pubescens) perhaps of cultivated origin.",
    "distribution; native status (hedged)",
    sec = "Genus Distribution",
    p = "p216"
  ),
  g$un(
    "Open areas, fully exposed to sunlight",
    "habitat; light",
    sec = "Genus Ecology",
    p = "p216"
  ),
  g$row(
    "flowering_time",
    "in Malaysia flowering throughout the year",
    "yyyyyyyyyyyy",
    "yyyyyyyyyyyy",
    sec = "Genus Ecology",
    region = "Malaysia",
    p = "p216",
    notes = "'flowering throughout the year'."
  ),
  g$row(
    "flower_dichogamy",
    "the flowers are protogynous",
    "protogynous",
    "protogynous",
    sec = "Genus Ecology",
    p = "p216"
  ),
  g$rng(
    "flower_lifespan",
    "the flowers are protogynous and last for 2--3 days",
    "2",
    "3",
    "d",
    sec = "Genus Ecology",
    p = "p216"
  ),
  g$un(
    "The fruits develop underwater often held down by a coiled pedicel.",
    "fruit | development under water; coiled pedicel",
    sec = "Genus Ecology",
    p = "p216"
  ),
  g$row(
    "dispersal_syndrome",
    "The seeds are dispersed by water, the fleshy, sac-like aril is air-filled so the seed floats (Mabberley, 2008).",
    "dispersed by water",
    "hydrochory",
    sec = "Genus Ecology",
    p = "p216",
    source_id = "Mabberley_2008"
  ),
  g$un(
    "Many waterlily species and hybrids are cultivated as ornamental plants. ...",
    "uses; cultivated ornamental species not in the treatment",
    sec = "Genus Uses",
    p = "p216"
  )
)

# 1. Nymphaea nouchali
s <- mk("Nymphaea nouchali", "species", region = PM)
n_nouchali <- bind_rows(
  s$row(
    "leaf_hairs_adult_leaves",
    "Leaves glabrous",
    "glabrous",
    "glabrous",
    p = "p217"
  ),
  s$rng("petiole_length", "petioles greenish brown, 17--50 cm long", "17", "50", "cm", p = "p217"),
  s$rng(
    "petiole_width",
    "2--3 mm diameter",
    "2",
    "3",
    "mm",
    p = "p217",
    notes = diam
  ),
  s$un("petioles greenish brown", "petiole | colour", p = "p217"),
  s$row(
    "leaf_shape",
    "lamina broadly elliptic to circular",
    "broadly_elliptic circular",
    "widely_elliptical orbicular",
    p = "p217"
  ),
  s$row(
    "leaf_surface_colour",
    "glossy green above",
    "green",
    "green",
    em = "upper surface",
    p = "p217"
  ),
  s$row(
    "leaf_surface_reflectivity",
    "glossy green above",
    "glossy",
    "shiny",
    em = "upper surface",
    p = "p217"
  ),
  s$un("sometimes with brownish blotches", "lamina | blotches", p = "p217"),
  s$row(
    "leaf_surface_colour",
    "purplish beneath",
    "purplish",
    "purple",
    em = "lower surface",
    p = "p217"
  ),
  s$ext(
    "leaf_length",
    "(5--)13--24(--45) x (4.5--)8--20(--32) cm",
    a = "5",
    b = "13",
    c = "24",
    d = "45",
    unit = "cm",
    p = "p217"
  ),
  s$ext(
    "leaf_width",
    "(5--)13--24(--45) x (4.5--)8--20(--32) cm",
    a = "4.5",
    b = "8",
    c = "20",
    d = "32",
    unit = "cm",
    p = "p217"
  ),
  s$row("leaf_base_shape", "base deeply cordate", "cordate", "cordate", p = "p217"),
  s$un("sinus open", "lamina | sinus", p = "p217"),
  s$row(
    "leaf_margin",
    "margin entire or slightly sinuate",
    "entire",
    "entire",
    p = "p217",
    notes = "'slightly sinuate' has no leaf_margin level and is not scored."
  ),
  s$un(
    "midrib thick, lateral veins 4--5 pairs, brown, but not prominent beneath",
    "veins | midrib, number, colour, relief",
    p = "p217"
  ),
  s$un(
    "Flowers held c. 30 cm above the water surface",
    "flower | height above water",
    p = "p217"
  ),
  s$ext(
    "flower_diameter",
    "opening 5--8(--12) cm across",
    b = "5",
    c = "8",
    d = "12",
    unit = "cm",
    p = "p217"
  ),
  s$un(
    "pedicels reddish brown 11--29 cm long",
    "pedicel | colour, length (candidate pedicel_length)",
    p = "p217"
  ),
  s$tri(
    "flower_sepal_length",
    "sepals 4, pale brown or green with a pink margin, lanceolate, (17--)26(--44) x 7(--14) mm",
    lo = "17",
    mid = "26",
    hi = "44",
    unit = "mm",
    p = "p217"
  ),
  s$un(
    "sepals 4, pale brown or green with a pink margin, lanceolate, ... x 7(--14) mm, joined at base for 6 mm, narrowed to a point",
    "sepals | number, colour, shape, width (candidate flower_sepal_width), fusion, apex",
    p = "p217"
  ),
  s$row(
    "flower_colour",
    "petals shorter than sepals, violet-blue, purple with bluish tinge, pale blue, rarely white (elsewhere rosy red)",
    "violet-blue purple pale_blue",
    "blue_purple",
    q = "usually",
    p = "p217"
  ),
  s$row(
    "flower_colour",
    "rarely white",
    "white",
    "white_cream",
    q = "rarely",
    p = "p217"
  ),
  s$un(
    "(elsewhere rosy red)",
    "flower | colour outside the flora region (not scored)",
    region = "",
    p = "p217"
  ),
  s$un("7--18, elliptic", "petals | number, shape", p = "p217"),
  s$ext(
    "flower_petal_length",
    "16--25(--40) x 4--11 mm",
    b = "16",
    c = "25",
    d = "40",
    unit = "mm",
    p = "p217"
  ),
  s$un("petals ... x 4--11 mm, apex acute", "petals | width, apex", p = "p217"),
  s$rng(
    "flower_fertile_stamens_count",
    "stamens 15--50",
    "15",
    "50",
    cnt,
    p = "p217"
  ),
  s$row(
    "flower_filament_length",
    "filaments broad, yellow c. 4 x 1.5 mm",
    "4",
    "4",
    "mean",
    "mm",
    p = "p217"
  ),
  s$un("filaments broad, yellow ... x 1.5 mm", "filaments | width, colour", p = "p217"),
  s$row(
    "flower_anther_colour",
    "anthers pale yellow to dirty white",
    "pale_yellow dirty_white",
    "yellow white",
    p = "p217"
  ),
  s$rng(
    "flower_anther_length",
    "anthers pale yellow to dirty white, 3--15 x c. 0.75 mm",
    "3",
    "15",
    "mm",
    p = "p217"
  ),
  s$un(
    "x c. 0.75 mm with a deep purple, pointed sterile appendage 1.5 mm long",
    "anthers | width, sterile appendage",
    p = "p217"
  ),
  s$rng(
    "flower_structural_carpels_count",
    "ovary with 10--20 locules",
    "10",
    "20",
    cnt,
    p = "p217",
    notes = loc
  ),
  s$un("stigma golden yellow", "stigma | colour", p = "p217"),
  s$row("fruit_type", "Berry globose to ovoid", "berry", "berry", p = "p217"),
  s$un("Berry globose to ovoid", "fruit | shape", p = "p217"),
  s$rng("fruit_length", "7--17 mm long", "7", "17", "mm", p = "p217"),
  s$rng("fruit_width", "8--25 mm diameter", "8", "25", "mm", p = "p217", notes = diam),
  s$row("seed_colour", "Seeds green, broadly ellipsoid", "green", "green", p = "p217"),
  s$row(
    "seed_shape",
    "Seeds green, broadly ellipsoid",
    "broadly_ellipsoid",
    "ellipsoid",
    p = "p217"
  ),
  s$rng("seed_length", "0.5--1 x 0.4--1 mm", "0.5", "1", "mm", p = "p217"),
  s$rng("seed_width", "0.5--1 x 0.4--1 mm", "0.4", "1", "mm", p = "p217"),
  s$un(
    "Africa, Asia to Australia; native in S and E Asia and W Malesia ... In Peninsular Malaysia, it has a scattered distribution in many places being associated with settlements making it difficult to distinguish wild populations from those of cultivated origin.",
    "distribution; native status",
    sec = "Distribution",
    p = "p217"
  ),
  s$un(
    "Least Concern (Assessed by C.L. Lim).",
    "conservation status",
    sec = "Conservation status",
    p = "p217"
  ),
  s$un(
    "In the lowlands, in ditches, margins of streams and formerly in paddy fields, in shallow, static or slow-flowing water (elsewhere growing in water 1--2 m deep). It is a recent introduction in Tasik Bera, Pahang ...",
    "habitat; altitude; water depth (elsewhere, not scored)",
    sec = "Ecology",
    p = "p217"
  ),
  s$row(
    "flower_scent_production",
    "The day-blooming flowers are slightly fragrant",
    "slightly fragrant",
    "scent_produced",
    sec = "Ecology",
    p = "p217"
  ),
  s$un(
    "The day-blooming flowers ... expanding in the morning and closing at night.",
    "flower | anthesis time",
    sec = "Ecology",
    p = "p217"
  ),
  s$row(
    "flower_visitor",
    "Cultivated plants attract Trigona bees in the morning",
    "Trigona bees",
    "stingless_bee",
    sec = "Ecology",
    p = "p218",
    notes = "Observed on cultivated plants; 'suggesting bee pollination' is hedged and not scored."
  ),
  s$un(
    "suggesting bee pollination",
    "pollination | bee (hedged 'suggesting'; not scored)",
    sec = "Ecology",
    p = "p218"
  ),
  s$un(
    "The great range in flower size depends on edaphic factors, such as water depth and nutrient availability.",
    "flower size | plasticity",
    sec = "Ecology",
    p = "p218"
  ),
  s$un(
    "A common ornamental waterlily in Asia. ... In Sri Lanka the flower stalks are eaten as a vegetable ...",
    "uses",
    sec = "Uses",
    region = "",
    p = "p218"
  ),
  s$un(
    "Plants with violet-blue flowers, which include those native in Peninsular Malaysia, have been named var. cyanea ... while those with white or pink flowers as var. versicolor ... they are best treated as forms.",
    "taxonomy | colour forms (no named form adopted)",
    sec = "Taxonomy",
    p = "p218"
  )
)

# 2. Nymphaea pubescens
s <- mk("Nymphaea pubescens", "species", region = PM)
n_pubescens <- bind_rows(
  s$un("petioles pubescent, greenish brown", "petiole | hairs, colour", p = "p219"),
  s$row(
    "petiole_length",
    "to 200 cm long",
    "200",
    "200",
    "maximum",
    "cm",
    p = "p219"
  ),
  s$rng("petiole_width", "2--8 mm diameter", "2", "8", "mm", p = "p219", notes = diam),
  s$row(
    "leaf_shape",
    "lamina elliptic to broadly elliptic to slightly peltate",
    "elliptic broadly_elliptic",
    "elliptical widely_elliptical",
    p = "p219"
  ),
  s$un(
    "to slightly peltate",
    "lamina | 'slightly peltate' (petiole attachment, qualified)",
    p = "p219"
  ),
  s$row(
    "leaf_surface_colour",
    "green above",
    "green",
    "green",
    em = "upper surface",
    p = "p219"
  ),
  s$row(
    "leaf_surface_colour",
    "purplish beneath",
    "purplish",
    "purple",
    em = "lower surface",
    p = "p219"
  ),
  s$rng("leaf_length", "12--50 x 8--28 cm", "12", "50", "cm", p = "p219"),
  s$rng("leaf_width", "12--50 x 8--28 cm", "8", "28", "cm", p = "p219"),
  s$row("leaf_base_shape", "base deeply cordate", "cordate", "cordate", p = "p219"),
  s$un("sinus narrow", "lamina | sinus", p = "p219"),
  s$row(
    "leaf_margin",
    "margin coarsely dentate",
    "dentate",
    "toothed_dentate",
    p = "p219"
  ),
  s$un(
    "teeth to 1--10 mm long and 6--22 mm apart",
    "leaf margin | tooth size, spacing",
    p = "p219"
  ),
  s$un(
    "midrib thick; lateral veins 5--8 pairs, prominent and densely yellow or brown tomentose beneath",
    "veins | number, relief, hairs",
    p = "p219"
  ),
  s$un(
    "Flowers held slightly above the water",
    "flower | height above water",
    p = "p219"
  ),
  s$rng("flower_diameter", "opening 5--15 cm across", "5", "15", "cm", p = "p219"),
  s$rng(
    "flower_sepal_length",
    "sepals 4, green, puberulous, elliptic, 30--45 x 13--25 mm",
    "30",
    "45",
    "mm",
    p = "p219"
  ),
  s$un(
    "sepals 4, green, puberulous, elliptic, ... x 13--25 mm, apex blunt",
    "sepals | number, colour, hairs, shape, width (candidate flower_sepal_width), apex",
    p = "p219"
  ),
  s$row(
    "flower_colour",
    "petals slightly shorter than sepals, white, sometimes tinged pink",
    "white",
    "white_cream",
    q = "usually",
    p = "p219"
  ),
  s$row(
    "flower_colour",
    "sometimes tinged pink",
    "tinged pink",
    "pink",
    q = "sometimes",
    p = "p219"
  ),
  s$un("12--30, elliptic", "petals | number, shape", p = "p219"),
  s$rng("flower_petal_length", "20--40 x 6--15 mm", "20", "40", "mm", p = "p219"),
  s$un("petals ... x 6--15 mm, apex obtuse", "petals | width, apex", p = "p219"),
  s$un("stamens yellow", "stamens | colour", p = "p219"),
  s$rng("flower_fertile_stamens_count", "stamens yellow, 38--46", "38", "46", cnt, p = "p219"),
  s$un("filaments narrowly triangular", "filaments | shape", p = "p219"),
  s$rng(
    "flower_stamen_length",
    "outer stamens 35--40 mm long",
    "35",
    "40",
    "mm",
    em = "outer stamens",
    p = "p219"
  ),
  s$un(
    "inner ones 6--17 x 1--9 mm",
    "inner stamens or filaments | size (referent unclear)",
    p = "p219"
  ),
  s$rng(
    "flower_anther_length",
    "anthers 5--15 mm long",
    "5",
    "15",
    "mm",
    p = "p219"
  ),
  s$un(
    "with a short appendage less 1 mm long",
    "anthers | appendage",
    p = "p219"
  ),
  s$rng(
    "flower_structural_carpels_count",
    "ovary with 13--16 locules",
    "13",
    "16",
    cnt,
    p = "p219",
    notes = loc
  ),
  s$un("stigma yellow", "stigma | colour", p = "p219"),
  s$row("fruit_type", "Berry depressed globose", "berry", "berry", p = "p219"),
  s$un("Berry depressed globose", "fruit | shape", p = "p219"),
  s$rng("fruit_length", "20--37 mm long", "20", "37", "mm", p = "p219"),
  s$rng("fruit_width", "30--55 mm diameter", "30", "55", "mm", p = "p219", notes = diam),
  s$row("seed_colour", "Seeds green, broadly ellipsoid", "green", "green", p = "p219"),
  s$row(
    "seed_shape",
    "Seeds green, broadly ellipsoid",
    "broadly_ellipsoid",
    "ellipsoid",
    p = "p219"
  ),
  s$row("seed_length", "c. 1.5 x 1.3 mm", "1.5", "1.5", "mean", "mm", p = "p219"),
  s$row("seed_width", "c. 1.5 x 1.3 mm", "1.3", "1.3", "mean", "mm", p = "p219"),
  s$un(
    "From Pakistan and Sri Lanka to China (Yunnan) and Taiwan, throughout Malesia to tropical NE Australia. ... In Peninsular Malaysia, it has been collected in Melaka, Penang and Terengganu, where the populations could be of cultivated origin, but it was collected in Tasik Chini, Sg. Pahang, in 1913 (Burn-Murdoch 308) suggesting that it is native there.",
    "distribution; native status (hedged)",
    sec = "Distribution",
    p = "p219"
  ),
  s$un(
    "Least Concern (Assessed by C.L. Lim).",
    "conservation status",
    sec = "Conservation status",
    p = "p219"
  ),
  s$un(
    "Lowlands, in pools, ditches and lake margins with shallow, stagnant or slow flowing water.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p219"
  ),
  s$row(
    "flower_scent_production",
    "Its moderately sweetly scented flowers",
    "moderately sweetly scented",
    "scent_produced",
    sec = "Ecology",
    p = "p219"
  ),
  s$un(
    "flowers open in the evening and close before the following afternoon",
    "flower | anthesis time (nocturnal)",
    sec = "Ecology",
    p = "p219"
  ),
  s$row(
    "flower_lifespan",
    "The flowers last for 3 days.",
    "3",
    "3",
    "mean",
    "d",
    sec = "Ecology",
    p = "p219"
  ),
  s$un(
    "The pollinator is unknown but is possibly beetles.",
    "pollination | beetle (hedged 'possibly'; not scored)",
    sec = "Ecology",
    p = "p219"
  ),
  s$un(
    "Cultivated as an ornamental for its fragrant white flowers.",
    "uses",
    sec = "Uses",
    p = "p219"
  )
)

# ---- Copy-down ----------------------------------------------------------------
# Family and genus values copied to member taxa silent on the trait. Every
# value below is a single, unqualified level; members that score the trait
# themselves all agree or are left out (checked by hand):
#   - family leaf_arrangement 'spiral' and Ecology plant_growth_substrate
#     'aquatic_rooted' are not copied: B. rotundifolia scores rosette and
#     semiaquatic.
#   - family flower_perianth_phyllotaxis and androecium phyllotaxis 'spiral'
#     are not copied: Barclaya petals are connate and stamens epipetalous.
#   - genus seed_shape is not copied (Barclaya two levels; Nymphaea 'ovoid'
#     disagrees with the species 'ellipsoid').
own <- bind_rows(b_longifolia, b_motleyi, b_rotundifolia, n_nouchali, n_pubescens)
higher <- bind_rows(family, barclaya_g, nymphaea_g)
bar <- c("Barclaya longifolia", "Barclaya motleyi", "Barclaya rotundifolia")
nym <- c("Nymphaea nouchali", "Nymphaea pubescens")
copy <- bind_rows(
  tibble(
    from = "Nymphaeaceae",
    trait = c(
      "leaf_compoundness",
      "flower_perianth_symmetry",
      "flower_structural_sex_type",
      "inflorescence_type",
      "flower_gynoecium_placentation"
    ),
    to = list(c(bar, nym))
  ),
  tribble(
    ~from, ~trait, ~to,
    "Barclaya", "plant_growth_form", "Barclaya rotundifolia",
    "Barclaya", "life_history", c("Barclaya longifolia", "Barclaya motleyi"),
    "Barclaya", "leaf_arrangement", c("Barclaya longifolia", "Barclaya motleyi"),
    "Barclaya", "fruit_dehiscence", c("Barclaya motleyi", "Barclaya rotundifolia"),
    "Barclaya", "vegetative_reproduction_ability", bar,
    "Nymphaea", "plant_growth_form", nym,
    "Nymphaea", "life_history", nym,
    "Nymphaea", "stem_growth_habit", nym,
    "Nymphaea", "flower_style_differentiation", nym,
    "Nymphaea", "dispersal_appendage", nym,
    "Nymphaea", "flowering_time", nym,
    "Nymphaea", "flower_dichogamy", nym,
    "Nymphaea", "dispersal_syndrome", nym
  )
) |>
  unnest(to)

copied <- copy |>
  inner_join(higher, by = c(from = "taxon_name", "trait"), relationship = "many-to-many") |>
  mutate(
    notes = str_squish(paste("Copied from the", from, "description.", notes)),
    taxon_rank = "species"
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

d <- bind_rows(higher, own, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Kiew_2015/raw/trait_data.csv", na = "")
write_csv(flora_unmapped_summary(d), "data/Kiew_2015/raw/unmapped_terms.csv", na = "")

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Nymphaeaceae", "family", "accepted", "",
  "Barclaya", "genus", "accepted", "",
  "Barclaya longifolia", "species", "accepted", "",
  "Barclaya motleyi", "species", "accepted", "Includes B. motleyi var. kunstleri and B. kunstleri, reduced to synonymy here.",
  "Barclaya rotundifolia", "species", "accepted", "",
  "Nymphaea", "genus", "accepted", "",
  "Nymphaea nouchali", "species", "accepted", "Colour varieties (var. cyanea, var. versicolor) treated as forms without names; no form context.",
  "Nymphaea pubescens", "species", "accepted", "Possibly of cultivated origin in Peninsular Malaysia (genus Distribution)."
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Kiew_2015/raw/taxon_flags.csv", na = "")
