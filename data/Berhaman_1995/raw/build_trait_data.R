# Hand extraction of Bignoniaceae (A. Berhaman 1995, Tree Flora of Sabah and
# Sarawak vol. 1: 33--44). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Bignoniaceae.pdf, the 2007 electronic version of
# the 1995 volume (L.G. Saw & B. Aslina). Text pages carry embedded fonts and
# no page images, so the text layer is what renders (checked against page
# renders); it is retyped text, and no scan of the 1995 print was available.
# Figure pages (pdf 8, 13, 14, 17 = printed 34, 39, 40, 43) are images; their
# captions are used for vouchers only. PDF page = printed page - 26.
#
# Small family (5 described taxa), so rows are written by hand with
# flora_hand() (Yao_2010 pattern). Source repairs are in raw/source_fixes.csv
# and are already applied to the verbatim clauses below. Higher-rank values are
# copied down by hand under the flora_copy_down() rules (unqualified, single
# level, every member that scores the trait agrees, most specific rank wins).
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(33:44, \(p) c(p - 26L, p)), paste0("p", 33:44))
SS <- "Sabah and Sarawak"

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
f <- mk("Bignoniaceae", "family", "family", sec = "Family description")
FE <- "Family Ecology"
family <- bind_rows(
  f$row(
    "plant_growth_form",
    "Trees, shrubs or woody climbers",
    "trees shrubs woody_climbers",
    "tree shrub climber_woody",
    p = "p33"
  ),
  f$un("twigs often lenticellate", "twigs | lenticels", p = "p33"),
  f$un("stipules absent", "stipules | absent (negative phrase)", p = "p33"),
  f$row(
    "leaf_phyllotaxis",
    "Leaves generally opposite and 1--2--4-times pinnate, or simple and whorled",
    "opposite whorled",
    "opposite whorled",
    p = "p33",
    notes = "'generally' qualifies the opposite, pinnate leaves; both states kept in one row."
  ),
  f$row(
    "leaf_compoundness",
    "Leaves generally opposite and 1--2--4-times pinnate, or simple and whorled",
    "pinnate simple",
    "compound simple",
    p = "p33",
    notes = "'generally' qualifies the opposite, pinnate leaves; both states kept in one row."
  ),
  f$un(
    "1--2--4-times pinnate",
    "leaf | degree of division (ambiguous range)",
    p = "p33"
  ),
  f$un(
    "often with glands on the lower surface of the leaflets; domatia sometimes present",
    "leaflet | glands, domatia",
    p = "p33"
  ),
  f$row(
    "inflorescence_type",
    "Inflorescences generally terminal, sometimes borne on the trunks and branches",
    "terminal",
    "terminal",
    q = "usually",
    p = "p33"
  ),
  f$un(
    "sometimes borne on the trunks and branches",
    "inflorescence | cauliflory, ramiflory",
    p = "p33"
  ),
  f$row(
    "flower_structural_sex_type",
    "Flowers bisexual, usually very showy",
    "bisexual",
    "bisexual",
    p = "p33"
  ),
  f$un("usually very showy", "flower | showiness", p = "p33"),
  f$un(
    "calyx tubular, subtruncate or with 5 lobes or teeth, closed in bud and later splitting into lobes, often glandular outside",
    "calyx | shape, lobes, glands",
    p = "p33"
  ),
  f$row(
    "flower_perianth_symmetry",
    "corolla in most genera bilaterally symmetrical, in some genera somewhat radially symmetrical",
    "bilaterally symmetrical",
    "zygomorphic",
    q = "usually",
    p = "p33",
    notes = "'in most genera' read as usually; 'somewhat radially symmetrical' is hedged and not scored."
  ),
  f$row(
    "flower_shape",
    "basally tubular, upper part expanded, generally campanulate or salver-shaped",
    "campanulate salver-shaped",
    "campanulate salverform",
    q = "usually",
    p = "p33"
  ),
  f$un(
    "with 5 lobes, 2 on the upper side, 3 on the lower",
    "corolla | lobe number and arrangement",
    p = "p33"
  ),
  f$row(
    "flower_fertile_stamens_count",
    "stamens 5, almost equal, or (mostly) 4 and didynamous (2 long and 2 short)",
    "4",
    "4",
    "mean",
    "{count}",
    q = "usually",
    p = "p33",
    notes = "'(mostly)' read as usually."
  ),
  f$row(
    "flower_fertile_stamens_count",
    "stamens 5, almost equal, or (mostly) 4 and didynamous (2 long and 2 short)",
    "5",
    "5",
    "mean",
    "{count}",
    p = "p33"
  ),
  f$un(
    "almost equal ... didynamous (2 long and 2 short), attached to the corolla (epipetalous)",
    "stamens | relative length, epipetalous",
    p = "p33"
  ),
  f$row(
    "flower_ovary_position",
    "ovary superior, 2-celled",
    "superior",
    "superior",
    p = "p33"
  ),
  f$row(
    "flower_structural_carpels_count",
    "ovary superior, 2-celled",
    "2",
    "2",
    "mean",
    "{count}",
    p = "p33",
    notes = "Ovary cells scored as carpels (one locule per carpel assumed)."
  ),
  f$un("style long, stigma 2-lobed", "style, stigma | length, lobes", p = "p33"),
  f$row(
    "flower_nectar_production",
    "nectary usually present and ring-like",
    "nectary present",
    "nectar_produced",
    q = "usually",
    p = "p33"
  ),
  f$un(
    "ovules several to many per cell, arranged longitudinally in 1--2 rows on the septum",
    "ovules | number per cell, arrangement",
    p = "p33"
  ),
  f$un(
    "Fruit a pod divided by a longitudinal partition into 2 compartments",
    "fruit | type 'pod', septum",
    p = "p33"
  ),
  f$row(
    "fruit_dehiscence",
    "Fruit a pod divided by a longitudinal partition into 2 compartments, splitting open when ripe",
    "splitting open",
    "dehiscent",
    p = "p33"
  ),
  f$un(
    "Seeds many, flat; embryo near the centre",
    "seed | number, shape, embryo position",
    p = "p33"
  ),
  f$row(
    "dispersal_appendage",
    "peripheral wing very thin and transparent or thickened and opaque",
    "wing",
    "wings",
    p = "p33"
  ),
  f$row(
    "seedling_germination_location",
    "germination epigeal",
    "epigeal",
    "epigeal",
    p = "p33"
  ),
  f$un(
    "About 120 genera and 650 species in the tropics and subtropics. In Sabah and Sarawak, 10 species in 5 genera including 5 apparently unnamed species (4 woody climbers and 1 tree). A number of exotic species have been introduced as ornamental plants: ...",
    "distribution and species counts; introduced ornamentals",
    sec = "Family Distribution",
    p = "p33"
  ),
  f$un(
    "Mostly evergreen but Oroxylum indicum and Dolichandrone spathacea have been noted as leafless for many months during the dry season of seasonal parts of Asia.",
    "leaf phenology | outside the flora region (seasonal Asia)",
    sec = FE,
    p = "p33"
  ),
  f$un(
    "Dolichandrone spathacea is confined to the mangroves, Deplanchea bancana is found in kerangas (heath) and peat swamp forest, some woody climber species appear restricted to limestones, and some Radermachera species may be restricted to ultramafic soils.",
    "habitat, substrate",
    sec = FE,
    p = "p33"
  ),
  f$un(
    "In many species, the flowers open at night, and some have fragrant white flowers with long tubes which are pollinated by moths.",
    "flower | anthesis time, moth pollination (unnamed species)",
    sec = FE,
    p = "p33"
  ),
  f$row(
    "dispersal_syndrome",
    "The winged seeds are dispersed by wind.",
    "dispersed by wind",
    "anemochory",
    sec = FE,
    p = "p33"
  ),
  f$un(
    "The species generally have soft and light wood, at present without commercial value. ...",
    "uses; wood softness",
    sec = "Family Uses",
    p = "p35"
  ),
  f$un(
    "The family is closely related to the Scrophulariaceae. ...",
    "taxonomy",
    sec = "Family Taxonomy",
    p = "p35"
  )
)

# ---- 1. Deplanchea ----------------------------------------------------------
g <- mk("Deplanchea", "genus", "genus", sec = "Genus description")
deplanchea_g <- bind_rows(
  g$row("plant_growth_form", "Small to medium-sized trees", "trees", "tree", p = "p36"),
  g$un("Bark often lenticellate. Sapwood soft.", "bark, sapwood | lenticels, softness", p = "p36"),
  g$row(
    "leaf_compoundness",
    "Leaves simple, in whorls",
    "simple",
    "simple",
    p = "p36"
  ),
  g$row(
    "leaf_phyllotaxis",
    "Leaves simple, in whorls",
    "in whorls",
    "whorled",
    p = "p36"
  ),
  g$row(
    "leaf_shape",
    "quite large, elliptic to obovate",
    "elliptic obovate",
    "elliptical obovate",
    p = "p36"
  ),
  g$un(
    "quite large ... chartaceous to coriaceous; base with two large saucer-shaped glands on upper surface",
    "leaf | size, texture, basal glands",
    p = "p36"
  ),
  g$row(
    "inflorescence_type",
    "Inflorescence terminal, a thyrse",
    "terminal thyrse",
    "terminal panicle",
    p = "p36",
    notes = "Thyrse scored as panicle."
  ),
  g$un(
    "a thyrse with a somewhat flat-topped structure, the branches many (more than 20) and held horizontally at almost right angles to the main inflorescence axis, large and conspicuous",
    "inflorescence | shape, branch number and angle",
    p = "p36"
  ),
  g$un("Flowers large; calyx 5-lobed", "flower, calyx | size, lobes", p = "p36"),
  g$row(
    "flower_perianth_symmetry",
    "corolla irregular (zygomorphic)",
    "zygomorphic",
    "zygomorphic",
    p = "p36"
  ),
  g$row(
    "flower_shape",
    "corolla irregular (zygomorphic), funnel-shaped",
    "funnel-shaped",
    "funnelform",
    p = "p36"
  ),
  g$un(
    "tube with a short constricted basal part and a flared upper part",
    "corolla tube | shape",
    p = "p36"
  ),
  g$row("flower_colour", "lobes 5, yellow", "yellow", "yellow_orange", p = "p36"),
  g$row(
    "flower_fertile_stamens_count",
    "stamens strongly exserted, 4 (rarely 5), didynamous",
    "4",
    "4",
    "mean",
    "{count}",
    q = "usually",
    p = "p36"
  ),
  g$row(
    "flower_fertile_stamens_count",
    "stamens strongly exserted, 4 (rarely 5), didynamous",
    "5",
    "5",
    "mean",
    "{count}",
    q = "rarely",
    p = "p36"
  ),
  g$un(
    "stamens strongly exserted ... didynamous; disc annular to lobed",
    "stamens, disc | exsertion, didynamy, disc shape",
    p = "p36"
  ),
  g$row(
    "flower_structural_carpels_count",
    "ovary 2-celled",
    "2",
    "2",
    "mean",
    "{count}",
    p = "p36",
    notes = "Ovary cells scored as carpels (one locule per carpel assumed)."
  ),
  g$un("style long exserted; stigma 2-lobed", "style, stigma | exsertion, lobes", p = "p36"),
  g$row(
    "fruit_type",
    "Fruit a 2-valved capsule",
    "capsule",
    "capsule",
    p = "p36"
  ),
  g$un("Fruit a 2-valved capsule", "fruit | valve number", p = "p36"),
  g$row(
    "dispersal_appendage",
    "Seeds many, thinly winged",
    "winged",
    "wings",
    p = "p36"
  ),
  g$un("Seeds many", "seed | number", p = "p36"),
  g$un(
    "5 species, Malesia to N Australia. 1 species in Borneo (Sabah, Sarawak, Brunei, and Kalimantan).",
    "distribution",
    sec = "Genus Distribution",
    p = "p36"
  ),
  g$un(
    "Being light-demanding, species of the genus are mainly found in secondary rain forests (including kerangas forest), woodland savannahs, and grasslands, from sea-level to 1000 m.",
    "light demand; habitat; altitude",
    sec = "Genus Ecology",
    p = "p36"
  )
)

s <- mk("Deplanchea bancana", "species", region = SS)
deplanchea_bancana <- bind_rows(
  s$row(
    "plant_growth_form",
    "Small to large trees 10--45 m tall",
    "trees",
    "tree",
    p = "p36"
  ),
  s$rng("plant_height", "Small to large trees 10--45 m tall", "10", "45", "m", p = "p36"),
  s$rng(
    "plant_diameter_breast_height",
    "25--70 cm diameter",
    "25",
    "70",
    "cm",
    p = "p36",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$row(
    "stem_base_form",
    "bole fluted at base, buttresses small or large",
    "fluted buttresses",
    "fluted buttressed",
    p = "p36"
  ),
  s$row(
    "bark_colour",
    "Bark white to brown, lenticellate and slightly fissured",
    "white brown",
    "white brown",
    p = "p36"
  ),
  s$row(
    "bark_texture",
    "Bark white to brown, lenticellate and slightly fissured",
    "fissured",
    "furrowed",
    p = "p36"
  ),
  s$un("lenticellate", "bark | lenticels", p = "p36"),
  s$un(
    "inner bark yellow to pale brown. Sapwood white to pale yellow, soft.",
    "inner bark, sapwood | colour, softness",
    p = "p36"
  ),
  s$un(
    "Young twigs lenticellate, glabrous to pale hairy, pale brown to yellow.",
    "young twigs | lenticels, hairs, colour",
    p = "p36"
  ),
  s$row(
    "leaf_shape",
    "Leaves obovate to elliptic, 9--34 x 5.5--20 cm",
    "obovate elliptic",
    "obovate elliptical",
    p = "p36"
  ),
  s$rng("leaf_length", "Leaves obovate to elliptic, 9--34 x 5.5--20 cm", "9", "34", "cm", p = "p36"),
  s$rng("leaf_width", "Leaves obovate to elliptic, 9--34 x 5.5--20 cm", "5.5", "20", "cm", p = "p36"),
  s$un("chartaceous to coriaceous", "leaf | texture", p = "p36"),
  s$row(
    "leaf_base_shape",
    "base cordate to cuneate",
    "cordate cuneate",
    "cordate cuneate",
    p = "p36"
  ),
  s$row(
    "leaf_apex_shape",
    "apex rounded, rarely acuminate-acute",
    "rounded",
    "rounded",
    q = "usually",
    p = "p36"
  ),
  s$un("rarely acuminate-acute", "leaf apex | 'acuminate-acute' (no single level)", p = "p36"),
  s$un(
    "midrib glabrous to yellow-hairy on lower surface; lateral veins 7--8 pairs, raised and glabrous to yellow-hairy on the lower surface",
    "midrib, veins | hairs, number, relief",
    p = "p36"
  ),
  s$rng(
    "petiole_length",
    "stalk 3--6 cm long, glabrous to yellow-hairy",
    "3",
    "6",
    "cm",
    p = "p36",
    notes = "Leaf 'stalk' read as petiole."
  ),
  s$un("stalk ... glabrous to yellow-hairy", "petiole | hairs", p = "p36"),
  s$rng(
    "peduncle_length",
    "Inflorescences showy, stalk 5--20 cm long",
    "5",
    "20",
    "cm",
    p = "p36",
    notes = "Inflorescence 'stalk' read as peduncle."
  ),
  s$un("Inflorescences showy ... Flowers showy", "inflorescence, flower | showiness", p = "p36"),
  s$un(
    "calyx 12--16 mm long, with scattered glands, teeth 5",
    "calyx | length (no calyx trait), glands, teeth",
    p = "p36"
  ),
  s$row("flower_colour", "corolla c. 3.5 x 2.5 cm, yellow, 5-lobed", "yellow", "yellow_orange", p = "p36"),
  s$un(
    "corolla c. 3.5 x 2.5 cm ... 5-lobed, the upper 2 lobes recurved",
    "corolla | length and width (candidate), lobes",
    p = "p36"
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens 4",
    "4",
    "4",
    "mean",
    "{count}",
    p = "p36"
  ),
  s$un(
    "filaments erect and curved over the upper corolla-lobes, base glandular hairy",
    "filaments | posture, hairs",
    p = "p36"
  ),
  s$un(
    "ovary subsessile, each locule with two placentas; style greenish yellow, exserted and curved like the stamens; stigma bilobed",
    "ovary, style, stigma | placentas, colour, exsertion, lobes",
    p = "p36"
  ),
  s$rng("fruit_length", "Fruits oblong, 10--14 x 3.5 cm", "10", "14", "cm", p = "p36"),
  s$row("fruit_width", "Fruits oblong, 10--14 x 3.5 cm", "3.5", "3.5", "mean", "cm", p = "p36"),
  s$un("Fruits oblong", "fruit | shape", p = "p36"),
  s$row(
    "seed_length",
    "Seeds flat, c. 3 x 2 cm; wing broad, very thinly transparent, irregularly lobed",
    "3",
    "3",
    "mean",
    "cm",
    p = "p36",
    notes = "Not stated whether the size includes the wing."
  ),
  s$row(
    "seed_width",
    "Seeds flat, c. 3 x 2 cm; wing broad, very thinly transparent, irregularly lobed",
    "2",
    "2",
    "mean",
    "cm",
    p = "p36",
    notes = "Not stated whether the size includes the wing."
  ),
  s$row(
    "dispersal_appendage",
    "wing broad, very thinly transparent, irregularly lobed",
    "wing",
    "wings",
    p = "p36"
  ),
  s$un("Seeds flat ... wing broad, very thinly transparent, irregularly lobed", "seed | shape; wing shape and texture", p = "p36"),
  s$un(
    "Sumatra, Peninsular Malaysia and Borneo. In Sabah and Sarawak uncommon.",
    "distribution; abundance",
    sec = "Distribution",
    p = "p36"
  ),
  s$un(
    "In Sabah and Sarawak, it occurs in heath forests on white sands or in peat swamps, in primary and secondary forests, from sea-level to 1000 m.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p36"
  )
)

# ---- 2. Dolichandrone -------------------------------------------------------
g <- mk("Dolichandrone", "genus", "genus", sec = "Genus description")
dolichandrone_g <- bind_rows(
  g$row("plant_growth_form", "Medium-sized trees", "trees", "tree", p = "p37"),
  g$row("leaf_compoundness", "Leaves once pinnate", "pinnate", "compound", p = "p37"),
  g$row(
    "leaf_lamina_division",
    "Leaves once pinnate",
    "once pinnate",
    "pinnately_compound",
    p = "p37"
  ),
  g$row(
    "leaf_shape",
    "leaflets elliptic, entire",
    "elliptic",
    "elliptical",
    em = "leaflet",
    p = "p37"
  ),
  g$row(
    "leaf_margin",
    "leaflets elliptic, entire",
    "entire",
    "entire",
    em = "leaflet",
    p = "p37"
  ),
  g$row(
    "inflorescence_type",
    "Inflorescence a terminal raceme",
    "terminal raceme",
    "terminal raceme",
    p = "p37"
  ),
  g$row(
    "flower_scent_production",
    "Flowers fragrant, nocturnal",
    "fragrant",
    "scent_produced",
    p = "p37"
  ),
  g$un("Flowers ... nocturnal", "flower | anthesis at night", p = "p37"),
  g$un("calyx spathaceous, somewhat curved", "calyx | shape", p = "p37"),
  g$row(
    "flower_shape",
    "corolla funnel-shaped, equally 5-lobed",
    "funnel-shaped",
    "funnelform",
    p = "p37"
  ),
  g$un("equally 5-lobed", "corolla | lobes (equal)", p = "p37"),
  g$row(
    "flower_fertile_stamens_count",
    "stamens 4, didynamous, included",
    "4",
    "4",
    "mean",
    "{count}",
    p = "p37"
  ),
  g$un(
    "didynamous, included; disc annular; ovary with many ovules in many rows; style slender",
    "stamens, disc, ovules, style",
    p = "p37"
  ),
  g$row(
    "fruit_type",
    "Fruit a subcylindric capsule",
    "capsule",
    "capsule",
    p = "p37"
  ),
  g$un("subcylindric", "fruit | shape", p = "p37"),
  g$row("dispersal_appendage", "Seeds hyaline-winged", "winged", "wings", p = "p37"),
  g$un(
    "9 species, from East Africa through tropical Asia to Australia; 4 species in tropical SE Asia; 1 species in Sabah and Sarawak.",
    "distribution",
    sec = "Genus Distribution",
    p = "p37"
  )
)

s <- mk("Dolichandrone spathacea", "species", region = SS)
dolichandrone_spathacea <- bind_rows(
  s$row("plant_growth_form", "Tree to 25 m tall", "tree", "tree", p = "p37"),
  s$row("plant_height", "Tree to 25 m tall", "25", "25", "maximum", "m", p = "p37"),
  s$rng(
    "plant_diameter_breast_height",
    "10--40 cm diameter",
    "10",
    "40",
    "cm",
    p = "p37",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$row("bark_colour", "Bark pale brown, smooth to fissured", "pale_brown", "brown_light", p = "p37"),
  s$row(
    "bark_texture",
    "Bark pale brown, smooth to fissured",
    "smooth fissured",
    "smooth furrowed",
    p = "p37"
  ),
  s$un(
    "inner bark laminated, reddish pink. Sapwood white, soft. Young twigs lenticellate.",
    "inner bark, sapwood, young twigs",
    p = "p37"
  ),
  s$row("leaf_compoundness", "Leaves once pinnate, opposite", "pinnate", "compound", p = "p37"),
  s$row(
    "leaf_lamina_division",
    "Leaves once pinnate, opposite",
    "once pinnate",
    "pinnately_compound",
    p = "p37"
  ),
  s$row("leaf_phyllotaxis", "Leaves once pinnate, opposite", "opposite", "opposite", p = "p37"),
  s$un("rachis smooth, brown", "rachis | surface, colour", p = "p37"),
  s$un(
    "leaflets usually 3--4 pairs with one terminal leaflet",
    "leaflets | number of pairs (usually 3--4) plus terminal leaflet",
    p = "p37"
  ),
  s$row(
    "leaf_shape",
    "leaflets ... ovate-oblong to lanceolate",
    "lanceolate",
    "lanceolate",
    em = "leaflet",
    p = "p37",
    notes = "'ovate-oblong' has no level and is kept unmapped."
  ),
  s$un("leaflets ... ovate-oblong", "leaflet | shape 'ovate-oblong' (no level)", p = "p37"),
  s$rng(
    "leaflet_length",
    "leaflets ... 5.5--15 x 2.5--6 cm",
    "5.5",
    "15",
    "cm",
    em = "leaflet",
    p = "p37"
  ),
  s$rng(
    "leaflet_width",
    "leaflets ... 5.5--15 x 2.5--6 cm",
    "2.5",
    "6",
    "cm",
    em = "leaflet",
    p = "p37"
  ),
  s$un("drying dark to brown-black", "leaflet | colour on drying", p = "p37"),
  s$row("leaf_base_shape", "base oblique", "oblique", "oblique", em = "leaflet", p = "p37"),
  s$row(
    "leaf_apex_shape",
    "apex acuminate pointed",
    "acuminate",
    "acuminate",
    em = "leaflet",
    p = "p37"
  ),
  s$un(
    "lateral veins 5--8 pairs, lower surface with hairy domatia; stalks short in lateral leaflets but 2--4 cm in terminal leaflet",
    "veins, domatia, petiolules | number, presence, length",
    p = "p37"
  ),
  s$un(
    "Inflorescences 2--8-flowered, with one flower opening at a time",
    "inflorescence | flowers per inflorescence, anthesis sequence",
    p = "p37"
  ),
  s$row("flower_scent_production", "Flowers fragrant", "fragrant", "scent_produced", p = "p37"),
  s$un(
    "calyx conical, closed and with hooked apex in bud, with scattered glands, splitting along one side",
    "calyx | shape, glands, splitting",
    p = "p37"
  ),
  s$row("flower_colour", "corolla white", "white", "white_cream", p = "p37"),
  s$row(
    "flower_tube_length",
    "corolla white, with a tube to 14.5 cm long",
    "14.5",
    "14.5",
    "maximum",
    "cm",
    p = "p37"
  ),
  s$un(
    "bell-shaped near the mouth, 3--4 cm wide, with many scattered glands outside, lobes 5 with frilled margin",
    "corolla | mouth shape and width, glands, lobes",
    p = "p37"
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens 4, didynamous with a fifth rudiment, included",
    "4",
    "4",
    "mean",
    "{count}",
    p = "p37"
  ),
  s$un(
    "didynamous with a fifth rudiment, included; style long, exserted, persistent in young fruit; anthers glabrous, bilocular",
    "stamens, staminode, style, anthers",
    p = "p37"
  ),
  s$rng("fruit_length", "Fruits 20--33.5 x 1.5--3 cm", "20", "33.5", "cm", p = "p37"),
  s$rng("fruit_width", "Fruits 20--33.5 x 1.5--3 cm", "1.5", "3", "cm", p = "p37"),
  s$un(
    "generally curved, hanging in clusters from the ends of the branches",
    "fruit | shape, position",
    p = "p37"
  ),
  s$row(
    "seed_colour",
    "Seeds many, dark grey, in many rows",
    "dark_grey",
    "grey",
    p = "p37"
  ),
  s$row(
    "seed_length",
    "c. 15 x 9 mm including the thick corky wings",
    "15",
    "15",
    "mean",
    "mm",
    p = "p37",
    notes = "Size includes the wings."
  ),
  s$row(
    "seed_width",
    "c. 15 x 9 mm including the thick corky wings",
    "9",
    "9",
    "mean",
    "mm",
    p = "p37",
    notes = "Size includes the wings."
  ),
  s$row(
    "dispersal_appendage",
    "including the thick corky wings",
    "wings",
    "wings",
    p = "p37"
  ),
  s$un("Seeds many ... in many rows ... thick corky wings", "seed | number, arrangement, wing texture", p = "p37"),
  s$un(
    "From the coasts of Malabar to tropical SE Asia and the whole of Malesia.",
    "distribution",
    sec = "Distribution",
    p = "p37"
  ),
  s$un(
    "Restricted to the back mangroves and along banks of tidal rivers and estuaries. In Sabah, common in the mangrove forests of the west coast; in Sarawak occasional on clay soils at inland limits of mangroves.",
    "habitat (mangrove); abundance; soil",
    sec = "Ecology",
    p = "p37"
  ),
  s$row(
    "wood_density",
    "The wood is soft and light with density of about 480 kg/m3",
    "480",
    "480",
    "mean",
    "kg/m3",
    sec = "Uses",
    region = "",
    p = "p38",
    notes = "Region of the measurement not stated; moisture content not stated."
  ),
  s$un(
    "easy to saw and work with and seasons well although it must be protected against sapstains. So far, there is no record of its use in Sabah and Sarawak but in various parts of its geographical range, the wood is used for wooden clogs, net-floats, scabbards and household utensils. ...",
    "wood working properties; uses",
    sec = "Uses",
    region = "",
    p = "p38"
  )
)

# ---- 3. Oroxylum ------------------------------------------------------------
g <- mk("Oroxylum", "genus", "genus", sec = "Genus description")
oroxylum_g <- bind_rows(
  g$row("plant_growth_form", "Small to medium-sized trees", "trees", "tree", p = "p38"),
  g$row("leaf_phyllotaxis", "Leaves opposite, 2--4-times pinnate", "opposite", "opposite", p = "p38"),
  g$row("leaf_compoundness", "Leaves opposite, 2--4-times pinnate", "pinnate", "compound", p = "p38"),
  g$row(
    "leaf_lamina_division",
    "Leaves opposite, 2--4-times pinnate",
    "2--4-times pinnate",
    "bipinnate tripinnate",
    p = "p38",
    notes = "4-times pinnate has no level; scored as bipinnate and tripinnate."
  ),
  g$un(
    "leaflets with 4--5 pairs of lateral veins, glands on the lower surface present only in young and some mature leaves, and then only at the basal part of leaflets",
    "leaflet | vein number, glands",
    p = "p38"
  ),
  g$row(
    "inflorescence_type",
    "Inflorescence a terminal raceme, erect, long",
    "terminal raceme",
    "terminal raceme",
    p = "p38"
  ),
  g$un("erect, long", "inflorescence | posture, length", p = "p38"),
  g$row("flower_scent_production", "Flowers fragrant, showy", "fragrant", "scent_produced", p = "p38"),
  g$un(
    "showy; calyx closed in bud, with an apical pore opening as a somewhat bell-shaped structure",
    "flower, calyx | showiness, opening",
    p = "p38"
  ),
  g$row("flower_colour", "corolla purplish", "purplish", "blue_purple", p = "p38"),
  g$un("with 5 subequal lobes", "corolla | lobes (subequal)", p = "p38"),
  g$row(
    "flower_fertile_stamens_count",
    "stamens 5",
    "5",
    "5",
    "mean",
    "{count}",
    p = "p38"
  ),
  g$un("ovules in many rows in each cell", "ovules | arrangement", p = "p38"),
  g$row(
    "fruit_type",
    "Fruit a long, flattened woody capsule",
    "capsule",
    "capsule",
    p = "p38"
  ),
  g$un("long, flattened woody", "fruit | shape, texture", p = "p38"),
  g$row(
    "dispersal_appendage",
    "Seeds quite large, with thin wings all around, in many rows",
    "wings",
    "wings",
    p = "p38"
  ),
  g$un("Seeds quite large ... in many rows", "seed | size, arrangement", p = "p38"),
  g$un(
    "Monotypic genus distributed in India, Sri Lanka, and throughout SE Asia.",
    "distribution",
    sec = "Genus Distribution",
    p = "p38"
  )
)

s <- mk("Oroxylum indicum", "species", region = SS)
oroxylum_indicum <- bind_rows(
  s$row(
    "plant_growth_form",
    "Medium-sized tree, partly deciduous, 20--40 m tall",
    "tree",
    "tree",
    p = "p38"
  ),
  s$row(
    "leaf_phenology",
    "Medium-sized tree, partly deciduous",
    "partly deciduous",
    "semi_deciduous",
    p = "p38"
  ),
  s$rng("plant_height", "20--40 m tall", "20", "40", "m", p = "p38"),
  s$rng(
    "plant_diameter_breast_height",
    "10--40 cm diameter",
    "10",
    "40",
    "cm",
    p = "p38",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$row("bark_texture", "Bark smooth, whitish", "smooth", "smooth", p = "p38"),
  s$row("bark_colour", "Bark smooth, whitish", "whitish", "white", p = "p38"),
  s$un(
    "inner bark pale yellowish. Sapwood pale whitish, soft.",
    "inner bark, sapwood | colour, softness",
    p = "p38"
  ),
  s$row(
    "leaf_compoundness",
    "Leaves 3--4-times pinnate",
    "pinnate",
    "compound",
    p = "p38"
  ),
  s$row(
    "leaf_lamina_division",
    "Leaves 3--4-times pinnate",
    "3--4-times pinnate",
    "tripinnate",
    p = "p38",
    notes = "4-times pinnate has no level; scored as tripinnate."
  ),
  s$row(
    "leaf_length",
    "Leaves 3--4-times pinnate, up to 2 m long",
    "2",
    "2",
    "maximum",
    "m",
    em = "compound leaf",
    p = "p38"
  ),
  s$un("tufted at the branch ends", "leaf | arrangement 'tufted'", p = "p38"),
  s$un(
    "petioles long, rachis swollen at points of articulation",
    "petiole, rachis | length (qualitative), swellings",
    p = "p38"
  ),
  s$row(
    "leaf_shape",
    "leaflets ovate to oblong",
    "ovate oblong",
    "ovate oblong",
    em = "leaflet",
    p = "p38"
  ),
  s$rng(
    "leaflet_length",
    "leaflets ovate to oblong, 6--9.5 x 2.5--5 cm",
    "6",
    "9.5",
    "cm",
    em = "leaflet",
    p = "p38"
  ),
  s$rng(
    "leaflet_width",
    "leaflets ovate to oblong, 6--9.5 x 2.5--5 cm",
    "2.5",
    "5",
    "cm",
    em = "leaflet",
    p = "p38"
  ),
  s$un("terminal one always bigger", "leaflet | terminal leaflet size", p = "p38"),
  s$row(
    "leaf_base_shape",
    "base cuneate or mostly oblique",
    "cuneate oblique",
    "cuneate oblique",
    em = "leaflet",
    p = "p38",
    notes = "'mostly' qualifies oblique; both states kept in one row."
  ),
  s$row(
    "leaf_apex_shape",
    "apex acute to acuminate",
    "acute acuminate",
    "acute acuminate",
    em = "leaflet",
    p = "p38"
  ),
  s$un(
    "lateral veins 4--5 pairs; intercostal veins reticulate",
    "veins | number, pattern",
    p = "p38"
  ),
  s$un(
    "glands scattered on the flower surface",
    "glands | 'flower surface' printed within the leaf clauses; organ unclear",
    p = "p38"
  ),
  s$rng("inflorescence_length", "Inflorescence 25--100 cm long", "25", "100", "cm", p = "p38"),
  s$un("Flowers nocturnal", "flower | anthesis at night", p = "p38"),
  s$row(
    "flower_length",
    "Flowers nocturnal, c. 9 cm long",
    "9",
    "9",
    "mean",
    "cm",
    p = "p38"
  ),
  s$un(
    "calyx coriaceous, 2.5--3.5 cm long, 1.5--2 cm across, containing water in bud, campanulate, dirty brown-violet, becoming almost woody in the fruit",
    "calyx | texture, length and width (no calyx trait), water, shape, colour, persistence",
    p = "p38"
  ),
  s$un(
    "corolla c. 10 cm long ... fleshy",
    "corolla | length (candidate), texture",
    p = "p41"
  ),
  s$row(
    "flower_colour",
    "corolla c. 10 cm long, reddish outside",
    "reddish",
    "red_brown",
    em = "corolla outer surface",
    p = "p41"
  ),
  s$row(
    "flower_colour",
    "yellowish to pinkish inside",
    "yellowish pinkish",
    "yellow_orange pink",
    em = "corolla inner surface",
    p = "p41"
  ),
  s$row("flower_shape", "fleshy, funnel-shaped", "funnel-shaped", "funnelform", p = "p41"),
  s$un("the 5 unequal lobes with wrinkled margin", "corolla | lobes, margin", p = "p41"),
  s$un(
    "stamens all fertile, hairy at the base, included",
    "stamens | fertility, hairs, exsertion",
    p = "p41"
  ),
  s$rng("fruit_length", "Fruit 25--100 x 5--10 cm", "25", "100", "cm", p = "p41"),
  s$rng("fruit_width", "Fruit 25--100 x 5--10 cm", "5", "10", "cm", p = "p41"),
  s$un(
    "hanging conspicuously resembling the big flat blade of a slash-knife",
    "fruit | position, shape",
    p = "p41"
  ),
  s$rng(
    "seed_length",
    "Seeds numerous, 4--8 x 2.5--4 cm including the membranous and transparent wing",
    "4",
    "8",
    "cm",
    p = "p41",
    notes = "Size includes the wing."
  ),
  s$rng(
    "seed_width",
    "Seeds numerous, 4--8 x 2.5--4 cm including the membranous and transparent wing",
    "2.5",
    "4",
    "cm",
    p = "p41",
    notes = "Size includes the wing."
  ),
  s$row(
    "dispersal_appendage",
    "including the membranous and transparent wing",
    "wing",
    "wings",
    p = "p41"
  ),
  s$un("Seeds numerous ... membranous and transparent wing", "seed | number, wing texture", p = "p41"),
  s$un("As for genus.", "distribution", sec = "Distribution", p = "p41"),
  s$un(
    "Secondary forest, mostly below 1000 m.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p41"
  ),
  # Family Ecology names this species (p. 33); no region stated.
  s$row(
    "pollination_syndrome",
    "O. indicum has flowers with lurid colour, musky odour, fleshy funnel-shaped corollas, which are pollinated by nectarivorous bats.",
    "pollinated by nectarivorous bats",
    "bat",
    sec = FE,
    region = "",
    p = "p33"
  ),
  s$row(
    "flower_scent_production",
    "O. indicum has flowers with lurid colour, musky odour, fleshy funnel-shaped corollas",
    "musky odour",
    "scent_produced",
    sec = FE,
    region = "",
    p = "p33"
  ),
  s$un(
    "O. indicum has flowers with lurid colour",
    "flower | colour 'lurid'",
    sec = FE,
    region = "",
    p = "p33"
  )
)

# ---- 4. Radermachera --------------------------------------------------------
g <- mk("Radermachera", "genus", "genus", sec = "Genus description")
radermachera_g <- bind_rows(
  g$row("plant_growth_form", "Trees or shrubs", "trees shrubs", "tree shrub", p = "p41"),
  g$un("Twigs lenticellate", "twigs | lenticels", p = "p41"),
  g$row("leaf_compoundness", "Leaves 1--3-times pinnate, opposite", "pinnate", "compound", p = "p41"),
  g$row(
    "leaf_lamina_division",
    "Leaves 1--3-times pinnate, opposite",
    "1--3-times pinnate",
    "pinnately_compound bipinnate tripinnate",
    p = "p41"
  ),
  g$row("leaf_phyllotaxis", "Leaves 1--3-times pinnate, opposite", "opposite", "opposite", p = "p41"),
  g$un(
    "leaflets often with gland-fields on the lower surface",
    "leaflet | glands",
    p = "p41"
  ),
  g$row(
    "inflorescence_type",
    "Inflorescence a raceme or thyrse, terminal on branches or borne on the trunk and branches",
    "raceme thyrse terminal",
    "raceme panicle terminal",
    p = "p41",
    notes = "Thyrse scored as panicle; cauliflory and ramiflory kept unmapped."
  ),
  g$un(
    "or borne on the trunk and branches (plant cauliflorous or ramiflorous)",
    "inflorescence | cauliflory, ramiflory",
    p = "p41"
  ),
  g$row(
    "flower_colour",
    "Flowers white, pink or greenish yellow",
    "white pink greenish_yellow",
    "white_cream pink green yellow_orange",
    p = "p41"
  ),
  g$un(
    "calyx 2--5-lobed, often glandular; corolla 5-lobed, slightly two-lipped",
    "calyx, corolla | lobes, glands, two-lipped (hedged)",
    p = "p41"
  ),
  g$rng(
    "flower_fertile_stamens_count",
    "stamens 4 or 5",
    "4",
    "5",
    "{count}",
    p = "p41"
  ),
  g$un(
    "ovary elongate, glabrous or covered with tiny scales or tubercles",
    "ovary | shape, surface",
    p = "p41"
  ),
  g$row(
    "fruit_type",
    "Fruit a straight or twisted capsule with a corky central placenta",
    "capsule",
    "capsule",
    p = "p41"
  ),
  g$un("straight or twisted ... with a corky central placenta", "fruit | shape, placenta", p = "p41"),
  g$row(
    "dispersal_appendage",
    "Seeds many, tiny and narrow, the ends winged",
    "winged",
    "wings",
    p = "p41"
  ),
  g$un("Seeds many, tiny and narrow", "seed | number, size, shape", p = "p41"),
  g$un(
    "15 species; tropical Asia, from India and south China to Malesia; 3 species in Sabah and Sarawak.",
    "distribution",
    sec = "Genus Distribution",
    p = "p41"
  )
)

# The only infraspecific taxon in the flora region: its description is the
# local description of the species (TFSS guide), kept at subspecies rank.
s <- mk("Radermachera pinnata subsp. acuminata", "subspecies", region = SS)
radermachera_pinnata <- bind_rows(
  s$row("plant_growth_form", "Tree 8--40 m tall", "tree", "tree", p = "p42"),
  s$rng("plant_height", "Tree 8--40 m tall", "8", "40", "m", p = "p42"),
  s$row(
    "plant_diameter_breast_height",
    "30 cm diameter",
    "30",
    "30",
    "mean",
    "cm",
    p = "p42",
    notes = "Stem diameter as printed; height of measurement not stated."
  ),
  s$row("stem_base_form", "buttresses to 2 m high", "buttresses", "buttressed", p = "p42"),
  s$un("buttresses to 2 m high", "buttress | height", p = "p42"),
  s$row(
    "bark_texture",
    "Bark scaly to flaky, light brown to grey",
    "flaky",
    "flaky",
    p = "p42",
    notes = "'scaly' has no level and is kept unmapped."
  ),
  s$un("Bark scaly", "bark | texture 'scaly' (no level)", p = "p42"),
  s$row(
    "bark_colour",
    "Bark scaly to flaky, light brown to grey",
    "light_brown grey",
    "brown_light grey",
    p = "p42"
  ),
  s$un("inner bark pale yellow. Sapwood white.", "inner bark, sapwood | colour", p = "p42"),
  s$row("leaf_compoundness", "Leaves 1--2-times pinnate", "pinnate", "compound", p = "p42"),
  s$row(
    "leaf_lamina_division",
    "Leaves 1--2-times pinnate",
    "1--2-times pinnate",
    "pinnately_compound bipinnate",
    p = "p42"
  ),
  s$rng("petiole_length", "petioles 8--10 cm long", "8", "10", "cm", p = "p42"),
  s$row(
    "leaf_shape",
    "leaflets obovate to obovate-elliptic",
    "obovate",
    "obovate",
    em = "leaflet",
    p = "p42",
    notes = "'obovate-elliptic' has no level and is kept unmapped."
  ),
  s$un("obovate-elliptic", "leaflet | shape 'obovate-elliptic' (no level)", p = "p42"),
  s$rng(
    "leaflet_length",
    "leaflets obovate to obovate-elliptic, 7--9 x 2.5--4 cm",
    "7",
    "9",
    "cm",
    em = "leaflet",
    p = "p42"
  ),
  s$rng(
    "leaflet_width",
    "leaflets obovate to obovate-elliptic, 7--9 x 2.5--4 cm",
    "2.5",
    "4",
    "cm",
    em = "leaflet",
    p = "p42"
  ),
  s$un("coriaceous to chartaceous", "leaflet | texture", p = "p42"),
  s$row("leaf_base_shape", "base cuneate", "cuneate", "cuneate", em = "leaflet", p = "p42"),
  s$row(
    "leaf_apex_shape",
    "apex acute to rounded",
    "acute rounded",
    "acute rounded",
    em = "leaflet",
    p = "p42"
  ),
  s$un("lateral veins 7--9 pairs", "veins | number", p = "p42"),
  s$row(
    "inflorescence_type",
    "Inflorescence a terminal thyrse",
    "terminal thyrse",
    "terminal panicle",
    p = "p42",
    notes = "Thyrse scored as panicle."
  ),
  s$row(
    "flower_scent_production",
    "Flowers mildly fragrant",
    "mildly fragrant",
    "scent_produced",
    p = "p42"
  ),
  s$un(
    "calyx purple, c. 1.5 cm long, c. 1--1.5 cm across, with a few scattered glands outside, lobes 3",
    "calyx | colour, length and width (no calyx trait), glands, lobes",
    p = "p42"
  ),
  s$un(
    "corolla about 1.5 cm long, c. 1--1.2 cm across",
    "corolla | length and width (candidate)",
    p = "p42"
  ),
  s$row(
    "flower_colour",
    "basal part narrow tubular and white, upper inflated portion pale lilac, throat and medium inner surface of the lobes orange",
    "white pale_lilac orange",
    "white_cream blue_purple yellow_orange",
    em = "corolla",
    p = "p42",
    notes = "Colours of different corolla parts in one row; pale lilac scored as blue_purple."
  ),
  s$un(
    "basal part narrow tubular ... upper inflated portion ... lobes 5 with pale, minute hairs on the margin",
    "corolla | shape, lobes, marginal hairs",
    p = "p42"
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens 4, didynamous",
    "4",
    "4",
    "mean",
    "{count}",
    p = "p42"
  ),
  s$un("ovary glabrous, style glabrous", "ovary, style | hairs", p = "p42"),
  s$rng("flower_style_length", "style glabrous, 1.2--2 cm long", "1.2", "2", "cm", p = "p42"),
  s$row(
    "fruit_type",
    "Fruit a capsule, hanging in clusters at the end of branches",
    "capsule",
    "capsule",
    p = "p42"
  ),
  s$un("hanging in clusters at the end of branches", "fruit | position", p = "p42"),
  s$rng("fruit_length", "16--27.5 x 0.5--1 cm", "16", "27.5", "cm", p = "p42"),
  s$rng("fruit_width", "16--27.5 x 0.5--1 cm", "0.5", "1", "cm", p = "p42"),
  s$un("persistent calyx c. 1.8 cm long", "fruiting calyx | length", p = "p42"),
  s$row(
    "seed_length",
    "Seeds c. 2.5 x 0.5 cm, winged",
    "2.5",
    "2.5",
    "mean",
    "cm",
    p = "p42",
    notes = "Not stated whether the size includes the wings."
  ),
  s$row(
    "seed_width",
    "Seeds c. 2.5 x 0.5 cm, winged",
    "0.5",
    "0.5",
    "mean",
    "cm",
    p = "p42",
    notes = "Not stated whether the size includes the wings."
  ),
  s$row("dispersal_appendage", "Seeds c. 2.5 x 0.5 cm, winged", "winged", "wings", p = "p42"),
  s$un(
    "Peninsular Thailand, Sumatra, Peninsular Malaysia, Borneo, and the Philippines. In Sabah and Sarawak, generally uncommon but localised populations may be encountered in ultramafic and limestone areas ...",
    "distribution; abundance; substrate",
    sec = "Distribution",
    p = "p42"
  ),
  s$un(
    "Primary and secondary forest, to 800 m, associated with ultramafic soils (Sabah) or limestones (Sarawak).",
    "habitat; altitude; substrate",
    sec = "Ecology",
    p = "p42"
  ),
  s$row(
    "flowering_time",
    "Flowering in May, July--March",
    "yyynynyyyyyy",
    "yyynynyyyyyy",
    sec = "Ecology",
    p = "p42",
    notes = "Months as printed."
  ),
  s$row(
    "fruiting_time",
    "fruiting in May--November",
    "nnnnyyyyyyyn",
    "nnnnyyyyyyyn",
    sec = "Ecology",
    p = "p42",
    notes = "Months as printed."
  ),
  s$un(
    "This species consists of two subspecies ... in subsp. acuminata the veins are not conspicuous or prominent. Subsp. acuminata is slightly variable in leaflet shape and texture.",
    "taxonomy; intercostal venation; variability",
    sec = "Taxonomy",
    p = "p42"
  )
)

s <- mk("Radermachera ramiflora", "species", region = SS)
radermachera_ramiflora <- bind_rows(
  s$row("plant_growth_form", "Medium-sized tree, 30--45 m tall", "tree", "tree", p = "p42"),
  s$rng(
    "plant_height",
    "Medium-sized tree, 30--45 m tall",
    "30",
    "45",
    "m",
    p = "p42",
    notes = "Range dash restored (raw/source_fixes.csv)."
  ),
  s$rng(
    "plant_diameter_breast_height",
    "30--50 cm diameter",
    "30",
    "50",
    "cm",
    p = "p42",
    notes = "Range dash restored (raw/source_fixes.csv). Stem diameter as printed; height of measurement not stated."
  ),
  s$row("stem_base_form", "trunk fluted at base", "fluted", "fluted", p = "p42"),
  s$row("bark_colour", "Bark greyish, fissured", "greyish", "grey", p = "p42"),
  s$row("bark_texture", "Bark greyish, fissured", "fissured", "furrowed", p = "p42"),
  s$un(
    "inner bark whitish, turning yellowish on exposure. Sapwood yellowish white. Twigs with corky bark.",
    "inner bark, sapwood, twigs | colour, oxidation, corky bark",
    p = "p42"
  ),
  s$row("leaf_compoundness", "Leaves 3-times pinnate", "pinnate", "compound", p = "p42"),
  s$row(
    "leaf_lamina_division",
    "Leaves 3-times pinnate",
    "3-times pinnate",
    "tripinnate",
    p = "p42"
  ),
  s$row(
    "leaf_arrangement",
    "crowded at the branch-tips",
    "crowded",
    "crowded",
    p = "p42"
  ),
  s$un("leaflets 4-6 pairs", "leaflets | number of pairs", p = "p42"),
  s$un(
    "elliptic-lanceolate, coriaceous",
    "leaflet | shape 'elliptic-lanceolate' (no level), texture",
    p = "p42"
  ),
  s$rng(
    "leaflet_length",
    "3.5--8.5 x 1.5--3.5 cm",
    "3.5",
    "8.5",
    "cm",
    em = "leaflet",
    p = "p42",
    notes = "Range dash restored (raw/source_fixes.csv)."
  ),
  s$rng(
    "leaflet_width",
    "3.5--8.5 x 1.5--3.5 cm",
    "1.5",
    "3.5",
    "cm",
    em = "leaflet",
    p = "p42",
    notes = "Range dash restored (raw/source_fixes.csv)."
  ),
  s$un("apex caudate", "leaflet apex | 'caudate' (no level)", p = "p42"),
  s$un(
    "lateral veins 5-7 pairs and raised below, lower surface with scattered small glands near the base",
    "veins, glands | number, relief",
    p = "p44"
  ),
  s$row(
    "inflorescence_type",
    "Inflorescence a raceme, borne on the trunk and branches",
    "raceme",
    "raceme",
    p = "p44"
  ),
  s$un(
    "borne on the trunk and branches (plants cauliflorous or ramiflorous)",
    "inflorescence | cauliflory, ramiflory",
    p = "p44"
  ),
  s$rng("inflorescence_length", "11--14 cm long", "11", "14", "cm", p = "p44"),
  s$row(
    "flower_colour",
    "Flowers yellow | corolla deep ochre-yellow, the limb red",
    "yellow red",
    "yellow_orange red_brown",
    p = "p44",
    notes = "Corolla deep ochre-yellow with a red limb."
  ),
  s$row(
    "flower_orientation",
    "Flowers yellow, erect on curved pedicels",
    "erect",
    "up",
    p = "p44"
  ),
  s$un(
    "on curved pedicels of c. 20 cm long",
    "pedicel | length c. 20 cm (unit doubtful; not scored)",
    p = "p44"
  ),
  s$un(
    "calyx dull reddish green to dark brown when dry, in the bud pear-shaped with many scattered glands outside especially near the base, 18--21 x 6--8 mm, lobes 3",
    "calyx | colour, shape, glands, size (no calyx trait), lobes",
    p = "p44"
  ),
  s$row(
    "flower_shape",
    "narrowly salver-shaped with scattered 2--4 glands outside, slightly curved",
    "salver-shaped",
    "salverform",
    p = "p44"
  ),
  s$rng(
    "flower_tube_length",
    "the narrow basal part 3--5 cm long",
    "3",
    "5",
    "cm",
    p = "p44",
    notes = "Narrow basal part of the corolla read as the tube."
  ),
  s$un(
    "scattered 2--4 glands outside, slightly curved ... lobes 5",
    "corolla | glands, curvature, lobes",
    p = "p44"
  ),
  s$row(
    "flower_fertile_stamens_count",
    "stamens 4, exserted, didynamous",
    "4",
    "4",
    "mean",
    "{count}",
    p = "p44"
  ),
  s$un("exserted, didynamous, hairy at the base; ovary ribbed", "stamens, ovary", p = "p44"),
  s$rng(
    "flower_style_length",
    "style to 3--3.5 cm long",
    "3",
    "3.5",
    "cm",
    p = "p44",
    notes = "'to 3--3.5' recorded as a range."
  ),
  s$rng("fruit_length", "Fruit straight or twisted, 35--70 cm long", "35", "70", "cm", p = "p44"),
  s$un("Fruit straight or twisted", "fruit | shape", p = "p44"),
  s$rng(
    "seed_length",
    "Seeds 4--5 x 2.4 mm, the wing 6--7 mm",
    "4",
    "5",
    "mm",
    p = "p44",
    notes = "Wing given separately; size excludes the wing."
  ),
  s$row(
    "seed_width",
    "Seeds 4--5 x 2.4 mm, the wing 6--7 mm",
    "2.4",
    "2.4",
    "mean",
    "mm",
    p = "p44",
    notes = "Wing given separately; size excludes the wing."
  ),
  s$row("dispersal_appendage", "the wing 6--7 mm", "wing", "wings", p = "p44"),
  s$un("the wing 6--7 mm", "seed wing | length", p = "p44"),
  s$un(
    "Endemic to Sabah, known from Mt. Kinabalu, Ranau, Kota Marudu, and Mt. Rara.",
    "distribution",
    sec = "Distribution",
    region = "Sabah",
    p = "p44"
  ),
  s$un(
    "Rainforest, also in disturbed forest on hill sides, to 1500 m, associated with ultramafic soils.",
    "habitat; altitude; substrate",
    sec = "Ecology",
    p = "p44"
  ),
  s$un(
    "Flowering in January to March and around August",
    "flowering | months; 'around August' is hedged, not scored",
    sec = "Ecology",
    p = "p44"
  ),
  s$row(
    "fruiting_time",
    "fruiting recorded in April and December",
    "nnnynnnnnnny",
    "nnnynnnnnnny",
    sec = "Ecology",
    p = "p44",
    notes = "Months as printed."
  ),
  s$un(
    "van Steenis l.c. (1977) noted that the calyx is eglandular, but the specimen Chew & Corner RSNB 4290, examined by him, has a few scattered glands outside the calyx.",
    "calyx | glands (note)",
    sec = "Note",
    p = "p44"
  )
)

s <- mk("Radermachera sp. A", "species", region = SS)
radermachera_sp_a <- s$un(
  "The species is poorly known; the young flower buds are not sufficient for describing it properly, but the winged secondary rachis and cuspidate leaflet tips clearly distinguish it from R. ramiflora.",
  "differential note only; no description",
  sec = "Note",
  p = "p44"
)

# ---- Copy-down ----------------------------------------------------------------
# Family and genus values copied to member taxa silent on the trait. Every
# value below is a single, unqualified level; members that score the trait
# themselves all agree or are left out (checked by hand):
#   - genus leaf_shape (Dolichandrone 'elliptic') and flower_colour (Oroxylum
#     'purplish') are not copied: the species descriptions give other values.
#   - family dispersal_appendage is not copied: every species scores it.
own <- bind_rows(
  deplanchea_bancana,
  dolichandrone_spathacea,
  oroxylum_indicum,
  radermachera_pinnata,
  radermachera_ramiflora
)
higher <- bind_rows(
  family,
  deplanchea_g,
  dolichandrone_g,
  oroxylum_g,
  radermachera_g
)
copy <- tribble(
  ~from, ~trait, ~to,
  "Bignoniaceae", "flower_structural_sex_type", "*",
  "Bignoniaceae", "flower_ovary_position", "*",
  "Bignoniaceae", "fruit_dehiscence", "*",
  "Bignoniaceae", "seedling_germination_location", "*",
  "Bignoniaceae", "dispersal_syndrome", "*",
  "Deplanchea", "leaf_compoundness", "Deplanchea bancana",
  "Deplanchea", "leaf_phyllotaxis", "Deplanchea bancana",
  "Deplanchea", "flower_perianth_symmetry", "Deplanchea bancana",
  "Deplanchea", "flower_shape", "Deplanchea bancana",
  "Deplanchea", "fruit_type", "Deplanchea bancana",
  "Dolichandrone", "leaf_margin", "Dolichandrone spathacea",
  "Dolichandrone", "flower_shape", "Dolichandrone spathacea",
  "Dolichandrone", "fruit_type", "Dolichandrone spathacea",
  "Oroxylum", "leaf_phyllotaxis", "Oroxylum indicum",
  "Oroxylum", "fruit_type", "Oroxylum indicum",
  "Radermachera", "leaf_phyllotaxis", "Radermachera pinnata subsp. acuminata",
  "Radermachera", "leaf_phyllotaxis", "Radermachera ramiflora",
  "Radermachera", "fruit_type", "Radermachera ramiflora"
) |>
  mutate(to = map(to, \(x) if (x == "*") unique(own$taxon_name) else x)) |>
  unnest(to)

copied <- copy |>
  inner_join(higher, by = c(from = "taxon_name", "trait"), relationship = "many-to-many") |>
  mutate(
    notes = paste0("Copied from the ", from, " description."),
    taxon_rank = own$taxon_rank[match(to, own$taxon_name)]
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

d <- bind_rows(higher, own, copied, radermachera_sp_a) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Berhaman_1995/raw/trait_data.csv", na = "")
write_csv(flora_unmapped_summary(d), "data/Berhaman_1995/raw/unmapped_terms.csv", na = "")

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Bignoniaceae", "family", "accepted", "",
  "Deplanchea", "genus", "accepted", "",
  "Deplanchea bancana", "species", "accepted", "",
  "Dolichandrone", "genus", "accepted", "",
  "Dolichandrone spathacea", "species", "accepted", "",
  "Nyctocalos", "genus", "key annotation", "Woody climbers; annotation inside the key to genera (4 unnamed species in Sabah and Sarawak), no detailed treatment; not scored.",
  "Oroxylum", "genus", "accepted", "Monotypic genus.",
  "Oroxylum indicum", "species", "accepted", "",
  "Radermachera", "genus", "accepted", "",
  "Radermachera pinnata", "species", "accepted", "Species heading only; the description belongs to its one infraspecific taxon in the region (subsp. acuminata).",
  "Radermachera pinnata subsp. acuminata", "subspecies", "accepted", "Only infraspecific taxon in the region; its description is the local description of the species (TFSS guide).",
  "Radermachera ramiflora", "species", "accepted", "",
  "Radermachera sp. A", "species", "undescribed species", "Informal name as printed; no description (flower buds only), differential note kept as an unmapped record."
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Berhaman_1995/raw/taxon_flags.csv", na = "")
