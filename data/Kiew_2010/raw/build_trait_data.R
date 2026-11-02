# Hand extraction of Balanophoraceae (R. Kiew 2010, Flora of Peninsular
# Malaysia Series II vol. 1: 67--81). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/FPM/Balanophoraceae.pdf, scanned_ocr (page
# scans with an Acrobat OCR layer). Every clause below was read from the
# image-checked transcription raw/Balanophoraceae_transcription.txt (pdf
# 8--22 = pp. 67--81; plates 9--10 on pdf 32--33 = pp. 301--302). PDF page =
# printed page - 59 for the treatment pages.
#
# Small family (2 genera, 7 accepted taxa), so rows are written by hand with
# flora_hand() (Kiew_2015 pattern). Higher-rank values are copied down by hand
# under the flora_copy_down() rules (unqualified, single level, every member
# that scores the trait agrees, most specific rank wins; numbers never).
# Hedged statements (suggests, possible, appears) are not scored. Fruit and
# seed values are not copied: every species says "Fruits and Seeds unknown".
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(67:81, \(p) c(p - 59L, p)), paste0("p", 67:81))
PM <- "Peninsular Malaysia"
HB <- "Hambali_1980"
K98 <- "Kiew_1998"

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
cpi <- "{count}/{count}"
mf <- "male flowers"
ff <- "female flowers"
mi <- "male inflorescences"
fi <- "female inflorescences"
cal <- function(months) {
  paste(if_else(1:12 %in% months, "y", "n"), collapse = "")
}
tep <- paste(
  "Tepal length scored as flower_sepal_length (one perianth whorl, no petals;",
  "provisional rule, Cheek_2012 question 4)."
)
anth <- "Anther count scored as fertile stamen count (anthers united in a synandrium)."
brct <- "Bract of the male inflorescence (one per flower) scored as flower_bract_length."
sess <- "Flowers also described as sessile; the sessile alternative (no pedicel) is not scored."
tb <- "flowering tuber-branch | length, width, shape (candidate plant_height_reproductive)"
sl <- "scale leaves | colour, count, arrangement, size, shape (bracts on the flowering tuber-branch; no leaf trait)"
icol <- "inflorescence | colour (candidate trait; not flower colour)"
icol_shape <- "inflorescence | colour (candidate trait; not flower colour), shape"

# ---- Family ---------------------------------------------------------------
f <- mk("Balanophoraceae", "family", "family", sec = "Family description")
family <- bind_rows(
  f$row(
    "plant_growth_form",
    "Herbaceous, fleshy root parasites without roots, stems or chlorophyll",
    "herbaceous",
    "herb",
    p = "p67"
  ),
  f$row(
    "parasitic",
    "Herbaceous, fleshy root parasites without roots, stems or chlorophyll",
    "root parasites",
    "root_parasitic",
    p = "p67",
    notes = "Family Ecology repeats it: 'Root parasites not showing host specificity'."
  ),
  f$row(
    "plant_photosynthetic_organ",
    "Herbaceous, fleshy root parasites without roots, stems or chlorophyll",
    "without chlorophyll",
    "non-photosynthetic_plant",
    p = "p67"
  ),
  f$un(
    "Herbaceous, fleshy root parasites without roots, stems or chlorophyll",
    "plant | roots and stems absent (negative phrase)",
    p = "p67"
  ),
  f$un(
    "producing a subterranean cylindric or subspherical, branched or unbranched solid tuber at the point of contact with the host root",
    "tuber | position, shape, branching (host-parasite organ; candidate storage_organ)",
    p = "p67"
  ),
  f$un(
    "tuber, scale leaves and inflorescences deep crimson, orange or pale brown (elsewhere yellowish white or rosy pink)",
    "tuber, scale leaves, inflorescence | colour",
    p = "p67"
  ),
  f$un(
    "Flowering tuber-branches producing inflorescences endogenously, leafless (e.g., Helosis) or with crimson or yellow-brown scale leaves (bracts) spirally arranged or opposite or whorled and enclosing the developing inflorescence (e.g., Balanophora)",
    "flowering tuber-branch | inflorescence origin; scale leaves present or absent, colour, arrangement",
    p = "p67"
  ),
  f$row(
    "inflorescence_type",
    "Inflorescences spicate, with or without peduncles",
    "spicate",
    "spike",
    p = "p67"
  ),
  f$un(
    "with or without peduncles, fleshy",
    "inflorescence | peduncle presence; texture",
    p = "p67"
  ),
  f$row(
    "flower_structural_sex_type",
    "fleshy with many unisexual flowers",
    "unisexual",
    "unisexual",
    p = "p67"
  ),
  f$un(
    "spikes unisexual or bisexual with the male flowers below and/or above the female flowers or with male and female flowers intermixed; in a few genera (e.g., Helosis) until anthesis covered by hexagonal scales",
    "inflorescence | sex, position of male and female flowers; covering scales",
    p = "p67"
  ),
  f$un(
    "Male flowers pedicellate or sessile, with or without bracteoles",
    "male flowers | pedicel, bracteoles",
    p = "p67"
  ),
  f$rng(
    "flower_perianth_merism",
    "Male flowers ... 2–6-merous",
    2,
    6,
    cnt,
    em = mf,
    p = "p67"
  ),
  f$un(
    "tepals free, 2–4(–6) in one series, or lacking",
    "male flowers | tepal fusion, number, whorls (qualified by 'or lacking')",
    p = "p67"
  ),
  f$rng(
    "flower_fertile_stamens_count",
    "stamens 2–4, united into a synandrium, opposite the tepals",
    2,
    4,
    cnt,
    em = mf,
    p = "p67"
  ),
  f$un(
    "united into a synandrium, opposite the tepals; anther locules 2, 4 or many",
    "stamens | fusion (synandrium), position; anther locule number",
    p = "p67"
  ),
  f$un(
    "Female flowers minute and very numerous, perianth minute, 2-lobed and joined to the ovary or absent",
    "female flowers | size, number; perianth",
    p = "p67"
  ),
  f$row(
    "flower_ovary_position",
    "ovary superior (elsewhere rarely inferior)",
    "superior",
    "superior",
    em = ff,
    p = "p67",
    notes = "The '(elsewhere rarely inferior)' aside is outside the flora region and not scored."
  ),
  f$rng(
    "flower_structural_carpels_count",
    "carpels 2–3, connate, each carpel with 1 embryo",
    2,
    3,
    cnt,
    em = ff,
    p = "p67"
  ),
  f$un(
    "connate, each carpel with 1 embryo; styles 1 or 2; embryo minute, without recognizable nucellus or integuments, consisting of a few cells embedded in scant endosperm",
    "carpels | fusion; embryo; style number",
    p = "p67"
  ),
  f$row(
    "fruit_type",
    "Fruits minute, 1-seeded achenes",
    "achenes",
    "achene",
    p = "p67",
    notes = "Not copied down: every species description says 'Fruits and Seeds unknown'."
  ),
  f$un("Fruits minute, 1-seeded achenes", "fruit | size; seed number", p = "p67"),
  f$un(
    "About 45 species in 17 genera worldwide in the tropics and subtropics. In Peninsular Malaysia, 2 genera, Balanophora represented by 6 species, one endemic (B. hansenii), and Helosis by a single endemic species, H. ruficeps.",
    "distribution",
    sec = "Distribution",
    p = "p67"
  ),
  f$un(
    "Root parasites not showing host specificity, but parasitizing a wide range of trees (elsewhere parasitizing herbs and bamboo).",
    "host | specificity, host range",
    sec = "Ecology",
    p = "p67"
  ),
  f$un(
    "Balanophoraceae is divided into two subfamilies: Balanophoroideae has wax-like balanophorin in its tubers and an ovary with a single style; and Helosidoideae lacks balanophorin and has an ovary with 2 (rarely 2–5 styles). ... it remains unplaced among the orders of dicotyledons",
    "taxonomy",
    sec = "Taxonomy",
    p = "p67"
  )
)

# ---- Balanophora (genus) ----------------------------------------------------
g <- mk("Balanophora", "genus", "genus", sec = "Genus description")
balanophora_g <- bind_rows(
  g$row(
    "sex_type",
    "Plant dioecious or monoecious with the male flowers below the female flowers",
    "dioecious monoecious",
    "dioecious monoecious",
    p = "p68"
  ),
  g$un(
    "with the male flowers below the female flowers (elsewhere with male flowers intermixed with or above the female flowers)",
    "inflorescence | position of male and female flowers",
    p = "p68"
  ),
  g$un(
    "Tuber an irregularly knobbly, compact, subspherical mass, 1–25 cm across; surface finely granular to coarsely tessellate, with or without stellate warts",
    "tuber | shape, size, surface",
    p = "p68"
  ),
  g$un(
    "Flowering tuber-branches ovoid, ellipsoid or obovoid, sometimes cylindric or almost spherical, 1–6 cm long and wide, in a few species repeatedly branching with elongated, cylindrical flowering tuber-branches forming an entangled mass 10–30 cm diameter",
    tb,
    p = "p68"
  ),
  g$un(
    "Scale leaves 2–40, broad-based; spirally arranged, opposite, distichous or whorled and covering the developing inflorescence",
    sl,
    p = "p68"
  ),
  g$un("Flowers pedicellate or sessile", "flowers | pedicel presence", p = "p68"),
  g$row(
    "inflorescence_type",
    "Male inflorescences spicate (elsewhere racemose) ... Female inflorescences spicate",
    "spicate",
    "spike",
    p = "p68",
    notes = "The '(elsewhere racemose)' aside is outside the flora region and not scored."
  ),
  g$rng("inflorescence_length", "Male inflorescences ... 1–18 cm long", 1, 18, "cm", em = mi, p = "p68"),
  g$rng(
    "inflorescence_diameter",
    "Male inflorescences ... 0.5–7 cm diameter at anthesis",
    0.5,
    7,
    "cm",
    em = mi,
    p = "p68"
  ),
  g$un("bracts non-peltate, usually short and truncate", "male inflorescence bracts | shape", p = "p68"),
  g$row(
    "flower_perianth_symmetry",
    "Male flowers: perianth actinomorphic or zygomorphic",
    "actinomorphic zygomorphic",
    "actinomorphic_general zygomorphic",
    em = mf,
    p = "p68"
  ),
  g$un(
    "tepals ovate to lanceolate, acute or almost square and truncate",
    "male flowers | tepal shape",
    p = "p68"
  ),
  g$ext(
    "flower_perianth_parts_count",
    "tepals ... 3–6(–14)",
    b = 3,
    c = 6,
    d = 14,
    unit = cnt,
    em = mf,
    p = "p68"
  ),
  g$un(
    "stamens connate and forming a more-or-less elongated synandrium; anthers 4–5 or number indeterminable",
    "stamens | fusion (synandrium); anther number (alternative 'or number indeterminable')",
    p = "p68"
  ),
  g$row(
    "flower_anther_dehiscence",
    "locules dehiscing longitudinally, sometimes transversely divided into smaller locules",
    "dehiscing longitudinally",
    "longitudinal_slit",
    em = mf,
    p = "p68"
  ),
  g$un(
    "sometimes transversely divided into smaller locules",
    "anther locules | division",
    p = "p68"
  ),
  g$un(
    "Female inflorescences spicate, ovoid, ellipsoid, obovoid or spherical",
    "female inflorescence | shape",
    p = "p68"
  ),
  g$rng("inflorescence_length", "Female inflorescences ... 0.5–7 cm long", 0.5, 7, "cm", em = fi, p = "p68"),
  g$rng(
    "inflorescence_diameter",
    "Female inflorescences ... 0.5–8.5 cm diameter",
    0.5,
    8.5,
    "cm",
    em = fi,
    p = "p68"
  ),
  g$un(
    "with thousands of flowers; spadicles (bracts) more-or-less club-shaped, 0.5–2.5 mm long, surrounding female flowers or in some species female flowers situated on the lower, narrow part of each spadicle",
    "female inflorescence | flower number (not counted); spadicle shape, length, position",
    p = "p68"
  ),
  g$un(
    "Female flowers without a perianth or distinct carpel",
    "female flowers | perianth and carpel absent (negative phrase)",
    p = "p68"
  ),
  g$rng(
    "flower_length",
    "Female flowers ... 0.2–0.7 × 0.15–0.4 mm",
    0.2,
    0.7,
    "mm",
    em = ff,
    p = "p68",
    notes = "First dimension of L x W taken as length."
  ),
  g$un(
    "0.2–0.7 × 0.15–0.4 mm",
    "female flowers | width",
    p = "p68"
  ),
  g$rng(
    "flower_style_length",
    "with a single style 0.5–1.5 mm long",
    0.5,
    1.5,
    "mm",
    em = ff,
    p = "p68"
  ),
  g$un(
    "with a single style ... apparently stigmatoid at and near apex, where pollen grains are often found attached",
    "style | number, stigmatic surface",
    p = "p68"
  ),
  g$un(
    "About 15 species in temperate to tropical Asia, throughout Malesia, Pacific Islands, tropical Australia, Comoros, Madagascar and tropical Africa (Congo); 6 species in Peninsular Malaysia.",
    "distribution",
    sec = "Distribution",
    p = "p68"
  ),
  g$un(
    "In Peninsular Malaysia, most species are found in montane forest above 1000 m altitude; only Balanophora abbreviata and B. reflexa are lowland species that occur at or below 1000 m. Only one collection (B. latisepala) is from limestone at low altitude.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p68"
  ),
  g$un(
    "It is often difficult to determine the host from the tangle of roots but none is host specific.",
    "host | specificity",
    sec = "Ecology",
    p = "p68"
  ),
  g$un(
    "Most are seasonal with annual tubers that begin to develop from the beginning of the year with peak flowering between August and October, the infructescences dying by the year's end. The exception is B. papuana that is found in flower throughout the year.",
    "phenology | seasonal tubers, peak flowering ('most' species; not copied); B. papuana scored from its own Ecology",
    sec = "Ecology",
    p = "p68"
  ),
  g$row(
    "pollination_syndrome",
    "The flowers are insect pollinated – the scale leaves and inflorescences are coloured, the male flowers produce conspicuous sticky white pollen, and the female inflorescences produce glistening droplets of nectar.",
    "insect pollinated",
    "insect",
    sec = "Ecology",
    p = "p69",
    notes = "Sentence starts on p. 68 and continues on p. 69. Stated without a hedge; the reasons given are floral features, so scored as syndrome, not as a known vector."
  ),
  g$row(
    "flower_nectar_production",
    "the female inflorescences produce glistening droplets of nectar",
    "produce nectar",
    "nectar_produced",
    em = fi,
    sec = "Ecology",
    p = "p69",
    notes = "Nectar is stated for the inflorescence, not for single flowers."
  ),
  g$un(
    "the male flowers produce conspicuous sticky white pollen",
    "pollen | colour, stickiness",
    sec = "Ecology",
    p = "p69"
  ),
  g$un(
    "The strong smell produced by mature inflorescences of B. reflexa on G. Kinabalu suggests it is pollinated by beetles or flies.",
    "pollination | beetle or fly (hedged 'suggests'; Sabah, outside the flora region; not scored)",
    sec = "Ecology",
    p = "p69"
  ),
  g$un(
    "Balanophora hansenii is also reported to have a disagreeable smell.",
    "scent | B. hansenii (scored from its own Ecology, Hambali 1980)",
    sec = "Ecology",
    p = "p69"
  ),
  g$un(
    "There are no observations on fruit dispersal in Peninsular Malaysia, but for B. abbreviata on Christmas Island, Ridley (J. Str. Br. Roy. As. Soc. 45 (1906) 218) observed that 'a puff of wind blew those [fruits] away like the seeds of an orchid'. In India, Govindappa & Shivamurthy (Ann. Bot. 39 (1975) 977) found fruits in wet soil around the parent plant and suggested that water percolation was the probable mode of dispersal. The fact that tubers of montane species in Peninsular Malaysia emerge close to the previous year's one supports this latter view.",
    "dispersal | wind, water (outside the flora region or hedged; not scored)",
    sec = "Ecology",
    p = "p69"
  ),
  g$un(
    "Germination has been documented for B. abbreviata in India by Arekal & Shivamurthy (Phytomorphology 26 (1976) 135) who observed that the seed at first produces a sticky tubular extension that attaches itself to a rootlet. A haustorium then penetrates the host rootlet and taps into the vascular system. Growth of the host root is then stimulated to produce the subterranean tuber that is penetrated by the vascular system of the parasite.",
    "germination | haustorium, tuber formation (India; outside the flora region)",
    sec = "Ecology",
    p = "p69"
  ),
  g$un(
    "In Java, the wax-filled tuber-branches of Balanophora elongata Blume are used as torches (Steenis, Mt. Fl. Java (1972) pl. 5.1).",
    "uses",
    sec = "Uses",
    p = "p69"
  ),
  g$un(
    "All species contain waxy balanophorin in varying amounts but among the Peninsular Malaysian species, only B. hansenii has wax in any quantity in its cortical cells.",
    "tuber | wax (balanophorin) content",
    sec = "Notes",
    p = "p69"
  ),
  g$un(
    "Among flowering plants, the female flowers and fruits of Balanophora are the smallest. Hansen (1972) estimated there are between 10^5–10^7 flowers in a single female inflorescence and the fruits weigh as little as 0.007 mg.",
    "female inflorescence | flower number (above flowers_per_inflorescence range); fruit mass (genus-wide estimate)",
    sec = "Notes",
    p = "p69"
  ),
  g$un(
    "Few species, e.g. B. abbreviata, are monoecious and produce bisexual inflorescences. In species with separate male and female inflorescences, they have never been observed to emerge from the same tuber confirming that they are in fact dioecious. Size of the inflorescence appears in part to be dependant on the size of the host root (Hansen, 1972).",
    "sex type | evidence for dioecy; inflorescence size and host root",
    sec = "Notes",
    p = "p69"
  )
)

# ---- Balanophora abbreviata ---------------------------------------------------
s <- mk("Balanophora abbreviata", "species", region = PM)
b_abbreviata <- bind_rows(
  s$row("sex_type", "Monoecious plants", "monoecious", "monoecious", p = "p70"),
  s$un(
    "creamy-white to pale yellow or light brown",
    "plant | colour",
    p = "p70"
  ),
  s$un(
    "Tubers single or several together in a mass, about 5 cm wide, branching from the base",
    "tuber | number, size, branching",
    p = "p70"
  ),
  s$un(
    "Flowering tuber-branches single, erect, cylindric, narrow at base, (1–)2.5–3.5(–10) cm long, (1–)2.5(–3) cm wide; surface finely granular, with or without scattered stellate warts",
    tb,
    p = "p70"
  ),
  s$un("Inflorescences bisexual", "inflorescence | sex (see sex_type)", p = "p70"),
  s$row("peduncle_length", "peduncle c. 1.2 cm long", 1.2, 1.2, "mean", "cm", p = "p70"),
  s$un(
    "Scale leaves light yellow, 3–7, distichous, evenly spaced, slightly imbricate, cucullate, 1–2 × 0.75–1.5 cm, ovate, obtuse or emarginate",
    sl,
    p = "p70"
  ),
  s$rng(
    "flowers_per_inflorescence",
    "Male flowers 10–20 in a zone 0.5–2 cm below female flowers",
    10,
    20,
    cpi,
    em = mf,
    p = "p70",
    notes = "Male flowers per bisexual inflorescence."
  ),
  s$un(
    "in a zone 0.5–2 cm below female flowers",
    "inflorescence | male zone length and position",
    p = "p70"
  ),
  s$row(
    "pedicel_length",
    "pedicels extremely short c. 1 mm or the flowers sessile",
    1,
    1,
    "mean",
    "mm",
    em = mf,
    p = "p70",
    notes = sess
  ),
  s$ext(
    "flower_perianth_merism",
    "flowers (3–)4–5(–8)-merous",
    a = 3,
    b = 4,
    c = 5,
    d = 8,
    unit = cnt,
    em = mf,
    p = "p70"
  ),
  s$rng("flower_length", "1.5–2 mm long", 1.5, 2, "mm", em = mf, p = "p70"),
  s$row(
    "flower_perianth_symmetry",
    "zygomorphic",
    "zygomorphic",
    "zygomorphic",
    em = mf,
    p = "p70"
  ),
  s$row("flower_perianth_parts_count", "tepals 4", 4, 4, "mean", cnt, em = mf, p = "p70"),
  s$un(
    "median tepals two, broadly truncate, almost square 1.5–2 mm wide; lateral tepals two, narrow, ovate and acute",
    "tepals | shape, width",
    p = "p70"
  ),
  s$un(
    "synandrium white with the fertile part c. 1 mm long, slightly compressed, 1.5–2 mm wide and 0.5–0.75 mm thick, anthers with 16–20 parallel longitudinal locules",
    "synandrium | colour, size; anther locules",
    p = "p70"
  ),
  s$un(
    "Female part dark red, ovoid, (1–)1.5(–2.5) cm long, (0.5–)1(–2.25) cm wide",
    "female part of the bisexual inflorescence | colour, shape, size",
    p = "p70"
  ),
  s$un(
    "Female flowers on main axis of inflorescences as well as on the lower part of spadicles; largest flowers with ovaries ovoid to ellipsoid, c. 0.4 mm long, 0.2 mm diameter",
    "female flowers | position; ovary shape and size",
    p = "p70"
  ),
  s$row("flower_style_length", "styles c. 1 mm long", 1, 1, "mean", "mm", em = ff, p = "p70"),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p70"),
  s$un(
    "Tropical Africa (Congo), Comoros and Madagascar, India ... In Peninsular Malaysia, recorded only three times from N Perak (Lenggong), Pahang (Pulau Tioman) and Johor (Pulau Aur).",
    "distribution",
    sec = "Distribution",
    p = "p70"
  ),
  s$un(
    "Critically Endangered B1b(iii). Known from three localities, one very much disturbed while the other two occur on islands often visited by tourists. The species has not been recorded for the past fifty years.",
    "conservation status",
    sec = "Conservation status",
    p = "p71"
  ),
  s$un(
    "In evergreen forest, sea level to 1000 m altitude on G. Kajang, Pulau Tioman.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p71"
  )
)

# ---- Balanophora fungosa (species and subsp. paragraphs only) -----------------
bf_sp <- mk("Balanophora fungosa", "species", region = PM)
bf_ss <- mk("Balanophora fungosa subsp. indica", "subspecies", region = PM)
b_fungosa_higher <- bind_rows(
  bf_sp$un(
    "India, Sri Lanka, Myanmar, Thailand and Indo-China (Vietnam and Cambodia) to S China (including Hainan), Malesia, N Australia (Queensland) and Pacific Islands.",
    "distribution",
    sec = "Distribution",
    p = "p71"
  ),
  bf_sp$un(
    "The species is divided into two subspecies: subsp. fungosa that is monoecious with male flowers below the female on a single inflorescence, and subsp. indica that is dioecious. ...",
    "taxonomy; sex type of subspecies (subsp. indica var. indica scored from its own description)",
    sec = "Taxonomy",
    p = "p71"
  ),
  bf_ss$un(
    "India, Sri Lanka, Myanmar, Thailand and Indo-China (Vietnam and Cambodia) to S China (including Hainan); Malesia (Sumatra, Peninsular Malaysia, Java, one collection from Luzon, the Philippines); Micronesia (Carolina and Mariana Islands) and N Australia.",
    "distribution",
    sec = "Distribution",
    p = "p72"
  ),
  bf_ss$un(
    "Evergreen forest from 600 m but usually at 1200–2000 m altitude.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p72"
  ),
  bf_ss$un(
    "Hansen (1972) recognised three varieties, two rare (var. globosa ... from Java ...; and var. minor ... from India and Thailand ...); and one, var. indica, widespread.",
    "taxonomy (varieties outside the flora region)",
    sec = "Taxonomy",
    p = "p72"
  )
)

# ---- Balanophora fungosa subsp. indica var. indica ---------------------------
s <- mk("Balanophora fungosa subsp. indica var. indica", "variety", region = PM)
b_fungosa <- bind_rows(
  s$row("sex_type", "Dioecious plants", "dioecious", "dioecious", p = "p72"),
  s$un(
    "Tubers tawny brown (elsewhere yellow to orange-yellow or pink), single or in a lumpy mass more-or-less branched to 25 cm across, surface coarsely tessellate to finely granular with stellate warts",
    "tuber | colour, size, surface",
    p = "p72"
  ),
  s$un(
    "Flowering tuber-branches stout, subspherical to broadly ellipsoid, 0.5–5.5(–11.5) cm long, 1–3.5 cm wide",
    tb,
    p = "p72"
  ),
  s$un(
    "Scale leaves dark crimson, 10–20, spirally arranged, imbricate, 1.5–4 × 1.2–2.5 cm, obtuse, slightly cucullate, not covering the inflorescence at anthesis",
    sl,
    p = "p72"
  ),
  s$ext(
    "peduncle_length",
    "Inflorescences with peduncles 0.5–4(–6) cm long",
    b = 0.5,
    c = 4,
    d = 6,
    unit = "cm",
    p = "p72"
  ),
  s$un("Male inflorescences ovoid to ellipsoid", "male inflorescence | shape", p = "p72"),
  s$tri(
    "inflorescence_length",
    "Male inflorescences ... (0.75–) 2.5(–5) cm long",
    0.75,
    2.5,
    5,
    "cm",
    em = mi,
    p = "p72"
  ),
  s$row(
    "flower_bract_length",
    "bracts truncate, c. 5 × 4 mm",
    5,
    5,
    "mean",
    "mm",
    em = mi,
    p = "p72",
    notes = brct
  ),
  s$un("bracts truncate, c. 5 × 4 mm", "male inflorescence bracts | shape, width", p = "p72"),
  s$rng("pedicel_length", "Male flowers: pedicels 7–10 mm", 7, 10, "mm", em = mf, p = "p72"),
  s$ext(
    "flower_perianth_merism",
    "(3–)4–5(–6)-merous",
    a = 3,
    b = 4,
    c = 5,
    d = 6,
    unit = cnt,
    em = mf,
    p = "p72"
  ),
  s$row(
    "flower_perianth_symmetry",
    "actinomorphic",
    "actinomorphic",
    "actinomorphic_general",
    em = mf,
    p = "p72"
  ),
  s$rng(
    "flower_sepal_length",
    "tepals 3–7 × 1–2.5 mm",
    3,
    7,
    "mm",
    em = mf,
    p = "p72",
    notes = tep
  ),
  s$un(
    "tepals 3–7 × 1–2.5 mm, elliptic-lanceolate, apex acute",
    "tepals | width, shape, apex",
    p = "p72"
  ),
  s$un(
    "synandrium white with fertile part slightly compressed, 2.5–5 mm long, 3.5 mm wide, 2 mm thick, often slightly obconical",
    "synandrium | colour, size, shape",
    p = "p72"
  ),
  s$ext(
    "flower_fertile_stamens_count",
    "anthers (3–)4–5(–6), horseshoe-shaped",
    a = 3,
    b = 4,
    c = 5,
    d = 6,
    unit = cnt,
    em = mf,
    p = "p72",
    notes = anth
  ),
  s$un("horseshoe-shaped", "anthers | shape", p = "p72"),
  s$un(
    "Female inflorescences dark red, subspherical or obovoid or ellipsoid ... apex rounded",
    icol_shape,
    p = "p72"
  ),
  s$tri(
    "inflorescence_length",
    "Female inflorescences ... (0.5–)4.5(–6) cm long",
    0.5,
    4.5,
    6,
    "cm",
    em = fi,
    p = "p72"
  ),
  s$un(
    "Female flowers on main axis of inflorescences as well as on cylindrical part of the spadicles; spadicles with an obovoid top, c. 1.7 mm long; largest female flowers with ovary c. 0.25–0.5 mm long",
    "female flowers | position; spadicles; ovary length",
    p = "p72"
  ),
  s$rng("flower_style_length", "style c. 1–1.5 mm long", 1, 1.5, "mm", em = ff, p = "p72"),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p72"),
  s$un(
    "As for the subspecies, except absent from Java. In Peninsular Malaysia, on west coast mountains from Perak to Selangor, also in Kelantan (G. Stong area), Terengganu (G. Lawit), Pahang (G. Benom, G. Tahan and G. Tapis) and Johor (G. Ledang and G. Panti).",
    "distribution",
    sec = "Distribution",
    p = "p72"
  ),
  s$un("Least concern.", "conservation status", sec = "Conservation status", p = "p72"),
  s$un(
    "Lower and upper montane forest in deeply shaded, humus-rich places usually above 1200 m altitude, occasionally as low as 600 m (G. Tapis).",
    "habitat; altitude",
    sec = "Ecology",
    p = "p72"
  ),
  s$un(
    "The only Malaysian species confirmed as a host is a conifer, Dacrydium sp. (Ridley s.n. 1899).",
    "host | named host",
    sec = "Ecology",
    p = "p72"
  ),
  s$rng(
    "inflorescence_length",
    "The plants on G. Benom and G. Tahan, Pahang, have exceptionally long male inflorescences, 11–11.5 cm as compared with up to 5.5 cm elsewhere.",
    11,
    11.5,
    "cm",
    em = mi,
    region = "G. Benom and G. Tahan, Pahang",
    sec = "Note",
    p = "p72",
    notes = "Local population with exceptionally long male inflorescences."
  ),
  s$un(
    "as compared with up to 5.5 cm elsewhere",
    "male inflorescence | length elsewhere (5.5 cm; differs from the description's 5 cm)",
    sec = "Note",
    p = "p72"
  ),
  # Genus Ecology statements naming this taxon (p. 69).
  s$row(
    "flower_nectar_production",
    "Both B. fungosa subsp. indica var. indica and B. papuana produce nectar that accumulates in a hollow on the upper surface of the bracts in both the male and female inflorescence.",
    "produce nectar",
    "nectar_produced",
    em = "male and female inflorescences",
    sec = "Genus Ecology",
    p = "p69",
    notes = "Nectar is stated for the inflorescence bracts, not for single flowers."
  ),
  s$row(
    "flower_visitor",
    "Vespa velutina Lepeletier has been observed to systematically forage for this nectar.",
    "Vespa velutina",
    "wasp",
    sec = "Genus Ecology",
    p = "p69",
    notes = "'This nectar' is that of B. fungosa subsp. indica var. indica and B. papuana. Whether visitors are pollinators is not known."
  ),
  s$row(
    "flower_visitor",
    "Trigona stingless bees collect pollen from B. fungosa, and a variety of insects visit its male and female inflorescences (Kiew, FRIM Res. Pamph. 121 (1998) 35) but whether they are pollinators is not known.",
    "stingless_bee insect",
    "stingless_bee insect",
    sec = "Genus Ecology",
    p = "p69",
    source_id = K98,
    notes = "Named B. fungosa; var. indica is its only taxon in the flora region. Whether visitors are pollinators is not known."
  )
)

# ---- Balanophora hansenii -----------------------------------------------------
s <- mk("Balanophora hansenii", "species", region = PM)
b_hansenii <- bind_rows(
  s$row("sex_type", "Dioecious plants", "dioecious", "dioecious", p = "p74"),
  s$un("dull to bright coral-red", "plant | colour", p = "p74"),
  s$un(
    "Tubers light brown, surface finely granular and minutely puberulous, hairs abrading with age, with scattered white stellate warts, cortex of tuber and its branches filled with wax",
    "tuber | colour, surface, hairs, wax",
    p = "p74"
  ),
  s$un(
    "Flowering tuber-branches erect and columnar, branches with male inflorescences 4–11.5 cm long and 1.5–2.75 cm diameter tapering to 2 cm at top; branches with female inflorescences to 6 cm long and 2 cm diameter, tapering at the apex to 0.75 cm",
    tb,
    p = "p74"
  ),
  s$row("peduncle_length", "Inflorescences with peduncles to 1 cm long", 1, 1, "maximum", "cm", p = "p74"),
  s$un(
    "Scale leaves light red or orange, 9–13, arranged in a tight spiral and appearing whorled, ovate with a broad base, apex acute, slightly cucullate, thin, faintly striate with 5–8 longitudinal veins, lower three to six scale leaves 1.25–3.75 × 1–2 cm, upper scale leaves longer, 3.25–4.5 × 1.75–2.75 cm and completely enclosing the inflorescences during and after anthesis and forming a compact ovoid pointed head, not reflexing after anthesis",
    sl,
    p = "p74"
  ),
  s$un("Male inflorescences yellow or pale yellow", icol, p = "p74"),
  s$rng("inflorescence_length", "Male inflorescences ... 2–2.7 cm long", 2, 2.7, "cm", em = mi, p = "p74"),
  s$rng(
    "inflorescence_diameter",
    "Male inflorescences ... 1.2–1.8 cm diameter",
    1.2,
    1.8,
    "cm",
    em = mi,
    p = "p74"
  ),
  s$rng(
    "flowers_per_inflorescence",
    "Male inflorescences ... with 19–20 flowers",
    19,
    20,
    cpi,
    em = mi,
    p = "p74"
  ),
  s$rng(
    "flower_bract_length",
    "bracts yellow, thick, 1–2.5 × 4–5.5 mm, truncate, apex blunt",
    1,
    2.5,
    "mm",
    em = mi,
    p = "p74",
    notes = paste(brct, "First dimension of L x W taken as length.")
  ),
  s$un(
    "bracts yellow, thick, 1–2.5 × 4–5.5 mm, truncate, apex blunt",
    "male inflorescence bracts | colour, texture, width, shape",
    p = "p74"
  ),
  s$row("flower_colour", "Male flowers pink", "pink", "pink", em = mf, p = "p74"),
  s$tri("flower_perianth_merism", "(3–)4(–5)-merous", 3, 4, 5, cnt, em = mf, p = "p74"),
  s$row(
    "flower_perianth_symmetry",
    "zygomorphic",
    "zygomorphic",
    "zygomorphic",
    em = mf,
    p = "p74"
  ),
  s$un(
    "not becoming reflexed at anthesis",
    "male flowers | orientation at anthesis",
    p = "p74"
  ),
  s$rng("pedicel_length", "pedicels 2–10 mm long", 2, 10, "mm", em = mf, p = "p74"),
  s$un(
    "median tepals broad, truncate, 2.5–4 × 2–3.5 cm",
    "median tepals | size printed in cm (probable source typo for mm; question 4), shape",
    p = "p74"
  ),
  s$rng(
    "flower_sepal_length",
    "lateral tepals narrowly obtuse to acute, 2–4 × 1–3 mm",
    2,
    4,
    "mm",
    em = "male flowers, lateral tepals",
    p = "p74",
    notes = tep
  ),
  s$un(
    "lateral tepals narrowly obtuse to acute, 2–4 × 1–3 mm",
    "lateral tepals | shape, width",
    p = "p74"
  ),
  s$un(
    "synandrium white, 0.5–2.5 cm long, elevated on a torus 1–1.5 mm long; anther locules c. 20",
    "synandrium | colour, length (printed in cm; probable source typo, question 4); torus; anther locules",
    p = "p74"
  ),
  s$row(
    "flower_anther_dehiscence",
    "anther locules c. 20, dehiscing longitudinally",
    "dehiscing longitudinally",
    "longitudinal_slit",
    em = mf,
    p = "p74"
  ),
  s$un(
    "Female inflorescences ellipsoid ... base truncate, tapering to a rounded apex",
    "female inflorescence | shape",
    p = "p74"
  ),
  s$rng("inflorescence_length", "Female inflorescences ellipsoid, 1.25–3.5 cm long", 1.25, 3.5, "cm", em = fi, p = "p74"),
  s$rng(
    "inflorescence_diameter",
    "Female inflorescences ... 0.75–1.5 cm diameter",
    0.75,
    1.5,
    "cm",
    em = fi,
    p = "p74"
  ),
  s$un(
    "spadicles club-shaped, c. 1 mm long. Female flowers: ovary ellipsoid, c. 0.2–0.25 mm long",
    "spadicles | shape, length; ovary shape, length",
    p = "p74"
  ),
  s$rng(
    "flower_style_length",
    "style 0.8–1 mm long, projecting beyond the spadicles",
    0.8,
    1,
    "mm",
    em = ff,
    p = "p74"
  ),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p74"),
  s$un(
    "Endemic in Peninsular Malaysia, known only from G. Ulu Kali, Pahang.",
    "distribution",
    sec = "Distribution",
    p = "p74"
  ),
  s$un(
    "Critically Endangered B1ab(iii). Although this species has been collected several times, it has not been seen in the past 20 years. Its single known locality has been severely disturbed.",
    "conservation status",
    sec = "Conservation status",
    p = "p75"
  ),
  s$un(
    "Upper montane forest at 1600–1800 m altitude growing in moist humus-rich soil, frequently among thickets of Pandanus klossii Ridl. (Pandanaceae).",
    "habitat; altitude; soil; associated species",
    sec = "Ecology",
    p = "p75"
  ),
  s$row(
    "parasitic",
    "Hambali (1980) reported it parasitises the roots of Pentaphylax euryoides Gard. & Champ. (Pentaphylacaceae) and Weinmannia fraxinea (D.Don) Miq. (Cunoniaceae).",
    "parasitises the roots",
    "root_parasitic",
    sec = "Ecology",
    p = "p75",
    source_id = HB
  ),
  s$un(
    "Pentaphylax euryoides Gard. & Champ. (Pentaphylacaceae) and Weinmannia fraxinea (D.Don) Miq. (Cunoniaceae)",
    "host | named hosts",
    sec = "Ecology",
    p = "p75",
    source_id = HB
  ),
  s$row(
    "flower_scent_production",
    "Male inflorescences have a faint, disagreeable smell reminiscent of Laurentia longiflora (L.) Peterm., Campanulaceae (Hambali, 1980).",
    "faint, disagreeable smell",
    "scent_produced",
    em = mi,
    sec = "Ecology",
    p = "p75",
    source_id = HB,
    notes = "Smell stated for the male inflorescence."
  ),
  s$row(
    "flowering_time",
    "It flowers between August and December.",
    "August to December",
    cal(8:12),
    sec = "Ecology",
    p = "p75"
  ),
  s$un(
    "First reported from G. Ulu Kali as Balanophora elongata Blume (Stone, 1980), which is a completely different species with ... compared with the columnar flowering tuber-branches of B. hansenii with a very short peduncle and congested scale leaves that are flat and cover the inflorescence at and after anthesis. The inflorescences of B. hansenii are also much shorter than those of B. elongata. Balanophora hansenii was described from male plants but since then female plants have been collected (FRI 21959) and are here described for the first time.",
    "comparison with B. elongata; female plants first described",
    sec = "Note",
    p = "p75"
  )
)

# ---- Balanophora latisepala ---------------------------------------------------
s <- mk("Balanophora latisepala", "species", region = PM)
b_latisepala <- bind_rows(
  s$row("sex_type", "Dioecious plants", "dioecious", "dioecious", p = "p75"),
  s$un("yellowish white to yellow or fawn", "plant | colour", p = "p75"),
  s$un(
    "Tubers in a mass, 4.5–15 cm across, little branched from the base, surface finely granular with few to numerous stellate warts",
    "tuber | size, branching, surface",
    p = "p75"
  ),
  s$un(
    "Flowering tuber-branches slender, ellipsoid, 2–4.25 cm long, 1.75–2.5 cm diameter",
    tb,
    p = "p75"
  ),
  s$rng("peduncle_length", "Inflorescences with peduncle 2.3–6.5 cm long", 2.3, 6.5, "cm", p = "p75"),
  s$un(
    "Scale leaves 3–4(–6), ellipsoid, 2–2.3(–3.5) × 0.75–1.2(–2) cm, well-spaced, distichous, pale brown or fawn, patent",
    sl,
    p = "p75"
  ),
  s$un("Male inflorescences long-ellipsoid", "male inflorescence | shape", p = "p75"),
  s$rng("inflorescence_length", "Male inflorescences long-ellipsoid, 5–9 cm long", 5, 9, "cm", em = mi, p = "p75"),
  s$rng(
    "inflorescence_diameter",
    "Male inflorescences ... 1.5–2.5 cm diameter",
    1.5,
    2.5,
    "cm",
    em = mi,
    p = "p75"
  ),
  s$row(
    "flower_bract_length",
    "bracts truncate, c. 3 × 5 mm",
    3,
    3,
    "mean",
    "mm",
    em = mi,
    p = "p75",
    notes = paste(brct, "First dimension of L x W taken as length.")
  ),
  s$un(
    "bracts truncate, c. 3 × 5 mm, sometimes with the middle part reduced and resembling 2 teeth",
    "male inflorescence bracts | shape, width",
    p = "p75"
  ),
  s$ext(
    "pedicel_length",
    "Male flowers sessile or with pedicels (1.5–)2–3(–6) mm long",
    a = 1.5,
    b = 2,
    c = 3,
    d = 6,
    unit = "mm",
    em = mf,
    p = "p75",
    notes = sess
  ),
  s$row(
    "flower_perianth_symmetry",
    "zygomorphic and compressed",
    "zygomorphic",
    "zygomorphic",
    em = mf,
    p = "p75"
  ),
  s$un("zygomorphic and compressed", "male flowers | compressed", p = "p75"),
  s$ext(
    "flower_perianth_merism",
    "4–5(–9–14)-merous",
    b = 4,
    c = 5,
    d = 14,
    unit = cnt,
    em = mf,
    p = "p75",
    notes = "Upper extreme printed as a range (–9–14); its maximum (14) is scored."
  ),
  s$rng(
    "flower_sepal_length",
    "in 4-merous flowers with two wide, median tepals nearly square, truncate, 3–3.5 × 3–4 mm",
    3,
    3.5,
    "mm",
    em = "male flowers, median tepals",
    p = "p75",
    notes = tep
  ),
  s$rng(
    "flower_sepal_length",
    "two narrow, ovate, acute lateral tepals 3–3.5 × 1.5–2 mm",
    3,
    3.5,
    "mm",
    em = "male flowers, lateral tepals",
    p = "p75",
    notes = tep
  ),
  s$un(
    "two wide, median tepals nearly square, truncate, 3–3.5 × 3–4 mm and two narrow, ovate, acute lateral tepals 3–3.5 × 1.5–2 mm",
    "tepals | shape, width",
    p = "p75"
  ),
  s$un(
    "synandrium white with fertile part laterally elongated, anther locules 16–20 or more, parallel and longitudinal",
    "synandrium | colour, shape; anther locules",
    p = "p75"
  ),
  s$row(
    "flower_anther_dehiscence",
    "dehiscing longitudinally",
    "dehiscing longitudinally",
    "longitudinal_slit",
    em = mf,
    p = "p75"
  ),
  s$un(
    "Female inflorescences pale orange-brown ... ellipsoid or almost cylindric, apex obtuse",
    icol_shape,
    p = "p75"
  ),
  s$ext(
    "inflorescence_length",
    "Female inflorescences ... (1–)4–6(–7) cm long",
    a = 1,
    b = 4,
    c = 6,
    d = 7,
    unit = "cm",
    em = fi,
    p = "p75"
  ),
  s$ext(
    "inflorescence_diameter",
    "Female inflorescences ... (0.5–)1–1.5(–2) cm diameter",
    a = 0.5,
    b = 1,
    c = 1.5,
    d = 2,
    unit = "cm",
    em = fi,
    p = "p75"
  ),
  s$un(
    "spadicles with obovoid top, c. 1 mm long. Female flowers: ovaries ovoid, c. 0.35 mm long",
    "spadicles | shape, length; ovary shape, length",
    p = "p75"
  ),
  s$rng("flower_style_length", "style c. 0.5–1 mm long", 0.5, 1, "mm", em = ff, p = "p75"),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p75"),
  s$un(
    "Thailand, Indo-China, N Sumatra, Peninsular Malaysia and Borneo (Kuching Division, Sarawak). In Peninsular Malaysia recorded from Perlis (Bt. Lagi) and on west coast mountains—Taiping Hill and the Main Range from Perak (G. Korbu) to Selangor (G. Nuang).",
    "distribution",
    sec = "Distribution",
    p = "p76"
  ),
  s$un(
    "Vulnerable B1ab(iii). It is much rarer than the other montane balanophoras, B. fungosa and B. papuana. The limestone population is presumed extinct due to habitat destruction.",
    "conservation status",
    sec = "Conservation status",
    p = "p76"
  ),
  s$un(
    "In Peninsular Malaysia not common, usually in montane forest between 1200–1700 m but in Perlis, with a monsoon climate, it has been collected at low altitude on limestone (Bt. Lagi). In Thailand, it has also been collected from limestone as well as evergreen forest at 100–1700 m altitude. Host trees have not been recorded.",
    "habitat; altitude; substrate (limestone); Thailand (outside the flora region)",
    sec = "Ecology",
    p = "p76"
  ),
  s$row(
    "flowering_time",
    "Most flowering is recorded between August and November, with a few in February and March.",
    "August to November",
    cal(8:11),
    q = "most",
    sec = "Ecology",
    p = "p76"
  ),
  s$row(
    "flowering_time",
    "Most flowering is recorded between August and November, with a few in February and March.",
    "February and March",
    cal(2:3),
    q = "a few",
    sec = "Ecology",
    p = "p76"
  )
)

# ---- Balanophora papuana ------------------------------------------------------
s <- mk("Balanophora papuana", "species", region = PM)
b_papuana <- bind_rows(
  s$row("sex_type", "Dioecious plants", "dioecious", "dioecious", p = "p76"),
  s$un(
    "dark crimson or sometimes brown (elsewhere yellow to orange-yellow or red)",
    "plant | colour",
    p = "p76"
  ),
  s$un(
    "Tubers a much branched coral-like mass, 14–24 cm diameter with 3–12 branches, surface tessellate with polygons 1–2 mm across with scattered stellate white warts",
    "tuber | shape, size, branch number, surface",
    p = "p76"
  ),
  s$un(
    "Flowering tuber-branches slender, (1–)3.5(–6) cm long, 1.5–2.5 cm diameter",
    tb,
    p = "p76"
  ),
  s$row("peduncle_length", "Inflorescences with peduncles c. 1.5 cm long", 1.5, 1.5, "mean", "cm", p = "p76"),
  s$un(
    "Scale leaves deep red, opposite or decussate in 2–4(–5) pairs, spaced on the branch, in the upper part sometimes appearing whorled, but not covering the inflorescence at anthesis, obovate, equal-sized, 1.5–3 × 0.75–1.5 cm, cucullate",
    sl,
    p = "p76",
    notes = "Sentence runs onto p. 77."
  ),
  s$rng("inflorescence_length", "Male inflorescences 1.5–3.5 cm long", 1.5, 3.5, "cm", em = mi, p = "p77"),
  s$rng(
    "inflorescence_diameter",
    "Male inflorescences ... 1.5–3 cm diameter",
    1.5,
    3,
    "cm",
    em = mi,
    p = "p77"
  ),
  s$row(
    "flower_bract_length",
    "bracts rudimentary, to 1 mm long",
    1,
    1,
    "maximum",
    "mm",
    em = mi,
    p = "p77",
    notes = brct
  ),
  s$un("bracts rudimentary", "male inflorescence bracts | rudimentary", p = "p77"),
  s$un(
    "Male flowers often in conspicuous vertical rows",
    "male flowers | arrangement",
    p = "p77"
  ),
  s$rng("pedicel_length", "pedicels 2–7 mm long", 2, 7, "mm", em = mf, p = "p77"),
  s$row(
    "flower_perianth_symmetry",
    "zygomorphic",
    "zygomorphic",
    "zygomorphic",
    em = mf,
    p = "p77"
  ),
  s$row("flower_perianth_merism", "4(–5)-merous", 4, 4, "mean", cnt, "usually", em = mf, p = "p77"),
  s$row(
    "flower_perianth_merism",
    "4(–5)-merous",
    5,
    5,
    "maximum",
    cnt,
    "extreme_range",
    em = mf,
    p = "p77"
  ),
  s$row(
    "flower_sepal_length",
    "median tepals wide and truncate, c. 4.5 × 3.5 mm",
    4.5,
    4.5,
    "mean",
    "mm",
    em = "male flowers, median tepals",
    p = "p77",
    notes = tep
  ),
  s$row(
    "flower_sepal_length",
    "lateral tepals narrow and acute, c. 4.5 × 1 mm",
    4.5,
    4.5,
    "mean",
    "mm",
    em = "male flowers, lateral tepals",
    p = "p77",
    notes = tep
  ),
  s$un(
    "median tepals wide and truncate, c. 4.5 × 3.5 mm, lateral tepals narrow and acute, c. 4.5 × 1 mm",
    "tepals | shape, width",
    p = "p77"
  ),
  s$un(
    "synandrium with fertile part often slightly obconic, laterally elongated, c. 3 × 7 mm and 3 mm thick, anther locules parallel",
    "synandrium | shape, size; anther locules",
    p = "p77"
  ),
  s$row(
    "flower_anther_dehiscence",
    "anther locules parallel, opening longitudinally",
    "opening longitudinally",
    "longitudinal_slit",
    em = mf,
    p = "p77"
  ),
  s$un("Female inflorescences obovoid, deep red", icol_shape, p = "p77"),
  s$ext(
    "inflorescence_length",
    "Female inflorescences ... (0.75–)1.5–3(–4) cm long",
    a = 0.75,
    b = 1.5,
    c = 3,
    d = 4,
    unit = "cm",
    em = fi,
    p = "p77"
  ),
  s$ext(
    "inflorescence_diameter",
    "Female inflorescences ... (0.5–)1–2.5(–3) cm diameter",
    a = 0.5,
    b = 1,
    c = 2.5,
    d = 3,
    unit = "cm",
    em = fi,
    p = "p77"
  ),
  s$un(
    "spadicles c. 1 mm long, obconic at the top. Female flowers: ovaries c. 0.18 mm",
    "spadicles | length, shape; ovary length",
    p = "p77"
  ),
  s$row("flower_style_length", "style up to 1 mm long", 1, 1, "maximum", "mm", em = ff, p = "p77"),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p77"),
  s$un(
    "Peninsular Malaysia, Borneo (G. Murud, Sarawak, and G. Kinabalu, Sabah), the Philippines (Luzon, Mindoro and Mindanao), Sulawesi and New Guinea. In Peninsular Malaysia, it is recorded from Kelantan (G. Stong); Perak (Taiping Hills), Main Range from Perak (Ulu Batang Padang) to Selangor (Ulu Ampang), and south to Negeri Sembilan (G. Angsi) and Johor (G. Ledang, G. Panti and G. Belumut).",
    "distribution",
    sec = "Distribution",
    p = "p77"
  ),
  s$un("Least concern.", "conservation status", sec = "Conservation status", p = "p77"),
  s$un(
    "Montane forest at 1000–2000 m, rarely as low as 300 m in the Gombak valley, Selangor.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p77"
  ),
  s$row(
    "flowering_time",
    "It is recorded in flower throughout the year.",
    "throughout the year",
    cal(1:12),
    sec = "Ecology",
    p = "p77",
    notes = "'Throughout the year' scored as every month. The genus Ecology (p. 68) says the same."
  ),
  s$un(
    "Its bright red and slender much-branched habit led Ridley (1924) and Henderson (1959) to name it the Coral Plant.",
    "vernacular name; plant colour and branching",
    sec = "Ecology",
    p = "p77"
  ),
  s$un(
    "Hansen (1972) identified one of the two specimens (Poore s.n., 1964) from Jengka Triangle ... as B. papuana, but re-examination of this specimen shows it to be a female plant of B. reflexa.",
    "specimen identification",
    sec = "Note",
    p = "p77"
  ),
  # Genus Ecology statements naming this species (p. 69).
  s$row(
    "flower_nectar_production",
    "Both B. fungosa subsp. indica var. indica and B. papuana produce nectar that accumulates in a hollow on the upper surface of the bracts in both the male and female inflorescence.",
    "produce nectar",
    "nectar_produced",
    em = "male and female inflorescences",
    sec = "Genus Ecology",
    p = "p69",
    notes = "Nectar is stated for the inflorescence bracts, not for single flowers."
  ),
  s$row(
    "flower_visitor",
    "Vespa velutina Lepeletier has been observed to systematically forage for this nectar.",
    "Vespa velutina",
    "wasp",
    sec = "Genus Ecology",
    p = "p69",
    notes = "'This nectar' is that of B. fungosa subsp. indica var. indica and B. papuana. Whether visitors are pollinators is not known."
  )
)

# ---- Balanophora reflexa ------------------------------------------------------
s <- mk("Balanophora reflexa", "species", region = PM)
b_reflexa <- bind_rows(
  s$row("sex_type", "Dioecious plants", "dioecious", "dioecious", p = "p78"),
  s$un("red or dark red", "plant | colour", p = "p78"),
  s$un(
    "Tubers several together in a stout mass 2–3.5 cm diameter, branching from the base, surface coarsely tessellate, without conspicuous stellate white warts",
    "tuber | number, size, branching, surface",
    p = "p78"
  ),
  s$un(
    "Flowering tuber-branches stout, ovoid-ellipsoid-obovoid, (1.5–)3–6(–14) cm long, (1–)2–4(–4.5) cm diameter",
    tb,
    p = "p78"
  ),
  s$rng("peduncle_length", "Inflorescences with peduncles 2–5 cm long", 2, 5, "cm", p = "p78"),
  s$un(
    "Scale leaves 3–8, spirally arranged, not covering the inflorescence at anthesis, broadly elliptic, cucullate, crimson, 2–3.5 × 1.5–2.25 cm",
    sl,
    p = "p78"
  ),
  s$rng("inflorescence_length", "Male inflorescences 1.5–5 cm long", 1.5, 5, "cm", em = mi, p = "p78"),
  s$rng(
    "flower_bract_length",
    "upper bracts entire, truncate, 3–4 mm long",
    3,
    4,
    "mm",
    em = "male inflorescences, upper bracts",
    p = "p78",
    notes = brct
  ),
  s$un(
    "upper bracts entire, truncate ... lower bracts divided to the base into 4–6(–7) teeth, each about 1–2 × 0.5 mm long",
    "male inflorescence bracts | shape; lower bract teeth",
    p = "p78"
  ),
  s$un(
    "Male flowers 7–9 or more",
    "male flowers | number per inflorescence (open-ended 'or more')",
    p = "p78"
  ),
  s$ext(
    "pedicel_length",
    "lowermost flowers with pedicels (10–)15–17.5(–20) mm long",
    a = 10,
    b = 15,
    c = 17.5,
    d = 20,
    unit = "mm",
    em = "male flowers, lowermost",
    p = "p78"
  ),
  s$row("flower_perianth_merism", "4-merous", 4, 4, "mean", cnt, em = mf, p = "p78"),
  s$row(
    "flower_perianth_symmetry",
    "zygomorphic",
    "zygomorphic",
    "zygomorphic",
    em = mf,
    p = "p78"
  ),
  s$un(
    "compressed, c. 3 × 2 mm in cross-section, before anthesis pointing upwards and closely appressed to the axis of the inflorescence, during and after anthesis strongly reflexed",
    "male flowers | cross-section; orientation before and after anthesis",
    p = "p78"
  ),
  s$ext(
    "bud_length",
    "buds much compressed, (3.75–)5–6 (–7) × (4.5–)6–8 (–9) mm and (2.5–)3(–3.5) mm thick",
    a = 3.75,
    b = 5,
    c = 6,
    d = 7,
    unit = "mm",
    em = "male flower buds",
    p = "p78",
    notes = "First dimension of L x W taken as length."
  ),
  s$ext(
    "bud_width",
    "buds much compressed, (3.75–)5–6 (–7) × (4.5–)6–8 (–9) mm and (2.5–)3(–3.5) mm thick",
    a = 4.5,
    b = 6,
    c = 8,
    d = 9,
    unit = "mm",
    em = "male flower buds",
    p = "p78",
    notes = "Second dimension of L x W taken as width."
  ),
  s$un("(2.5–)3(–3.5) mm thick", "male flower buds | thickness", p = "p78"),
  s$un(
    "median tepals 2, almost square, truncate; lateral tepals 2, narrow, lanceolate, acute",
    "tepals | number per kind, shape",
    p = "p78"
  ),
  s$un(
    "synandrium with fertile part much compressed, almost fan-shaped; anther locules parallel and longitudinal",
    "synandrium | shape; anther locules (orientation, not dehiscence)",
    p = "p78"
  ),
  s$un(
    "Female inflorescences spherical or ellipsoid-obovoid, deep red",
    icol_shape,
    p = "p78"
  ),
  s$ext(
    "inflorescence_length",
    "Female inflorescences ... (0.75–)1.5–3(–4) cm long",
    a = 0.75,
    b = 1.5,
    c = 3,
    d = 4,
    unit = "cm",
    em = fi,
    p = "p78"
  ),
  s$ext(
    "inflorescence_diameter",
    "Female inflorescences ... (0.75–)1–2(–3) cm diameter",
    a = 0.75,
    b = 1,
    c = 2,
    d = 3,
    unit = "cm",
    em = fi,
    p = "p78"
  ),
  s$un(
    "spadicles c. 1 mm long, obconic at the top. Female flowers: ovary c. 0.27 mm long",
    "spadicles | length, shape; ovary length",
    p = "p78"
  ),
  s$row("flower_style_length", "style c. 0.9 mm long", 0.9, 0.9, "mean", "mm", em = ff, p = "p78"),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p78"),
  s$un(
    "Peninsular Malaysia, Borneo (Sarawak, Sabah (G. Kinabalu) and Kalimantan). In Peninsular Malaysia, known only from Bt. Bintang, Perak and Jengka Triangle, Pahang.",
    "distribution",
    sec = "Distribution",
    p = "p78"
  ),
  s$un(
    "Critically Endangered A4c. Land use change has occurred in its two known localities.",
    "conservation status",
    sec = "Conservation status",
    p = "p78"
  ),
  s$un(
    "In Peninsular Malaysia, it has been collected from only two localities and is extremely rare. ... It is possible that this species is now extinct in Peninsular Malaysia. Compared with Borneo, where it has been collected from 300 to 3000 m altitude, it was collected only at 30–100 m altitude from lowland forest in Peninsular Malaysia.",
    "habitat; altitude; rarity",
    sec = "Ecology",
    p = "p78"
  ),
  s$un(
    "Flowering specimens on G. Kinabalu smell strongly of fox or mice (Kiew, 1978).",
    "scent | Sabah (outside the flora region; not scored)",
    sec = "Ecology",
    p = "p78"
  )
)

# ---- Helosis (genus) ----------------------------------------------------------
g <- mk("Helosis", "genus", "genus", sec = "Genus description")
helosis_g <- bind_rows(
  g$row("sex_type", "Monoecious plants, without scale leaves", "monoecious", "monoecious", p = "p79"),
  g$un("without scale leaves", "scale leaves absent (negative phrase)", p = "p79"),
  g$un(
    "Flowering tuber-branches produced from elongated, horizontal rhizome-like tubers",
    "tuber | shape, orientation (rhizome-like)",
    p = "p79"
  ),
  g$un(
    "Inflorescences without scale leaves, spadix-like, bisexual, at first covered by peltate bracts with a conical head and margins touching, these caducous at anthesis",
    "inflorescence | spadix-like (family 'spicate' copied as spike); sex; peltate bracts",
    p = "p79"
  ),
  g$un("Flowers densely mixed with numerous hairs", "inflorescence | hairs among flowers", p = "p79"),
  g$row(
    "flower_perianth_merism",
    "Male flowers minute, predominately trimerous",
    3,
    3,
    "mean",
    cnt,
    "predominately",
    em = mf,
    p = "p79"
  ),
  g$row(
    "flower_perianth_fusion_categorical",
    "perianth shortly tubular at base",
    "shortly tubular at base",
    "perianth_parts_fused_at_base",
    em = mf,
    p = "p79"
  ),
  g$un(
    "stamens with free filaments, the anthers joined to form a synandrium with many anther locules",
    "stamens | filaments free; anthers fused (synandrium); locules",
    p = "p79"
  ),
  g$un(
    "Female flowers minute with compressed ovaries with 2 styles with minute capitate stigmas",
    "female flowers | size; ovary shape; style number; stigma",
    p = "p79"
  ),
  g$un(
    "Remarkable for its disjunct distribution between S America (3 species) and Peninsular Malaysia (1 endemic species).",
    "distribution",
    sec = "Distribution",
    p = "p79"
  ),
  g$un(
    "Steenis (1932) drew attention to Rhopalocnemis ruficeps Ridl. ... They therefore transferred Exorhopala ruficeps to Helosis.",
    "taxonomy",
    sec = "Taxonomy",
    p = "p79"
  )
)

# ---- Helosis ruficeps -----------------------------------------------------------
s <- mk("Helosis ruficeps", "species", region = PM)
h_ruficeps <- bind_rows(
  s$un(
    "Tuber horizontal, branching, at least 15 cm long and 0.5–1 cm diameter, slightly rugose, yellow becoming crimson or orange-brown",
    "tuber | orientation, size, surface, colour",
    p = "p79"
  ),
  s$un(
    "Flowering tuber-branches smooth, (4–)8–10.5 cm long, 7–13 mm diameter, yellow or brown",
    tb,
    p = "p79"
  ),
  s$un("Inflorescences deep rosy red, ellipsoid", icol_shape, p = "p79"),
  s$ext(
    "inflorescence_length",
    "Inflorescences ... (2.75–) 5–7(–10) cm long",
    a = 2.75,
    b = 5,
    c = 7,
    d = 10,
    unit = "cm",
    p = "p79"
  ),
  s$rng(
    "inflorescence_diameter",
    "diameter including peltate bracts 2–3.25 cm",
    2,
    3.25,
    "cm",
    p = "p79",
    notes = "Diameter includes the peltate bracts."
  ),
  s$un(
    "apex rounded or slightly narrowed; peltate bracts deep crimson, stalk 1.5–2 mm long, head sharply conical, 4–6-sided, c. 4 × 4–5 mm",
    "inflorescence apex; peltate bracts | colour, size, shape",
    p = "p79"
  ),
  s$un(
    "Flowers embedded in a dense layer of nectariferous hairs",
    "inflorescence | nectariferous hairs (nectar from hairs, not stated for flowers)",
    p = "p79"
  ),
  s$row(
    "flower_length",
    "Male flowers c. 2.5 mm long and grouped in pairs",
    2.5,
    2.5,
    "mean",
    "mm",
    em = mf,
    p = "p81",
    notes = "The heading 'Male' ends p. 79; the value is on p. 81, after Fig. 2 (p. 80)."
  ),
  s$un("grouped in pairs", "male flowers | arrangement", p = "p81"),
  s$row(
    "flower_perianth_fusion_categorical",
    "perianth scarcely joined at the base",
    "scarcely joined at the base",
    "perianth_parts_fused_at_base",
    em = mf,
    p = "p81"
  ),
  s$tri("flower_perianth_merism", "lobes (2–) 3(–4) oblong, truncate", 2, 3, 4, cnt, em = mf, p = "p81"),
  s$un("lobes ... oblong, truncate", "perianth lobes | shape", p = "p81"),
  s$un(
    "stamens the same number as the tepal lobes, filaments free, anthers joined to form a globose synandrium, in 3-merous flowers with 10–16 locules arranged in more than one layer; pistillode absent",
    "stamens | number (relative), filaments free; synandrium shape; locules; pistillode absent",
    p = "p81"
  ),
  s$rng("flower_length", "Female flowers 2–3 mm long", 2, 3, "mm", em = ff, p = "p81"),
  s$un(
    "perianth absent; ovary ellipsoid with 2 styles, stigma minute, capitate; upper third of inflorescence with single-styled and sterile female flowers",
    "female flowers | perianth absent; ovary shape; style number; stigma; sterile flowers",
    p = "p81"
  ),
  s$un("Fruits and Seeds unknown", "fruit, seed | unknown", p = "p81"),
  s$un(
    "Narrowly endemic in Peninsular Malaysia, known only from four localities: Kedah (Baling, and G. Jerai), Penang Hill and Perak (Taiping Hill), more than half the collections are from Penang where it is recorded from at least four different places.",
    "distribution",
    sec = "Distribution",
    p = "p81"
  ),
  s$un(
    "Endangered B1ab(iii). The four localities from where it is known are either disturbed or are vulnerable to disturbance.",
    "conservation status",
    sec = "Conservation status",
    p = "p81"
  ),
  s$un(
    "Hill forest between 100 m and 1300 m altitude.",
    "habitat; altitude",
    sec = "Ecology",
    p = "p81"
  ),
  s$un(
    "It has been collected between August and February.",
    "phenology | collection months (not stated as flowering; question 6)",
    sec = "Ecology",
    p = "p81"
  ),
  s$un(
    "Nothing is known about its biology, no insect visitors have been observed, nor have the host species been recorded.",
    "flower visitors not observed; host unknown",
    sec = "Ecology",
    p = "p81"
  )
)

# ---- Copy-down ----------------------------------------------------------------
# Family and genus values copied to member taxa silent on the trait. Every
# value is a single, unqualified level and every member that scores the trait
# agrees (checked by hand):
#   - family parasitic 'root_parasitic' is not copied to B. hansenii, which
#     scores it itself (Ecology, Hambali 1980).
#   - family inflorescence_type 'spike' goes to Helosis ruficeps only; the
#     Balanophora genus states 'spicate' itself (most specific rank).
#   - family fruit_type 'achene' is not copied: every member says 'Fruits and
#     Seeds unknown' (organ unknown; Beentje_2023 question 12).
#   - genus sex_type and perianth symmetry name two levels: not copied.
#   - genus anther dehiscence goes to the three species silent on it.
#   - genus nectar production (female inflorescences) goes to the four
#     species not named in the genus Ecology.
own <- bind_rows(
  b_abbreviata,
  b_fungosa,
  b_hansenii,
  b_latisepala,
  b_papuana,
  b_reflexa,
  h_ruficeps
)
higher <- bind_rows(family, balanophora_g, helosis_g, b_fungosa_higher)
bal <- c(
  "Balanophora abbreviata",
  "Balanophora fungosa subsp. indica var. indica",
  "Balanophora hansenii",
  "Balanophora latisepala",
  "Balanophora papuana",
  "Balanophora reflexa"
)
allm <- c(bal, "Helosis ruficeps")
copy <- bind_rows(
  tibble(
    from = "Balanophoraceae",
    trait = c(
      "plant_growth_form",
      "plant_photosynthetic_organ",
      "flower_structural_sex_type",
      "flower_ovary_position"
    ),
    to = list(allm)
  ),
  tribble(
    ~from, ~trait, ~to,
    "Balanophoraceae", "parasitic", setdiff(allm, "Balanophora hansenii"),
    "Balanophoraceae", "inflorescence_type", "Helosis ruficeps",
    "Balanophora", "inflorescence_type", bal,
    "Balanophora", "pollination_syndrome", bal,
    "Balanophora", "flower_anther_dehiscence",
    c(
      "Balanophora abbreviata",
      "Balanophora fungosa subsp. indica var. indica",
      "Balanophora reflexa"
    ),
    "Balanophora", "flower_nectar_production",
    c(
      "Balanophora abbreviata",
      "Balanophora hansenii",
      "Balanophora latisepala",
      "Balanophora reflexa"
    ),
    "Helosis", "sex_type", "Helosis ruficeps"
  )
) |>
  unnest(to)

copied <- copy |>
  inner_join(
    higher |> filter(trait != ""),
    by = c(from = "taxon_name", "trait"),
    relationship = "many-to-many"
  ) |>
  mutate(
    notes = str_squish(paste("Copied from the", from, "description.", notes)),
    taxon_rank = if_else(
      taxon_rank == "family" | taxon_rank == "genus",
      if_else(str_detect(to, "var\\."), "variety", "species"),
      taxon_rank
    ),
    population_region = ""
  ) |>
  select(-from) |>
  rename(taxon_name = to)

# Copy-down checks: each source row is unqualified and single-level, and no
# target already scores the trait with the same entity.
stopifnot(
  nrow(copied) == nrow(copy),
  all(copied$commonness_qualifier == ""),
  !any(str_detect(copied$trait_value_clean, " ")),
  nrow(semi_join(copied, own, by = c("taxon_name", "trait", "entity_measured"))) == 0
)

d <- bind_rows(higher, own, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Kiew_2010/raw/trait_data.csv", na = "")
write_csv(flora_unmapped_summary(d), "data/Kiew_2010/raw/unmapped_terms.csv", na = "")

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Balanophoraceae", "family", "accepted", "",
  "Balanophora", "genus", "accepted", "",
  "Balanophora abbreviata", "species", "accepted", "Not recorded in Peninsular Malaysia for fifty years.",
  "Balanophora fungosa", "species", "accepted", "Species heading with Distribution and Taxonomy only; described at variety rank.",
  "Balanophora fungosa subsp. indica", "subspecies", "accepted", "Subspecies heading with Distribution, Ecology and Taxonomy only.",
  "Balanophora fungosa subsp. indica var. indica", "variety", "accepted", "Only infraspecific taxon in the region; its description is scored at variety rank.",
  "Balanophora hansenii", "species", "accepted", "Endemic; female plants first described here (FRI 21959).",
  "Balanophora latisepala", "species", "accepted", "",
  "Balanophora papuana", "species", "accepted", "",
  "Balanophora reflexa", "species", "accepted", "Possibly extinct in Peninsular Malaysia (Ecology).",
  "Helosis", "genus", "accepted", "",
  "Helosis ruficeps", "species", "accepted", "Endemic."
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Kiew_2010/raw/taxon_flags.csv", na = "")
