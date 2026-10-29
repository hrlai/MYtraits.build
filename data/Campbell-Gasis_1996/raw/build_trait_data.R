# Hand extraction of Scyphostegiaceae (E.J.F. Campbell-Gasis 1996, Tree Flora
# of Sabah and Sarawak vol. 2: 375--378). Run from the repository root.
#
# Source: inst/extdata/myflora/pdf/TFSS/Scyphostegiaceae.pdf, the 2007
# electronic version of the 1996 volume (L.G. Saw & B. Aslina); text_layer =
# born_digital in assets/myflora_metadata.csv. Rows come from the PDF text
# layer (pdftools::pdf_text). flora_layer_flags() flagged no bad glyphs and no
# page-number breaks on the treatment pages; PDF p. 9 (printed 377, Fig. 1) is
# a plate with no text layer, so its image was checked for the caption only
# (voucher). PDF page = printed page - 368. Treatment: PDF pp. 7--10.
#
# One family, one genus, one species: rows are written by hand with
# flora_hand() (Yao_2010 / Berhaman_1995 pattern). The family is monogeneric,
# so there is no family description (TFSS guide); the description under
# Scyphostegia is the genus description. Genus values are copied down to the
# species by hand under the flora_copy_down() rules (unqualified, single
# level, never numbers, species silent on the trait). The source repair in
# raw/source_fixes.csv ("chartaccous") is already applied below.
# Writes trait_data.csv, unmapped_terms.csv and taxon_flags.csv.

source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements

pg <- set_names(map(375:378, \(p) c(p - 368L, p)), paste0("p", 375:378))
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

# Typical range with a parenthetical maximum, a--b(--c): typical range on
# `usually` rows, extreme on an `extreme_range` row (flora_num_rows() rule).
rng_x <- function(h, trait, raw, lo, hi, xhi, unit, ...) {
  bind_rows(
    h$rng(trait, raw, lo, hi, unit, q = "usually", ...),
    h$row(trait, raw, xhi, xhi, "maximum", unit, "extreme_range", ...)
  )
}

# ---- Family -----------------------------------------------------------------
f <- mk("Scyphostegiaceae", "family", "family", sec = "Family")
family <- bind_rows(
  f$un(
    "A monogeneric family with one species endemic to Borneo.",
    "family | genus and species count; distribution",
    p = "p375"
  ),
  f$un(
    "The systematic position of the family has been controversial. ... The interpretation of the inflorescence structure used here differs from that of van Steenis l.c. and was kindly provided by Dr. K. M. Wong.",
    "taxonomy",
    sec = "Family Taxonomy",
    p = "p375"
  )
)

# ---- Scyphostegia (genus description) ---------------------------------------
g <- mk("Scyphostegia", "genus", "genus", sec = "Genus description")
genus <- bind_rows(
  g$row("sex_type", "Dioecious shrubs or trees", "dioecious", "dioecious", p = "p375"),
  g$row(
    "plant_growth_form",
    "Dioecious shrubs or trees",
    "shrubs trees",
    "shrub tree",
    p = "p375"
  ),
  g$un("without exudate", "exudate | absent (negative phrase)", p = "p375"),
  g$un(
    "Twigs often strongly 3--4-angular, slender, more or less zig-zag, brown, nodes swollen.",
    "twigs | angularity (often), zig-zag (hedged), colour, nodes",
    p = "p375"
  ),
  g$un(
    "Stipules free, triangular, caducous.",
    "stipules | fusion, shape, persistence",
    p = "p375"
  ),
  g$row(
    "leaf_compoundness",
    "Leaves simple, alternate, pinnately veined",
    "simple",
    "simple",
    p = "p375"
  ),
  g$row(
    "leaf_phyllotaxis",
    "Leaves simple, alternate, pinnately veined",
    "alternate",
    "alternate",
    p = "p375"
  ),
  g$un("pinnately veined", "leaf | venation", p = "p375"),
  g$row(
    "inflorescence_type",
    "Inflorescence basic units of short-peduncled racemes",
    "racemes",
    "raceme",
    p = "p375",
    notes = "The basic unit is scored; the arrangement of units (axillary or on a terminal main stalk) is kept unmapped."
  ),
  g$un(
    "bearing 1--12 (in male) or 1--2 (in female) serially imbricate, tubular, obtuse bracts, with each bract subtending a single flower; the tubular-bracteate raceme units solitary in the distal leaf axils or arranged on a slender flowering main stalk terminal to a leafy branch",
    "inflorescence | bract number and form; position of raceme units (axillary or on a terminal main stalk)",
    p = "p375"
  ),
  g$row(
    "flower_structural_sex_type",
    "Flowers unisexual, radially symmetrical",
    "unisexual",
    "unisexual",
    p = "p375"
  ),
  g$row(
    "flower_perianth_symmetry",
    "Flowers unisexual, radially symmetrical",
    "radially symmetrical",
    "actinomorphic_general",
    p = "p375"
  ),
  g$un(
    "developing one at time from the base upwards on each raceme-like unit; pedicel flattened, persistent",
    "flower | anthesis sequence; pedicel | shape, persistence",
    p = "p375"
  ),
  g$row(
    "flower_perianth_parts_count",
    "perianth 6-lobed (3 outer and 3 inner tepals)",
    "6",
    "6",
    "mean",
    "{count}",
    p = "p375",
    notes = "Perianth lobes (tepals) counted as perianth parts."
  ),
  g$row(
    "flower_perianth_whorls_count",
    "perianth 6-lobed (3 outer and 3 inner tepals)",
    "2",
    "2",
    "mean",
    "{count}",
    p = "p375",
    notes = "3 outer and 3 inner tepals read as two whorls."
  ),
  g$row(
    "flower_perianth_merism",
    "perianth 6-lobed (3 outer and 3 inner tepals)",
    "3",
    "3",
    "mean",
    "{count}",
    p = "p375",
    notes = "3 tepals per whorl read as merism 3."
  ),
  g$un(
    "imbricate in bud, orbicular, petaloid, connate, fleshy",
    "perianth | aestivation, shape, petaloid, fusion (degree not stated), texture",
    p = "p375"
  ),
  g$un(
    "male flowers: pedicels thin, dry and membranous, lengthwise 2-veined; perianth lobes shorter than the tube, inner ones shorter than and alternate with the outer ones, distinctly thickened at apex",
    "male flower | pedicel texture and veins; perianth lobe vs tube length, inner vs outer lobes",
    em = "male flower",
    p = "p375"
  ),
  g$row(
    "flower_fertile_stamens_count",
    "stamens 3, connate into in a column",
    "3",
    "3",
    "mean",
    "{count}",
    em = "male flower",
    p = "p375"
  ),
  g$un(
    "stamens 3, connate into in a column",
    "stamens | fused into a column (candidate flower_filament / flower_filament_fusion)",
    em = "male flower",
    p = "p375"
  ),
  g$row(
    "flower_anther_connective_extension",
    "with common apical connective produced above the anthers",
    "connective produced above the anthers",
    "present_general",
    em = "male flower",
    p = "p375"
  ),
  g$un("anthers 4-locular", "anther | locule number", em = "male flower", p = "p375"),
  g$row(
    "flower_anther_orientation",
    "anthers 4-locular, extrorse, dehiscing lengthwise",
    "extrorse",
    "extrorse",
    em = "male flower",
    p = "p375"
  ),
  g$row(
    "flower_anther_dehiscence",
    "anthers 4-locular, extrorse, dehiscing lengthwise",
    "dehiscing lengthwise",
    "longitudinal_slit",
    em = "male flower",
    p = "p375"
  ),
  g$un(
    "disc-glands 3, oblong, fleshy, opposite the stamens",
    "disc glands | number, shape, position",
    em = "male flower",
    p = "p375"
  ),
  g$un(
    "female flowers: pedicels shorter than in males; perianth lobes longer than the tube, equal, more or less persistent, recurved in fruit",
    "female flower | pedicel length vs male; perianth lobe vs tube length, persistence (hedged), posture in fruit",
    em = "female flower",
    p = "p375"
  ),
  g$un(
    "disc present, disc-glands absent",
    "disc | presence; disc glands absent (negative phrase)",
    em = "female flower",
    p = "p376"
  ),
  g$row(
    "flower_ovary_position",
    "ovary superior, 1-locular, globose",
    "superior",
    "superior",
    em = "female flower",
    p = "p376"
  ),
  g$un(
    "ovary superior, 1-locular, globose",
    "ovary | locule number (not scored as carpels: 9--12 stigmatic lobes and parietal placentation), shape",
    em = "female flower",
    p = "p376"
  ),
  g$un(
    "style discoid with a narrow central aperture, stigmatic lobes 9--12, fleshy, largely fused to form a thick plate-like stigma with recurved margin",
    "style, stigma | shape, lobe number, fusion",
    em = "female flower",
    p = "p376"
  ),
  g$un(
    "ovules numerous, crowded on the slightly convex base of the ovary, surrounded at base by a fleshy, collar-like aril",
    "ovules | number, position, aril",
    em = "female flower",
    p = "p376"
  ),
  g$row(
    "flower_gynoecium_placentation",
    "placentation parietal",
    "parietal",
    "parietal",
    em = "female flower",
    p = "p376"
  ),
  g$row(
    "fruit_type",
    "Fruits fleshy, dehiscent capsules",
    "capsules",
    "capsule",
    p = "p376"
  ),
  g$row(
    "fruit_fleshiness",
    "Fruits fleshy, dehiscent capsules",
    "fleshy",
    "fleshy",
    p = "p376"
  ),
  g$row(
    "fruit_dehiscence",
    "Fruits fleshy, dehiscent capsules",
    "dehiscent",
    "dehiscent",
    p = "p376"
  ),
  g$un(
    "crowned by persistent styles and stigmas, 9--12-ribbed, at maturity breaking up at the mouth into 9--12 segments",
    "fruit | persistent style, ribs, dehiscence pattern",
    p = "p376"
  ),
  g$un(
    "Seeds numerous, dry, densely packed, funiculate",
    "seed | number, texture, packing, funicle",
    p = "p376"
  ),
  g$row(
    "seed_surface_hairs",
    "shortly erect-patent hairy over whole surface",
    "hairy",
    "hairs",
    p = "p376"
  ),
  g$un(
    "shortly erect-patent hairy over whole surface, their needle-like bases surrounded and partly joined with spongy tissue from the base",
    "seed | hair length and posture; seed base shape, spongy tissue",
    p = "p376"
  )
)

# ---- Scyphostegia borneensis -------------------------------------------------
s <- mk("Scyphostegia borneensis", "species", region = SS)
sp <- bind_rows(
  s$row(
    "plant_growth_form",
    "Shrub or tree to 20 m tall",
    "shrub tree",
    "shrub tree",
    p = "p376"
  ),
  s$row("plant_height", "Shrub or tree to 20 m tall", "20", "20", "maximum", "m", p = "p376"),
  s$row(
    "plant_diameter_breast_height",
    "Shrub or tree to 20 m tall, 40 cm diameter",
    "40",
    "40",
    "maximum",
    "cm",
    p = "p376",
    notes = "'to' read as governing both height and diameter; height of measurement not stated."
  ),
  s$un("low branching; crown spreading", "stem, crown | branching height, crown shape", p = "p376"),
  s$row(
    "bark_colour",
    "Bark dark green to dark brown or blackish, smooth, thin",
    "dark_green dark_brown blackish",
    "green brown_dark black",
    p = "p376"
  ),
  s$row(
    "bark_texture",
    "Bark dark green to dark brown or blackish, smooth, thin",
    "smooth",
    "smooth",
    p = "p376"
  ),
  s$un(
    "thin, sometimes lenticellate",
    "bark | thickness (qualitative), lenticels (sometimes)",
    p = "p376"
  ),
  s$un(
    "inner bark yellow to brown or grey. Wood white or slightly yellowish.",
    "inner bark, wood | colour",
    p = "p376"
  ),
  s$un(
    "Twigs sometimes lenticellate, with knob-like stipule scars",
    "twigs | lenticels (sometimes), stipule scars",
    p = "p376"
  ),
  s$un(
    "stipules 1--2 x 1 mm",
    "stipules | length and width (candidate stipule size)",
    p = "p376"
  ),
  s$un(
    "Leaves drying chartaceous",
    "leaf | texture on drying | source typo 'chartaccous' repaired (raw/source_fixes.csv)",
    p = "p376"
  ),
  s$row(
    "leaf_shape",
    "narrow-oblong to obovate-oblong",
    "narrow-oblong",
    "narrowly_oblong",
    p = "p376",
    notes = "'obovate-oblong' has no level and is kept unmapped."
  ),
  s$un("obovate-oblong", "leaf | shape 'obovate-oblong' (no level)", p = "p376"),
  rng_x(s, "leaf_length", "7--18(--24) x 2.5--6.8 (--8.5) cm", "7", "18", "24", "cm", p = "p376"),
  rng_x(s, "leaf_width", "7--18(--24) x 2.5--6.8 (--8.5) cm", "2.5", "6.8", "8.5", "cm", p = "p376"),
  s$row(
    "leaf_base_shape",
    "base rounded, often decurrent down the petiole",
    "rounded",
    "rounded",
    p = "p376"
  ),
  s$row(
    "petiole_shape",
    "base rounded, often decurrent down the petiole",
    "decurrent",
    "decurrent",
    q = "often",
    p = "p376",
    notes = "Leaf base decurrent down the petiole."
  ),
  s$row(
    "leaf_margin",
    "margin serrulate, the teeth with thickened tips",
    "serrulate",
    "toothed_serrate",
    p = "p376"
  ),
  s$un("the teeth with thickened tips", "leaf margin | tooth tips", p = "p376"),
  s$row(
    "leaf_apex_shape",
    "apex acute to acuminate, acumen to c. 13 mm long",
    "acute acuminate",
    "acute acuminate",
    p = "p376"
  ),
  s$un("acumen to c. 13 mm long", "leaf apex | acumen length", p = "p376"),
  s$un(
    "lateral veins 6--11 pairs, spreading at a narrow angle from the midrib, arching in the lower half and joining in faint loops 1--2 mm from the margin, flat or sunken above; intercostal veins scalariform, more or less parallel to the midrib",
    "veins | number, angle, looping, relief; intercostal veins pattern (hedged)",
    p = "p376"
  ),
  rng_x(s, "petiole_length", "petioles 2--5(--6) mm long", "2", "5", "6", "mm", p = "p376"),
  s$row(
    "petiole_width",
    "petioles 2--5(--6) mm long, 1(--3) mm thick",
    "1",
    "1",
    "mean",
    "mm",
    "usually",
    p = "p376",
    notes = "Petiole thickness scored as petiole_width."
  ),
  s$row(
    "petiole_width",
    "petioles 2--5(--6) mm long, 1(--3) mm thick",
    "3",
    "3",
    "maximum",
    "mm",
    "extreme_range",
    p = "p376",
    notes = "Petiole thickness scored as petiole_width."
  ),
  s$un("furrowed above", "petiole | furrow", p = "p376"),
  # Male inflorescence
  s$rng(
    "inflorescence_length",
    "Inflorescences: males 6.5--30 cm long",
    "6.5",
    "30",
    "cm",
    em = "male inflorescence",
    p = "p376"
  ),
  s$un(
    "raceme units 1--2 cm long",
    "male raceme unit | length (candidate)",
    em = "male inflorescence",
    p = "p376"
  ),
  s$rng(
    "peduncle_length",
    "peduncles 2.5--20 mm long",
    "2.5",
    "20",
    "mm",
    em = "male raceme unit",
    p = "p376",
    notes = "Peduncle read as that of the raceme unit (genus: 'short-peduncled racemes')."
  ),
  s$rng(
    "flower_bract_length",
    "bracts 5--10 mm long, 2--3 mm wide",
    "5",
    "10",
    "mm",
    em = "male flower",
    p = "p376",
    notes = "Each tubular bract subtends a single flower (genus description)."
  ),
  s$un("bracts ... 2--3 mm wide", "bract | width (candidate)", em = "male flower", p = "p376"),
  s$rng(
    "pedicel_length",
    "pedicels 4--6 mm long, c. 1 mm thick",
    "4",
    "6",
    "mm",
    em = "male flower",
    p = "p376"
  ),
  s$un("pedicels ... c. 1 mm thick", "pedicel | thickness", em = "male flower", p = "p376"),
  # Female inflorescence
  s$rng(
    "inflorescence_length",
    "females 5--8.5 cm long",
    "5",
    "8.5",
    "cm",
    em = "female inflorescence",
    p = "p376"
  ),
  s$un(
    "raceme units 5--8 mm long",
    "female raceme unit | length (candidate)",
    em = "female inflorescence",
    p = "p376"
  ),
  s$rng(
    "peduncle_length",
    "peduncles 12--35 mm long",
    "12",
    "35",
    "mm",
    em = "female raceme unit",
    p = "p376",
    notes = "Peduncle read as that of the raceme unit (genus: 'short-peduncled racemes')."
  ),
  s$rng(
    "flower_bract_length",
    "bracts 1--4 x 2--3 mm",
    "1",
    "4",
    "mm",
    em = "female flower",
    p = "p376",
    notes = "Each tubular bract subtends a single flower (genus description). First dimension read as length."
  ),
  s$un("bracts 1--4 x 2--3 mm", "bract | width (candidate)", em = "female flower", p = "p376"),
  s$un(
    "pedicels shorter than in the males",
    "pedicel | length relative to male",
    em = "female flower",
    p = "p376"
  ),
  # Male flower
  s$rng(
    "flower_tube_length",
    "Flower: males: perianth tube 4--5 mm long",
    "4",
    "5",
    "mm",
    em = "male flower",
    p = "p376",
    notes = "Perianth (tepal) tube scored as flower tube."
  ),
  s$un(
    "the outer lobes orbicular-oblong or spathulate, c. 3 x 2 mm, the inner ones c. 2 x 1.5 mm",
    "tepals | shape, outer and inner lobe length and width (candidate tepal size)",
    em = "male flower",
    p = "p376"
  ),
  s$un(
    "disc-glands c. 0.75 x 0.4 mm",
    "disc glands | size",
    em = "male flower",
    p = "p376"
  ),
  s$un(
    "staminal column 2--3 mm long",
    "staminal column | length (candidate; column of fused stamens, not a single stamen)",
    em = "male flower",
    p = "p376"
  ),
  s$row(
    "flower_anther_length",
    "anthers c. 2 mm long",
    "2",
    "2",
    "mean",
    "mm",
    em = "male flower",
    p = "p376"
  ),
  # Female flower
  s$row(
    "flower_tube_length",
    "females: perianth tube c. 2 mm long",
    "2",
    "2",
    "mean",
    "mm",
    em = "female flower",
    p = "p376",
    notes = "Perianth (tepal) tube scored as flower tube."
  ),
  s$un(
    "lobes c. 6 x 4 mm",
    "tepals | lobe length and width (candidate tepal size)",
    em = "female flower",
    p = "p376"
  ),
  s$un("ovary c. 4 mm", "ovary | size (dimension not stated)", em = "female flower", p = "p376"),
  s$row("fruit_length", "Fruits c. 18 x 15 mm", "18", "18", "mean", "mm", p = "p376"),
  s$row("fruit_width", "Fruits c. 18 x 15 mm", "15", "15", "mean", "mm", p = "p376"),
  s$rng("seed_length", "Seeds 5--6 x 0.8--1 mm", "5", "6", "mm", p = "p376"),
  s$rng("seed_width", "Seeds 5--6 x 0.8--1 mm", "0.8", "1", "mm", p = "p376"),
  s$un("funicle c. 3 mm long", "funicle | length", p = "p376"),
  s$un(
    "In Sabah, known from the Crocker Range through the interior to the east coast and southwards from Sapulut to Tawau; also in Kudat at the northern most tip of Sabah. In Sarawak, on the Rejang river from Kapit to Belaga and headwaters of the rivers to the north; also on the Balleh river south of Kapit and on the Baram river near Lobang Garam. Also known in Kalimantan.",
    "distribution",
    sec = "Distribution",
    p = "p376"
  ),
  s$un(
    "In Sabah, found in primary rain forest from flat and sloping ridge areas to steep-sided riverbanks, streams and gorges, and also in logged and disturbed forests.",
    "habitat; disturbance",
    sec = "Ecology",
    region = "Sabah",
    p = "p376"
  ),
  s$un(
    "In Sarawak, often growing along banks of swift-flowing rivers, at 20--1700 m.",
    "habitat (riparian); altitude",
    sec = "Ecology",
    region = "Sarawak",
    p = "p376"
  ),
  s$un(
    "Flowering and fruiting all the year around with local variation.",
    "flowering and fruiting time | all year 'with local variation' (qualified; not scored)",
    sec = "Ecology",
    p = "p376"
  ),
  s$un(
    "In Sarawak, the leaves are pounded together with tuba roots and used as fish-poison.",
    "uses (fish poison)",
    sec = "Uses",
    region = "Sarawak",
    p = "p378"
  ),
  s$un(
    "This species can be mistaken for Flacourtia rukam (Flacourtiaceae) ... the 4-angular, zig-zag twigs, the serrulate leaf margin, and the fruit being a dehiscent fleshy capsule rather than an indehiscent drupe clearly distinguish S. borneensis from F. rukam.",
    "differential note (twigs, leaf margin, fruit)",
    sec = "Notes",
    p = "p378"
  )
)

# ---- Copy-down ----------------------------------------------------------------
# Monotypic genus in a monogeneric family: single, unqualified genus values are
# copied to the species, which is silent on every trait below. Numbers
# (perianth parts, whorls, merism, stamens) and the multi-level growth form are
# not copied; the species scores its own growth form.
copy_traits <- c(
  "sex_type",
  "leaf_compoundness",
  "leaf_phyllotaxis",
  "inflorescence_type",
  "flower_structural_sex_type",
  "flower_perianth_symmetry",
  "flower_anther_connective_extension",
  "flower_anther_orientation",
  "flower_anther_dehiscence",
  "flower_ovary_position",
  "flower_gynoecium_placentation",
  "fruit_type",
  "fruit_fleshiness",
  "fruit_dehiscence",
  "seed_surface_hairs"
)
copied <- genus |>
  filter(trait %in% copy_traits) |>
  mutate(
    taxon_name = "Scyphostegia borneensis",
    taxon_rank = "species",
    notes = str_squish(paste("Copied from the Scyphostegia description.", notes))
  )

stopifnot(
  nrow(copied) == length(copy_traits),
  all(copied$commonness_qualifier == ""),
  all(copied$value_type == "mode"),
  !any(str_detect(copied$trait_value_clean, " ")),
  !any(copied$trait %in% sp$trait)
)

d <- bind_rows(family, genus, sp, copied) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)

stopifnot(all(d$trait[d$trait != ""] %in% names(traits)))
write_csv(d, "data/Campbell-Gasis_1996/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(d),
  "data/Campbell-Gasis_1996/raw/unmapped_terms.csv",
  na = ""
)

n_rows <- count(d, taxon_name)
tribble(
  ~taxon_name, ~taxon_rank, ~status, ~flag,
  "Scyphostegiaceae", "family", "accepted", "Monogeneric family: no family description (TFSS guide); family statement and Taxonomy kept unmapped.",
  "Scyphostegia", "genus", "accepted", "Monotypic genus; its description is copied down to the species.",
  "Scyphostegia borneensis", "species", "accepted", "Endemic to Borneo; also known in Kalimantan."
) |>
  left_join(n_rows, by = "taxon_name") |>
  mutate(n_rows = coalesce(n, 0L), .before = flag) |>
  select(-n) |>
  write_csv("data/Campbell-Gasis_1996/raw/taxon_flags.csv", na = "")
