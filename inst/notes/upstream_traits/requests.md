# Trait requests from MYtraits to the AusTraits Plant Dictionary

Generated 2026-11-04 by `inst/notes/upstream_traits/make_report.R`. Do not edit by hand.

MYtraits extracts traits from Malaysian flora treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and Sarawak). These characters are described often but have no trait in `traits.yml`. We ask only for the easy ones: each copies an existing APD trait for a different organ, so the definition, type and units can follow that template.

Counts come from 25 flora datasets. Taxa are summed across datasets. Upstream trait list checked against austraits.build `develop` branch.

## Requested now

| Proposed trait | Type | Modelled on | Flora wording (organ \| character) | Taxa | Datasets | Families | Example (source, printed page) | Notes |
|---|---|---|---|---:|---:|---:|---|---|
| `leaf_lateral_vein_pairs_count` | numeric ({count}) | `leaflet_count` | lateral veins \| count; leaf lateral veins \| count; veins \| count | 630 | 11 | 10 | "lateral veins 7--12 pairs" (Kochummen_2000, p. 185) | Pairs of secondary veins. |
| `stipule_length` | numeric (mm) | `petiole_length` | stipules \| size | 384 | 6 | 5 | "Stipules to 5 x 0.8 cm" (Ashton_2004, p. 92) | Width often given too. |
| `petiole_hairs` | categorical (glabrous / hairy) | `stem_hairs` | petiole \| indumentum; rachis \| indumentum | 272 | 10 | 9 | "greyish brown short-hairy" (Ashton_2004, p. 73) | Includes the rachis of compound leaves. |
| `bark_inner_colour` | categorical | `bark_colour` | inner bark \| colour | 225 | 6 | 5 | "inner bark homogeneous pale brown" (Ashton_2004, p. 79) | Slash colour of tree floras; levels as bark_colour. |
| `flower_ovary_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | ovary \| indumentum | 215 | 8 | 7 | "pubescent around base" (Ashton_2004, p. 79) |  |
| `sapwood_colour` | categorical | `bark_colour` | sapwood \| colour | 211 | 9 | 7 | "Sapwood yellowish" (Soepadmo_2000, p. 7) | Slash colour of tree floras; levels as bark_colour. |
| `leaf_acumen_length` | numeric (mm) | `petiole_length` | leaf acumen \| size | 174 | 4 | 4 | "acumen c. 10 mm long" (Soepadmo_2000, p. 8) | Length of the drip tip. |
| `flower_petal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | petals \| indumentum | 138 | 7 | 6 | "petals often thick, with few divisions at apex, the sides often incurved (‘flanged’) and with 2 basal pits, nearly all species have dense indumentum inside but with glabrous areas between keel and ..." (Coode_2018, p. 36) |  |
| `flower_style_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | style \| indumentum | 126 | 5 | 4 | "glabrous" (Ashton_2004, p. 75) |  |
| `flower_sepal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | sepals \| indumentum | 114 | 5 | 5 | "sepals persistent after petals and stamens, 5--6 mm long, glabrous to sparsely hairy outside, glabrous inside, verrucose outside at anthesis" (Coode_2018, p. 52) |  |
| `inflorescence_hairs` | categorical (glabrous / hairy) | `stem_hairs` | inflorescences \| indumentum | 103 | 8 | 7 | "often brown scurfy" (DeWilde_2018, p. 184) |  |
| `stipule_hairs` | categorical (glabrous / hairy) | `stem_hairs` | stipules \| indumentum | 95 | 5 | 4 | "hispid with brown hairs" (Kochummen_2000, p. 185) |  |
| `flower_filament_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | filaments \| indumentum | 89 | 7 | 6 | "filaments glabrous or hairy" (Adema_1996, p. 263) |  |
| `flower_staminodes_count` | numeric ({count}) | `flower_fertile_stamens_count` | staminodes \| count | 87 | 4 | 3 | "staminodes 6--12 or absent" (Soepadmo_2000, p. 1) |  |
| `petiolule_length` | numeric (mm) | `petiole_length` | petiolules \| size | 81 | 2 | 2 | "petiolules 1--8 mm long" (Adema_1996, p. 273) |  |
| `flower_petal_count` | numeric ({count}) | `flower_perianth_parts_count` | petals \| count | 79 | 6 | 5 | "petals absent or 2--6" (Adema_1996, p. 263) | flower_perianth_parts_count counts all perianth parts together. |
| `leaf_domatia` | categorical (absent / present) |  | domatia \| presence | 33 | 1 | 1 | "without domatia" (Coode_2018, p. 52) |  |

## Not requested now

10 further characters need new categories or concepts, so we are not asking for them yet: `leaf_texture`, `leaf_tertiary_venation_pattern`, `leaf_midrib_upper_surface`, `fruit_shape`, `stipule_persistence`, `fruit_wall_texture`, `bract_persistence`, `inflorescence_position`, `fruit_mesocarp_texture`, `fruit_endocarp_texture`.

## Already in AusTraits since our last request

None yet.

