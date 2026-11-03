# Trait requests from MYtraits to the AusTraits Plant Dictionary

Generated 2026-11-03 by `inst/notes/upstream_traits/make_report.R`. Do not edit by hand.

MYtraits extracts traits from Malaysian flora treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and Sarawak). These characters are described often but have no trait in `traits.yml`. We ask only for the easy ones: each copies an existing APD trait for a different organ, so the definition, type and units can follow that template.

Counts come from 22 flora datasets. Taxa are summed across datasets. Upstream trait list checked against austraits.build `develop` branch.

## Requested now

| Proposed trait | Type | Modelled on | Flora wording (organ \| character) | Taxa | Datasets | Families | Example (source, printed page) | Notes |
|---|---|---|---|---:|---:|---:|---|---|
| `leaf_lateral_vein_pairs_count` | numeric ({count}) | `leaflet_count` | lateral veins \| venation; lateral veins \| count; veins \| count | 318 | 8 | 7 | "lateral veins 6--10 pairs" (Soepadmo_2000, p. 7) | Pairs of secondary veins. |
| `petiole_hairs` | categorical (glabrous / hairy) | `stem_hairs` | petiole \| indumentum; rachis \| indumentum | 132 | 7 | 6 | "glabrous" (Soepadmo_2000, p. 7) | Includes the rachis of compound leaves. |
| `flower_petal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | petals \| indumentum | 128 | 6 | 5 | "petals often thick, with few divisions at apex, the sides often incurved (‘flanged’) and with 2 basal pits, nearly all species have dense indumentum inside but with glabrous areas between keel and ..." (Coode_2018, p. 36) |  |
| `sapwood_colour` | categorical | `bark_colour` | sapwood \| colour | 128 | 6 | 4 | "Sapwood yellowish" (Soepadmo_2000, p. 7) | Slash colour of tree floras; levels as bark_colour. |
| `flower_ovary_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | ovary \| indumentum | 126 | 6 | 5 | "ovary 3--5-locular, ovules 2 per locule, styles tapering to a point, if hairy at base then glabrous distally" (Coode_2018, p. 36) |  |
| `stipule_length` | numeric (mm) | `petiole_length` | stipules \| size | 115 | 4 | 3 | "c. 1.5 x 1 mm" (Soepadmo_2000, p. 7) | Width often given too. |
| `bark_inner_colour` | categorical | `bark_colour` | inner bark \| colour | 114 | 4 | 3 | "brownish yellow" (Soepadmo_2000, p. 7) | Slash colour of tree floras; levels as bark_colour. |
| `flower_sepal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | sepals \| indumentum | 111 | 4 | 4 | "sepals persistent after petals and stamens, 5--6 mm long, glabrous to sparsely hairy outside, glabrous inside, verrucose outside at anthesis" (Coode_2018, p. 52) |  |
| `flower_staminodes_count` | numeric ({count}) | `flower_fertile_stamens_count` | staminodes \| count | 87 | 4 | 3 | "staminodes 6--12 or absent" (Soepadmo_2000, p. 1) |  |
| `leaf_acumen_length` | numeric (mm) | `petiole_length` | leaf acumen \| size | 86 | 2 | 2 | "acumen c. 10 mm long" (Soepadmo_2000, p. 8) | Length of the drip tip. |
| `petiolule_length` | numeric (mm) | `petiole_length` | petiolules \| size | 81 | 2 | 2 | "petiolules 1--8 mm long" (Adema_1996, p. 273) |  |
| `flower_petal_count` | numeric ({count}) | `flower_perianth_parts_count` | petals \| count | 78 | 5 | 4 | "petals absent or 2--6" (Adema_1996, p. 263) | flower_perianth_parts_count counts all perianth parts together. |
| `flower_filament_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | filaments \| indumentum | 70 | 5 | 4 | "filaments glabrous or hairy" (Adema_1996, p. 263) |  |
| `inflorescence_hairs` | categorical (glabrous / hairy) | `stem_hairs` | inflorescences \| indumentum | 52 | 5 | 4 | "hairy" (Adema_1996, p. 362) |  |
| `flower_style_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | style \| indumentum | 42 | 4 | 3 | "glabrous" (Jones_1995, p. 361) |  |
| `leaf_domatia` | categorical (absent / present) |  | domatia \| presence | 33 | 1 | 1 | "without domatia" (Coode_2018, p. 52) |  |
| `stipule_hairs` | categorical (glabrous / hairy) | `stem_hairs` | stipules \| indumentum | 30 | 3 | 2 | "densely tomentose or woolly pubescent" (Soepadmo_2000, p. 95) |  |

## Not requested now

10 further characters need new categories or concepts, so we are not asking for them yet: `leaf_texture`, `leaf_tertiary_venation_pattern`, `fruit_wall_texture`, `leaf_midrib_upper_surface`, `fruit_shape`, `inflorescence_position`, `fruit_mesocarp_texture`, `fruit_endocarp_texture`, `stipule_persistence`, `bract_persistence`.

## Already in AusTraits since our last request

None yet.

