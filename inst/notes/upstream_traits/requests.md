# Trait requests from MYtraits to the AusTraits Plant Dictionary

Generated 2026-11-06 by `inst/notes/upstream_traits/make_report.R`. Do not edit by hand.

MYtraits extracts traits from Malaysian flora treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and Sarawak). These characters are described often but have no trait in `traits.yml`. We ask only for the easy ones: each copies an existing APD trait for a different organ, so the definition, type and units can follow that template.

Counts come from 28 flora datasets. Taxa are summed across datasets. Upstream trait list checked against austraits.build `develop` branch.

## Requested now

| Proposed trait | Type | Modelled on | Flora wording (organ \| character) | Taxa | Datasets | Families | Example (source, printed page) | Notes |
|---|---|---|---|---:|---:|---:|---|---|
| `leaf_lateral_vein_pairs_count` | numeric ({count}) | `leaflet_count` | leaf lateral veins \| count; lateral veins \| count; veins \| count | 814 | 13 | 12 | "lateral veins 8--22 pairs" (Ashton_2004, p. 73) | Pairs of secondary veins. |
| `stipule_length` | numeric (mm) | `petiole_length` | stipules \| size | 400 | 7 | 5 | "Stipules to 5 x 0.8 cm" (Ashton_2004, p. 92) | Width often given too. |
| `petiole_hairs` | categorical (glabrous / hairy) | `stem_hairs` | petiole \| indumentum; rachis \| indumentum | 368 | 13 | 11 | "sparsely pale hairy" (Turner_2014, p. 12) | Includes the rachis of compound leaves. |
| `fruit_wing_length` | numeric (cm) | `fruit_length` | fruit calyx \| size | 254 | 1 | 1 | "calyx tube to 1.2 cm diameter" (Ashton_2004, p. 73) | Dipterocarp fruit wings (enlarged calyx lobes); main dispersal character (Ashton_2004 q1, #171). |
| `flower_ovary_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | ovary \| indumentum | 243 | 10 | 8 | "pubescent around base" (Ashton_2004, p. 79) |  |
| `bark_inner_colour` | categorical | `bark_colour` | inner bark \| colour | 239 | 7 | 5 | "inner bark homogeneous pale brown" (Ashton_2004, p. 79) | Slash colour of tree floras; levels as bark_colour. |
| `sapwood_colour` | categorical | `bark_colour` | sapwood \| colour | 222 | 10 | 7 | "Sapwood yellowish" (Soepadmo_2000, p. 7) | Slash colour of tree floras; levels as bark_colour. |
| `flower_sepal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | sepals \| indumentum | 212 | 6 | 6 | "hairy outside" (Turner_2014, p. 80) |  |
| `leaf_acumen_length` | numeric (mm) | `petiole_length` | leaf acumen \| size | 199 | 5 | 5 | "acumen c. 10 mm long" (Soepadmo_2000, p. 8) | Length of the drip tip. |
| `petiole_thickness` | numeric (mm) | `petiole_length` | petiole \| size | 187 | 7 | 7 | "0.5--1 mm thick" (Turner_2014, p. 12) | Unmapped petiole sizes are mostly thickness (Turner_2014, 115 taxa; #183); check for lengths missed. |
| `flower_petal_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | petals \| indumentum | 173 | 9 | 7 | "petals often thick, with few divisions at apex, the sides often incurved (‘flanged’) and with 2 basal pits, nearly all species have dense indumentum inside but with glabrous areas between keel and ..." (Coode_2018, p. 36) |  |
| `fruit_wing_count` | numeric ({count}) | `flower_perianth_parts_count` | fruit calyx \| count | 167 | 1 | 1 | "5-merous" (Ashton_2004, p. 63) | Number of wing-like calyx lobes (#171). |
| `flower_style_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | style \| indumentum | 151 | 7 | 5 | "glabrous" (Ashton_2004, p. 75) |  |
| `inflorescence_hairs` | categorical (glabrous / hairy) | `stem_hairs` | inflorescences \| indumentum | 138 | 11 | 9 | "often brown scurfy" (DeWilde_2018, p. 184) |  |
| `fruit_monocarp_count` | numeric ({count}) | `seeds_per_fruit` | monocarps \| count | 128 | 1 | 1 | "with 1–many seeds arranged in 1 or more rows" (Turner_2014, p. 2) | Monocarps per apocarpous fruit (Turner_2014 q1; #183). |
| `flower_petal_count` | numeric ({count}) | `flower_perianth_parts_count` | petals \| count | 122 | 8 | 6 | "petals absent or 2--6" (Adema_1996, p. 263) | flower_perianth_parts_count counts all perianth parts together. |
| `fruit_monocarp_stipe_length` | numeric (mm) | `pedicel_length` | monocarp stipe \| size | 111 | 1 | 1 | "stipes 3--10 mm long" (Turner_2014, p. 12) | Stipe of each monocarp (Turner_2014 q1; #183). |
| `petiolule_length` | numeric (mm) | `petiole_length` | petiolules \| size | 101 | 3 | 2 | "petiolules 1--8 mm long" (Adema_1996, p. 273) |  |
| `flower_filament_hairs` | categorical (glabrous / hairy) | `fruit_surface_hairs` | filaments \| indumentum | 98 | 8 | 6 | "filaments glabrous or hairy" (Adema_1996, p. 263) |  |
| `stipule_hairs` | categorical (glabrous / hairy) | `stem_hairs` | stipules \| indumentum | 97 | 6 | 4 | "hispid with brown hairs" (Kochummen_2000, p. 185) |  |
| `flower_staminodes_count` | numeric ({count}) | `flower_fertile_stamens_count` | staminodes \| count | 92 | 5 | 4 | "staminodes 6--12 or absent" (Soepadmo_2000, p. 1) |  |
| `buttress_height` | numeric (m) | `plant_height` | buttresses \| size; buttress \| size | 40 | 8 | 6 | "to 4 m tall and continuing as ribs up bole to 10 m" (Ashton_2004, p. 73) | Buttress height (#171). |
| `flower_anther_locule_count` | numeric ({count}) | `flower_fertile_stamens_count` | male flower anther locules \| count | 35 | 1 | 1 | "anther locules 12--20" (DeWilde_2018, p. 151) | Myristicaceae anther locules (DeWilde_2018 q3; #177). |
| `leaf_domatia` | categorical (absent / present) |  | domatia \| presence | 33 | 1 | 1 | "without domatia" (Coode_2018, p. 52) |  |

## Not requested now

15 further characters need new categories or concepts, so we are not asking for them yet: `leaf_texture`, `leaf_tertiary_venation_pattern`, `leaf_midrib_upper_surface`, `fruit_shape`, `stipule_persistence`, `bract_persistence`, `fruit_wall_texture`, `fruit_mesocarp_texture`, `inflorescence_position`, `fruit_endocarp_texture`, `exudate_colour`, `aril_colour`, `leaflet_glands`, `fruit_valve_texture`, `fruit_spines`.

## Already in AusTraits since our last request

None yet.

