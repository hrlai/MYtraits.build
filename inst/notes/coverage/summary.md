# What MYtraits holds: a coverage summary


Generated 2026-11-13 by `inst/notes/coverage/summary.qmd` from `export/data/curr/MYtraits.rds` (version 0.0.1, commit 4f3f4fc). Do not edit by hand.

MYtraits compiles plant trait data for Malaysia from two kinds of source. **Flora** data are characters transcribed from family treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and Sarawak) and taxon descriptions, so each value describes a species from herbarium specimens. **Field-measured** data come from studies that measured plants in plots, gardens or nurseries. Taxon names are aligned with taxonAlign to the World Checklist of Vascular Plants (WCVP), restricted to taxa with GBIF records in Malaysia, Brunei and Singapore; a taxon is one aligned name, which may be a species, an infraspecific taxon, or a genus or family placeholder (`Genus sp. [...]`).

## Overview

| Source         | Datasets |  Taxa | Species-rank taxa | Families | Traits |   Rows |
|:---------------|---------:|------:|------------------:|---------:|-------:|-------:|
| Flora          |       28 | 1,476 |             1,167 |       25 |    141 | 44,949 |
| Field-measured |       18 |   739 |               671 |       75 |     62 | 41,741 |
| All            |       46 | 2,018 |             1,648 |       85 |    200 | 86,690 |

Every taxon has a family.

A further 12,309 values were read from the sources but excluded during the build and are not counted above: missing unit conversion (10,393); value out of allowable range (1,728); observation excluded in metadata (169); unsupported trait value (19).

## Overlap between flora and field data

- **197** taxa have both flora and field-measured data (27% of field-measured taxa, 13% of flora taxa).
- **66** taxon × trait combinations have values from both sources, so flora and field values can be compared directly.
- For taxa in both, the median taxon has 27 flora traits and 17 field-measured traits.

Families with the most taxa in both:

| Family           | Taxa in both |
|:-----------------|-------------:|
| Dipterocarpaceae |           56 |
| Annonaceae       |           38 |
| Fagaceae         |           15 |
| Sapindaceae      |           15 |
| Fabaceae         |           14 |
| Burseraceae      |           13 |
| Moraceae         |           12 |
| Myristicaceae    |           10 |
| Melastomataceae  |            9 |
| Rutaceae         |            5 |

## Coverage by family

The 20 families with the most taxa, of 85. *Taxon × trait* counts the combinations with at least one value. *Fill* is that count as a share of taxa × traits recorded for the family.

| Family           | Taxa | Flora taxa | Field taxa | Traits | Taxon × trait |  Rows | Fill (%) |
|:-----------------|-----:|-----------:|-----------:|-------:|--------------:|------:|---------:|
| Dipterocarpaceae |  310 |        297 |         69 |    102 |          9045 | 24728 |       29 |
| Annonaceae       |  206 |        195 |         49 |     83 |          4304 |  6819 |       25 |
| Moraceae         |  175 |        171 |         16 |     85 |          2508 |  3425 |       17 |
| Fagaceae         |  112 |        105 |         22 |     89 |          3593 |  5506 |       36 |
| Sapindaceae      |   98 |         95 |         18 |     94 |          2709 |  4261 |       29 |
| Myristicaceae    |   92 |         82 |         20 |     90 |          2664 |  4520 |       32 |
| Burseraceae      |   70 |         68 |         15 |     92 |          1763 |  2568 |       27 |
| Fabaceae         |   69 |         58 |         25 |     94 |          1838 |  3293 |       28 |
| Rutaceae         |   68 |         68 |          5 |     86 |          1506 |  2036 |       26 |
| Elaeocarpaceae   |   59 |         58 |          5 |     90 |          2112 |  3157 |       40 |
| Pandanaceae      |   54 |         54 |          0 |     33 |           635 |   938 |       36 |
| Phyllanthaceae   |   48 |          0 |         48 |     35 |           638 |  1933 |       38 |
| Melastomataceae  |   46 |         43 |         12 |     48 |           545 |   815 |       25 |
| Meliaceae        |   43 |          0 |         43 |     36 |           571 |  1572 |       37 |
| Lauraceae        |   41 |          0 |         41 |     34 |           601 |  2051 |       43 |
| Rubiaceae        |   40 |          0 |         40 |     44 |           357 |   854 |       20 |
| Selaginellaceae  |   36 |         36 |          0 |     18 |           359 |   503 |       55 |
| Symplocaceae     |   35 |         34 |          4 |     68 |           816 |  1305 |       34 |
| Myrtaceae        |   35 |          0 |         35 |     37 |           471 |  1645 |       36 |
| Euphorbiaceae    |   31 |          0 |         31 |     45 |           543 |  3218 |       39 |

## Traits with the most taxa

The 20 traits with the most taxa, of 200.

| Trait                          | Taxa | Flora taxa | Field taxa | Families | Datasets |
|:-------------------------------|-----:|-----------:|-----------:|---------:|---------:|
| `plant_growth_form`            | 1412 |       1412 |          0 |       25 |       28 |
| `plant_height`                 | 1179 |       1101 |        106 |       47 |       26 |
| `leaf_shape`                   | 1164 |       1164 |          0 |       23 |       25 |
| `leaf_base_shape`              | 1091 |       1091 |          0 |       22 |       23 |
| `leaf_apex_shape`              | 1062 |       1062 |          0 |       24 |       26 |
| `petiole_length`               | 1001 |       1001 |          0 |       20 |       22 |
| `leaf_compoundness`            |  977 |        977 |          0 |       21 |       23 |
| `inflorescence_type`           |  969 |        969 |          0 |       20 |       22 |
| `leaf_length`                  |  963 |        963 |          0 |       22 |       25 |
| `leaf_width`                   |  929 |        929 |          0 |       20 |       21 |
| `plant_diameter_breast_height` |  929 |        708 |        259 |       60 |       22 |
| `flower_structural_sex_type`   |  916 |        916 |          0 |       16 |       17 |
| `fruit_type`                   |  910 |        910 |          0 |       20 |       22 |
| `fruit_length`                 |  879 |        879 |          0 |       21 |       22 |
| `fruit_width`                  |  824 |        824 |          0 |       19 |       20 |
| `inflorescence_length`         |  658 |        658 |          0 |       18 |       20 |
| `flower_perianth_symmetry`     |  644 |        644 |          0 |       11 |       12 |
| `flower_fertile_stamens_count` |  613 |        613 |          0 |       18 |       20 |
| `leaf_margin`                  |  605 |        605 |          0 |       20 |       22 |
| `leaf_phyllotaxis`             |  586 |        586 |          0 |       15 |       17 |

The 10 traits with the most field-measured taxa:

| Trait                     | Field taxa | Flora taxa | Datasets |
|:--------------------------|-----------:|-----------:|---------:|
| `leaf_mass_per_area`      |        572 |          0 |        7 |
| `leaf_area`               |        558 |          0 |        6 |
| `leaf_dry_matter_content` |        549 |          0 |        3 |
| `leaf_P_per_dry_mass`     |        384 |          0 |        4 |
| `leaf_N_per_dry_mass`     |        383 |          0 |        6 |
| `leaf_thickness`          |        380 |          0 |        4 |
| `wood_density`            |        363 |          1 |        6 |
| `leaf_dry_mass`           |        352 |          0 |        2 |
| `leaf_Ca_per_dry_mass`    |        297 |          0 |        3 |
| `leaf_K_per_dry_mass`     |        297 |          0 |        3 |
