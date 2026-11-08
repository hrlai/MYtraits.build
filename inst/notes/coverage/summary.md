# What MYtraits holds: a coverage summary


Generated 2026-11-08 by `inst/notes/coverage/summary.qmd` from `export/data/curr/MYtraits.rds` (version 0.0.1, commit 450d116). Do not edit by hand.

MYtraits compiles plant trait data for Malaysia from two kinds of source. **Flora** data are characters transcribed from family treatments (Flora of Peninsular Malaysia, Tree Flora of Sabah and Sarawak) and taxon descriptions, so each value describes a species from herbarium specimens. **Field-measured** data come from studies that measured plants in plots, gardens or nurseries. Taxon names are aligned to World Flora Online; a taxon is one aligned name, which may be a species, an infraspecific taxon or a genus.

## Overview

| Source         | Datasets |  Taxa | Species-rank taxa | Families | Traits |   Rows |
|:---------------|---------:|------:|------------------:|---------:|-------:|-------:|
| Flora          |       28 | 1,522 |               184 |       13 |    141 | 44,949 |
| Field-measured |       18 |   723 |               676 |       76 |     62 | 41,741 |
| All            |       46 | 2,053 |               676 |       76 |    200 | 86,690 |

1,338 taxa are not yet aligned to World Flora Online, so they have no family and are counted under their original name. Family counts and the family table understate coverage until they are.

A further 12,309 values were read from the sources but excluded during the build and are not counted above: missing unit conversion (10,393); value out of allowable range (1,728); observation excluded in metadata (169); unsupported trait value (19).

## Overlap between flora and field data

- **192** taxa have both flora and field-measured data (27% of field-measured taxa, 13% of flora taxa).
- **63** taxon × trait combinations have values from both sources, so flora and field values can be compared directly.
- For taxa in both, the median taxon has 27 flora traits and 16.5 field-measured traits.

Families with the most taxa in both:

| Family           | Taxa in both |
|:-----------------|-------------:|
| Dipterocarpaceae |           54 |
| Annonaceae       |           35 |
| Fagaceae         |           15 |
| Sapindaceae      |           15 |
| Fabaceae         |           14 |
| Moraceae         |           13 |
| Burseraceae      |           12 |
| Melastomataceae  |           10 |
| Myristicaceae    |           10 |
| Elaeocarpaceae   |            4 |

## Coverage by family

The 20 families with the most taxa, of 76. *Taxon × trait* counts the combinations with at least one value. *Fill* is that count as a share of taxa × traits recorded for the family.

| Family | Taxa | Flora taxa | Field taxa | Traits | Taxon × trait | Rows | Fill (%) |
|:---|---:|---:|---:|---:|---:|---:|---:|
| (no family: name not aligned) | 1338 | 1330 | 8 | 150 | 28538 | 38279 | 14 |
| Dipterocarpaceae | 67 | 54 | 67 | 92 | 2822 | 17096 | 46 |
| Annonaceae | 49 | 35 | 49 | 78 | 1283 | 2727 | 34 |
| Phyllanthaceae | 47 | 0 | 47 | 35 | 639 | 1947 | 39 |
| Meliaceae | 42 | 0 | 42 | 36 | 569 | 1572 | 38 |
| Lauraceae | 40 | 0 | 40 | 34 | 598 | 2051 | 44 |
| Rubiaceae | 38 | 0 | 38 | 44 | 357 | 854 | 21 |
| Myrtaceae | 35 | 0 | 35 | 37 | 471 | 1645 | 36 |
| Euphorbiaceae | 31 | 0 | 31 | 45 | 543 | 3218 | 39 |
| Ebenaceae | 29 | 0 | 29 | 34 | 332 | 1059 | 34 |
| Fabaceae | 25 | 14 | 25 | 88 | 795 | 1766 | 36 |
| Myristicaceae | 20 | 10 | 20 | 77 | 540 | 1076 | 35 |
| Malvaceae | 20 | 0 | 20 | 47 | 306 | 875 | 33 |
| Fagaceae | 19 | 15 | 19 | 85 | 850 | 1537 | 53 |
| Sapindaceae | 18 | 15 | 18 | 86 | 701 | 1516 | 45 |
| Anacardiaceae | 17 | 0 | 17 | 40 | 226 | 589 | 33 |
| Moraceae | 16 | 13 | 16 | 79 | 602 | 955 | 48 |
| Sapotaceae | 14 | 0 | 14 | 35 | 240 | 524 | 49 |
| Melastomataceae | 13 | 10 | 13 | 48 | 167 | 321 | 27 |
| Burseraceae | 12 | 12 | 12 | 72 | 521 | 1091 | 60 |

## Traits with the most taxa

The 20 traits with the most taxa, of 200.

| Trait                          | Taxa | Flora taxa | Field taxa | Families | Datasets |
|:-------------------------------|-----:|-----------:|-----------:|---------:|---------:|
| `plant_growth_form`            | 1449 |       1449 |          0 |       13 |       28 |
| `plant_height`                 | 1204 |       1126 |        104 |       39 |       26 |
| `leaf_shape`                   | 1192 |       1192 |          0 |       13 |       25 |
| `leaf_base_shape`              | 1119 |       1119 |          0 |       13 |       23 |
| `leaf_apex_shape`              | 1090 |       1090 |          0 |       12 |       26 |
| `petiole_length`               | 1027 |       1027 |          0 |       12 |       22 |
| `leaf_compoundness`            |  994 |        994 |          0 |       11 |       23 |
| `inflorescence_type`           |  991 |        991 |          0 |       13 |       22 |
| `leaf_length`                  |  983 |        983 |          0 |       10 |       25 |
| `leaf_width`                   |  949 |        949 |          0 |       10 |       21 |
| `plant_diameter_breast_height` |  948 |        729 |        256 |       56 |       22 |
| `flower_structural_sex_type`   |  935 |        935 |          0 |        9 |       17 |
| `fruit_type`                   |  934 |        934 |          0 |       11 |       22 |
| `fruit_length`                 |  896 |        896 |          0 |       13 |       22 |
| `fruit_width`                  |  842 |        842 |          0 |       12 |       20 |
| `inflorescence_length`         |  677 |        677 |          0 |       11 |       20 |
| `flower_perianth_symmetry`     |  662 |        662 |          0 |        6 |       12 |
| `flower_fertile_stamens_count` |  631 |        631 |          0 |       11 |       20 |
| `leaf_margin`                  |  617 |        617 |          0 |       10 |       22 |
| `leaf_phyllotaxis`             |  602 |        602 |          0 |        7 |       17 |

The 10 traits with the most field-measured taxa:

| Trait                     | Field taxa | Flora taxa | Datasets |
|:--------------------------|-----------:|-----------:|---------:|
| `leaf_mass_per_area`      |        565 |          0 |        7 |
| `leaf_area`               |        553 |          0 |        6 |
| `leaf_dry_matter_content` |        542 |          0 |        3 |
| `leaf_P_per_dry_mass`     |        378 |          0 |        4 |
| `leaf_N_per_dry_mass`     |        377 |          0 |        6 |
| `leaf_thickness`          |        374 |          0 |        4 |
| `wood_density`            |        360 |          1 |        6 |
| `leaf_dry_mass`           |        348 |          0 |        2 |
| `leaf_Ca_per_dry_mass`    |        295 |          0 |        3 |
| `leaf_K_per_dry_mass`     |        295 |          0 |        3 |
