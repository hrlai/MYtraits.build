library(tidyverse)
library(readxl)


# clean up trait data
species <-
  read_excel(
    "data/Carle_2025/raw/malua_seedling_trait_data.xlsx",
    sheet = 3
  ) |>
  select(spp = Name, species = `Taxon name`)

traits <-
  read_excel(
    "data/Carle_2025/raw/malua_seedling_trait_data.xlsx",
    sheet = 4,
    skip = 10
  ) |>
  left_join(species) |>
  mutate(across(chl_mean:nest, parse_number)) |>
  mutate(
    root_shoot_ratio = akar_d / (pokok_d + daun_d),
    leaf_mass_to_stem_mass_ratio = daun_d / pokok_d,
    stem_mass_to_shoot_mass_ratio = pokok_d / (pokok_d + daun_d)
  ) |>
  select(
    tag_no,
    site,
    plot,
    species,
    chl_mean,
    osmpot,
    slamean,
    amean,
    wd,
    root_shoot_ratio,
    leaf_mass_to_stem_mass_ratio,
    stem_mass_to_shoot_mass_ratio,
    rmf,
    d13c,
    # d13air,  # not in traits.yml
    # discrim, # not in traits.yml
    wuei,
    nest
  ) |>
  pivot_longer(cols = chl_mean:nest, names_to = "trait", values_drop_na = TRUE)

write_csv(traits, "data/Carle_2025/data.csv")


# also export locations
locations <-
  read_excel(
    "data/Carle_2025/raw/malua_seedling_trait_data.xlsx",
    sheet = 2
  ) |>
  rename(
    location = `Location name`,
    `latitude (deg)` = Latitude,
    `longitude (deg)` = Longitude
  )
write_csv(locations, "data/Carle_2025/raw/locations.csv")
