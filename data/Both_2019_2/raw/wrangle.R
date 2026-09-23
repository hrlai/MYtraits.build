library(tidyverse)
library(readxl)
library(sf)


# trait data
traits <-
  read_excel(
    "data/Both_2019_2/raw/Both_wood_chemistry.xlsx",
    sheet = 4,
    skip = 6
  ) |>
  mutate(species = str_replace(species, "\\.", " ")) |>
  # generate unique tree ID
  mutate(uid = paste(location, tree_id, sep = "_")) |>
  # aggregated repeated measurements to individual means because this is the
  # lowest hierarchy that is accepted by traits.build standards
  group_by(location, uid, species) |>
  summarise(
    N_perc = mean(N_perc, na.rm = TRUE),
    C_perc = mean(C_perc, na.rm = TRUE),
    replicate = sum(replicate)
  ) |>
  ungroup()

write_csv(traits, "data/Both_2019_2/data.csv")

# coordinates from the SAFE gazetteer
locations <-
  st_read("data/Both_2019/raw/gazetteer.geojson") |>
  st_drop_geometry() |>
  select(
    location,
    `longitude (deg)` = centroid_x,
    `latitude (deg)` = centroid_y,
    description = type
  ) |>
  filter(location %in% unique(traits$location))

write_csv(locations, "data/Both_2019_2/raw/locations.csv")
# traits.build::metadata_add_locations("Both_2019", locations)
