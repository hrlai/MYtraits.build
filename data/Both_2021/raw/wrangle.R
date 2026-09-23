library(tidyverse)
library(readxl)
library(sf)


# trait data
traits <-
  read_excel(
    "data/Both_2021/raw/Both_wood_anatomical_traits_complete_dataset.xlsx",
    sheet = 4,
    skip = 6
  ) |>
  mutate(species = str_replace(species, "\\.", " ")) |>
  # generate unique tree ID
  mutate(uid = paste(location, tree_id, sep = "_")) |>
  # aggregated repeated measurements to individual means because this is the
  # lowest hierarchy that is accepted by traits.build standards
  group_by(location, uid, species) |>
  # calculate stem_vessel_density
  mutate(stem_vessel_density = No.vessel.wedge / Wedge.area.micron2) |>
  summarise_at(
    vars(stem_vessel_density, Vessel.diameter.micron:vessel.lumen.f.wedge),
    ~ mean(., na.rm = TRUE)
  ) |>
  ungroup()

write_csv(traits, "data/Both_2021/data.csv")

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

write_csv(locations, "data/Both_2021/raw/locations.csv")
# traits.build::metadata_add_locations("Both_2021", locations)
