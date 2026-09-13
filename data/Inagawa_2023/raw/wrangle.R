library(tidyverse)
library(readxl)


file <- "data/Inagawa_2023/raw/Inagawa_TreeComponentData_Nutrients_WoodDensity_SAFEdatabase_2023-07-10.xlsx"

# tree data
trees <-
  read_excel(file, sheet = 4, skip = 6) |>
  select(TreeNumber, location = SamplingLocation)

# nutrient data
nutrient <-
  read_excel(file, sheet = 5, skip = 6) |>
  group_by(TreeNumber, Species, TissueType) |>
  summarise(across(ends_with("_total"), mean), .groups = "drop") |>
  pivot_wider(names_from = TissueType, values_from = ends_with("_total"))

# wood density data
wd <-
  read_excel(file, sheet = 6, skip = 6) |>
  group_by(TreeNumber, Species) |>
  summarise(WoodDensity = mean(WoodDensity), .groups = "drop")

# combine and write out
dat <- nutrient |>
  left_join(wd) |>
  left_join(trees)
write_csv(dat, "data/Inagawa_2023/data.csv")

# additional process locations
read_excel(file, sheet = 2) |>
  rename(
    location = `Location name`,
    `latitude (deg)` = Latitude,
    `longitude (deg)` = Longitude
  ) |>
  write_csv("data/Inagawa_2023/locations.csv")
