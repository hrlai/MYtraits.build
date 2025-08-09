library(tidyverse)
library(readxl)
library(sf)


# clean up dataset
# taxon information
taxa <- 
  read_excel("data/Both_2019/raw/Both_tree_functional_traits.xlsx",
             sheet = 2)

# trait data
both <- 
  read_excel("data/Both_2019/raw/Both_tree_functional_traits.xlsx",
             sheet = 4,
             skip = 6) %>% 
  # generate unique tree ID
  mutate(uid = paste(location, tree_id, "_")) %>% 
  mutate_at(vars(height.m:lignin_recalcitrants_mg.mm2),
            as.numeric) %>% 
  # aggregated these to individual means because this is the lowest hierarchy 
  # that is accepted by traits.build standards
  group_by(location, 
           uid,
           species) %>% 
  summarise_at(vars(height.m:lignin_recalcitrants_mg.mm2), 
               ~ mean(., na.rm = TRUE)) %>% 
  ungroup %>% 
  left_join(
    taxa %>% 
      select(species = Name, taxon_name = `Taxon name`, `Taxon type`)
  )

write_csv(both, "data/Both_2019/data.csv")

# coordinates from the SAFE gazetteer
locations <- 
  st_read("data/Both_2019/raw/gazetteer.geojson") %>% 
  st_drop_geometry() %>% 
  select(
    location,
    `longitude (deg)` = centroid_x,
    `latitude (deg)` = centroid_y,
    description = type
  ) %>% 
  filter(location %in% unique(both$location))

write_csv(locations, "data/Both_2019/raw/locations.csv")
# traits.build::metadata_add_locations("Both_2019", locations)