library(tidyverse)



# Cleanup individual data sheets ------------------------------------------

# Size traits
crown <-
  read_delim("data/Wright_2024/raw/Pasoh_crown_by_ind.txt") %>%
  # I am going to take the maximum size for DBH, crown diameter and height
  group_by(sp, family, genus, species, variety) %>%
  summarise_at(vars(dbh, diameter, height), max) %>%
  pivot_longer(cols = c(dbh, diameter, height),
               names_to = "trait_name",
               values_to = "trait_value")

# leaf morphology
leaf_morph <-
  read_delim("data/Wright_2024/raw/Pasoh_leaf_traits_by_leaf.txt") %>%
  mutate(leaf.mass.dry = ifelse(leaflets == 1, mass.dry, NA),
         leaflet.mass.dry = ifelse(leaflets > 1, mass.dry, NA)) %>%
  group_by(sp, family, genus, species, variety, tag) %>%
  summarise(
    leaf.area = mean(leaf.area, na.rm = TRUE),
    leaflet.area = mean(leaflet.area, na.rm = TRUE),
    leaf.mass.dry = mean(leaf.mass.dry, na.rm = TRUE),
    leaflet.mass.dry = mean(leaflet.mass.dry, na.rm = TRUE),
    LDMC = mean(LDMC, na.rm = TRUE),
    LMA = mean(LMA, na.rm = TRUE),
    lamina.thickness = mean(lamina.thickness, na.rm = TRUE),
    lamina.density = mean(lamina.density, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = leaf.area:lamina.density,
               names_to = "trait_name",
               values_to = "trait_value",
               values_drop_na = TRUE) %>%
  mutate(tag = as.character(tag))

# leaf nutrients
leaf_nutrient <-
  read_delim("data/Wright_2024/raw/Pasoh_NP.txt") %>%
  mutate(N = as.numeric(N)) %>%
  group_by(sp, family, genus, species, variety) %>%
  summarise_at(vars(N, P), ~ mean(., na.rm = TRUE)) %>%
  pivot_longer(cols = c(N, P),
               names_to = "trait_name",
               values_to = "trait_value",
               values_drop_na = TRUE) %>%
  # convert from % to g/g
  mutate(trait_value = trait_value / 100)

# wood density
wd <-
  read_delim("data/Wright_2024/raw/Pasoh_Wood_Density.txt") %>%
  mutate(tag = paste("numero", numero, sep = "_")) %>%
  select(sp, family, genus, species, variety, tag, wood.density) %>%
  group_by(sp, family, genus, species, variety, tag) %>%
  pivot_longer(cols = wood.density,
               names_to = "trait_name",
               values_to = "trait_value",
               values_drop_na = TRUE)


# Combine all data into a long format -------------------------------------
# we separate individual- and species-level traits because traits.build needs
# individual_id to either be about individual or NA to pass dataset_test

# individual-level traits
trait_ind <-
  bind_rows(leaf_morph, wd) %>%
  mutate(taxon_name = ifelse(is.na(variety),
                             paste(genus, species),
                             paste(genus, species, "var.", variety)))

# species-level traits
trait_spp <- bind_rows(crown, leaf_nutrient) %>%
  mutate(taxon_name = ifelse(is.na(variety),
                             paste(genus, species),
                             paste(genus, species, "var.", variety)))

# save individual-level traits as Wright_2024
write_csv(trait_ind, "data/Wright_2024/data.csv")

# save species-level traits as Wright_2024_2
write_csv(trait_spp, "data/Wright_2024_2/data.csv")
