library(tidyverse)


# Separate morphological traits from nutrient traits because the former was
# measured from individual seedlings, but the latter was pooled across seedlings
# within the same plot

traits <- 
  read_csv("data/Burslem_2022/raw/danum_valley_seedling_traits.csv") %>% 
  # generate unique ID
  mutate(uid = paste(Seedling.Plot, Species, Individual, sep = "_")) %>% 
  # for Shorea johorensis there are three sets of measurements from "OUT" plots
  # I presume these are opportunistic sampling outside of the normal seedling plots
  # and the authors did not assign different OUT plot IDs, but I think they are
  # different S. johorensis seedlings because they were measured on different
  # date. To make them unique, we need to paste date to their unique IDs
  mutate(uid = ifelse(Species == "Shorea johorensis",
                      paste(Seedling.Plot, Date, Species, Individual, sep = "_"),
                      uid)) %>% 
  # mutate(uid = row_number()) %>% 
  # calculate leaf work to punch from force divided by punch rod circumference
  # diameter of 1 mm2
  mutate(leaf_work_to_punch = LFP / (2 * pi * sqrt(1/pi)))

morphology <- 
  traits %>% 
  select(-(Leaf.N:`Leaf.N:P`))

nutrient <- 
  traits %>% 
  group_by(Date, Forest, Management, Species, Family, Seedling.Plot) %>% 
  summarise_at(vars(Leaf.N:`Leaf.N:P`), mean)

write_csv(morphology, "data/Burslem_2022/data.csv")
write_csv(nutrient,   "data/Burslem_2022_2/data.csv")
