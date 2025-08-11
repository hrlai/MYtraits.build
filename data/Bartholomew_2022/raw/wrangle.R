library(tidyverse)
library(readxl)


traits <- 
  read_excel("data/Bartholomew_2022/raw/Bartholomew_2022_FunEcol_data.xlsx",
             sheet = 1) %>% 
  mutate(species = str_to_sentence(species)) %>% 
  mutate_at(vars(height, Asat_area:mg.area), as.numeric)

write_csv(traits, "data/Bartholomew_2022/data.csv")
