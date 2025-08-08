library(tidyverse)
library(readxl)


# clean up dataset
taxa <- 
  read_excel("data/Both_2019/raw/Both_tree_functional_traits.xlsx",
             sheet = 2)

both <- 
  read_excel("data/Both_2019/raw/Both_tree_functional_traits.xlsx",
             sheet = 4,
             skip = 6) %>% 
  mutate_at(vars(height.m:lignin_recalcitrants_mg.mm2),
            as.numeric) %>% 
  left_join(
    taxa %>% 
      select(species = Name, taxon_name = `Taxon name`, `Taxon type`)
  )
  
write_csv(both, "data/Both_2019/data.csv")
