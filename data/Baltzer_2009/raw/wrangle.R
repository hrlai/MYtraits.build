library(tidyverse)
library(readxl)


# Read table from supplementary Word doc ----------------------------------

dat <-
  read_excel("data/Baltzer_2009/raw/Baltzer_2009.xlsx") %>%
  select(-Dist) %>%
  pivot_longer(
    cols = -Species,
    names_to = "trait_name"
  ) %>%
  separate(value, c("mean", "se"), sep = " ± ") %>%
  pivot_longer(
    cols = c(mean, se),
    names_to = "value_type"
  ) %>%
  mutate(
    trait_name = paste(trait_name, value_type, sep = "_"),
    .keep = "unused"
  )

# save output
write_csv(dat, "data/Baltzer_2009/data.csv")
