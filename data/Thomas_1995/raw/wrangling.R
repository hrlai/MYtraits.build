library(tidyverse)


# Read data that were extracted from the llm.R script in the same directory
# then clean up
dat <-
  read_csv("data/Thomas_1995/raw/data_llm_extract.csv") %>%
  pivot_longer(cols = starts_with("area_"),
               names_to = "trait_name") %>%
  separate(value, c("mean", "sd"), sep = " ± ") %>%
  pivot_longer(cols = c(mean, sd),
               names_to = "value_type") %>%
  mutate(trait_name = paste(trait_name, value_type, sep = "_"),
         .keep = "unused")

# save cleaned data
write_csv(dat, "data/Thomas_1995/data.csv")
