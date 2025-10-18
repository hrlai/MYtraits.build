library(tidyverse)


# Read data that were extracted from the llm.R script in the same directory
# then clean up
dat <-
  read_csv("data/Thomas_1995/raw/data_llm_extract.csv") %>%
  separate(area_sapling, c("area_sapling_mean", "area_sapling_sd"), sep = " ± ") %>%
  separate(area_adult, c("area_adult_mean", "area_adult_sd"), sep = " ± ")

# save cleaned data
# this will be used for sapling traits
write_csv(dat, "data/Thomas_1995/data.csv")

# save another copy to a sister directory for adult traits
write_csv(dat, "data/Thomas_1995_2/data.csv")
