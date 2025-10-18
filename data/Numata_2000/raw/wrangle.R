library(tidyverse)


# read and update punch test to N/m
# diameter of the rod (mm)
rod_diam <- 2 * pi * 0.5

dat <-
  read_csv("data/Numata_2000/raw/data.csv") %>%
  mutate(toughness = toughness / rod_diam,
         toughness_sd = toughness_sd / rod_diam)

write_csv(dat, "data/Numata_2000/data.csv")
