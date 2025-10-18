library(tidyverse)
library(readxl)


# read and clean up data
dat <- read_excel("data/Iida_2011/raw/fec_1884_sm_tables1.xls", skip = 1) %>%
  select(Species, Dmax = `D*`)

write_csv(dat, "data/Iida_2011/data.csv")
