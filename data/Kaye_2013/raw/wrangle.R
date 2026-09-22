library(tidyverse)
library(readxl)


data <-
  read_excel("data/Kaye_2013/raw/All leaves.xlsx") |>
  filter(!is.na(species)) |>
  mutate(code = coalesce(`code...2`, `code...8`)) |>
  mutate(across(
    c(`pressure to puncture (N)`, `area (mm2)`),
    ~ parse_number(
      .,
      na = c("", "NA", "?", "~", "not ©", "No rachis", "no rachis")
    )
  )) |>
  # calculate leaf work to punch from force divided by punch rod circumference
  # with a diameter of 1 mm2
  mutate(
    leaf_work_to_punch = `pressure to puncture (N)` / (2 * pi * sqrt(1 / pi))
  ) |>
  # aggregate from leaf to individual level
  group_by(number, genus, species, code) |>
  summarise(
    across(
      c(leaf_work_to_punch, `area (mm2)`, LDMC, SLA),
      ~ mean(., na.rm = TRUE)
    ),
    Date = min(Date),
    .groups = "drop"
  ) |>
  mutate(
    Date = ymd(Date),
    species = ifelse(species == "NA", "", species),
    taxon_name = paste(genus, species)
  )

write_csv(data, "data/Kaye_2013/data.csv")
