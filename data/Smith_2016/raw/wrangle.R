library(tidyverse)
library(readxl)


dat <-
  read_excel("data/Smith_2016/raw/table_1.xlsx") |>
  select(
    species = Species,
    n = No.,
    fruit_mass = `Fruit mass (g)`,
    fruit_mass_sans_sepal = `Fruit mass excluding sepals (g)`,
    wing_area = `Scanned wing area (cm2)`
  ) |>
  # preprocess NA to parse mean and SE later
  mutate(
    across(
      c(fruit_mass, fruit_mass_sans_sepal, wing_area),
      ~ replace_na(.x, "")
    )
  ) |>
  # split traits into *_mean and *_se columns using the ± delimiter
  separate_wider_delim(
    cols = c(fruit_mass, fruit_mass_sans_sepal, wing_area),
    delim = "±",
    names = c("mean", "se"),
    names_sep = "_",
    too_few = "align_start"
  ) |>
  mutate(
    across(matches("_(mean|se)$"), ~ na_if(.x, "")),
    across(matches("_(mean|se)$"), parse_number)
  )

write_csv(dat, "data/Smith_2016/data.csv")
