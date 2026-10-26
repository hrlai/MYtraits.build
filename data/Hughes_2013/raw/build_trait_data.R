# Extract directly stated, configured traits from image-verified Hughes (2013) transcription.
library(tidyverse)
source("data/Hughes_2013/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

traits <- yaml::read_yaml("config/traits.yml")$traits$elements
# The transcription is already corrected from rendered page images. Do not apply
# OCR repairs again after segmentation.
taxa <- paras |>
  filter(rank %in% c("species", "variety")) |>
  distinct(block, rank, status) |>
  mutate(
    has_description = block %in% filter(paras, section == "Description")$block,
    parent_species = case_when(
      str_detect(
        block,
        "^Memecylon acuminatum var\\."
      ) ~ "Memecylon acuminatum",
      str_detect(
        block,
        "^Memecylon corticosum var\\."
      ) ~ "Memecylon corticosum",
      str_detect(
        block,
        "^Memecylon dichotomum var\\."
      ) ~ "Memecylon dichotomum",
      str_detect(
        block,
        "^Memecylon scutellatum var\\."
      ) ~ "Memecylon scutellatum",
      TRUE ~ block
    )
  )
desc <- paras |>
  filter(section == "Description", rank %in% c("species", "variety"))

add_rows <- function(p) {
  text <- flora_norm(p$text)
  low <- str_to_lower(text)
  out <- list()
  add <- function(
    trait,
    raw,
    value,
    clean = value,
    type = "mode",
    unit = "",
    entity = "",
    note = "",
    cq = NA_character_
  ) {
    out[[length(out) + 1]] <<- tibble(
      block = p$block,
      rank = p$rank,
      trait,
      trait_value_raw = raw,
      trait_value = as.character(value),
      trait_value_clean = as.character(clean),
      value_type = type,
      unit,
      entity_measured = entity,
      notes = note,
      cq,
      pdf_page = p$pdf_page,
      printed_page = p$printed_page,
      source_section = "Description"
    )
  }
  add_num <- function(trait, raw, entity = "", note = "") {
    parsed <- flora_parse_range(raw)
    if (is.null(parsed)) {
      return()
    }
    flora_num_rows(parsed) |>
      filter(is.na(cq) | cq != "extreme_range") |>
      pwalk(\(value, value_type, cq, unit) {
        add(trait, raw, value, value, value_type, unit, entity, note, cq)
      })
  }
  add_dims <- function(trait_length, trait_width, raw, entity = "") {
    dims <- flora_parse_dims(raw)
    if (is.null(dims)) {
      return()
    }
    dims$len |>
      filter(is.na(cq) | cq != "extreme_range") |>
      pwalk(\(value, value_type, cq, unit) {
        add(
          trait_length,
          raw,
          value,
          value,
          value_type,
          unit,
          entity,
          "First dimension interpreted as length.",
          cq
        )
      })
    dims$wid |>
      filter(is.na(cq) | cq != "extreme_range") |>
      pwalk(\(value, value_type, cq, unit) {
        add(
          trait_width,
          raw,
          value,
          value,
          value_type,
          unit,
          entity,
          "Second dimension interpreted as width.",
          cq
        )
      })
  }
  add_cat <- function(trait, raw, value, clean = value, entity = "") {
    add(trait, raw, value, clean, "mode", "", entity)
  }

  if (
    str_detect(
      low,
      "^(?:a |an )?(?:small |medium-sized |medium |large |slender |straggly |often crooked )?shrub"
    )
  ) {
    add_cat(
      "plant_growth_form",
      str_extract(text, "(?i)^.{0,50}?shrub"),
      "shrub"
    )
  }
  if (
    str_detect(
      low,
      "^(?:a |an )?(?:small |medium-sized |medium |large |slender |often crooked )?(?:tree|treelet)"
    )
  ) {
    add_cat(
      "plant_growth_form",
      str_extract(text, "(?i)^.{0,50}?(?:tree|treelet)"),
      "tree"
    )
  }
  if (str_detect(low, "^leaves? (?:are )?opposite")) {
    add_cat("leaf_phyllotaxis", "opposite", "opposite")
  }
  height <- str_extract(
    text,
    "(?i)(?<![\\d.])(?:to |about |c\\. )?(?:\\(\\d+–\\))?\\d+(?:–\\d+)?(?:\\(–\\d+\\))? m (?:tall|high|in height)"
  )
  if (!is.na(height)) {
    add_num("plant_height", height)
  }

  petiole <- str_extract(
    text,
    "(?i)petioles?(?: (?:long|distinct))?,? (?:\\(\\d+–\\))?(?:c\\. |to )?\\d+(?:–\\d+)?(?:\\(–\\d+\\))? mm long"
  )
  if (!is.na(petiole)) {
    add_num("petiole_length", petiole)
  }
  lamina <- str_extract(
    text,
    "(?i)lamina[^.;]*?(?:\\(\\d+(?:\\.\\d+)?–\\))?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? × (?:\\(\\d+(?:\\.\\d+)?–\\))?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? cm"
  )
  if (!is.na(lamina)) {
    add_dims("leaf_length", "leaf_width", lamina, "lamina")
  }
  shapes <- c(
    "elliptic" = "elliptical",
    "lanceolate" = "lanceolate",
    "obovate" = "obovate",
    "ovate" = "ovate",
    "oblong" = "oblong"
  )
  for (x in names(shapes)) {
    if (str_detect(low, paste0("lamina[^.;]*\\b", x, "\\b"))) {
      add("leaf_shape", x, shapes[[x]])
    }
  }
  if (str_detect(low, "^leaves? (?:are )?opposite")) {
    add_cat("leaf_phyllotaxis", "opposite", "opposite")
  }
  for (x in c("cuneate", "rounded", "attenuate", "obtuse", "acute")) {
    if (str_detect(low, paste0("base ", ".{0,40}\\b", x, "\\b"))) {
      add("leaf_base_shape", x, x)
    }
  }
  for (x in c("acuminate", "acute", "obtuse", "rounded", "apiculate")) {
    if (str_detect(low, paste0("apex ", ".{0,40}\\b", x, "\\b"))) {
      add("leaf_apex_shape", x, x)
    }
  }
  if (str_detect(low, "leaves?:[^.]*\\bglabrous\\b")) {
    add("leaf_hairs_adult_leaves", "glabrous", "glabrous")
  }
  if (str_detect(low, "leaves?:[^.]*\\b(pubesc|puberul|hairy)")) {
    add("leaf_hairs_adult_leaves", "pubescent", "hairy")
  }

  if (str_detect(low, "inflorescences?[^.]*\\bcym")) {
    add("inflorescence_type", "cyme", "cyme")
  }
  if (str_detect(low, "inflorescences?[^.;]*\\bglomerulate")) {
    add_cat("inflorescence_type", "glomerulate", "head")
  }
  if (str_detect(low, "inflorescences?[^.;]*\\bumbell")) {
    add_cat("inflorescence_type", "umbellate", "umbel")
  }
  if (str_detect(low, "inflorescences?[^.;]*\\bpaniculate")) {
    add_cat("inflorescence_type", "paniculate", "panicle")
  }
  pedicel <- str_extract(
    text,
    "(?i)pedicels? (?:quite long )?(?:\\(\\d+(?:\\.\\d+)?–\\))?(?:c\\. |to )?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? mm(?: long)?"
  )
  if (!is.na(pedicel)) {
    add_num("pedicel_length", pedicel)
  }
  petal_colour <- str_match(
    low,
    "petals? (?:are )?(white|whitish|cream|pale blue|light blue|deep blue|blue|lilac|pale pink|pink|pinkish|purple|deep purple-blue|greenish)"
  )
  if (!is.na(petal_colour[1, 2])) {
    colour <- petal_colour[1, 2]
    clean <- case_when(
      colour %in% c("white", "whitish", "cream") ~ "white_cream",
      colour %in%
        c(
          "pale blue",
          "light blue",
          "deep blue",
          "blue",
          "lilac",
          "purple",
          "deep purple-blue"
        ) ~ "blue_purple",
      colour %in% c("pale pink", "pink", "pinkish") ~ "pink",
      colour == "greenish" ~ "green"
    )
    add_cat("flower_colour", colour, clean, entity = "petals")
  }
  if (str_detect(low, "anthers?[^.;]*\\bslits")) {
    add(
      "flower_anther_dehiscence",
      "dehiscing through slits",
      "short_apical_slits"
    )
  }

  fruit_dims <- str_extract(
    text,
    "(?i)berry[^.;]*?(?:\\(\\d+(?:\\.\\d+)?–\\))?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? × (?:\\(\\d+(?:\\.\\d+)?–\\))?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)?(?:\\(–\\d+(?:\\.\\d+)?\\))? mm"
  )
  if (!is.na(fruit_dims)) {
    add_dims("fruit_length", "fruit_width", fruit_dims, "berry")
  }
  fruit_diameter <- str_extract(
    text,
    "(?i)berry[^.;]*?(?:c\\. )?\\d+(?:\\.\\d+)?(?:–\\d+(?:\\.\\d+)?)? mm diameter"
  )
  if (!is.na(fruit_diameter)) {
    add_num(
      "fruit_width",
      fruit_diameter,
      "berry",
      "Fruit diameter scored as fruit_width."
    )
  }
  if (str_detect(low, "\\bberry\\b")) {
    add("fruit_type", "berry", "berry")
  }
  ripe <- str_match(
    low,
    "ripen(?:ing)?(?: through [a-z -]+)? (?:to |)(black|blue|purple|red|brown|yellow|cream|pink|white|green)"
  )
  if (!is.na(ripe[1, 2])) {
    add_cat("fruit_colour", ripe[1, 2], ripe[1, 2])
  }
  bind_rows(out)
}

desc <- paras |>
  filter(
    section == "Description",
    rank %in% c("family", "genus", "species", "variety")
  )
own <- desc |>
  mutate(rows = map(seq_len(n()), \(i) add_rows(desc[i, ]))) |>
  select(rows) |>
  unnest(rows)

unmapped <- bind_rows(
  paras |>
    filter(
      section == "Description",
      rank %in% c("family", "genus", "species", "variety")
    ) |>
    transmute(
      block,
      rank,
      trait_value_raw = map(text, flora_split_sentences),
      pdf_page,
      printed_page,
      source_section = "Description"
    ) |>
    unnest(trait_value_raw),
  paras |>
    filter(
      section %in% c("Ecology", "Distribution", "Notes", "Uses"),
      rank %in% c("family", "genus", "species", "variety")
    ) |>
    transmute(
      block,
      rank,
      trait_value_raw = map(text, flora_split_sentences),
      pdf_page,
      printed_page,
      source_section = section
    ) |>
    unnest(trait_value_raw)
) |>
  transmute(
    block,
    rank,
    trait = NA_character_,
    trait_value_raw,
    trait_value = NA_character_,
    trait_value_clean = NA_character_,
    value_type = NA_character_,
    unit = NA_character_,
    entity_measured = NA_character_,
    notes = paste0("UNMAPPED: text | ", str_to_lower(source_section)),
    pdf_page,
    printed_page,
    source_section
  )

infra_members <- taxa |>
  transmute(block, species = parent_species, rank)
higher <- own |>
  rename(rank_scored_from = rank)
species_own <- own |>
  filter(rank == "species")
copied <- flora_copy_down(
  higher_rows = own,
  own_rows = own,
  members = infra_members |>
    mutate(genus = str_extract(block, "^[A-Z][a-z]+"), group = NA_character_)
)
inherited <- flora_inherit_infraspecific(
  own_rows = own,
  members = infra_members
)

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
  inherited,
  unmapped |> mutate(rank_scored_from = NA_character_)
) |>
  left_join(taxa |> select(block, taxon_rank = rank), by = "block") |>
  transmute(
    taxon_name = block,
    taxon_rank,
    rank_scored_from,
    commonness_qualifier = cq,
    population_region = NA_character_,
    form = NA_character_,
    entity_measured,
    trait,
    trait_value_raw,
    trait_value,
    trait_value_clean,
    value_type,
    unit,
    source_section,
    pdf_page,
    printed_page,
    notes,
    source_id = NA_character_
  ) |>
  flora_merge_duplicate_keys() |>
  flora_finalise(traits)
write_csv(out, "data/Hughes_2013/raw/trait_data.csv", na = "")
write_csv(
  flora_unmapped_summary(out),
  "data/Hughes_2013/raw/unmapped_terms.csv",
  na = ""
)
flags <- taxa |>
  left_join(
    out |> count(taxon_name, name = "n_rows"),
    by = c("block" = "taxon_name")
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank = rank,
    status,
    n_rows = coalesce(n_rows, 0L),
    flag = if_else(
      rank == "variety",
      "Infraspecific treatment; description retained under printed variety heading.",
      ""
    )
  )
write_csv(flags, "data/Hughes_2013/raw/taxon_flags.csv", na = "")
