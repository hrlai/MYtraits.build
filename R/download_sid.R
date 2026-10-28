# Download and tabulate trait data from the SER/INSR/RBG Kew Seed Information
# Database (SID), https://ser-sid.org/
#
# SID's web app reads from a public Supabase REST API. Its anonymous key is
# embedded in the site's JavaScript bundle, so it is looked up at run time.
# Cite: Society for Ecological Restoration, International Network for Seed Based
# Restoration and Royal Botanic Gardens Kew. (2023) Seed Information Database
# (SID). Available from: https://ser-sid.org/

library(httr2)
library(dplyr)
library(purrr)
library(readr)

sid_url <- "https://fyxheguykvewpdeysvoh.supabase.co/rest/v1"
out_dir <- file.path("inst", "extdata", "sid")

sid_anon_key <- function() {
  home <- request("https://ser-sid.org/") |> req_perform() |> resp_body_string()
  bundle <- regmatches(home, regexpr("/assets/index-[^\"]+\\.js", home))
  js <- request(paste0("https://ser-sid.org", bundle)) |>
    req_perform() |>
    resp_body_string()
  sub('.*SUPABASE_ANON_KEY="([^"]+)".*', "\\1", js)
}

sid_table <- function(table, key, page_size = 1000) {
  req <- request(sid_url) |>
    req_url_path_append(table) |>
    req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    req_retry(max_tries = 5) |>
    req_throttle(rate = 5)

  pages <- list()
  offset <- 0
  repeat {
    rows <- req |>
      req_url_query(
        select = "*",
        order = "id",
        offset = offset,
        limit = page_size
      ) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    if (length(rows) == 0) {
      break
    }
    pages[[length(pages) + 1]] <- as_tibble(rows)
    if (nrow(rows) < page_size) {
      break
    }
    offset <- offset + page_size
  }
  message(table, ": ", sum(map_int(pages, nrow)), " rows")
  bind_rows(pages)
}

key <- sid_anon_key()
tables <- c(
  "families",
  "species",
  "references",
  "seed_weights",
  "germination",
  "storage_behaviour",
  "dispersal",
  "oil_content",
  "protein_content",
  "morphology"
)
sid <- set_names(tables) |> map(sid_table, key = key)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
iwalk(sid, \(df, nm) {
  write_csv(df, file.path(out_dir, paste0(nm, ".csv")), na = "")
})

# Trait tables link to species via species$int_id
taxa <- sid$species |>
  select(
    int_id,
    family_id = family,
    genus,
    epithet,
    infraspecies_rank,
    infraspecies_epithet,
    authority
  ) |>
  left_join(
    sid$families |>
      select(family_id = id, family = any_of(c("family", "name"))),
    by = "family_id"
  ) |>
  mutate(
    taxon_name = trimws(paste(
      genus,
      epithet,
      coalesce(infraspecies_rank, ""),
      coalesce(infraspecies_epithet, "")
    ))
  )

summary_by_species <- list(
  sid$seed_weights |>
    summarise(
      seed_weight_n = n(),
      thousand_seed_weight_g_median = median(thousandseedweight, na.rm = TRUE),
      .by = species_id
    ),
  sid$storage_behaviour |>
    summarise(
      storage_behaviour = paste(
        sort(unique(na.omit(storage_behaviour))),
        collapse = "; "
      ),
      .by = species_id
    ),
  sid$oil_content |>
    summarise(
      oil_content_pct_median = median(oil_content, na.rm = TRUE),
      .by = species_id
    ),
  sid$protein_content |>
    summarise(
      protein_content_pct_median = median(protein_content, na.rm = TRUE),
      .by = species_id
    ),
  sid$germination |>
    summarise(germination_n = n(), .by = species_id),
  sid$dispersal |>
    summarise(dispersal_n = n(), .by = species_id),
  sid$morphology |>
    summarise(morphology_n = n(), .by = species_id)
) |>
  reduce(full_join, by = "species_id") |>
  left_join(taxa, by = c("species_id" = "int_id")) |>
  relocate(species_id, family, taxon_name, authority)

write_csv(
  summary_by_species,
  file.path(out_dir, "sid_species_traits.csv"),
  na = ""
)
