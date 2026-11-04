# Run from the repository root: uvr run .github/skills/extract-traits-from-floras-my/scripts/test-flora-rerun.R
source(".github/skills/extract-traits-from-floras-my/scripts/flora_rerun.R")
ids <- flora_ids()
stopifnot(length(ids) > 20, "Jones_1995" %in% ids)
# Jones_1995 keeps the family Uses sentence naming Toddalia as unmapped text
hit <- flora_unmapped_find("including Toddalia", "Jones_1995")
stopifnot(nrow(hit) == 1, hit$taxon_name == "Rutaceae")
stopifnot(nrow(flora_unmapped_find("zzzz-no-match", "Jones_1995")) == 0)
