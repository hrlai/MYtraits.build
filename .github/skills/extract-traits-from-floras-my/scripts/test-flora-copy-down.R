# Run from the repository root: uvr run .github/skills/extract-traits-from-floras-my/scripts/test-flora-copy-down.R
# flora_copy_down() keeps entity_measured (male / female) values apart.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
h <- tibble(block = "G", rank = "genus", value_type = "mode", cq = NA_character_,
  trait = "inflorescence_position", trait_value = c("axillary", "terminal"),
  trait_value_clean = c("axillary", "terminal"), trait_value_raw = trait_value,
  entity_measured = c("male inflorescence", "female inflorescence"),
  pdf_page = 1, printed_page = 1)
own <- tibble(block = c("s1", "s2"), trait = "inflorescence_position",
  trait_value_clean = "axillary", entity_measured = "male inflorescence")
mem <- tibble(block = c("s1", "s2", "s3"), genus = "G", group = NA)
out <- flora_copy_down(h, own, mem)
stopifnot(
  nrow(out) == 4,
  all(out$trait_value_clean[out$entity_measured == "male inflorescence"] == "axillary"),
  all(out$trait_value_clean[out$entity_measured == "female inflorescence"] == "terminal"),
  setequal(out$block[out$entity_measured == "male inflorescence"], "s3")
)
# no entity_measured anywhere: same as before
h2 <- mutate(h[1, ], entity_measured = NA_character_)
own2 <- mutate(own, entity_measured = NA_character_)
o2 <- flora_copy_down(h2, own2, mem)
stopifnot(nrow(o2) == 1, o2$block == "s3")
cat("ok\n")
