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
# a statement naming an unscored alternative is not copied (Adema_1996)
h3 <- mutate(h2, trait_value_raw = "base symmetric or slightly axillary")
o3 <- flora_copy_down(h3, own2, mem)
stopifnot(nrow(o3) == 0, attr(o3, "rejected")$reason == "alternative")
stopifnot(
  .flora_names_alt("introrse or latero-introrse", "introrse"),
  !.flora_names_alt("Leaves imparipinnate or paripinnate", "imparipinnate paripinnate"),
  !.flora_names_alt("Dioecious shrubs or trees", "dioecious"),
  !.flora_names_alt("Fruit a straight or twisted capsule", "capsule"),
  .flora_names_alt("thyrsoid or corymbose", "corymb"),
  !.flora_names_alt("base slightly to strongly asymmetric", "asymmetric")
)
o4 <- flora_copy_down(mutate(h2, trait_value_raw = "axillary (Hydrocleys)"), own2, mem)
stopifnot(nrow(o4) == 0)
# flora_merge_duplicate_keys(): shared keys merge words; a single row keeps a
# repeated word (Jutta_2012).
m <- tibble(taxon_name = c("A", "A", "B"), trait = "t", value_type = "mode",
  trait_value = c("yellow", "orange", "cracks in the rock in"),
  trait_value_clean = trait_value, trait_value_raw = trait_value)
mm <- flora_merge_duplicate_keys(m)
stopifnot(
  mm$trait_value[mm$taxon_name == "A"] == "yellow orange",
  mm$trait_value[mm$taxon_name == "B"] == "cracks in the rock in"
)
cat("ok\n")
