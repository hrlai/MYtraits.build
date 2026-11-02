# Run from the repository root: uvr run .github/skills/extract-traits-from-floras-my/scripts/test-flora-hand.R
# un() rejects labels that flora_unmapped_summary() would group wrongly.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
h <- flora_hand("Genus species", list(p1 = c(1, 1)))
ok <- \(what, notes = "") {
  !inherits(try(h$un("x", what, notes, p = "p1"), silent = TRUE), "try-error")
}
stopifnot(
  ok("inflorescence apex; peltate bracts | colour"),
  ok("leaf | texture | typo repaired"),
  ok("leaf | texture", notes = "detail"),
  !ok("flower | anthesis sequence; pedicel | shape"),
  !ok("a | b | c | d")
)
cat("test-flora-hand: ok\n")
