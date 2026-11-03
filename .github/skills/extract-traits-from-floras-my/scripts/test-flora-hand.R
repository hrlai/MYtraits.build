# Run from the repository root: uvr run .github/skills/extract-traits-from-floras-my/scripts/test-flora-hand.R
# un() rejects labels that flora_unmapped_summary() would group wrongly, and
# flora_unmapped_note() keeps the character to the listed set.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_hand.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")
h <- flora_hand("Genus species", list(p1 = c(1, 1)))
ok <- \(what, notes = "") {
  !inherits(try(h$un("x", what, notes, p = "p1"), silent = TRUE), "try-error")
}
stopifnot(
  ok("inflorescence apex; peltate bracts | colour"),
  ok("leaf | texture | typo repaired"),
  ok("leaf | texture", notes = "detail"),
  !ok("flower | anthesis sequence; pedicel | shape"),
  !ok("a | b | c | d"),
  !ok("stipules absent (negative phrase)"),
  !ok(" | colour")
)
stopifnot(
  identical(h$un("x", "leaf | colour", p = "p1")$notes, "UNMAPPED: leaf | colour"),
  identical(
    flora_unmapped_note(
      "flowering tuber-branch",
      "length, width, shape (candidate plant_height_reproductive)"
    ),
    paste0(
      "UNMAPPED: flowering tuber-branch | description | length, width, ",
      "shape (candidate plant_height_reproductive)"
    )
  ),
  identical(
    flora_unmapped_note("stipules", "absent (negative phrase)", "x"),
    "UNMAPPED: stipules | presence | absent (negative phrase); x"
  ),
  identical(flora_unmapped_note("fruit", "number"), "UNMAPPED: fruit | count"),
  identical(
    flora_unmapped_note("section", "text", "distribution"),
    "UNMAPPED: section | text | distribution"
  ),
  inherits(try(flora_unmapped_note(NA, "size"), silent = TRUE), "try-error"),
  # an organ noun that qualifies the character is not a second character
  identical(
    flora_unmapped_note("leaves", "midrib relief"),
    "UNMAPPED: leaves | surface | midrib relief"
  ),
  identical(
    flora_unmapped_character(c(
      "domatia presence", "lateral vein count / prominence",
      "height at first flowering", "veins", "length, lobes"
    )),
    c("presence", "count", "size", "venation", "description")
  ),
  # a scalar character keeps one detail per row
  identical(
    flora_unmapped_note("x", "description", c("a", "b")),
    c("UNMAPPED: x | description | a", "UNMAPPED: x | description | b")
  ),
  identical(flora_unmapped_note(character(0), "description", "a"), character(0))
)
cat("test-flora-hand: ok\n")
