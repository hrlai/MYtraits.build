# Run from the repository root: uvr run .github/skills/extract-traits-from-floras-my/scripts/test-flora-parse.R
# Line-end hyphen rejoin (flora_segment.R) and flora_term_level() row priority.
source(".github/skills/extract-traits-from-floras-my/scripts/flora_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_segment.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_parse.R")

# rejoined only when the joined word is in the vocabulary (Soepadmo_2000 p. 90)
t <- c("leaves coria-", "ceous, elliptic-", "oblong, mar-", "Gin")
out <- flora_unhyphen_lines(t, c("coriaceous", "margin"))
stopifnot(
  out[1] == "leaves coria",
  out[2] == "ceous, elliptic-",
  # next line must start lower case
  out[3] == "oblong, mar-",
  out[4] == "Gin",
  identical(flora_unhyphen_lines("one line-", "linex"), "one line-")
)

# first matching dictionary row wins, as in the old per-row loop
dict <- list(leaf_shape = c(
  "elliptic-oblong" = "",
  "elliptic" = "elliptic",
  "oblong" = "oblong"
))
stopifnot(
  flora_term_level("leaf_shape", "elliptic-oblong", dict) == "",
  flora_term_level("leaf_shape", "Elliptic", dict) == "elliptic",
  flora_term_level("leaf_shape", "narrowly_oblong", dict) == "oblong",
  flora_term_level("leaf_shape", "ovate", dict) == ""
)
cat("ok\n")
