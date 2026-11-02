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
# place-name abbreviations do not end a sentence (Prance_1995 p. 158)
s <- flora_split_sentences(paste(
  "Endemic to Sabah; recorded from Lahad Datu, Karamuak, Mt. Tawai and",
  "Mt. Silam. Sarawak: Bt. Raya, Sg. Pahang, 4th Div. Kapit. Fruits red."
))
stopifnot(
  length(s) == 3,
  startsWith(s[2], "Sarawak: Bt. Raya, Sg. Pahang, 4th Div. Kapit")
)

# flora_cat_rows() keeps a term that a blank row claimed in context, although
# the term alone matches a level (Adema_1996 p. 324)
d <- list(flower_colour = c(
  "greenish(?= white)" = "",
  "white" = "white_cream",
  "greenish" = "green"
))
stopifnot(
  flora_cat_rows("flower_colour", "creamish or greenish white", d)$
    trait_value_clean == "white_cream"
)

# shared dictionary seed rows
seed <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
)
clean <- \(x, trait) flora_score_cat(x, trait, seed)$trait_value_clean
stopifnot(
  # "greenish white" is white (Prance_2012 p. 29, Adema_1996 p. 324)
  clean("petals greenish white", "flower_colour") == "white_cream",
  clean("yellowish-white", "flower_colour") == "white_cream",
  clean("greenish yellow", "flower_colour") == "green yellow_orange",
  # resin colour is not bark colour (Kochummen_1995 p. 45)
  clean("grey with black or white resinous gum", "bark_colour") == "grey",
  clean("blackish brown", "bark_colour") == "black brown",
  clean("grey lanate pubescence", "leaf_hairs_adult_leaves") == "hairy",
  clean("arachnoid when young", "stem_hairs") == "hairy"
)

cat("ok\n")
