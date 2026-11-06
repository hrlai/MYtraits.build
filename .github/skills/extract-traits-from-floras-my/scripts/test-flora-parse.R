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
# infrageneric rank abbreviations do not end a sentence (Kochummen_2000 p. 187)
s <- flora_split_sentences(paste(
  "Leaves spirally arranged (subgen. Artocarpus) or distichous (subgen.",
  "Pseudojaca), simple. Inflorescences unisexual, in sect. Ficus solitary."
))
stopifnot(length(s) == 2, endsWith(s[1], "simple"))

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

# shared dictionary rows
seed <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary.csv"
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

# a leading "to" applies to both dimensions (Jaman_2020 p. 83)
d1 <- flora_parse_dims("pinnules to 9 × 1.5 cm")
d2 <- flora_parse_dims("5 to 9 × 2 cm")
d3 <- flora_parse_dims("8–10 × 3–4 mm")
stopifnot(
  d1$len$value_type == "maximum", d1$len$value == 9,
  d1$wid$value_type == "maximum", d1$wid$value == 1.5,
  d2$len$value_type == "mean", d2$wid$value_type == "mean",
  identical(d3$len$value_type, c("minimum", "maximum")),
  identical(d3$wid$value, c(3, 4))
)
# U+037E for ";" is a bad glyph (Ashton_2004, TFSS vol. 5)
stopifnot(str_detect("leaves; twigs", .flora_bad_glyph_re))
# "more often" is its own commonness word (Jaman_2020 p. 89)
stopifnot(str_match("more often c. 45 cm", flora_qual_re)[, 2] == "more often")
# a full stop with no space ends a sentence (DeWilde_2018 p. 161), but not
# after a capital ("E.glaber")
stopifnot(
  flora_norm("bark pinkish.Twigs grey") == "bark pinkish. Twigs grey",
  flora_norm("as in E.glaber") == "as in E.glaber"
)
# "A rarely B" is "A or rarely B" (Hou_2000, Saraca hullettii p. 165)
stopifnot(
  flora_norm("apex acuminate rarely caudate") ==
    "apex acuminate or rarely caudate",
  flora_norm("acuminate, rarely caudate") == "acuminate, rarely caudate"
)
# "(rarely more)" moves after the keyword (Hou_2000 p. 152)
stopifnot(
  flora_norm("up to 45 m (rarely more) tall") ==
    "up to 45 m tall (rarely more)",
  flora_norm("100 cm (or more) diameter") == "100 cm diameter (or more)"
)
# a count takes the commonness word of its chunk (Turner_2014,
# Polyalthia dolichopoda "generally single-flowered, occasionally 2-flowered")
stopifnot(
  flora_count_rows("t", "occasionally 2-flowered", "2")$cq == "occasionally",
  is.na(flora_count_rows("t", "2-flowered", "2")$cq),
  flora_count_rows("t", "generally single-flowered", "1")$cq == "generally",
  # a later word qualifies another value (Adema_1996 p. 273, p. 344)
  all(is.na(flora_count_rows("t", "stamens 8 (rarely fewer)", "8")$cq)),
  all(is.na(flora_count_rows("t", "ovary 2- rarely 3-locular", "2")$cq))
)
# "seemingly" is a hedge (Turner_2014, Polyalthia dolichopoda)
stopifnot(
  is.null(flora_cat_rows("flower_colour", "seemingly darkening to red", seed))
)
# "or more frequently strangler" (Kochummen_2000, Ficus xylophylla p. 317)
stopifnot(
  str_match("or more frequently strangler", flora_qual_re)[, 2] ==
    "more frequently"
)

cat("ok\n")
