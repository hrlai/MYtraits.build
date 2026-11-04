# Turner_2014: extract traits from the Tree Flora of Sabah and Sarawak
# Annonaceae treatment (I.M. Turner et al. 2014, TFSS vol. 8: 1--200). Input
# is the PDF text layer (born-digital), segmented in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Turner_2014/raw/trait_data.csv      mapped and unmapped records
#   data/Turner_2014/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Turner_2014/raw/taxon_flags.csv     headings processed, rank, flags
#
# Clause parser after Ashton_2004: each description is split into sentences
# (one organ each), ";" pieces and "," chunks. A chunk takes the organ it
# starts with, else the organ of its piece or sentence. Scorers run per organ;
# every chunk that scores nothing stays as an unmapped row. Later sections
# (Distribution, Ecology, Uses, Notes, vernacular names) are kept one sentence
# per row. Keys and figure captions are not read. The habit of the climbing
# genera annotated in the key to genera is scored by hand in
# raw/manual_rows.csv. Names are as printed.
#
# Annonaceae specifics:
# - The fruit is apocarpous: each monocarp is the fruitlet and is scored as
#   the fruit (entity_measured = monocarp), as for the fruitlets of a syncarp;
#   the number of monocarps per flower and the stipe of each monocarp have no
#   trait (unmapped).
# - Colours on dried specimens ("drying black", "drying grey-brown above") are
#   herbarium colours, not the living colour, and are not scored. Fruit colour
#   is the ripe colour ("green ripening red" -> red).
# - Outer and inner petals are kept apart by entity_measured.
suppressPackageStartupMessages(library(tidyverse))
source("data/Turner_2014/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Turner_2014"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary.csv"
)
dict <- dict[names(dict) %in% names(traits)]

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

prep <- function(x) {
  x |>
    str_replace_all("º", "°") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    # "2–3 cm × 5–10 mm": keep the first unit next to its number
    str_replace_all("(?<=\\d) (mm|cm) [x×] ", " \\1 × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|to\\s)?", num,
  "(?:–", num, ")?(?:\\s?\\(–", num, "\\))?"
)
unit_re <- "(mm|cm|m)\\b"
cnt <- "(?:\\(\\d+–\\)\\s?)?\\d+(?:–\\d+)?(?:\\s?\\(–\\d+\\))?"

grab <- function(x, keyword) {
  str_extract(x, paste0(rng, "\\s?", unit_re, "\\s?", keyword))
}
# "a × b unit" and "a unit × b unit" (outer petals "2–3 cm × 5–10 mm"); the
# first part keeps its own unit, which flora_parse_range() reads first
grab_dims <- function(x) {
  m <- str_match(
    x,
    paste0("(", rng, ")(?:\\s?(mm|cm)\\b)?\\s?×\\s?(", rng, ")\\s?", unit_re)
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  p1 <- if (is.na(m[1, 3])) m[1, 2] else paste(m[1, 2], m[1, 3])
  list(raw = m[1, 1], parts = c(p1, m[1, 4]), unit = m[1, 5])
}

hedged <- \(x) str_detect(x, flora_hedge_re)
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no|non|none)\\b")
hair_re <- paste0(
  "pubescen|tomentose|tomentum|tomentellous|hairy|hairs|hirsute|pilose|",
  "villous|villose|sericeous|velutinous|velvety|woolly|puberul|setose|",
  "strigose|scabrid|lanate|floccose|stellate"
)
dry_re <- "\\bdr(y|ying|ied)\\b"

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the TFSS guide). The first sentence
# of a description is the habit.
sent_organs <- tribble(
  ~organ, ~re,
  "indumentum", "^(indumentum|foliage)\\b",
  "bark", "^(outer |inner )?bark\\b",
  "twigs", "^(young(est|er)? )?twigs?\\b",
  "leaves", "^leaves|^leaf\\b",
  "inflorescences", "^((male|female) )?inflorescences?\\b",
  "flowers", "^((male|female|staminate|pistillate) )?flowers?\\b|^outer petals",
  "fruiting pedicels", "^fruiting (pedicels?|stalks?)",
  "monocarps", "^(fruits?|monocarps?)\\b",
  "seeds", "^seeds?\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "bark", "^(outer |inner )?bark\\b",
  "twigs", "^(young(est|er)? |older )?twigs?\\b",
  "lamina", "^(leaf )?blades?\\b",
  "base", "^bases?\\b",
  "margin", "^margins?\\b",
  # "apex of connective triangular": the stamen connective, not an apex
  "connective", "^((apex|top) of (the )?(stamen )?)?connectives?\\b|^(stamen|anther) connectives?\\b",
  "apex", "^(apex|apices)\\b",
  "midrib", "^midribs?\\b",
  "lateral veins", "^(lateral|main lateral|intramarginal) veins?\\b|^veins?\\b",
  "intercostal venation", "^(intercostal|tertiary|higher order|minor)",
  "petiole", "^petioles?\\b",
  "peduncle", "^(woody )?peduncles?\\b",
  "bracts", "^((basal|medial|upper|lower) )?(bracts?|bracteoles?)\\b",
  "pedicels", "^pedicels?\\b",
  "sepals", "^(sepals?|calyx)\\b",
  "outer petals", "^(blades? of )?outer petals?\\b",
  "inner petals", "^(blades? of )?inner petals?\\b",
  "petals", "^(petals?|corolla|perianth)\\b",
  "torus", "^torus\\b",
  "stamens", "^stamens?\\b",
  "staminodes", "^staminodes?\\b",
  "carpels", "^carpels?\\b",
  "ovules", "^ovules?\\b",
  "stigma", "^stigmas?\\b",
  "stipes", "^stipes?\\b",
  "wall", "^(fruit )?(wall|pericarp)\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text, first_par = TRUE) {
  # "Flowers: pedicels ..., sepals ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- if (si == 1 && first_par) "habit" else match_organ(s, sent_organs)
    # an unrecognised sentence keeps its first word as organ
    if (is.na(s_org)) s_org <- str_to_lower(word(s, 1)) |> str_remove("[^a-z]+$")
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) {
        p_org <- if (pi > 1 && identical(s_org, "leaves")) "lamina" else s_org
      }
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      cur_text <- chunks[1]
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        named <- !is.na(c_org) || ci == 1
        if (!is.na(c_org)) {
          cur <<- c_org
          cur_text <<- chunks[ci]
        }
        tibble(
          sentence = si,
          piece = pi,
          chunk = ci,
          sent_organ = s_org,
          sent_text = s,
          piece_organ = p_org,
          organ = cur,
          named = named,
          organ_text = cur_text,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- row helpers ------------------------------------------------------------------

row <- flora_row
num_rows <- flora_value_rows
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text)
}
cat_rows <- function(trait, x, em = NA_character_) {
  flora_cat_rows(trait, flora_drop_asides(x), dict, em)
}
mono <- "monocarp"
mut <- \(r, ...) if (is.null(r)) NULL else mutate(r, ...)
mono_note <- "Monocarp (fruitlet of the apocarpous fruit) scored as the fruit."

# ---- scorers --------------------------------------------------------------------

hairs_only <- paste0(
  "^((sparsely|densely|minutely|finely|shortly|thinly|very|rather|evenly|",
  "loosely|softly|faintly|slightly|appressed|adpressed|simple|persistently|",
  "short|long|dense|golden|silvery|brownish|brown|greyish|grey|whitish|",
  "yellowish|rusty|reddish|red-brown|pale|buff|tawny|rufous|fulvous|cream|",
  "covered with|or|and|to|-)[ -]?)*",
  "(glabrous|", hair_re, ")[a-z]*$"
)
hair_core <- \(x) str_squish(str_remove(x, "\\s+with [a-z ,-]*\\bhairs?$"))
change_re <- "glabrescent|caducous|when young|initially|becoming|at first|early|young"
# markings after the ground colour ("yellow with reddish streaks", "red with
# white flecks") are not the colour: drop them before scoring
marks_re <- "\\s*\\b(with|and) [^,;]*\\b(streak|stripe|mark|fleck|spot|blotch)[a-z]*\\b.*$"
# colour of a perianth part, not of its hairs, a marking or one surface
perianth_colour_ok <- function(lx) {
  !str_detect(lx, "^(or|to|and) ") &&
    !str_detect(lx, paste0(
      hair_re, "|", dry_re, "|\\bat (the )?(base|apex)\\b|tinge|stripe|",
      "spot|blotch|fleck|streak|mark|veins?|outside|inside|exposed|margin|",
      "surface|tip|flushed|suffused|mottled|when|covered|seemingly|appear"
    ))
}
# ripe colour: after "ripening" or "turning", or before "when ripe"
ripe_colour_text <- function(lx) {
  lx <- str_remove(lx, marks_re)
  if (str_detect(lx, dry_re)) {
    lx <- str_remove(lx, paste0("\\s*", dry_re, ".*$"))
  }
  if (str_detect(lx, "ripening|turning|on ripening|when ripe")) {
    if (str_detect(lx, "when ripe")) {
      return(str_remove(lx, "\\s*when ripe.*$"))
    }
    return(str_remove(str_remove(lx, "^.*\\b(ripening|turning)\\b"), "\\s*on ripening.*$"))
  }
  NA_character_
}

score_chunk <- function(cl) {
  organ <- cl$organ
  x <- cl$text
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  # another taxon named in the chunk is not this taxon's value
  if (hedged(lx) || str_detect(x, "\\b[A-Z]\\. [a-z]{4,}\\b")) {
    return(NULL)
  }
  # dried-specimen colours and surfaces are not scored (the ripe fruit colour
  # before "drying" is read in the monocarp scorer)
  dried <- str_detect(lx, dry_re)
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  if (is.na(organ)) {
    return(NULL)
  }
  switch(
    organ,
    habit = {
      pre <- str_remove(str_extract(lx, "^[^0-9]*"), "\\s*\\b(up to|to|c\\.)\\s*$")
      add(cat_rows("plant_growth_form", pre))
      add(cat_rows("sex_type", lx))
      if (str_detect(lx, "buttress") && !negative(lx)) {
        add(row("stem_base_form", "buttressed", cq = qualifier(lx), raw = x))
      }
      if (str_detect(lx, "stilt[- ]?root") && !negative(lx)) {
        add(row("root_system_type", "stilt-rooted", "stilt_roots", cq = qualifier(lx), raw = x))
      }
      h <- grab(x, "(tall|high)\\b")
      # "Tree to 20 m, usually smaller": the first habit chunk may lack "tall"
      if (is.na(h) && cl$chunk <= 2) h <- str_extract(x, paste0("\\bto ", rng, "\\s?m\\b(?!\\s?(diam|thick))"))
      if (!is.na(h)) add(num_rows("plant_height", h, cq_text = x))
      d <- grab(x, "(in )?(diam(eter|\\.)?|dbh)")
      if (!is.na(d)) {
        add(num_rows("plant_diameter_breast_height", str_remove(d, "\\s?(in )?(diam|dbh).*"), cq_text = x))
      }
    },
    bark = if (so == "bark" && !dried && !str_detect(lx, "^inner|aromatic|lenticel|patch|inside|when cut|slash")) {
      add(cat_rows("bark_colour", lx))
      add(cat_rows("bark_texture", lx))
    },
    twigs = if (so == "twigs" && !dried) {
      core <- hair_core(str_remove(lx, "^(young(est|er)? )?twigs? "))
      if (str_detect(core, hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("stem_hairs", core))
      }
    },
    leaves = if (so == "leaves" && !dried) {
      # "a few scattered hairs" is not the leaf arrangement
      if (!str_detect(lx, hair_re)) {
        add(cat_rows("leaf_arrangement", lx))
        add(cat_rows("leaf_phyllotaxis", lx))
      }
      # "not white or glaucous", "pale or glaucous", papillae "giving a
      # glaucous appearance" (not wax) are not scored
      if (!str_detect(lx, "when fresh|appearance| or ") && !negative(lx)) {
        add(cat_rows("leaf_glaucousness", lx))
      }
      if (str_detect(lx, "\\bsimple\\b")) add(row("leaf_compoundness", "simple", raw = x))
    },
    lamina = if (so == "leaves" && !dried) {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("leaf_length", "leaf_width"), x))
      } else if (!str_detect(lx, "\\d|shiny|dull|glossy|above|below|beneath")) {
        add(cat_rows("leaf_shape", lx))
      }
    },
    base = if (so == "leaves" && !dried && !str_detect(lx, "gland|petiole")) {
      add(cat_rows("leaf_base_shape", lx))
    },
    margin = if (so == "leaves" && !dried) {
      add(cat_rows("leaf_margin", lx))
    },
    apex = if (so == "leaves" && !dried) add(cat_rows("leaf_apex_shape", lx)),
    petiole = if (so == "leaves") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("petiole_length", l, cq_text = x))
    },
    inflorescences = if (so == "inflorescences") {
      add(cat_rows("inflorescence_type", lx))
      # "1- or 2-flowered" is 1–2
      lf <- str_replace(lx, "\\b(\\d+)- or (\\d+)-flowered", "\\1–\\2-flowered")
      m <- str_match(lf, paste0("\\b(", cnt, "|single)[- ]flowered\\b"))
      if (!is.na(m[1, 1]) && !str_detect(lf, "\\d[–-](few|several|many)")) {
        v <- if (m[1, 2] == "single") "1" else m[1, 2]
        add(mut(
          count_rows("flowers_per_inflorescence", x, v, unit = "{count}/{count}"),
          cq = coalesce(cq, qualifier(lx))
        ))
      }
    },
    flowers = if (so == "flowers" && !str_detect(lx, "^(male|female|staminate|pistillate)")) {
      add(cat_rows("flower_structural_sex_type", lx))
      add(cat_rows("flower_perianth_symmetry", lx))
      add(cat_rows("flower_scent_production", lx))
      add(cat_rows("flower_orientation", lx))
      lc <- str_remove(lx, marks_re)
      if (perianth_colour_ok(lc)) add(mut(cat_rows("flower_colour", lc), trait_value_raw = x))
      mer <- c(dimerous = 2, trimerous = 3, tetramerous = 4)
      for (w in names(mer)) {
        if (str_detect(lx, paste0("^(flowers )?((", flora_qual_words |> paste(collapse = "|"), ") )?", w, "\\b"))) {
          add(row("flower_perianth_merism", w, mer[[w]], cq = qualifier(lx), unit = "{count}", raw = x, vt = "mode"))
        }
      }
    },
    pedicels = if (so == "flowers") {
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("pedicel_length", l, cq_text = x))
    },
    sepals = if (so == "flowers") {
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_sepal_length", NA), x))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_sepal_length", l, cq_text = x))
      }
    },
    `outer petals` = ,
    `inner petals` = ,
    petals = if (so == "flowers") {
      em <- if (organ == "petals") NA_character_ else organ
      lc <- str_remove(lx, marks_re)
      if (perianth_colour_ok(lc)) add(mut(cat_rows("flower_colour", lc, em = em), trait_value_raw = x))
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_petal_length", NA), x, em = em))
      } else {
        l <- grab(x, "long")
        if (!is.na(l)) add(num_rows("flower_petal_length", l, em = em, cq_text = x))
      }
    },
    stamens = if (so == "flowers") {
      # a count, not a size ("c. 1.5 mm long") or an open count ("1 or more")
      m <- str_match(lx, paste0("^stamens? (c\\. ?)?(", cnt, ")(?![\\d.–]|\\s?(mm|cm|×|or |to ))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_fertile_stamens_count", x, paste0(coalesce(m[1, 2], ""), m[1, 3])))
      }
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_stamen_length", l, cq_text = x))
    },
    carpels = if (so %in% c("flowers", "habit") || cl$rank_ %in% c("family", "genus")) {
      m <- str_match(lx, paste0("^carpels? (c\\. ?)?(", cnt, ")(?![\\d.–]|\\s?(mm|cm|×|or |to ))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_structural_carpels_count", x, paste0(coalesce(m[1, 2], ""), m[1, 3])))
      }
      l <- grab(x, "long")
      if (!is.na(l)) add(num_rows("flower_carpel_length", l, cq_text = x))
    },
    ovules = {
      m <- str_match(lx, paste0("^ovules? (c\\. ?)?(", cnt, ")\\b(?!\\s?(mm|cm))(?! ?[–-] ?(few|several|many))"))
      if (!is.na(m[1, 1])) {
        add(count_rows("flower_ovules_per_functional_carpel_count", x, m[1, 3]))
      }
    },
    monocarps = if (so == "monocarps") {
      if (!str_detect(lx, "^(fruits?|monocarps?) ")) {
        if (!is.null(grab_dims(x))) {
          add(mut(dim_rows(c("fruit_length", "fruit_width"), x, em = mono), notes = paste(notes, mono_note)))
        } else {
          d <- grab(x, "(in )?(diam(eter|\\.)?|across|wide)")
          if (!is.na(d)) {
            add(num_rows("fruit_width", str_remove(d, "\\s?(in )?(diam|across|wide).*"), em = mono, notes = paste("Diameter scored as fruit_width.", mono_note), cq_text = x))
          } else {
            l <- grab(x, "long")
            if (!is.na(l)) add(num_rows("fruit_length", l, em = mono, notes = mono_note, cq_text = x))
          }
        }
      }
      rc <- ripe_colour_text(lx)
      if (!is.na(rc)) add(mut(cat_rows("fruit_colour", rc, em = mono), trait_value_raw = x))
      core <- hair_core(str_remove(lx, "^(fruits?|monocarps?) [^,]*"))
      if (!dried && str_detect(hair_core(lx), hairs_only) && !str_detect(str_to_lower(cl$piece_text), change_re)) {
        add(cat_rows("fruit_surface_hairs", hair_core(lx), em = mono))
      }
      if (!dried) add(cat_rows("fruit_dehiscence", lx, em = mono))
    },
    wall = if (so == "monocarps") {
      t <- grab(x, "thick")
      if (!is.na(t)) add(num_rows("fruit_wall_thickness", t, em = mono, notes = mono_note, cq_text = x))
    },
    seeds = if (so == "seeds") {
      # "Seeds 2 or 3 in each monocarp" is 2–3
      lx <- str_replace(lx, "^(seeds? )(\\d+) or (\\d+)\\b", "\\1\\2–\\3")
      m <- str_match(lx, paste0("^seeds? ((c\\.|generally|usually|mostly|typically) )?(", cnt, ")\\s+(in|per)\\s+(each )?monocarp"))
      if (!is.na(m[1, 1])) {
        add(mut(
          count_rows("seeds_per_fruit", x, paste0(if_else(coalesce(m[1, 3], "") == "c.", "c. ", ""), m[1, 4]), em = mono, unit = "{count}/{count}", notes = mono_note),
          cq = coalesce(cq, if (!is.na(m[1, 3]) && m[1, 3] != "c.") m[1, 3] else NA_character_)
        ))
      }
      if (!dried) {
        l <- grab(x, "long")
        w <- grab(x, "(diam(eter|\\.)?|across|wide|broad)")
        th <- grab(x, "thick")
        if (!is.na(l)) add(num_rows("seed_length", l, cq_text = x))
        if (!is.na(w)) add(num_rows("seed_width", str_remove(w, "\\s?(diam|across|wide|broad).*"), notes = "Seed diameter scored as seed_width.", cq_text = x))
        if (!is.na(th)) add(num_rows("seed_height", str_remove(th, "\\s?thick.*"), notes = "Seed thickness scored as seed_height (shortest dimension).", cq_text = x))
        if (!str_detect(lx, "\\d")) {
          add(cat_rows("seed_shape", lx))
          add(cat_rows("seed_surface_texture", lx))
          add(cat_rows("seed_surface_reflectivity", lx))
        }
      }
    }
  )
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  # "Male flowers ...; pedicels ...", "Female inflorescences ...": the parts
  # belong to the flowers of one sex (Stelechocarpus, Pseuduvaria)
  sex <- str_match(str_to_lower(cl$sent_text), "^(male|female|staminate|pistillate) (flowers?|inflorescences?)")[, 2]
  if (!is.na(sex) && so %in% c("flowers", "inflorescences")) {
    sex <- c(male = "male", staminate = "male", female = "female", pistillate = "female")[[sex]]
    out <- out |>
      mutate(entity_measured = str_squish(paste(
        paste(sex, if (so == "flowers") "flower" else "inflorescence"),
        coalesce(entity_measured, "")
      )))
  }
  out |> mutate(trait_value_raw = coalesce(trait_value_raw, x))
}

# Leaf indumentum is stated per surface in the first leaf piece ("glabrous
# above, sparsely tomentose below"): hairy when a surface is hairy, glabrous
# when both are; nothing when a surface is glabrescent (a change with age).
# "glabrous except for short hairs along midrib above": hairs on the midrib
# only do not make a surface hairy, so that is not scored either.
leaf_hairs <- function(piece) {
  lp <- str_to_lower(piece)
  if (!str_detect(lp, "^(leaves|leaf (under|lower|upper) ?surface|blades?)\\b")) {
    return(NULL)
  }
  if (str_detect(lp, paste0(
    "(glabr)[a-z]* (or|to) [a-z -]*(", hair_re, ")|",
    "(", hair_re, ")[a-z]* (or|to) [a-z -]*(glabr)|except|midrib|vein|",
    "margin|petiole|base|domatia"
  ))) {
    return(NULL)
  }
  ch <- str_to_lower(flora_split_pieces(piece, ","))
  ch <- str_remove(ch, "^leaves ")
  h <- ch[str_detect(ch, paste0("glabr|", hair_re))]
  if (!length(h) || any(str_detect(h, paste0(change_re, "|lepidot|scale"))) || hedged(piece)) {
    return(NULL)
  }
  hairy <- str_detect(h, hair_re) & !str_detect(h, "^glabrous\\b")
  if (any(hairy)) {
    return(row("leaf_hairs_adult_leaves", "hairy", raw = piece))
  }
  both <- any(str_detect(h, "both (sides|surfaces)|^glabrous$")) ||
    (any(str_detect(h, "above")) && any(str_detect(h, "below|beneath")))
  if (both) row("leaf_hairs_adult_leaves", "glabrous", raw = piece) else NULL
}

character_of <- function(x) {
  lx <- str_to_lower(flora_strip_specimens(x))
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, paste0("glabr|lepidot|", hair_re)) ~ "indumentum",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|fleshy|corky|rigid|fibrous") ~ "texture",
    str_detect(lx, "vein|venation|reticulat|scalariform") ~ "venation",
    str_detect(lx, "caducous|persistent|fugaceous|deciduous") ~ "persistence",
    str_detect(lx, "\\d") ~ "count",
    # word starts only: "paired", "flowered" are not red
    str_detect(lx, "\\b(white|yellow|green|brown|red|black|grey|purple|orange|pink|crimson|cream|mauve)") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "very rarely", "less often", "more rarely", "less commonly")

parse_description <- function(p) {
  text <- prep(p$text)
  cl <- split_clauses(text, p$first_par)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
  cl$rank_ <- p$rank
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5–25 cm long")
    if (!is.null(r) && !cl$named[i] && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
  cl$hair_done <- FALSE
  lp <- cl |> filter(sent_organ %in% "leaves", piece == 1, chunk == 1)
  for (i in lp$id) {
    h <- leaf_hairs(cl$piece_text[i])
    if (!is.null(h)) {
      cl$rows[[i]] <- bind_rows(cl$rows[[i]], h)
      hair_ids <- cl$id[cl$sentence == cl$sentence[i] & cl$piece == 1 &
        str_detect(str_to_lower(cl$text), paste0("glabr|", hair_re))]
      cl$hair_done[hair_ids] <- TRUE
    }
  }
  # locate each chunk after the previous one for its page
  pos <- rep(NA_integer_, nrow(cl))
  from <- 1L
  for (i in seq_len(nrow(cl))) {
    k <- str_locate(str_sub(text, from), fixed(str_sub(cl$text[i], 1, 25)))[1, 1]
    if (!is.na(k)) {
      pos[i] <- from + k - 1L
      from <- pos[i]
    }
  }
  pos <- round(coalesce(pos, 1L) * nchar(p$text) / max(nchar(text), 1))
  pages <- map(pos, \(k) flora_page_at(p, k))
  cl$pdf_page <- map_int(pages, \(x) as.integer(x[["pdf"]]))
  cl$printed_page <- map_int(pages, \(x) as.integer(x[["printed"]]))
  scored <- map_lgl(cl$rows, \(r) !is.null(r) && nrow(r) > 0)
  mapped <- cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    group_by(sentence, piece, trait, entity_measured) |>
    mutate(cq = if_else(
      (value_type == "mode" | coalesce(unit, "") == "{count}/{count}") &
        is.na(cq) & any(cq %in% rare_q),
      "usually",
      cq
    )) |>
    ungroup() |>
    select(-sentence, -piece)
  # only the first size of a trait in a piece is scored; later sizes stay
  # unmapped
  dup_size <- mapped |>
    filter(unit %in% c("mm", "cm", "m")) |>
    left_join(cl |> select(id, sentence, piece), by = "id") |>
    arrange(id) |>
    group_by(sentence, piece, trait, entity_measured) |>
    filter(id != first(id)) |>
    pull(id)
  mapped <- mapped |> filter(!(id %in% dup_size & unit %in% c("mm", "cm", "m")))
  scored[cl$id %in% dup_size & !cl$id %in% mapped$id] <- FALSE
  unmapped <- cl |>
    filter(!scored, !hair_done) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = flora_unmapped_note(
        case_when(
          organ %in% c("lamina", "base", "margin", "apex", "midrib", "lateral veins", "intercostal venation") &
            sent_organ %in% "leaves" ~ paste("leaf", str_replace(organ, "lamina", "blade")),
          organ %in% c("base", "apex", "margin") ~ paste(sent_organ, organ),
          organ == "stipes" ~ "monocarp stipe",
          TRUE ~ organ
        ),
        character_of(text),
        case_when(
          hedged(text) ~ "hedged",
          str_detect(text, "\\b[A-Z]\\. [a-z]{4,}\\b") ~ "names another taxon",
          str_detect(str_to_lower(text), dry_re) ~ "colour or surface on drying (herbarium)",
          organ == "fruiting pedicels" & character_of(text) == "size" ~ "candidate pedicel_length (fruiting pedicel)",
          organ == "stipes" & character_of(text) == "size" ~ "monocarp stipe size; candidate trait",
          organ == "monocarps" & str_detect(str_to_lower(text), "^(monocarps?|fruits?) (c\\. |to |up to )?\\d") ~ "number of monocarps per fruit; candidate trait",
          organ == "lateral veins" & str_detect(text, "pairs") ~ "lateral vein pairs; candidate trait",
          TRUE ~ ""
        )
      ),
      pdf_page,
      printed_page
    )
  bind_rows(mapped |> select(-id), unmapped) |>
    mutate(block = p$block, rank = p$rank, source_section = "Description")
}

# ---- descriptions ------------------------------------------------------------------

desc_paras <- paras |>
  filter(section == "Description", rank %in% c("family", "genus", "species")) |>
  group_by(block) |>
  mutate(first_par = row_number() == 1) |>
  ungroup() |>
  mutate(text = flora_apply_fixes(text, fixes, printed_page))
own <- map_dfr(seq_len(nrow(desc_paras)), \(i) parse_description(desc_paras[i, ]))

# ---- later sections ----------------------------------------------------------------

later <- paras |>
  filter(
    !section %in% c("Description", "Nomenclature"),
    rank %in% c("family", "genus", "species")
  ) |>
  mutate(sentence = map(text, \(t) flora_split_sentences(t))) |>
  select(block, rank, section, pdf_page, printed_page, sentence) |>
  unnest(sentence) |>
  mutate(sentence = str_squish(sentence)) |>
  filter(sentence != "")

later_unmapped <- later |>
  transmute(
    block,
    rank,
    trait = NA_character_,
    trait_value_raw = sentence,
    notes = flora_unmapped_note("section", "text", str_to_lower(section)),
    source_section = section,
    pdf_page,
    printed_page
  )

# ---- higher ranks, assembly -------------------------------------------------------

taxa <- paras |>
  filter(rank == "species") |>
  distinct(block, rank, status) |>
  mutate(
    genus = word(block, 1),
    species = block,
    has_description = block %in% desc_paras$block
  )
members <- taxa |>
  filter(has_description) |>
  transmute(block, genus, group = NA_character_, species, rank)

# Family and genus statements tied to named genera or to part of the group
# ("in some species", "Anaxagorea javanica") hold for those taxa only.
named_genus <- paste0(
  "\\b(", gen_re, ")\\b|\\bspecies\\b|\\bsect\\.|\\bsome\\b|\\bfew\\b|",
  "\\b[A-Z]\\. [a-z]{4,}"
)
higher <- own |> filter(!str_detect(coalesce(trait_value_raw, ""), named_genus))
copied <- flora_copy_down(
  higher_rows = higher,
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
# "Flowers unknown" blocks flower copy-down; no fruit or seed is unknown in
# a species that also has its own fruit description
flowers_unknown <- own |>
  filter(str_detect(trait_value_raw, "^Flowers (unknown|not (seen|known))")) |>
  distinct(block) |>
  pull(block)
flower_traits <- c(
  "flower_structural_sex_type", "flower_perianth_merism", "flower_colour",
  "flower_scent_production", "flower_orientation", "flower_perianth_symmetry",
  "flower_fertile_stamens_count", "flower_structural_carpels_count",
  "flower_ovules_per_functional_carpel_count"
)
copied <- copied |> filter(!(block %in% flowers_unknown & trait %in% flower_traits))

out <- bind_rows(
  own |> mutate(rank_scored_from = NA_character_),
  copied,
  later_unmapped |> mutate(rank_scored_from = NA_character_)
) |>
  left_join(
    bind_rows(
      taxa |> select(block, taxon_rank = rank),
      paras |> filter(rank %in% c("family", "genus")) |> distinct(block, taxon_rank = rank)
    ),
    by = "block"
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank,
    rank_scored_from,
    commonness_qualifier = cq,
    # a TFSS species description describes the species in Sabah and Sarawak
    population_region = if_else(
      taxon_rank == "species" & is.na(rank_scored_from) & !is.na(trait),
      "Sabah and Sarawak",
      NA_character_
    ),
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
  )

# habit of the climbing genera annotated only in the key to genera
manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c"),
  na = ""
) |>
  mutate(across(c(pdf_page, printed_page), as.integer))
out <- bind_rows(out, manual)

out <- out |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(match(taxon_rank, c("family", "genus", "species")), taxon_name, pdf_page) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")

climbers <- c(
  "Uvaria", "Artabotrys", "Desmos", "Sphaerocoryne", "Friesodielsia",
  "Fissistigma", "Pyramidanthe", "Mitrella"
)
flags <- taxa |>
  left_join(
    out |> filter(!is.na(trait), trait != "") |> count(taxon_name, name = "n_mapped_rows"),
    by = c("block" = "taxon_name")
  ) |>
  transmute(
    taxon_name = block,
    taxon_rank = rank,
    status,
    n_mapped_rows = coalesce(n_mapped_rows, 0L),
    flag = case_when(
      block %in% sole_species ~ "Only species of its genus in Sabah and Sarawak; heading unnumbered.",
      block %in% flowers_unknown ~ "Flowers unknown: no flower values copied down.",
      TRUE ~ ""
    )
  ) |>
  bind_rows(
    tibble(taxon_name = climbers) |>
      left_join(
        out |> filter(!is.na(trait), trait != "") |> count(taxon_name, name = "n_mapped_rows"),
        by = "taxon_name"
      ) |>
      transmute(
        taxon_name,
        taxon_rank = "genus",
        status = "key annotation",
        n_mapped_rows = coalesce(n_mapped_rows, 0L),
        flag = "Climbing genus described only as an annotation in the key to genera (pp. 7--10); only its habit is scored, at genus rank, from raw/manual_rows.csv; the annotation is kept as one unmapped row."
      )
  ) |>
  bind_rows(tibble(
    taxon_name = "Pseuduvaria bruneiensis",
    taxon_rank = "species",
    status = "key annotation",
    n_mapped_rows = 0L,
    flag = "Annotated in the key to Pseuduvaria (p. 171) with nomenclature only; known only from Brunei, outside the treatment's region; no description, not scored."
  ))
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
