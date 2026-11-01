# Beentje_2023: extract traits from the Flora of Peninsular Malaysia
# Pandanaceae treatment (H.J. Beentje & M.W. Callmander 2023, FPM Series II
# vol. 10: 117--215). Input is the PDF text layer (born-digital), segmented
# in raw/segment_source.R.
#
# Writes (run from the repository root):
#   data/Beentje_2023/raw/trait_data.csv      mapped and unmapped records
#   data/Beentje_2023/raw/unmapped_terms.csv  summary of the unmapped records
#   data/Beentje_2023/raw/taxon_flags.csv     headings processed, rank, flags
#
# Each description is split into sentences (one organ each), ";" pieces and
# "," chunks. A chunk takes the organ it starts with, else the organ of its
# piece or sentence. Scorers run per organ; every chunk that scores nothing
# stays as an unmapped row. Later sections (Distribution, Ecology, Uses,
# Taxonomy, Notes, vernacular names, conservation status) are kept one
# sentence per row; Ecology statements citing Corner (1978) are in
# raw/manual_rows.csv. Keys, the "Spot characters" and "Specific habitats"
# lists and figures are not read. Names are as printed.
#
# Family decisions (metadata.yml questions):
#   - the syncarp (compound fruiting head) is not the fruit: its size stays
#     unmapped; the drupe (Benstonea, Pandanus), the phalange of fused drupes
#     (Pandanus sect. with phalanges) and the berry (Freycinetia) are the
#     fruit. Ripe syncarp colour is scored as fruit_colour with
#     entity_measured = syncarp.
#   - arborescent pandans and pandan shrubs score palmoid (standing rule).
#   - taxa "Not accepted as native" are not scored; cultivated taxa are
#     scored with a "cultivated" note.
suppressPackageStartupMessages(library(tidyverse))
source("data/Beentje_2023/raw/segment_source.R")
source(".github/skills/extract-traits-from-floras-my/scripts/flora_assemble.R")

id <- "Beentje_2023"
raw_dir <- file.path("data", id, "raw")
traits <- yaml::read_yaml("config/traits.yml")$traits$elements
dict <- flora_load_dict(
  ".github/skills/extract-traits-from-floras-my/assets/flora_dictionary_seed.csv"
)
dict <- dict[names(dict) %in% names(traits)]
# Pandanaceae terms missing from the shared dictionary; first, so they win
# over shorter shared terms
local_terms <- list(
  # palm-like trees and shrubs score palmoid after the printed noun
  # (standing rule, user 2026-10-30)
  plant_growth_form = c(
    "\\bsub-?shrubs?\\b" = "",
    # the host tree of a climber or epiphyte
    "\\b(on|forest|the) trees?\\b|\\btree trunks?\\b" = "",
    "\\barborescent\\b" = "tree palmoid",
    "\\bshrubs?\\b|\\bshrubby\\b" = "shrub palmoid",
    "\\bwoody climbers?\\b|\\blianas?\\b" = "climber_woody",
    "\\bclimbers?\\b" = "climber"
  ),
  plant_growth_substrate = c(
    "\\bepiphyt[a-z]*" = "epiphyte",
    "\\bon (limestone|rocks?|boulders)\\b|growing on rocks" = "lithophyte"
  ),
  stem_growth_habit = c(
    "\\bsuberect\\b" = "",
    "\\bacaulescent\\b|\\bstemless\\b" = "acaulescent",
    "\\berect\\b" = "erect",
    "\\bdecumbent\\b" = "decumbent",
    "\\bprostrate\\b" = "prostrate",
    "\\bcreeping\\b" = "creeping",
    "\\btufted\\b" = "tufted",
    "\\brhizomatous\\b|\\brhizomes?\\b" = "rhizomatous",
    "\\bstolons?\\b|\\bstoloniferous\\b" = "stoloniferous",
    "\\bsprawling\\b" = "sprawling"
  ),
  # amplexicaul and clasping bases stay unmapped (Cheek_2012 question 8);
  # species silent on the base receive the family / genus "base sheathing"
  leaf_base_shape = c(
    "\\b(half-)?amplexicaul\\b|\\bclasping( the)? stem\\b" = "",
    "\\bsheathing\\b" = "sheathing"
  ),
  leaf_glaucousness = c(
    "\\bsubglaucous\\b|\\bslightly glaucous\\b" = "subglaucous",
    "\\bglaucous\\b" = "glaucous"
  ),
  fruit_type = c(
    "\\bbaccate\\b|\\bberry-like\\b|\\bberr(y|ies)\\b" = "berry",
    "\\bdrupaceous\\b|\\bdrupes?\\b" = "drupe"
  ),
  flower_scent_production = c(
    "fragrant|scented|sweet-smelling|perfumed" = "scent_produced"
  ),
  seed_shape = c(
    "\\bellipsoid-flattened\\b|\\bflattened[- ]ellipsoid\\b" = "ellipsoid_flattened"
  ),
  fruit_colour = c(
    "\\bbrick red\\b" = "red",
    "\\bblackish\\b|\\balmost black\\b" = "black"
  )
)
for (t in names(local_terms)) {
  dict[[t]] <- c(local_terms[[t]], dict[[t]])
}

fixes <- read_csv(
  file.path(raw_dir, "source_fixes.csv"),
  col_types = cols(.default = "c", page = "i")
)

# ---- text preparation ---------------------------------------------------------

prep <- function(x) {
  x |>
    # figure dash and spaced dashes inside ranges
    str_replace_all("‒", "–") |>
    str_replace_all("(?<=\\d)\\s?-\\s?(?=\\d)", "–") |>
    str_replace_all("\\(\\s?[-–]\\s?(?=\\d)", "(–") |>
    str_replace_all("(?<=\\d)\\s?[-–]\\s?\\)\\s?", "–)") |>
    str_replace_all("(?<=[\\d)])\\s?[x×]\\s?(?=[\\d(]|c\\.)", " × ") |>
    flora_norm()
}

num <- "\\d+(?:\\.\\d+)?"
rng <- paste0(
  "(?:\\(", num, "–\\)\\s?)?(?:c\\.\\s?|to\\s)?", num,
  "(?:–", num, ")?(?:\\s?\\(–", num, "\\))?"
)
unit_re <- "(mm|cm|m)\\b"

grab <- function(x, keyword) {
  str_extract(x, paste0(rng, "\\s?", unit_re, "\\s?", keyword))
}
grab_dims <- function(x) {
  m <- str_match(
    x,
    paste0("(", rng, ")\\s?×\\s?(", rng, ")(?:\\s?×\\s?(", rng, "))?\\s?", unit_re)
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  parts <- m[1, 2:4]
  list(raw = m[1, 1], parts = parts[!is.na(parts)], unit = m[1, 5])
}

hedged <- \(x) {
  str_detect(x, flora_hedge_re) |
    str_detect(x, "\\?|\\bassumed\\b|more-or-less (c\\. )?\\d")
}
# Text for scoring: asides dropped ("(juvenile c. 120 × 4 cm)", "(in young
# specimens ...)") and extremes with words or a query ("(–?5)", "(–435 in
# juvenile leaves)"); trait_value_raw keeps the whole chunk.
num_text <- function(x) {
  x |>
    flora_drop_asides() |>
    str_remove_all("\\(–[^)]*[a-z?][^)]*\\)") |>
    # "or less often solitary shrub": the word qualifies "solitary", not the
    # form noun (growth form wording rule)
    str_remove_all("\\b(less often|rarely|sometimes|often|occasionally) (?=(solitary|clustering)\\b)") |>
    str_squish()
}
# Chunks not scored: values contradicted elsewhere in the same description
# (metadata.yml question)
skip_chunks <- c(
  # F. imbricata: berry 24 mm wide in a syncarp 15--17 mm wide
  "berry to 10–26 × 24 mm"
)
qualifier <- \(x) str_match(x, flora_qual_re)[, 2]
negative <- \(x) str_detect(x, "\\b(without|not|absent|lacking|no|non|unknown)\\b")
# a measurement or state of young, immature or flowering-stage material
not_mature <- "\\bbefore (maturity|ripening)\\b|\\b(young|juvenile|immature|in flower|at first|flowering stage|rosette stage|seedlings?|unripe|pruned|harvested|cultivated plants)\\b"
ripe_re <- "\\b(ripe|ripening|mature|maturing|maturity)\\b"

# ---- organs ---------------------------------------------------------------------

# Sentence organs (the description order of the FPM guide)
sent_organs <- tribble(
  ~organ, ~re,
  "stems", "^(stems?|trunks?|ultimate branches)\\b",
  "roots", "^(prop|stilt|roots?)\\b",
  "leaves", "^leaves|^leaf\\b",
  "male inflorescences", "^(male|staminate) (inflorescences?|spadix|spikes|flowers)|^staminal phalanges",
  "female inflorescences", "^(female|pistillate) (inflorescences?|spadix|flowers)",
  "inflorescences", "^(inflorescences?|spathes)\\b",
  "syncarps", "^syncarps?\\b|^syncarp\\(s\\)",
  "drupes", "^(mature |individual )?drupes?\\b",
  "seeds", "^seeds?\\b",
  "endocarp", "^endocarp\\b",
  "perianth", "^perianth\\b"
)
# Piece and chunk labels inside a sentence
part_organs <- tribble(
  ~organ, ~re,
  "leaves", "^(mature |adult )leaves\\b",
  "roots", "^((basal|few|brown) )*(prop|stilt|prop/stilt|stilt/prop)[ -]?roots?|^roots?\\b",
  "stems", "^(stems?|trunks?)\\b",
  "auricles", "^(leaf base )?auricles?\\b",
  "base", "^(leaf )?bases?\\b|^at base\\b|^narrowed to",
  "apex", "^(leaf )?(apex|apices|tips?)\\b",
  "margin", "^(leaf )?margins?\\b",
  "midrib", "^midribs?\\b",
  "pleats", "^((upper|lateral|apical|ventral) )*(leaf )?(surface )?pleats?\\b|^upper (leaf )?surface|^\\d-pleated|^flat|^m-shaped|^more-or-less flat",
  "veins", "^(\\d|\\(|c\\. )?[\\d–() ]*(parallel |longitudinal )?veins|^(tertiary )?venation|^(not |slightly )?tessellate|^cross-?veins",
  "sheath", "^(the )?sheaths?\\b",
  "peduncle", "^peduncles?\\b",
  "bracts", "^((lower|upper|outer|inner|median|lowest|other|innermost|outermost|ultimate|the) )*(floral )?bracts?\\b",
  "spikes", "^spikes?\\b|^individual spike",
  "stamens", "^stamens?\\b",
  "filaments", "^filaments?\\b",
  "anthers", "^anthers?\\b",
  "pedicels", "^pedicels?\\b",
  "phalanges", "^phalanges\\b(?! of stamens)",
  "body", "^(the )?(drupe )?body\\b",
  "drupes", "^(mature |individual )?drupes?\\b",
  "berries", "^berr(y|ies)\\b",
  "pileus", "^pileus\\b",
  "styles", "^styles?\\b",
  "stigmas", "^(stigmas?|stigmatic grooves?)\\b",
  "endocarp", "^endocarp\\b",
  "mesocarp", "^((apical|basal|upper|lower|inner) )?mesocarp\\b",
  "seeds", "^seeds?\\b"
)
match_organ <- function(x, tab) {
  x <- str_to_lower(str_remove(x, "^(and|or|but|the|with) "))
  hit <- which(str_detect(x, tab$re))
  if (length(hit)) tab$organ[hit[1]] else NA_character_
}

split_clauses <- function(text, first_para) {
  # "Inflorescences: bracts ..." -> sentence organ + pieces
  text <- str_replace_all(text, "(?<=^|\\. )([A-Z][a-z]+( [a-z]+)?):\\s", "\\1; ")
  sents <- flora_split_sentences(text)
  map_dfr(seq_along(sents), \(si) {
    s <- str_remove(sents[si], "\\.$")
    s_org <- match_organ(s, sent_organs)
    # the first sentence of a description is the habit
    if (is.na(s_org) && si == 1 && first_para) s_org <- "habit"
    pieces <- flora_split_pieces(s, ";")
    map_dfr(seq_along(pieces), \(pi) {
      p_org <- match_organ(pieces[pi], part_organs)
      if (is.na(p_org)) p_org <- s_org
      chunks <- flora_split_pieces(pieces[pi], ",")
      cur <- p_org
      map_dfr(seq_along(chunks), \(ci) {
        c_org <- match_organ(chunks[ci], part_organs)
        # "in 2–3-locular phalanges of drupes 7.5–9.5 × 3–4.5 cm"
        if (str_detect(chunks[ci], "phalanges of drupes,? [\\d(]")) c_org <- "phalanges"
        # "base white and amplexicaul, with cucullate acute apex"
        if (is.na(c_org) && identical(s_org, "leaves") && identical(cur, "base") &&
          str_detect(chunks[ci], "\\b(apex|tip)\\b")) {
          c_org <- "apex"
        }
        named <- !is.na(c_org) || ci == 1
        if (!is.na(c_org)) cur <<- c_org
        tibble(
          sentence = si,
          piece = pi,
          chunk = ci,
          sent_organ = s_org,
          sent_text = s,
          piece_organ = p_org,
          organ = cur,
          named = named,
          piece_text = pieces[pi],
          text = chunks[ci]
        )
      })
    })
  })
}

# ---- row helpers ------------------------------------------------------------------

row <- flora_row
num_rows <- function(trait, raw, em = NA_character_, notes = NA_character_, cq_text = raw) {
  flora_value_rows(trait, raw, em, notes, cq_text, wide_bounds = TRUE)
}
count_rows <- flora_count_rows
dim_rows <- function(traits2, raw_text, em = NA_character_, cq_text = raw_text) {
  flora_dim_rows(traits2, raw_text, grab_dims, em, cq_text, wide_bounds = TRUE)
}
cat_rows <- function(trait, x, em = NA_character_, weak_re = NULL) {
  flora_cat_rows(trait, x, dict, em, weak_re = weak_re)
}
weak <- "\\b(almost|nearly|seemingly|possibly)\\b|\\?"

# ---- scorers --------------------------------------------------------------------

infl_kind <- function(so) {
  case_when(
    so == "male inflorescences" ~ "male inflorescence",
    so == "female inflorescences" ~ "female inflorescence",
    TRUE ~ "inflorescence"
  )
}

score_chunk <- function(cl) {
  organ <- cl$organ
  raw <- cl$text
  x <- num_text(raw)
  lx <- str_to_lower(x)
  so <- coalesce(cl$sent_organ, "")
  if (hedged(lx) || is.na(organ) || x == "" || raw %in% skip_chunks) {
    return(NULL)
  }
  # material from outside Peninsular Malaysia ("Male inflorescences not seen
  # in Peninsular Malaysia; elsewhere probably pendent ...")
  if (str_detect(str_to_lower(cl$sent_text), "not seen in peninsular malaysia") ||
    str_detect(str_to_lower(cl$piece_text), "\\belsewhere\\b")) {
    return(NULL)
  }
  out <- list()
  add <- function(r) {
    if (!is.null(r) && nrow(r)) out[[length(out) + 1]] <<- r
  }
  climber <- str_detect(str_to_lower(cl$sent_text), "\\bclimb")
  epiphyte <- str_detect(str_to_lower(cl$sent_text), "\\bepiphyt")
  switch(
    organ,
    habit = {
      # words of the chunk without its measurements ("Shrubs to 2 m tall or
      # climber with straggling branches")
      pre <- str_squish(str_remove_all(lx, paste0("\\b(up to |to |c\\. )?", rng, "\\s?", unit_re, "( (tall|high|long))?")))
      if (!negative(pre)) {
        add(cat_rows("plant_growth_form", pre, weak_re = weak))
        add(cat_rows("plant_growth_substrate", pre, weak_re = weak))
        # "lower erect part covered in dead leaves" is part of the stem
        if (!str_detect(pre, "\\bpart\\b")) add(cat_rows("stem_growth_habit", pre, weak_re = weak))
      }
      add(cat_rows("sex_type", lx))
      if (str_detect(lx, "\\bstolons?\\b")) {
        add(row("vegetative_reproduction_ability", "stolons", "vegetative", raw = x))
        add(row("clonal_spread_mechanism", "stolons", "stolon", raw = x))
      }
      if (str_detect(lx, "adventitious roots clasping")) {
        add(row("plant_climbing_mechanism", "adventitious roots clasping", "adventitious_roots", raw = x))
      }
      # heights of free-standing plants; a height after "trunk" or "stem(s)"
      # is the trunk's (entity_measured); climbers give a length
      h <- grab(x, "(tall|high)\\b")
      if (is.na(h) && !climber) h <- grab(x, "(?=,|$)")
      cq_x <- paste(cl$prev %||% "", x)
      if (!is.na(h) && !epiphyte) {
        before <- str_to_lower(str_sub(x, 1, str_locate(x, fixed(h))[1, 1] - 1))
        em <- case_when(
          str_detect(before, "\\btrunks?\\b") ~ "trunk",
          str_detect(before, "\\bstems?\\b") ~ "stem",
          TRUE ~ NA_character_
        )
        tr <- if (climber && !str_detect(lx, "shrub")) "plant_height_climbing_plant" else "plant_height"
        if (!str_detect(before, "\\bbranches\\b")) {
          add(num_rows(tr, h, em = em, cq_text = if (cl$named) x else cq_x))
        }
      }
      l <- grab(x, "long\\b")
      if (!is.na(l) && climber && str_detect(lx, "climbers?,? [\\d(]|^[\\d(]") && !str_detect(lx, "stems?|branches")) {
        add(num_rows(
          "plant_height_climbing_plant", l,
          notes = "Climber length ('m long') scored as plant_height_climbing_plant.",
          cq_text = x
        ))
      }
    },
    roots = ,
    stems = {
      if (str_detect(lx, "prop|stilt") && !str_detect(lx, "\\b(no|without|absent)\\b")) {
        q <- qualifier(lx)
        v <- c(
          if (str_detect(lx, "stilt")) "stilt_roots",
          if (str_detect(lx, "prop")) "prop_roots"
        )
        add(row(
          "root_system_type", str_extract(lx, "(prop/stilt|stilt/prop|prop|stilt)[ -]?roots?"),
          paste(v, collapse = " "), cq = q, raw = x
        ))
      }
      # trunk or stem height of a free-standing plant (tree-fern rule:
      # plant_height with entity_measured = trunk)
      h <- grab(x, "(tall|high)\\b")
      if (!is.na(h) && organ == "stems" && !epiphyte && !climber && !str_detect(lx, "\\blong\\b")) {
        em <- if (str_detect(str_to_lower(cl$sent_text), "^trunk|^stems?,? trunk") || str_detect(lx, "trunk")) "trunk" else "stem"
        add(num_rows("plant_height", h, em = em, cq_text = x))
      }
      if (str_detect(lx, "\\broots?\\b") && str_detect(lx, "clasping")) {
        add(row("plant_climbing_mechanism", "roots clasping", "adventitious_roots", raw = x))
      }
    },
    leaves = if (so == "leaves") {
      if (!str_detect(lx, not_mature)) {
        if (!is.null(grab_dims(x))) {
          add(dim_rows(c("leaf_length", "leaf_width"), x))
        } else {
          l <- grab(x, "long\\b")
          if (!is.na(l) && str_detect(lx, "^leaves")) add(num_rows("leaf_length", l, cq_text = x))
        }
      }
      if (!str_detect(lx, "\\d")) {
        add(cat_rows("leaf_shape", lx, weak_re = weak))
        if (str_detect(lx, "^(leaves )?simple$")) add(row("leaf_compoundness", "simple", raw = x))
        if (str_detect(lx, "crowded")) add(cat_rows("leaf_arrangement", lx, weak_re = weak))
      }
      if (!negative(lx) && !str_detect(lx, "hardly")) add(cat_rows("leaf_glaucousness", lx, weak_re = weak))
    },
    base = if (so == "leaves") {
      if (!negative(lx)) add(cat_rows("leaf_base_shape", lx, weak_re = weak))
    },
    auricles = if (so == "leaves") {
      if (!negative(lx) && cl$named) add(row("leaf_base_shape", "auricles", "auriculate", raw = x))
    },
    # "rounded-caudate" and the boat-prow (cucullate) apex have no level
    apex = if (so == "leaves" && !str_detect(lx, "prow|boat|cucullate") &&
      !(str_detect(lx, "caudate") && !str_detect(lx, "acuminate"))) {
      add(cat_rows("leaf_apex_shape", lx, weak_re = weak))
    },
    margin = if (so == "leaves") {
      # prickles on the margin are armature, not the margin shape
      if (!str_detect(lx, "prickl|spin")) add(cat_rows("leaf_margin", lx, weak_re = weak))
    },
    `male inflorescences` = ,
    `female inflorescences` = ,
    inflorescences = {
      em <- infl_kind(so)
      l <- grab(x, "long\\b")
      # words before the measurement, in this chunk and the earlier chunks of
      # its piece, must not name another organ ("spikes 23–26 cm long",
      # "with ... bracts, 4–5 cm long")
      lp <- str_to_lower(num_text(cl$piece_text))
      before <- if (is.na(l)) "" else str_sub(lp, 1, coalesce(str_locate(lp, fixed(str_to_lower(l)))[1, 1], 1L) - 1)
      # "Male inflorescences almost erect, 18 cm long": first piece only
      if (!is.na(l) && cl$piece == 1 && so != "inflorescences" &&
        !str_detect(before, "peduncle|spikes?|bracts?|axis|heads?")) {
        add(num_rows("inflorescence_length", l, em = em, cq_text = x))
      }
      add(cat_rows("sex_type", lx))
      add(cat_rows("flower_scent_production", lx, em = em, weak_re = weak))
    },
    spikes = ,
    bracts = if (str_detect(so, "inflorescences")) {
      add(cat_rows("flower_scent_production", lx, em = infl_kind(so), weak_re = weak))
    },
    anthers = if (str_detect(so, "male inflorescences|male flowers|inflorescences")) {
      # "anther c. 3 mm with ... apiculus 3–3.5 mm long": not the apiculus
      x <- str_remove(x, "\\s(with|including|plus)\\b.*")
      if (!is.null(grab_dims(x))) {
        add(dim_rows(c("flower_anther_length", NA), x, em = "male flower"))
      } else {
        l <- grab(x, "(long\\b|(?=,|$| with| including| of))")
        if (!is.na(l)) add(num_rows("flower_anther_length", l, em = "male flower", cq_text = x))
      }
    },
    filaments = if (str_detect(so, "inflorescences|flowers|phalanges")) {
      l <- grab(x, "(long\\b|(?=,|$| with))")
      if (!is.na(l)) add(num_rows("flower_filament_length", l, em = "male flower", cq_text = x))
    },
    syncarps = {
      if (str_detect(lx, ripe_re) && !str_detect(lx, not_mature) && !str_detect(lx, "shoulder|tips?\\b|apex|base")) {
        add(cat_rows("fruit_colour", lx, em = "syncarp", weak_re = weak))
      }
    },
    drupes = ,
    berries = ,
    phalanges = if (organ != "phalanges" || so %in% c("syncarps", "drupes")) {
      em <- if (organ == "phalanges") "phalange" else NA_character_
      if (cl$named && organ != "phalanges") {
        add(cat_rows("fruit_type", str_extract(lx, "^(mature |individual )?(drupes?|berr(y|ies))"), weak_re = weak))
      }
      if (!str_detect(lx, not_mature) && !str_detect(str_to_lower(cl$piece_text), "\\((immature|young)\\)")) {
        if (!is.null(grab_dims(x))) {
          add(dim_rows(c("fruit_length", "fruit_width", NA), x, em = em))
        } else {
          l <- grab(x, "long\\b")
          if (!is.na(l)) add(num_rows("fruit_length", l, em = em, cq_text = x))
        }
      }
      if (str_detect(lx, ripe_re) && !str_detect(lx, not_mature) && !str_detect(lx, "shoulder|tips?\\b|apex|base")) {
        add(cat_rows("fruit_colour", lx, em = em, weak_re = weak))
      }
    },
    seeds = if (so == "seeds" || cl$named) {
      if (!str_detect(lx, "wall|thick|cavit|locule")) {
        if (!is.null(grab_dims(x))) {
          add(dim_rows(c("seed_length", "seed_width"), x))
        } else {
          l <- grab(x, "(long\\b|(?=,|$))")
          if (!is.na(l)) add(num_rows("seed_length", l, cq_text = x))
        }
        if (!str_detect(lx, "\\d")) add(cat_rows("seed_shape", lx, weak_re = weak))
      }
    }
  )
  # genus and family statements on fruit type ("Syncarps of simple drupes",
  # "Syncarps berry-like", "baccate when ripe")
  if (cl$rank %in% c("genus", "family") && organ %in% c("syncarps", "female inflorescences")) {
    add(cat_rows("fruit_type", lx, weak_re = weak))
  }
  out <- bind_rows(out)
  if (!nrow(out)) {
    return(NULL)
  }
  out |> mutate(trait_value_raw = raw)
}

character_of <- function(x) {
  lx <- str_to_lower(x)
  case_when(
    str_detect(lx, paste0(num, "\\s?", unit_re)) ~ "size",
    str_detect(lx, "prickl|spin|armed|unarmed|thorn|toothlet") ~ "armature",
    str_detect(lx, "coriaceous|chartaceous|membranous|woody|bony|fleshy|fibrous|hard|stiff|rigid") ~ "texture",
    str_detect(lx, "vein|venation|tessellate|pleat") ~ "venation and pleats",
    str_detect(lx, "\\d") ~ "number",
    str_detect(lx, "white|yellow|green|brown|red|black|grey|purple|orange|pink|cream|glaucous") ~ "colour",
    TRUE ~ "description"
  )
}

rare_q <- c("sometimes", "rarely", "occasionally", "very rarely", "less often", "less commonly")

parse_description <- function(p) {
  text <- prep(p$text)
  cl <- split_clauses(text, p$first_para)
  if (!nrow(cl)) {
    return(NULL)
  }
  cl$id <- seq_len(nrow(cl))
  cl$rank <- p$rank
  cl <- cl |>
    group_by(sentence, piece) |>
    mutate(prev = lag(text)) |>
    ungroup()
  cl$rows <- map(seq_len(nrow(cl)), \(i) {
    r <- score_chunk(as.list(cl[i, ]))
    # A chunk that inherits its organ gives a number only when the number
    # comes first ("5–25 cm long")
    if (!is.null(r) && !cl$named[i] && !identical(cl$organ[i], "habit") && !str_detect(str_to_lower(cl$text[i]), "^((up )?to |c\\. ?)?[\\d(]")) {
      r <- r |> filter(!unit %in% c("mm", "cm", "m"))
    }
    r
  })
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
  mapped <- if (!any(scored)) tibble(id = integer()) else cl |>
    filter(scored) |>
    select(id, sentence, piece, pdf_page, printed_page, rows) |>
    unnest(rows) |>
    # "Arborescent pandan to 10 m, ..., or less often a shrub, 3–4.5 m tall":
    # the unqualified alternative is `usually`
    group_by(sentence, trait, entity_measured) |>
    mutate(cq = if_else(is.na(cq) & any(cq %in% rare_q), "usually", cq)) |>
    ungroup() |>
    select(-sentence, -piece)
  # a scored chunk keeps its other characters as an unmapped row when they
  # carry information the scorer did not use (sizes of unscored organs)
  unmapped <- cl |>
    filter(!scored) |>
    transmute(
      trait = NA_character_,
      trait_value_raw = text,
      notes = paste0(
        "UNMAPPED: ",
        case_when(
          organ %in% c("base", "apex", "margin", "midrib", "pleats", "veins", "auricles", "sheath") &
            sent_organ %in% "leaves" ~ paste("leaf", organ),
          organ %in% c("leaves", "syncarps", "drupes", "seeds", "stems", "roots") ~ organ,
          organ %in% c("bracts", "peduncle", "spikes", "pedicels") & !is.na(sent_organ) ~
            paste(str_remove(sent_organ, "s$"), organ),
          TRUE ~ coalesce(organ, "unknown organ")
        ),
        " | ",
        character_of(text),
        if_else(hedged(str_to_lower(text)), " | hedged", "")
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
  mutate(first_para = row_number() == 1) |>
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
    notes = paste0("UNMAPPED: ", str_to_lower(section), " | text"),
    source_section = section,
    pdf_page,
    printed_page
  )

manual <- read_csv(
  file.path(raw_dir, "manual_rows.csv"),
  col_types = cols(.default = "c", pdf_page = "i", printed_page = "i")
)

# ---- higher ranks, assembly -------------------------------------------------------

taxa <- paras |>
  filter(rank == "species") |>
  distinct(block, rank, status) |>
  mutate(genus = word(block, 1), has_description = block %in% desc_paras$block)
not_native <- taxa$block[taxa$status == "not accepted as native"]
cultivated <- taxa$block[taxa$status == "cultivated"]
own <- own |> filter(!(block %in% not_native & !is.na(trait)))

members <- taxa |>
  filter(has_description, !block %in% not_native) |>
  transmute(block, genus, group = NA_character_, species = block, rank)

# Family statements naming genera ("(Freycinetia)", "(in Benstonea)") and
# genus statements naming a species ("(in F. kamiana)") hold for those taxa only.
named_taxon <- "\\b(Benstonea|Freycinetia|Pandanus|Sararanga|Martellidendron)\\b|\\b[BFP]\\. [a-z]+"
copied <- flora_copy_down(
  higher_rows = own |> filter(!str_detect(coalesce(trait_value_raw, ""), named_taxon)),
  own_rows = own,
  members = members
)
rejected <- attr(copied, "rejected")
# Male-flower values are not copied to species whose males are unknown or not
# seen in Peninsular Malaysia ("Male inflorescence unknown")
no_males <- desc_paras |>
  filter(str_detect(text, "(?i)\\bmale (inflorescences?|plants)( unknown| not seen)")) |>
  pull(block) |>
  unique()
copied <- copied |> filter(!(block %in% no_males & entity_measured %in% c("male inflorescence", "male flower")))

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
    # an FPM species description describes the species in Peninsular Malaysia
    population_region = if_else(
      taxon_rank == "species" & is.na(rank_scored_from) & !is.na(trait),
      "Peninsular Malaysia",
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
    notes = if_else(
      taxon_name %in% cultivated & !is.na(trait),
      str_squish(paste(coalesce(notes, ""), "Cultivated in Peninsular Malaysia (treatment section 'Cultivated Plants').")),
      notes
    ),
    source_id = NA_character_
  ) |>
  bind_rows(manual |> mutate(across(c(pdf_page, printed_page), as.integer))) |>
  # unmapped clause rows whose clause a manual row scores
  filter(!(
    is.na(trait) & source_section == "Description" &
      map2_lgl(taxon_name, trait_value_raw, \(tn, r) {
        any(str_detect(manual$trait_value_raw[manual$taxon_name == tn], fixed(prep(r))))
      })
  )) |>
  flora_merge_duplicate_keys() |>
  flora_drop_redundant_ranks() |>
  arrange(match(taxon_rank, c("family", "genus", "species")), taxon_name, pdf_page) |>
  flora_finalise(traits)

write_csv(out, file.path(raw_dir, "trait_data.csv"), na = "")
write_csv(flora_unmapped_summary(out), file.path(raw_dir, "unmapped_terms.csv"), na = "")
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
      status == "not accepted as native" ~ "Not accepted as native: no Peninsular Malaysian record; described but not scored.",
      status == "cultivated" ~ "Cultivated plant; scored with a 'cultivated' note.",
      block %in% c("Freycinetia confusa") ~ "Female inflorescence unknown; syncarp and berry described from immature material.",
      block %in% c("Freycinetia kamiana") ~ "Male plants unknown; syncarps described from immature material.",
      TRUE ~ ""
    )
  )
write_csv(flags, file.path(raw_dir, "taxon_flags.csv"), na = "")
