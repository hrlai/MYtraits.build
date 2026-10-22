---
name: extract-traits-from-floras-my
description: Extract trait data from Malaysian Flora family treatments (Flora of Peninsular Malaysia, FPM; Tree Flora of Sabah and Sarawak, TFSS) and from single taxon-description papers into a long-format `data/<Author_Year>/raw/trait_data.csv`, a vouchers file, `data.csv` and `metadata.yml`. Covers source-text checks and transcription, segmenting treatments, clause parsing, copy-down from family/genus/group, QA and build checks. Use when the user hands over an FPM or TFSS family PDF (or a taxon-description paper) and asks to "extract traits from floras", "process the next family", or "extract traits from taxon description". Replaces the AusTraits skills `extract-traits-from-floras` and `extract-traits-from-taxon-description` for Malaysian sources.
---

# Extract traits from Malaysian floras (-my)

One family treatment (or one taxon-description paper) becomes one dataset
`data/<Author_Year>/`. Treat FPM and TFSS treatments as separate sources and
datasets, including when they cover the same family: different authors require
distinct source citations. Use each treatment's own author and year as its
dataset ID; if IDs collide, agree on distinct IDs before writing data. Process
one family at a time and stop for review after each. Worked examples, all in
this repository:

| Dataset | Family | Source | Shows |
| --- | --- | --- | --- |
| `data/Coode_2018` | Elaeocarpaceae, FPM vol. 7 | born-digital PDF, 59 taxa | scripted parser, groups, infraspecific taxa, informal forms, excluded species |
| `data/Lim_2010` | Araucariaceae, FPM vol. 1 | scanned PDF, hand transcription, 2 taxa | transcription workflow, conifer organs, genus over family |
| `data/Yao_2010` | Pentaphylacaceae, FPM vol. 1 | scanned PDF, hand transcription, 1 taxon | hand-written rows, unmapped rows kept in `trait_data.csv` |

Treat these three as precedents, not as templates to copy. They were left
unstandardised on purpose; the "Known inconsistencies" section lists what to
do differently.

Shared code lives in `scripts/` (sourced from the repository root, never
copied into the dataset folder). Do not copy a whole parser from a previous
family: `Lim_2010/raw/build_trait_data.R` is a 2,000-line copy of the
Coode_2018 one.

| Script | Does |
| --- | --- |
| `scripts/flora_source.R` | PDF text-layer check, render pages, read transcription, number comparison transcription vs layer |
| `scripts/flora_segment.R` | headings, blocks, paragraphs, sections, page lookups; heading patterns are a parameter (`assets/heading_config_examples.R`) |
| `scripts/flora_parse.R` | normalisation, `a--b` dashes, sentence and piece splitting outside parentheses, range and `a x b` parsers, commonness rows, dictionary scoring |
| `scripts/flora_assemble.R` | copy-down, species to infraspecific inheritance, duplicate merge, final column order, `data.csv`, unmapped summary |
| `scripts/flora_hand.R` | row helpers for tiny families and single papers (`row`, `un`, `tri`, `rng`) |
| `scripts/flora_qa.R` | `flora_qa(id)` tables; `flora_dataset_test(id)` |
| `assets/flora_dictionary_seed.csv` | 134 term regex to level rows (35 traits) taken from Coode_2018 and Lim_2010; grow it, do not fork it |

The scripts are tested against the three datasets (they reproduce the Coode
and Lim paragraph tables and all three `data.csv` value columns exactly). They
are read-only with respect to `data/`, apart from `flora_make_data()`, which
writes `data/<id>/data.csv`.

## Standing rules (user decisions)

- **Names as printed.** Do not match to APC, WFO or any backbone during
  extraction (none of these taxa are in APC); alignment is a separate step.
  Keep `sp. nov.` under its printed label and flag it.
- **Do not invent traits.** Only traits in `config/traits.yml` (MYtraits' own
  copy; re-read it each time) get a `trait`. Everything else is an unmapped
  row (below). Several traits used in the AusTraits skills do not exist here
  (`elevation`, `leaf_count`).
- **Score only what the text states.** No pollinator, dispersal or
  `inferred` typical values from morphology. Negative phrases ("without
  stipules") are not scored when the trait has no `absent` level; list them as
  unmapped.
- **Regions.** Keep records from Malaysia; accept a source's `Borneo` record
  even when it cannot be resolved to Sabah or Sarawak. Retain the stated region
  in `population_region`, the cited work in `source_id`, and the source wording
  and scope in `notes`. Do not assume an unspecified Borneo locality is in
  Malaysia. Exclude records explicitly located outside Malaysia, such as India
  or Thailand.
- **Informal forms** are merged into the species with a `form` context.
- **"sometimes"** is accepted in species treatments, never copied down from
  genus or family.
- **Tropical, aseasonal.** Never convert seasons to months; months only when
  the text gives them ("flowers throughout the year" -> `yyyyyyyyyyyy`, noted).
- **No fire block**, no southern-hemisphere seasons, no orchid or geophyte
  conventions from the AusTraits skills.
- **Source units** stay in `unit`; metadata `unit_in: unit` converts. Ranges in
  text columns are written `a--b`.
- **Mapped and unmapped records live together** in `raw/trait_data.csv`.
- **Metadata is edited by hand** (see step 10); no generator scripts kept.
- **The editor reformats R files on save** (air). Exact-match edits fail after
  a save: re-read the lines and edit by line number, or rewrite the file.

## Workflow

Each step is marked **G** (general: use the scripts) or **B** (bespoke: per
family, guided by the checklist).

### 1. Choose the source and check the text layer (G)

- Sources are `inst/extdata/FPM/<Family>.pdf` and
  `inst/extdata/TFSS/<Family>.pdf`. If both treat a family, extract each as a
  separate dataset under its own author/year citation; never merge their
  descriptions or attribute one author's values to the other.
- Run `flora_pdf_check()`. Of 79 FPM PDFs, 66 were saved by Acrobat Pro DC 20.12
  (the producer seen on the two scanned vol. 1 families) and 13 by InDesign
  (Elaeocarpaceae is one). Treat Acrobat-saved files as scans with an OCR layer
  until shown otherwise. TFSS PDFs come from several scanners and tools; read a
  TFSS page before assuming anything about its layout or layer.
- `pdftotext -layout` in Git Bash shows `�` for en dash and x; that is the
  terminal, not the PDF. Use `pdftools::pdf_text()`.
- The `layer_suspect` flag and `n_ocr_suspects` count are hints only.

### 2. Decide the text source (B, user rule)

Do not use an OCR layer of unknown origin.

- Born-digital: read the layer with `flora_read_pdf_lines()`.
- Scanned: render pages (`flora_render_pages()`, 220 dpi, top and bottom
  halves), transcribe to `raw/<Family>_transcription.txt`, and read it with
  `flora_read_transcription()`. Transcribe only text pages; plate pages hold a
  caption. Cost is about one read per half page; for a large scanned family ask
  for a born-digital PDF first.
- Keep printed slips verbatim in the transcription. Repairs go in
  `raw/source_fixes.csv` (columns `pattern, replacement, page, reason`) applied
  with `flora_apply_fixes()`, never in the parser.
- Check with `flora_compare_numbers()`; read each difference from the page
  image. Differences seen so far were page numbers, running heads, collector
  numbers and layer misreads (`SFNJ 1332`, `(0.5}-1`, `(--6)`, `fig.I`).

### 3. Segment the treatment (G engine, B headings)

Use `flora_segment()` with a heading configuration
(`assets/heading_config_examples.R` has the Coode_2018 and Lim_2010 ones). Only
these differ per family: genus names, group names, form labels, section labels,
running heads, first and last page. Checklist for the bespoke part:

- Which headings exist: family, genus, infrageneric groups, species,
  `subsp.`/`var.`, informal forms, "Excluded Species" and "Incompletely Known
  Species" sections.
- Running heads, bare page numbers, map and figure captions, caption fragments
  printed beside maps (`subsp. cuspidatus () and ...`).
- Wrapped key lines that look like headings (`subsp. ferrugineus` alone).
- A citation line followed by the description with no blank line; description
  detected by its first word, so extend `desc_start_re` (Lim_2010 needed
  `Bole`).
- Page turns inside a sentence (a new paragraph starts only after a sentence
  end).
- Descriptions that begin with a label ("Typical form") or give only
  differences (`Leaf:` / `Flowers:`), or are absent (differential paragraph
  only).
- Group membership from the group paragraphs. Do not guess: Coode_2018 placed
  *E. kunstleri* "still not certain" and *E. sp. nov.* "suggested" and gave
  neither a group copy-down.
- Check the output: the number of blocks, each block's sections, and that every
  scored taxon has a Description.

### 4. Parse descriptions (B, with G helpers)

Tiny family (about 1 to 3 taxa) or one taxon paper: hand-write rows with
`flora_hand()` (Yao_2010 pattern) after transcribing from page images. Larger
family: write `raw/build_trait_data.R` using `flora_parse.R` and the
dictionary CSV. The parser itself (organ detection, clause rules) is bespoke
because families differ; keep it short by reusing the helpers.

Bespoke checklist (decisions that changed per family):

- **Organ vocabulary.** Trees: bole, buttresses, stilt roots, bark, twigs,
  buds, stipules, domatia, racemes, stamens, staminodes, stones. Conifers:
  pollen cones, seed cones, sporophylls, cone scales, wings. Ferns: see the
  fern note below.
- **Clause subject owns the character.** "Twigs with hairy terminal buds": the
  hairs are on the buds. "Capsules yellowish, inside of valves red": only the
  first colour is the fruit colour. "Seeds enclosed in a red arillode": not
  seed colour. "Beginning to flower at 4.5 m tall" is not plant height.
  "Pedicels 1--3 cm long (elongating to 5 cm in fruit)": the aside is separate.
- **Labelled runs.** In "base attenuate, sometimes rounded, margin entire,
  apex ...", unlabelled alternatives belong to the last label until the next
  label or measurement (Lim_2010).
- **Adjacent organs.** Parse explicit organ shifts; young and mature bark
  separately; seed cones under fruit traits with `entity_measured = seed cone`
  (confirm per family, see Lim_2010 question 3).
- **Unisexual flowers.** Male and female flowers as `entity_measured`;
  staminode lengths are not fertile-stamen lengths (Coode_2018 question 1).
- **Dimension pairs `L x W`.** First dimension is length (state the assumption
  in a metadata question). Score the length trait; if no width trait exists,
  write the width as an unmapped row ("candidate flower_sepal_width").
- **Hedges.** Words such as probably, perhaps, possibly and "suggest" void the
  clause by default (`flora_hedge_re` covers the first five). If the user
  accepts a hedged statement (Yao_2010: "suggest wind dispersal"; "more-or-less
  continuously throughout the year"), keep the hedge in `notes` and add a
  metadata question. See open question 3.
- **Parentheses.** Drop non-extreme asides before splitting
  (`flora_drop_asides()`); never split on a comma inside parentheses.
- **Commonness rows.** Any commonness word splits the trait into rows
  (`usually` for the unqualified value). Several equal-weight values stay in one
  space-delimited cell in text order. Parenthetical extremes `(a--)b--c(--d)`
  go on `extreme_range` rows with the typical range on `usually` rows.
- **Numbers.** A range is minimum + maximum, a single or "c." value is a mean,
  "to X" is a maximum. Missing units (Coode_2018 *E. fraseri* lamina
  "14--18.5 x (7--)9--12") go in `source_fixes.csv` with the page.
- **Mapping misfits** (record every one as a metadata question): `spirally
  arranged` -> `leaf_arrangement: spiral`; `rugose` -> `rough`; `subglabrous` ->
  `glabrous`; `reddish brown` -> `brown`; fruit or bud "diameter" ->
  `fruit_width` / `bud_width`; `actinomorphic` -> `actinomorphic_general`;
  ovary locules -> carpels (one locule per carpel assumed).
- **Ambiguous sentences.** "glossy above, sparsely minutely pubescent,
  subglabrous beneath": score one reading, flag it.
- **Ecology, Distribution, Notes, Phenology** paragraphs are bespoke and
  hand-checked (`raw/manual_rows.csv`, same columns as `trait_data.csv`). Each
  manual row needs the verbatim text, page, `population_region`, and `source_id`
  when the flora cites another work. Exclude explicitly non-Malaysian
  observations (Coode_2018: India and Thailand). Accept Borneo when the flora
  uses it without a finer locality (Lim_2010: `Yii_1995`), but flag that the
  record is not localised to Sabah or Sarawak.
- **Excluded and incompletely known species** are not scored; list them in
  `taxon_flags.csv`.
- **Ferns** (first Malaysian fern family forthcoming). Treat AusTraits rules
  as hypotheses, not defaults: stipe = petiole, frond or lamina = leaf, pinnae
  and segments = leaflets, division in `leaf_type`, pinna counts in pairs, and
  growth form `fern` or `lycophyte`. When the user provides the first family,
  inspect its treatment and `config/traits.yml` before scoring these mappings;
  flag uncertain ones for review. Add tested fern guidance only after that pass.

### 5. Higher-rank values (G, with user rules)

- Use `flora_copy_down()`: unqualified, single-level, `mode` rows from the
  family, genus or group description are copied to members silent on the trait,
  only if every member that scores the trait includes the value. Never numbers.
  Most specific rank wins (group, genus, family). Rejected candidates are
  returned in `attr(, "rejected")`; read them.
- Use `flora_inherit_infraspecific()` for `subsp.`/`var.` silent on a trait.
- Monogeneric family: both family and genus are copied down
  (`rank_scored_from` = `genus` or `family`; Yao_2010).
- Keep family and genus clauses that are not copied (qualified, numeric,
  hedged, "most species") as unmapped rows at their own rank.

### 6. Unmapped records (G format, B content)

Every clause that maps to no trait stays in `trait_data.csv` with a blank
`trait`, blank levels, `trait_value_raw` = verbatim clause, page, taxon rank
and `notes` = `UNMAPPED: <organ> | <character>`. Keep whole Ecology and
Distribution paragraphs when no mapped trait applies (altitude, habitat, soil,
range, conservation status, uses). Do not split compound text into guessed
values. `raw/unmapped_terms.csv` is only the summary
(`flora_unmapped_summary()`); there is no `unmapped_values.csv`.
`config/traits.yml` changes are never made in a family branch: collect the
summaries and decide traits together.

### 7. Assemble `raw/trait_data.csv` (G)

Order: own rows, copy-down, inheritance, manual rows. Then
`flora_merge_duplicate_keys()`, optionally `flora_drop_redundant_ranks()`,
`flora_finalise()` (column order, `a--b` dashes, `x` for the multiplication
sign, `in_traits.yml` computed from `traits.yml`) and
`readr::write_csv(na = "")`. Columns:
`taxon_name, taxon_rank, entity_type, rank_scored_from, commonness_qualifier, population_region, form, entity_measured, trait, in_traits.yml, trait_value_raw, trait_value, trait_value_clean, value_type, unit, source_section, pdf_page, printed_page, notes, source_id`.
`source_id` is filled for every row that cites another work (never inferred by
regex from `notes`).

### 8. Vouchers (B, hand transcription)

`raw/<Author_Year>_vouchers.csv`: types, types of synonyms and figure
specimens, transcribed by hand; citations are too irregular for regex
(`holotype?`, `n.v.`, barcode ranges, mixed sheets, `s.n.`). This is a separate
pass and can be done by a cheaper model. Columns:
`taxon_name, specimen_status, institution_code, catalog_number, collector,
record_number, collection_date, locality, state, country, source_section,
notes`. Use one `specimen_status` vocabulary across datasets:
`holotype, isotype, lectotype, isolectotype, syntype, neotype, isoneotype,
figure specimen` plus `possible <status>` where the flora hedges. Dates ISO,
locality verbatim. One row per institution for isotypes. Flag inconsistencies
in `notes` (a type destroyed at Berlin, the same barcode cited twice, a
figure caption that contradicts the heading, as in Coode_2018 question 8).

### 9. `data.csv` (G)

`flora_make_data("<Author_Year>")` writes `data/<id>/data.csv` in long format:
`value` is `trait_value_clean` for numeric traits and the `y/n` calendars, else
the source term (`trait_value`) mapped by `substitutions:` in metadata;
`trait_scoring_method` comes from `rank_scored_from`
(`inferred_from_family|genus|species_group|species`). Unmapped rows stay out.
Do not use `convert-raw-data-file-into-data-csv`: floras stay long.

### 10. `metadata.yml` (B)

Edit directly. Start from `traits.build::metadata_create_template()` or copy
`data/Lim_2010/metadata.yml`, and change:

- `source.primary`: `bibtype: InBook`, the actual treatment author, `booktitle`
  (FPM Series II vol. N or TFSS vol. N), editors, series, publisher, pages.
  Keep FPM and TFSS primary citations in separate datasets even for the same
  family. Add every cited secondary source (for example `Yii_1995`) under
  `source.secondary`.
- `dataset`: `data_is_long_format: yes`, `basis_of_record: preserved_specimen`,
  `life_stage: adult`, `entity_type: species`, and a `sampling_strategy`
  describing the extraction (floras have no Methods text; say so in a
  question).
- `contexts`: `commonness_qualifier`, `population_region`, `entity_measured`,
  `trait_scoring_method`, plus `form` when forms are merged. Declare every
  context actually used in `data.csv`.
- `traits:` entries with `unit_in: unit`, `basis_of_value: expert_score`.
  Generate a draft once in the console from `data.csv`, paste it, keep no
  script. Quote every `questions:` and `methods:` string (YAML plain scalars
  cannot contain `:` or start with a quote); `yaml::read_yaml()` catches this
  before `dataset_test()`.
- `substitutions:` map source terms to levels (`trait_value` -> clean level),
  so mappings can change without re-extracting.
- `questions:` every judgement call, unmapped-term summary, source fix, value
  outside the allowed range, name-alignment note, and author without ORCID.

### 11. QA and build checks (G)

1. `flora_qa("<id>")`, read every table. Duplicate keys, unknown traits, levels
   or values the build will exclude, numeric ranges and outliers, mixed units
   per trait, measurement misses, leftover en dashes, calendar length, core
   trait coverage, contexts, traits and `source_id` used in `data.csv` but
   missing from `metadata.yml`.
2. `flora_dataset_test("<id>")`: quiet wrapper for
   `traits.build::dataset_test()`.
3. `traits.build::dataset_process()` (see `build.R` for the call): inspect
   `excluded_data`. Schema tests pass even when allowed-value mismatches exist.
   Yao_2010 was never processed.
4. Report: taxa, rows by mapped and unmapped, rows by `rank_scored_from`,
   exclusions, numbered questions.

## Known inconsistencies in the three datasets (do not repeat)

| Observed | Do instead |
| --- | --- |
| Segmenter, helpers and dictionary copied into each family (Lim 2,068 lines vs Coode 1,551) | source `scripts/`, keep terms in the dictionary CSV |
| Coode and Lim use the layer or a transcription depending on the PDF; the decision was not written down | steps 1 and 2 |
| Source repairs buried in `norm()` regexes (Coode, Lim) or typed into rows (Yao) | `raw/source_fixes.csv` |
| Mixed dashes: Coode raw text has 878 en dashes, with `4--6 (–7)` in one cell; Lim 47; Yao none | `flora_finalise()` applies the same `a--b` rule (parenthetical extremes included) to every text column |
| Unmapped records: Coode summary only, Lim separate `unmapped_values.csv`, Yao in `trait_data.csv` | step 6 (Yao pattern plus summary) |
| Duplicate keys: 23 Coode pairs (same value from two ranks), Yao failed `dataset_test()` | `flora_drop_redundant_ranks()`, `flora_merge_duplicate_keys()`, QA table |
| `source_id` set by regex on `notes` (Coode, Lim); Coode cites `Corner_1988`, `Roubik_2006`, `Ng_2014` with no `source.secondary` entry | `source_id` column, QA table, add the sources |
| `manual_rows.csv` columns differ (Coode has no `rank_scored_from`) | same columns as `trait_data.csv` |
| Voucher vocabulary differs (`illustrated specimen` vs `figure specimen`; `possible type`) | one vocabulary, step 8 |
| Hedged statements scored in Yao, skipped in Coode | decide once per family, ask |
| Values outside `traits.yml` levels or ranges reach `excluded_data` (Coode 6 terms and 2 ranges, Lim 3 terms) | QA lists them before the build; record as questions, change `traits.yml` centrally |
| One trait with mixed units (Coode `fruit_length` cm/mm; Lim DBH cm/m) | expected; check `unit` per row, QA lists them |
| `dataset_process()` not run (Yao) | step 11 |

## Single taxon-description papers

Same workflow with `flora_hand()`, plus: read the whole paper (introduction
group statements, key, description, diagnosis, phenology, distribution and
habitat, notes with subpopulation variation, figure captions). Skip pure
comparisons ("much hairier than H. basaltica"); they cannot be scored for
either taxon. When the paper's diagnosis disagrees with its description, keep
the description and put the disagreement in `notes`. Values that apply through
a genus or group statement use that `rank_scored_from`; never copy hedged
statements. Extent of occurrence, linear range and population size are not
traits; mention them in the report.

## Parallel processing

Only after the shared scripts and dictionary are stable. One family per agent,
one branch per family; issue per family with PDF path, dataset id and these
standing rules; definition of done is `dataset_test()` passing, QA tables
attached and a summary of unmapped terms and questions. Start with small
families to grow the dictionary, merge dictionary additions centrally, and
keep `config/traits.yml` changes out of family branches.

## Open question for the user

Hedged statements: default to skip, or default to score with a note?
