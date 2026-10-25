---
name: extract-traits-from-floras-my
description: Extract trait data from Malaysian Flora family treatments (Flora of Peninsular Malaysia, FPM; Tree Flora of Sabah and Sarawak, TFSS) and from single taxon-description papers into a long-format `data/<Author_Year>/raw/trait_data.csv`, a vouchers file, `data.csv` and `metadata.yml`. Covers source-text checks and transcription, segmenting treatments, clause parsing, copy-down from family/genus/group, QA and build checks. Use when the user hands over an FPM or TFSS family PDF (or a taxon-description paper) and asks to "extract traits from floras", "process the next family", or "extract traits from taxon description".
---

# Extract traits from Malaysian floras

One family treatment (or one taxon-description paper) becomes one dataset
`data/<Author_Year>/`. Treat FPM and TFSS treatments as separate sources and
datasets, even when they cover the same family: different authors require
distinct source citations. Use each treatment's own author and year as its
dataset ID; if IDs collide, agree on distinct IDs before writing data. Process
one family at a time; report results after each unless the user requests a
pause. Worked examples, all in this repository:

| Dataset | Family | Source | Shows |
| --- | --- | --- | --- |
| `data/Coode_2018` | Elaeocarpaceae, FPM vol. 7 | born-digital PDF, 59 taxa | scripted parser, groups, infraspecific taxa, informal forms, excluded species |
| `data/Lim_2010` | Araucariaceae, FPM vol. 1 | scanned PDF, hand transcription, 2 taxa | transcription workflow, conifer organs, genus over family |
| `data/Yao_2010` | Pentaphylacaceae, FPM vol. 1 | scanned PDF, 1 taxon | `flora_hand()`, monogeneric copy-down, hedges |
| `data/Nor-Ezzawanis_2010` | Cibotiaceae, FPM ferns vol. 1 | scanned PDF, 1 taxon | first fern; provisional fern mappings |
| `data/Hughes_2013` | Memecylaceae, FPM vol. 4 | scanned PDF; OCR-layer parser | **PROVISIONAL**; OCR-derived transcription is not image-checked, and the segmenter still reads the layer |
| `data/Adema_1996` | Sapindaceae, TFSS vol. 2 | scanned PDF; unverified text layer | **UNDONE — not release-ready**; partial species-only pass |

Treat the first four as precedents, not completeness benchmarks: the scanned
examples cover only 1--2 taxa. Coode and Lim still have copied parsers and
inline source fixes. For Hughes, check the OCR draft against page images and
switch `raw/segment_source.R` to `flora_read_transcription()` before calling it
complete; running `raw/transcribe_source.R` only copies the OCR layer. For
Adema, follow the completion checklist below; do not treat its zero-unmapped
rows or header-only voucher file as evidence of completeness.

## Adema_1996 completion checklist

There is no born-digital copy available. Use the scanned
`inst/extdata/TFSS/Sapindaceae.pdf` and keep `data/Adema_1996` marked UNDONE
until all these checks pass:

1. Render every treatment page (PDF pages 7--118); transcribe text from the
   page images into `raw/Sapindaceae_transcription.txt`. Verify numbers,
   measurements, headings and printed-page mapping against the images. Record
   image-verified source repairs in `raw/source_fixes.csv`; correct headings
   and page markers in the transcription before segmentation.
2. Segment the verified transcription with `flora_segment()`. Account for every
   family, genus, species and infraspecific heading, including exclusions and
   informal forms. Check section boundaries and page turns against the images.
3. Extract all eligible description and later-section clauses, not only the
   selected species terms in the current parser. Apply higher-rank copy-down
   and infraspecific inheritance where justified. Keep every unscored clause
   as an unmapped row in `raw/trait_data.csv` with its actual clause page;
   inspect dimensions and organ ownership against each image.
4. Hand-transcribe type, synonym-type and figure specimens into
   `raw/Adema_1996_vouchers.csv`; verify names and citations against the images.
5. Rebuild `raw/trait_data.csv`, `raw/unmapped_terms.csv`, `raw/taxon_flags.csv`
   and `data.csv`; update `metadata.yml` to describe verified scope and open
   decisions. Reconcile every accepted heading, section, unmapped clause and
   voucher against the source. Run `flora_qa("Adema_1996")`,
   `flora_dataset_test("Adema_1996")` and `dataset_process()`; inspect
   exclusions. Passing QA alone does not establish completeness.

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

- **Names as printed.** Do not match to a taxonomic backbone during extraction;
  alignment is a separate step. Keep `sp. nov.` under its printed label and
  flag it.
- **Do not invent traits.** Only traits in `config/traits.yml` (MYtraits' own
  copy; re-read it each time) get a `trait`. Everything else is an unmapped
  row (below).
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
- **Regional counterparts.** Do not score a value borrowed from another FPM or
  TFSS treatment, even when quoted in the current treatment. That flora will be
  ingested as its own source; retaining the borrowed value would duplicate it.
- **Informal forms** are merged into the species with a `form` context.
- **"sometimes"** is accepted in species treatments, never copied down from
  genus or family.
- **Tropical, aseasonal.** Never convert seasons to months; months only when
  the text gives them ("flowers throughout the year" -> `yyyyyyyyyyyy`, noted).
- **No fire block** and no southern-hemisphere, orchid or geophyte conventions.
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
- Run `flora_pdf_check()` for producer and OCR hints, not proof of source type
  or accuracy. Inspect page images and text for each treatment; FPM Acrobat
  files and TFSS volumes can have OCR layers. Use `pdftools::pdf_text()` rather
  than terminal `pdftotext` output to inspect special characters.

### 2. Decide the text source (B, user rule)

Do not block extraction while seeking a born-digital copy. Record whether the
PDF is scanned, OCR-layered, or born-digital; a producer name or readable layer
does not establish reliability. Use the available OCR layer to draft and locate
text, but verify each scored clause, number, taxon heading and page assignment
against the rendered page before accepting it. Rendering alone is not review.
If verification is incomplete, label the extraction provisional; do not call
unverified rows complete or release-ready.

- Born-digital with a checked text layer: read with `flora_read_pdf_lines()`.
- Scanned or OCR-layered: render the treatment pages (`flora_render_pages()`,
  220 dpi, top and bottom halves). Check the OCR draft against the page images,
  save corrected, image-checked text in `raw/<Family>_transcription.txt`, and
  feed `flora_read_transcription()` to `flora_segment()`. Do not discard useful
  OCR text just because it needs checking. Include text pages and relevant
  plate captions. `flora_read_pdf_lines()` reads the PDF layer again, not the
  corrected transcription; do not accidentally segment the unchecked layer.
- For a large treatment, work page by page and record which pages or clauses
  remain unchecked. Keep its status provisional until source reconciliation.
- Keep errors visible in the draft. Record image-verified repairs in
  `raw/source_fixes.csv` (`pattern, replacement, page, reason`) and apply them
  with `flora_apply_fixes()` using printed pages. The transcription fed to the
  segmenter must already have correct headings and page markers: this helper
  does not repair them or edit the file.
- Use `flora_compare_numbers()` as a review aid, not a pass/fail test. Check
  differences and measurements it misses against page images.

### 3. Segment the treatment (G engine, B headings)

Use `flora_segment()` with a family-specific heading configuration; see
`assets/heading_config_examples.R` for Coode_2018 and Lim_2010. Verify the page
range, heading patterns, captions, sections and description starts for each
source. Checklist for the bespoke part:

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
- Count accepted species and infraspecific headings against the source before
  parsing. Numbered varieties may start `2a. var.` or `7b. var.`, and a valid
  heading need not cite `Fig.` or `Map`; inspect candidate lines in context.
  Use `cancel_header` or `ignore_header` for false matches rather than requiring
  a figure or map label. Check each block's sections; document any accepted
  taxon with no Description instead of silently dropping it.

#### TFSS segmentation

TFSS layout and text layers vary by volume and scan. Configure and inspect each
treatment; do not reuse a page range, heading regex, or page-number offset from
another family. Sapindaceae is evidence for the checks below, not a template:
its treatment occupied PDF pages 7--118, had 60 species-level headings, and
contained infraspecific headings.

- Establish the treatment's first and last **PDF** page from page images before
  reading text. Separately verify the printed-page mapping; do not derive it
  from a fixed offset unless checked at both ends.
- For an OCR source, use the image-checked transcription from step 2. A
  readable layer can still lose characters, reorder lines or change numbers.
- If using `flora_read_pdf_lines()`, set `running_head_re` for the volume
  header. For a transcription, remove running heads and bare page numbers while
  checking images. Do not let either become description text.
- Treat figure and map captions as non-treatment text unless they supply a
  separately transcribed voucher. With `flora_read_pdf_lines()`, configure
  `caption_re`, `side_caption_re` and `side_indent`, and inspect dropped lines;
  for transcriptions, exclude captions while checking images. Caption fragments
  can resemble `subsp.` or `var.` headings.
- Define separate header specifications for species, subspecies, variety and
  forma headings. Species headers set the current species; infraspecific
  headers must retain that parent so `flora_inherit_infraspecific()` can run.
  Count accepted blocks against the images before parsing; check keys, citations
  and figure labels for false matches as in the general checklist.
- Segment with `flora_segment()` rather than flattening all treatment pages into
  one text string and slicing between species matches. Its line/page provenance
  and section handling preserve page turns and permit clauses in Ecology or
  other later sections to receive their actual page.
- Before parsing, save or inspect the paragraph table. Check every block has
  the intended rank and parent, captions and running heads are dropped, and
  Description, Ecology, Distribution and other labelled sections start and end
  correctly. Sample blocks at both page boundaries and near figures.

### 4. Parse descriptions (B, with G helpers)

Tiny family (about 1 to 3 taxa) or one taxon paper: create rows directly with
`flora_hand()` (Yao_2010 pattern), using the verified text source chosen in
step 2. A one-species family does not require hand transcription when its PDF
text layer is reliable. Larger family: write `raw/build_trait_data.R` using
`flora_parse.R` and the dictionary CSV. The parser itself (organ detection,
clause rules) is bespoke because families differ; keep it short by reusing the
helpers.

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
- **Hedges.** Do not score hedged claims by default. `flora_hedge_re` catches
  common words, not every hedge; inspect wording such as "suggest" and
  "more-or-less" too. Ask before scoring an exception, then record the wording,
  taxon, trait and decision in `metadata.yml` `questions:`.
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
- **Ecology, Distribution, Notes, Phenology** paragraphs require a reviewed
  pass. Use `raw/manual_rows.csv` when manual rows help; it has the same columns
  as `trait_data.csv`. Each row needs the source clause and page; set
  `population_region` when stated and `source_id` when citing another work.
  Exclude explicitly non-Malaysian observations (Coode_2018: India and
  Thailand). Accept Borneo when the flora uses it without a finer locality
  (Lim_2010: `Yii_1995`), but flag that the record is not localised to Sabah or
  Sarawak.
- **Excluded and incompletely known species** are not scored; list them in
  `taxon_flags.csv`.
- **Ferns.** Cibotiaceae is a provisional example, not a validated mapping
  template. Check each treatment and `config/traits.yml` before mapping stipe to
  petiole, lamina to leaf, or pinnae to leaflets; flag uncertain mappings.

### 5. Higher-rank values (G, with user rules)

- Review higher-rank descriptions even when species descriptions dominate.
  Use `flora_copy_down()` when unqualified, single-level `mode` rows from the
  family, genus or group description apply to members silent on the trait,
  only if every member that scores the trait includes the value. Never numbers.
  Most specific rank wins (group, genus, family). `attr(, "rejected")` contains
  agreement-test failures only; inspect it and preserve other ineligible clauses
  separately in the bespoke parser.
- Use `flora_inherit_infraspecific()` for `subsp.`/`var.` silent on a trait.
- Monogeneric family: both family and genus are copied down
  (`rank_scored_from` = `genus` or `family`; Yao_2010).
- Keep unscored family and genus clauses (qualified, numeric, hedged,
  "most species") as unmapped rows at their own rank. Preserve scored
  higher-rank rows even when they fail the copy-down agreement test.

### 6. Unmapped records (G format, B content)

Every unscored description clause stays in `trait_data.csv` with a blank
`trait`, blank levels, `trait_value_raw` = verbatim clause, page, taxon rank
and `notes` = `UNMAPPED: <organ> | <character>`. A full description paragraph
is an interim review record, not proof that each clause was assessed: split
or mark its clauses as reviewed before calling the extraction complete. Keep
whole Ecology and Distribution paragraphs when no mapped trait applies
(altitude, habitat, soil, range, conservation status, uses). Do not split
compound text into guessed values. `raw/unmapped_terms.csv` is only the summary
(`flora_unmapped_summary()`); there is no `unmapped_values.csv`.
`config/traits.yml` changes are never made in a family branch: collect the
summaries and decide traits together.

### 7. Assemble `raw/trait_data.csv` (G)

Order: own rows, copy-down, inheritance, manual rows. Then
`flora_merge_duplicate_keys()`, optionally `flora_drop_redundant_ranks()`,
`flora_finalise()` (column order, `a--b` dashes in `trait_value_raw`,
`trait_value` and `notes`; `x` for multiplication in `trait_value_raw`;
`in_traits.yml` computed from `traits.yml`) and `readr::write_csv(na = "")`.
Columns:
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
(`inferred_from_family|genus|species_group|species`). Verify that own rows are
also mapped to `inferred_from_species`; an empty scoring-method context can
make `dataset_test()` fail with an unnamed-column error. Unmapped rows stay out.
Do not use `convert-raw-data-file-into-data-csv`: floras stay long.

### 10. `metadata.yml` (B)

Edit directly. Start from `traits.build::metadata_create_template()` and change:

- `source.primary`: `bibtype: InBook`, the actual treatment author, `booktitle`
  (FPM Series II vol. N or TFSS vol. N), editors, series, publisher, pages.
  Keep FPM and TFSS primary citations in separate datasets even for the same
  family. Add every cited secondary source (for example `Yii_1995`) under
  `source.secondary`.
- `dataset`: `data_is_long_format: yes`, `basis_of_record: preserved_specimen`,
  `life_stage: adult`, `entity_type: species`, and a `sampling_strategy`
  describing the extraction. For OCR sources, state in `sampling_strategy`
  whether rows came from the unchecked layer, an OCR draft checked against page
  images, or a fully image-checked transcription. In `notes`, identify pages
  or clauses still unchecked, provisional status, and whether a future
  born-digital copy could improve text accuracy, coverage or page provenance.
  Recheck against that copy when available; do not present a hypothetical copy
  as a prerequisite to using the current scan.
- `contexts`: `commonness_qualifier`, `population_region`, `entity_measured`,
  `trait_scoring_method`, plus `form` when forms are merged. Declare every
  context actually used in `data.csv`.
- `traits:` entries use `unit_in: unit` for measured values and an appropriate
  `basis_of_value` (`measurement` for dimensions, `expert_score` for scored
  categorical values). Generate a draft once in the console from `data.csv`,
  paste it, keep no script. Use a literal block scalar for long free-flow
  fields, especially `notes: |` and `sampling_strategy: |`. Quote short
  `questions:` and `methods:` strings (YAML plain scalars cannot contain `:` or
  start with a quote); `yaml::read_yaml()` catches this before `dataset_test()`.
- `substitutions:` map source terms to levels (`trait_value` -> clean level),
  so mappings can change without re-extracting.
- `questions:` records only items requiring a curator or user decision, or an
  explicit confirmation. Do not add routine extraction facts, name-backbone
  status, or the absence of a field sampling design. Use consecutive keys
  (`question1`, `question2`, ...). Each question must state: the source wording
  or precise issue; affected taxon, trait and rows where relevant; the current
  treatment; and the decision requested. Include a source fix, uncertain
  mapping, accepted hedge, unresolved unmapped term, or out-of-range value only
  when it needs review. Keep a factual unmapped summary in `notes` or
  `raw/unmapped_terms.csv` instead.
- Use unquoted `.na` for absent optional metadata fields, not the literal string
  `'.na'`. The latter is data, not YAML null, and can make
  `process_flag_excluded_observations()` fail with an unnamed-column error.

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
   It also catches build failures after the schema checks. Yao_2010 was never
   processed.
4. Check completeness independently of schema tests: compare all accepted
   species and infraspecific headings, description and later sections, unmapped
   clauses, and type/figure vouchers against the source. For Adema_1996, use
   the completion checklist above; passing QA does not establish completeness.
5. Report: taxa, rows by mapped and unmapped, rows by `rank_scored_from`,
   exclusions, numbered questions.

## Known inconsistencies (do not repeat)

| Observed | Do instead |
| --- | --- |
| Segmenter, helpers and dictionary copied into each family (Lim 2,068 lines vs Coode 1,551) | source `scripts/`, keep terms in the dictionary CSV |
| Coode and Lim use the layer or a transcription depending on the PDF; the decision was not written down | steps 1 and 2 |
| Source repairs buried in `norm()` regexes (Coode, Lim) or typed into rows (Yao) | `raw/source_fixes.csv` |
| Mixed dashes in source text | `flora_finalise()` normalizes numeric ranges in `trait_value_raw`, `trait_value` and `notes`; check other text columns separately |
| Unmapped records were inconsistent (fixed 2026-10: all four keep rows in `trait_data.csv`) | step 6 |
| Duplicate keys: 23 Coode pairs (same value from two ranks), Yao failed `dataset_test()` | `flora_drop_redundant_ranks()`, `flora_merge_duplicate_keys()`, QA table |
| `source_id` set by regex on `notes` (Coode, Lim); Coode cites `Corner_1988`, `Roubik_2006`, `Ng_2014` with no `source.secondary` entry | `source_id` column, QA table, add the sources |
| `manual_rows.csv` columns differ (Coode has no `rank_scored_from`) | same columns as `trait_data.csv` |
| Voucher vocabulary differs (`illustrated specimen` vs `figure specimen`; `possible type`) | one vocabulary, step 8 |
| Hedged statements scored in Yao, skipped in Coode | decide once per family, ask |
| Values outside `traits.yml` levels or ranges reach `excluded_data` (Coode 6 terms and 2 ranges, Lim 3 terms) | QA lists them before the build; record as questions, change `traits.yml` centrally |
| One trait with mixed units (Coode `fruit_length` cm/mm; Lim DBH cm/m) | expected; check `unit` per row, QA lists them |
| `dataset_process()` not run (Yao) | step 11 |
| Optional metadata used quoted `'.na'`, so it was parsed as a literal string and broke exclusion handling | use unquoted `.na`; run `dataset_process()` as well as `dataset_test()` |

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
standing rules. Completion requires the source reconciliation in step 11, not
just `dataset_test()` passing; attach QA tables and a summary of unmapped terms
and questions. Start with small families to grow the dictionary, merge
dictionary additions centrally, and keep `config/traits.yml` changes out of
family branches.

## Open question for the user

Hedged statements: confirm whether accepted Yao_2010 hedges should remain
scored; default for new families is not scored.
