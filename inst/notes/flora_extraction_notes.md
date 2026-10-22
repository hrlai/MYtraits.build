# Flora extraction notes (FPM / TFSS)

Lessons from the first family processed, Elaeocarpaceae (Flora of Peninsular
Malaysia vol. 7, Coode 2018, dataset `data/Coode_2018`). They should guide the
next families and help turn the Coode_2018 scripts into general FPM/TFSS
tools. The austraits skills (`extract-traits-from-floras`,
`extract-traits-from-taxon-description`, `convert-raw-data-file-into-data-csv`,
`fill-metadata-yml`) will later be rewritten for the Malaysian floras; see the
last section.

## What was built

| File | Role |
| --- | --- |
| `raw/segment_source.R` | PDF -> paragraphs tagged with block (family, genus, group, species, infraspecific taxon, form), section and page |
| `raw/build_trait_data.R` | Clause parser, copy-down, inheritance; writes `trait_data.csv`, `unmapped_terms.csv`, `unmapped_values.csv`, `taxon_flags.csv` |
| `raw/manual_rows.csv` | Hand-checked values from Ecology / Notes / differential paragraphs |
| `raw/Coode_2018_vouchers.csv` | Types, types of synonyms, figure specimens (transcribed by hand) |
| `raw/extra_R_code_Coode_2018.R` | `trait_data.csv` -> `data.csv` (adds `value`, `trait_scoring_method`, `source_id`) |
| `metadata.yml` | Written once; edit it by hand from now on |

Result: 59 taxa, 3,023 rows, 60 traits; `dataset_test()` passed, 14 rows went
to `excluded_data` at build.

## Easiest tasks

- **Getting the text.** The FPM PDFs are born-digital. `pdftools::pdf_text()`
  gives clean UTF-8 per page (en dash, ×, curly quotes intact), so no OCR is
  needed. `pdftotext -layout` in Git Bash shows `�` for the same characters;
  this is the terminal encoding, not the PDF. Check TFSS PDFs the same way
  before assuming the same.
- **Finding headings.** Taxon headings are regular: `N. Genus epithet Author`,
  `Na. subsp./var. epithet`, `N. GENUS L.`, `Key to ...`, plus fixed section
  labels (`Distribution.`, `Ecology.`, `Conservation status.`, `Taxonomy.`,
  `Note.`, `Uses.`, `Vernacular name(s).`).
- **Numbers.** Measurements are uniform: `(a–)b–c(–d) unit`, `a–b × c–d cm`,
  `c. N`, `to N`, `N-merous`, `N-locular`, `ovules N per locule`. One range
  parser handles almost all of them.
- **Validation.** `traits.build::dataset_test()` and the `build.R` sequence
  (`dataset_configure()` -> `dataset_process()`) caught the remaining issues
  quickly.
- **Copy-down and inheritance** with explicit rules (unqualified single level,
  every member that describes the trait agrees) were simple to script and audit.

## Scanned PDFs (Lim_2010, FPM vol. 1)

- Some FPM PDFs are scans with an OCR text layer added later (Acrobat Pro DC,
  2020; check `pdftools::pdf_info()` and look for typos such as "Salish.",
  "1. 7-", "x" for ×). Elaeocarpaceae (vol. 7) is born-digital; Araucariaceae
  and Pentaphylacaceae (vol. 1) are not.
- User rule: do not use an OCR layer of unknown origin. Render pages
  (`pdftools::pdf_convert`, 220 dpi, top and bottom halves), transcribe to
  `raw/<Family>_transcription.txt` (one paragraph per line, `@page <pdf>
  <printed>` markers, verbatim text with true en dashes and ×), and point
  `segment_source.R` at that file. The parser then needs no OCR repairs.
- Check: compare the multiset of numbers per page between the transcription
  and the OCR layer. Read any difference again from the image. For
  Araucariaceae, all differences were page numbers, running heads and OCR
  misreads in citations; no measurement differed.
- Cost: about one read per half page. Fine for small families; for large ones
  ask for a born-digital PDF first.
- **Next time, keep mapped and unmapped trait records together in
  `trait_data.csv`.** Use the same long-format file, marking unmapped rows
  clearly and leaving trait names/levels unassigned. Keep `unmapped_terms.csv`
  as a summary only; do not create a separate `unmapped_values.csv`. Preserve
  verbatim clauses, taxon rank and page, including whole Ecology and
  Distribution text when no mapped trait applies. This will make it easy to
  include the unmapped traits when traits.yml is updated with new traits.

### Araucariaceae-specific lessons

- **Keep source transcription separate from parsing.** Lim 2010's PDF has an
  OCR text layer, but the extraction uses `raw/Araucariaceae_transcription.txt`
  transcribed from rendered page images. `segment_source.R` reads the
  transcription, not `pdftools::pdf_text()`. Keep printed slips verbatim in the
  transcription; document justified repairs in `norm()` and `metadata.yml`.
- **Treat taxon-specific phrases as labeled runs.** In "base attenuate,
  sometimes rounded, margin entire, recurved, apex ...", unlabelled alternatives
  remain attached to the preceding base, margin or apex until a new label or
  measurement begins. Otherwise they are scored against the wrong character.
- **Separate adjacent organs and entities.** Conifer descriptions discuss
  pollen cones, seed cones, sporophylls, scales and seed wings in one long
  paragraph. Parse the explicit organ shifts; keep seed-cone measurements under
  fruit traits with `entity_measured = seed cone`. Record young and mature bark
  separately where stated. Do not score a phrase beginning "without" as a
  positive character.
- **Resolve higher-rank conflicts by specificity.** When family and genus
  descriptions both state a trait, prefer the genus value over the family
  value. Do not copy both alternatives to species. Keep qualified values
  ("sometimes", "rarely", etc.) out of higher-rank copy-down.
- **Keep unsplittable characters.** For future families, store mapped and
  unmapped records together in `trait_data.csv`. Mark unmapped rows without
  assigning trait names or levels. Preserve the full source clause and page;
  do not split compound text into guessed values. Keep family/genus clauses at
  their source rank, not copied to each species. Retain whole Ecology and
  Distribution paragraphs when no mapped trait applies. `unmapped_terms.csv`
  remains a summary, not the value store.
- **Preserve provenance and scope in Ecology records.** Manually scored values
  need verbatim text, page, population region and `source_id` where the flora
  cites another work. The author accepts Borneo records for *A. borneensis*;
  keep the cited height and diameter with `population_region = Borneo` and
  `source_id = Yii_1995`, even though Borneo is not resolved to Sabah or Sarawak
  (but Yii_1995 ). Do not apply a blanket region filter when the author's
  treatment justifies the record.
- **Audit metadata against the final data.** Add every context used in the CSV
  (including `population_region`) and every cited secondary source to
  `metadata.yml`. Run `dataset_test()` and `dataset_process()` after metadata
  changes; inspect exclusions, since allowed-value mismatches remain even when
  schema checks pass. For Lim 2010, 271 tests pass; processing kept 91 values
  and excluded four unsupported categorical values.
- The earlier hand-coded Lim_2010 file had 334 rows, including 228 rows for
  traits not in `traits.yml`. Do not compare that count directly with the
  rebuilt `data.csv` (93 rows, 33 mapped traits). The candidate rows were not
  carried forward. For future extractions, keep unmapped source clauses in
  `trait_data.csv` alongside mapped records, without assigning invented trait
  names; `unmapped_values.csv` was only used for Lim_2010 and is not the
  preferred format going forward.

## Most challenging tasks

1. **Segmenting text into blocks.** Most of the time went here.
   - Running heads, page numbers and map captions break paragraphs. Map
     captions sit beside the text and leave fragments such as
     `subsp. cuspidatus () and E. kunstleri ().`.
   - A citation line is often followed by the description with no blank line
     between them.
   - Page turns split a sentence across a blank line. Only start a new
     paragraph when the previous line ends a sentence.
   - Some descriptions start with a label line ("Typical form", "Western form
     of subsp. apiculatus").
   - Infraspecific descriptions may start with `Leaf:` / `Flowers:` (differences
     only), or be absent (only a differential paragraph, e.g. subsp. krukovii).
   - Key lines that wrap (`subsp. ferrugineus` on its own line) look like
     headings. Exit "key" state only on a numbered heading (`7a.`) or a `Map N`
     tag.
   - Etymology, citations and types come before the description with no
     label; a description is detected by its first word (habit or organ).
2. **Assigning a clause to the right organ.** A colour, hair or size word must
   belong to the clause subject:
   - "Twigs with hairy ... terminal buds": the hairs are on the buds, not the
     twigs.
   - "Capsules yellowish ..., inside of valves red": only the first colour is
     the fruit colour.
   - "Seeds ..., enclosed in a red or orange arillode": the aril colour is not
     the seed colour.
   - "pedicels 1–3 cm long (elongating to 5 cm in fruit)".
   - "beginning to flower at 4.5 m tall" is not plant height.
   - Female-flower staminode lengths are not fertile-stamen lengths.
3. **Hedges inside parentheses.** "5–6 × c. 2 mm (possibly more, petals
   strongly incurved)" lost the measurement because the comma split ignored
   the parentheses. Remove parenthetical asides before splitting into pieces.
4. **Text order and commonness rows** for multi-term values (keep source order;
   a qualified alternative goes on its own row).
5. **Type citations** are too irregular for regex (several types per entry for
   synonyms, `holotype?`, barcode ranges, `n.v.`, mixed sheets). They were
   transcribed by hand. Allow time for this, or treat it as a separate pass.
6. **The editor reformats R files on save** (air formatter). Exact-match edits
   then fail. Edit by line number after re-reading, or write the whole file
   again.

## Australian vs Malaysian flora: what had to change

| Topic | Austraits convention | Malaysian flora |
| --- | --- | --- |
| Names | APC / APNI, `taxonomic_updates` from APCalign | None of these taxa are in APC. Keep names as printed; align to WFO/backbone as a **separate step** (user instruction). Do not match names during extraction. |
| Seasons | "spring = Sep–Nov" (southern hemisphere) | Aseasonal tropics: Ecology text gives months or "several times a year"; never convert seasons. |
| Habit | Shrubs, herbs, geophytes, fire, mallee | Mostly trees: buttresses, stilt roots, bole diameter, "beginning to flower at N m" |
| Fire traits | Many (post-fire flowering, resprouting, cues) | None in this family. Skip that whole block of the skill. |
| Ecology text | Habitat and soil phrases scored as terms | "Ecology." paragraph gives forest type and altitude. No trait for altitude or habitat yet: listed as unmapped. |
| Regions | `population_region` = Australian state or region | Cited records from India, Thailand, Sarawak. Keep Malaysian regions only (Peninsular, Sabah, Sarawak; user rule). |
| Informal taxa | Phrase names | Informal infraspecific forms (e.g. *E. angustifolius* forms): merge into the species with a `form` context (user rule). `sp. nov.` (unnamed) kept under its printed label and flagged. |
| Excluded / incompletely known species | Rare | A fixed section at the end of a genus. Not scored; flagged in `taxon_flags.csv`. |
| Infrageneric groups | Rare | Informal groups (Acronodia, Monocera ...) with their own descriptions. Copy down like a genus (`inferred_from_species_group`), with membership typed from the group paragraphs. |
| Unisexual flowers | Rare in the examples | Male / female flower sentences: use `entity_measured`; drop staminode lengths. |
| Units | Mostly mm | Mixed per trait (leaf cm, petiole cm or mm, height m). Keep source units in a `unit` column; traits.build converts. |
| "sometimes" | Commonness row | Only accepted in species treatments; never copied down from genus level (user rule). |
| Candidate traits | Extract every character as a candidate trait | User rule here: **do not invent traits**. Only traits in `config/traits.yml` go into `trait_data.csv`; everything else goes in `unmapped_terms.csv` as suggestions. |
| `inferred` typical values | Lim_2010 used `inferred` for substrate, photosynthetic organ | Not used; nothing was scored that the text does not state. |

Other changes from the austraits examples:

- The examples live on the austraits `flora-extractions` worktree
  (`~/GitHub/...`). Here they are under `.github/skills/.../examples`, and the
  working branch is `flora-extractions` in the main checkout.
- `config/traits.yml` is MYtraits' own copy (684 traits). Re-read it each time.
  Several traits used in the austraits skills (e.g. `elevation`,
  `leaf_count`) do not exist here.
- Lim_2010 (FPM Araucariaceae) is the closest in-repo precedent for
  `metadata.yml` and the `trait_data.csv` -> `data.csv` pattern.

## Metadata: what to do next time

- Write `metadata.yml` directly: `traits.build::metadata_create_template()`,
  then edit the file by hand (copy the structure of Lim_2010 / Coode_2018).
- **Mistake made here:** I split the file into `metadata_head.yml` /
  `metadata_tail.yml` and generated `traits:` and `substitutions:` with
  `write_metadata.R`. The reason was that the 60 trait entries and 85
  substitutions were derived from `data.csv`, and generating them avoided
  typing them by hand while the extraction was still changing. That is not
  worth three extra files to maintain. Those files were deleted; only
  `metadata.yml` is kept. If a draft trait or substitution list is needed
  again, generate it once in the console, paste it into `metadata.yml`, and
  keep no script.
- Map categorical values with value-level substitutions (`trait_value` source
  term -> `trait_value_clean`), so the mapping can change later without
  re-extracting.
- Recorded as `questions:` in `metadata.yml`: source fixes (e.g. a missing
  unit), unmapped source terms, values out of the allowed range, and
  judgement calls the user deferred.

## Making the scripts general (FPM + TFSS) (later)

Move the Coode_2018 code into a shared `R/flora_*.R` module with
family-specific settings only:

1. `flora_read_pdf(file)` -> lines with pdf page and printed page.
2. `flora_segment(lines, config)`: the heading patterns as a config list
   (genus names, group names, form labels, section labels). Only these differ
   between families and between FPM and TFSS.
3. `flora_parse_description(paragraph)`: organ detection, clause and comma
   splitting outside parentheses, the range / dimension parsers, colour
   scoping, hedge handling. Keep the dictionaries in a CSV
   (`term_regex, trait, level`) so they grow across families without code edits.
4. `flora_copy_down()` and `flora_inherit()`: rules as above.
5. Per-family inputs: `manual_rows.csv` (Ecology / Notes values, differential
   paragraphs), `source_fixes.csv` (typos with page and reason), group
   membership table.
6. Outputs per family are identical: `trait_data.csv`, `unmapped_terms.csv`,
   `unmapped_values.csv`, `taxon_flags.csv`, `vouchers.csv`, then `data.csv` +
   `metadata.yml`.

TFSS will need its own heading config (it is a tree flora with a different
layout); check one TFSS PDF first before generalising.

Add QA as functions run after every family:

- numeric ranges per trait and unit, min > max
- measurement misses (organ named with a number but no value)
- categorical term -> level table
- duplicate keys
- leftover digit–digit hyphens
- coverage of core traits per taxon
- `dataset_test()` + `dataset_process()` exclusions

## Processing other families in parallel (later)

- **One family per agent, one branch per family.** Each family is
  independent (own PDF, own `data/<Author_Year>` folder), so they parallelise
  well. Run the cloud agents (e.g. GitHub Copilot coding agent assigned to one
  issue per family, or several local subagents) after the shared `R/flora_*.R`
  module and its dictionaries are stable. Otherwise each agent forks the
  parser and the copies drift.
- **Issue template per family**: PDF path, dataset id, the user's standing
  rules (this file), definition of done (`dataset_test()` passes, QA tables
  attached, summary of unmapped terms and questions). The agent opens a PR;
  review is per family.
- **Order of work**: start with small families (Actinidiaceae, Aizoaceae,
  Ancistrocladaceae...) to grow the dictionaries, then the large ones.
  Batch-merge dictionary additions centrally, not per agent, to avoid
  conflicts.
- **Split the work by skill, not only by family**: one pass for traits (script),
  one for vouchers (hand transcription, slower). A cheaper model can do the
  vouchers pass.
- Keep `config/traits.yml` changes out of the family PRs; collect suggested
  traits from all `unmapped_terms.csv` files and decide them together.

## Improving the skills (later)

- **New `extract-traits-from-malaysian-flora` skill** merging the useful parts
  of the two austraits skills. Drop what doesn't apply (fire, orchids,
  southern seasons, APC, candidate traits). Add the rules above: names as
  printed, Malaysian regions only, forms as context, "sometimes" only at species
  level, no invented traits, staminodes, infrageneric groups, excluded /
  incompletely known species.
- Point the skill at the shared R module and the QA functions instead of
  "copy the closest example script".
- Add Coode_2018 and Lim_2010 as the worked examples; remove the austraits
  examples or move them to a reference folder.
- **`fill-metadata-yml`**: say "edit `metadata.yml` directly; do not generate
  it with a script"; give the Malaysian defaults (`basis_of_record:
  preserved_specimen`, `entity_type: species`, InBook source for FPM chapters,
  `form` and `population_region` contexts, `inferred_from_species_group`).
- **`convert-raw-data-file-into-data-csv`**: for floras the output stays long
  format (Lim_2010 / Coode_2018 pattern) with `value` as the build column; say
  so, since the skill describes a wide pivot.
- A small **vouchers skill** for FPM/TFSS type citations (synonym types, `?`,
  `n.v.`, barcode ranges, mixed sheets, figure specimens).
- Note in every skill: the editor reformats R files on save; edit by line
  number or rewrite whole files.
