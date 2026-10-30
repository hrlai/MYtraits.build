Extract traits from one Malaysian flora family treatment using the
`extract-traits-from-floras-my` skill. Load the skill first and follow it,
including its standing rules and its "Changing this skill" section.

## Source

Fill in this block for each family; the rest of the prompt refers to it.

- Family: Selaginellaceae
- Flora: `FPM` (Flora of Peninsular Malaysia)
- Series and volume: Series I: Ferns and Lycophytes, vol. 1 (2010)
- Author: K.M. Wong
- Dataset ID (`<ID>` below): Wong_2010

Get the PDF with `flora_fetch("<Family>", "<Flora>")`
(`scripts/flora_fetch.R`). It returns
`inst/extdata/myflora/pdf/<Flora>/<Family>.pdf` and downloads only if the file
is missing. If it fails, stop and report to me. Do not retry or fetch any other
family. Confirm the text layer in `assets/myflora_metadata.csv` and with
`flora_layer_flags()`.

## Branch

1. Start from an up-to-date, clean `main`. If `main` has uncommitted changes to
   tracked files, stop and ask me.
2. Create a branch named exactly as the dataset ID: `git switch -c <ID>`.
3. If `data/<ID>/` or a branch with that name already exists, stop and ask me
   for a distinct ID (the repo convention is a `_2` suffix).
4. Do all work and commits on this branch. Do not merge to `main`.
5. Stage files by path: only `data/<ID>/` and
   `.github/skills/extract-traits-from-floras-my/`. Never `git add -A` or
   `git add .`.

## Scope

- This one treatment only. Do not touch other datasets, except to rerun their
  builds for the regression check below.
- Never commit PDFs, rendered page images, or files with machine-specific
  paths (e.g. review manifests).
- Do not edit `config/traits.yml`. Put candidate traits and levels in
  `raw/unmapped_terms.csv` and the PR's "help wanted" section.

## Improving the skill

Part of the job is improving the skill for future families. You may edit
`SKILL.md`, `scripts/` and `assets/` under
`.github/skills/extract-traits-from-floras-my/`, following the skill's
"Changing this skill" section. In short:

- Change the skill only for gaps or errors that will recur in other families.
  Family-specific handling goes in `data/<ID>/raw/`.
- Do not change a standing rule (a user decision). Propose the change in the
  PR instead. A new rule is marked `provisional (<ID> question N)`.
- One commit per logical change, prefixed `skill:`, separate from data
  commits. The commit body states the problem, the evidence (taxon and page)
  and the change. Dictionary seed additions get their own commit.
- If you change a script, run the regression check in "Changing this skill"
  and report the result.
- If a script blocks you and the fix is not general, work around it in
  `data/<ID>/raw/build_trait_data.R` and report it under "Skill issues not
  fixed".

Order: extract the family first, then make skill fixes, then rerun this
family's build so the committed data match the committed skill.

## Work

Follow skill steps 1–11 in order: text-layer check, text source decision,
segmentation, parsing (use `flora_hand()` if the family has about 1–3 taxa),
higher-rank copy-down, unmapped rows, assembly, vouchers, `data.csv`,
`metadata.yml`, QA and build checks.

Do not stop for routine judgement calls. Apply the skill's default and record
a metadata question:

- hedged statements: not scored;
- uncertain trait mappings: best mapping, flagged;
- unclear clause ownership: verbatim, unmapped, flagged.

Stop and ask only for: a failed fetch, an existing ID or branch, unclear
treatment page boundaries, or a mapping decision that would change many rows
in other datasets.

## Context

This dataset is part of a proof-of-concept for botanist colleagues. Problems
are useful output. Follow skill step 10 for `questions:`: only items needing a
curator decision, each stating the issue, affected rows and decision needed.
Factual unmapped summaries go in `notes` and `raw/unmapped_terms.csv`.

## Report when done

- taxa count vs accepted headings in the source;
- rows: mapped vs unmapped, and by `rank_scored_from`;
- `excluded_data` from `dataset_process()`;
- numbered questions;
- top unmapped terms;
- "help wanted" items for botanists, in plain language;
- the time taken and the PDF pages processed;
- **Skill changes made**: a table with columns file, change, kind (`new`,
  `generalised`, `confirmed`, `removed`), reason, evidence (taxon and page),
  regression result;
- **Skill issues not fixed**: one line each,
  `SKILL.md § or file — what happened — suggested change — evidence`, grouped
  as skill text, shared scripts, dictionary seed, `traits.yml` candidates,
  and conflicts between this prompt and the skill.

## Pull request

Push the branch and open a PR to `main` with `gh pr create`:

- title: `Add <ID> (<Family>, <Flora> <series and volume>)`;
- body: the final report above, plus the QA tables and the unmapped-terms
  summary.

Do not merge the PR.
