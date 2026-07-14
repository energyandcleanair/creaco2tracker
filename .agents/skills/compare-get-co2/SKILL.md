---
name: compare-get-co2
description: >-
  Run, inspect, debug, and summarize cached get_co2 comparisons across commits
  or the current working copy in the creaco2tracker repository. Use when Codex
  needs to compare get_co2 outputs across refs, include uncommitted source
  changes, reuse .tmp/comparison artifacts, diagnose comparison cache behavior,
  inspect generated CSV or plot outputs, or produce a concise comparison
  summary.
---

# Compare get_co2

## Overview

Use this skill for the repository's `scripts/compare` workflow. The workflow collects
`get_co2()` output and external validation sources into cached artifacts, then builds version
and external comparison tables, plots, and summaries from those cached runs.

## Workflow

1. Inspect the working tree before running comparisons. With tracked changes or untracked
   unignored files, the wrapper compares against a working-copy snapshot by default. Ignored files
   are excluded.
2. For a local version comparison, run the comparison command from the repo root:

```bash
scripts/compare version [base] [target] --date-to <YYYY-MM-DD>
```

3. Prefer pinning `--date-to` when comparing repeatedly across days. If omitted, the wrapper uses
   the current UTC date and includes that resolved date in the cache key.
4. Leave `--runner auto` unless there is a specific reason to override it. Auto uses direct
  `Rscript` inside the devcontainer and `./rr run` outside it. Both runners collect from isolated
  source directories, so committed refs and working-copy targets follow the same source-selection
  rules. Use `COMPARE_RUNNER` or `COMPARE_GET_CO2_RUNNER` for non-interactive overrides.
5. Use target `worktree`, `working-tree`, or `.` to force a working-copy comparison. For
   `collect target` and `collect external`, use `--ref worktree`.
6. Read `references/artifact-guide.md` when you need the exact cache layout, metadata fields, or
   reuse rules.
7. Compare against external sources with:

```bash
scripts/compare external [target] --date-to <YYYY-MM-DD>
```

8. Use `scripts/compare collect base|target|external ...` only for CI split runs or advanced
   artifact debugging. Normal local comparisons do not need explicit collection commands.

## Interpretation

- Treat `comparison_totals_eu.csv` as the top-level result.
- Use `comparison_totals_country.csv` to find country-level changes.
- Use `comparison_totals_component.csv` to find country/fuel/sector contributors.
- Use `comparison_totals_eu_component.csv` to inspect EU aggregate fuel/sector changes.
- Use `monthly_country_metrics.csv`, `monthly_component_metrics.csv`, and
  `monthly_eu_component_metrics.csv` to check month-level fit.
- New completed comparison artifacts should include all required tables and a plots directory. If
  a table or plot is missing, report that as an incomplete artifact unless it is clearly from an
  older workflow.

## Safety

- Do not delete cached `ref_runs/` unless the user asks for a cache cleanup.
- Do not assume a cache miss is a regression; script changes, resolved dates, and collection
  arguments intentionally create new cache keys.
- Expect worktree comparisons to include tracked changes and untracked files that are not ignored
  by git. Ignored derived outputs remain excluded.
- Remember that cache identity is embedded in the artifact path. Raw-run identity is defined by
  the source, collection arguments, and `scripts/compare_lib/collect_get_co2.R`; comparison
  identity is defined by the input runs and the matching report script under
  `scripts/compare_lib/`.
- Keep credentials and generated `.tmp/comparison` outputs out of commits.
