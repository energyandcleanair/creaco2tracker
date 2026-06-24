# Cached get_co2 Comparison Artifacts

The comparison wrapper stores generated artifacts under `.tmp/comparison/`.

## Layout

```text
.tmp/comparison/
  ref_runs/
    <ref_run_hash_prefix>__commit_<short_sha>/
      raw.csv
      metadata.json
      complete.ok
    <ref_run_hash_prefix>__worktree_<head_short>_<worktree_hash>/
      raw.csv
      metadata.json
      worktree_status.txt
      worktree_diff.patch
      untracked_files.txt
      complete.ok
  external_sources/
    <external_source_hash_prefix>__<target_label>/
      external_sources.csv
      source_status.csv
      metadata.json
      complete.ok
  comparisons/
    <comparison_hash_prefix>__<base_label>__<target_label>/
      comparison_totals_eu.csv
      comparison_totals_eu_component.csv
      comparison_totals_country.csv
      comparison_totals_component.csv
      monthly_eu_component_metrics.csv
      monthly_country_metrics.csv
      monthly_component_metrics.csv
      plots/
      summary.md
      complete.ok
  external_comparisons/
    <external_comparison_hash_prefix>__<target_label>__external/
      normalized_external_sources.csv
      crea_totals.csv
      annual_pairs.csv
      monthly_pairs.csv
      comparison_totals_eu.csv
      comparison_totals_country.csv
      source_coverage.csv
      source_status.csv
      plots/
      summary.md
      complete.ok
```

Committed refs use `commit_<short_sha>` labels and working-copy snapshots use
`worktree_<head_short>_<worktree_hash>` labels. The full cache key is embedded at the front of
the directory name and is the cache identity.

## Hashes

- `collect_script_hash`: SHA-256 of the working-copy
  `scripts/compare_lib/collect_get_co2.R`.
- `raw_cache_key`: Full hash of the raw-run cache key. Raw-run keys combine source identity
  (commit SHA or working-copy hash), effective collection arguments, and `collect_script_hash`.
- `ref_run_hash_prefix`: Leading 7 hex chars of `raw_cache_key`, used in ref-run directory names.
- `run_hash`: Short hash of `raw_cache_key`, used in logging and summary context.
- `compare_script_hash`: SHA-256 of the working-copy
  `scripts/compare_lib/compare_get_co2.R` or external report script.
- `report_cache_key`: Full hash of the comparison cache key. Comparison keys combine the base
  raw-run key, target raw-run key, and `compare_script_hash`.
- `comparison_hash_prefix`: Leading 7 hex chars of `report_cache_key`, used in comparison
  directory names.
- `report_hash`: Short hash of `report_cache_key`, used in logging and summary context.
- `runner`: Resolved R execution mode used for the artifact run, either `rscript` or `rr`.
- `worktree_hash`: Short hash combining `HEAD`, `git diff HEAD --binary`, and untracked
  unignored file paths and contents.

Changing only the report script should reuse `ref_runs/` and regenerate the comparison report.
Changing collection arguments, `scripts/compare_lib/collect_get_co2.R`, the committed source SHA,
or worktree source files should create new ref-run cache directories for affected sources.
Changing only the wrapper driver should not invalidate raw runs.

## Explicit Inputs

The wrapper passes required values explicitly to downstream scripts.

- `scripts/compare_lib/compare_get_co2.R` receives `--raw-base`, `--raw-target`,
  short SHAs, refs, `--date-to`, `--runner`, and hash values, then writes `summary.md`.
- `scripts/compare_lib/compare_get_co2_external.R` receives the target raw run, normalized
  external source file, source status file, target label, and `--date-to`, then writes
  `summary.md`.

## Working-Copy Mode

The public `version` and `external` commands, and the CI-oriented `collect` subcommands, use a
working-copy target when:

- `version` or `external` is run without an explicit target, and tracked files or untracked
  unignored files differ from `HEAD`;
- `collect target` or `collect external` is run without `--ref`, and tracked files or untracked
  unignored files differ from `HEAD`;
- `--ref worktree`, `--ref working-tree`, or `--ref .` is passed.

The snapshot is built from `git archive HEAD`, then `git diff HEAD --binary` is applied, then
untracked files reported by `git ls-files --others --exclude-standard` are copied in. Ignored
files and directories such as `.tmp/`, `.r-lib/`, caches, diagnostics, and reports remain excluded.

The wrapper stores `worktree_status.txt`, `worktree_diff.patch`, and `untracked_files.txt` in the
ref-run artifact for debugging. The full source snapshot is removed after a successful collection.

Committed refs are collected from temporary detached git worktrees, and working-copy targets are
collected from materialized snapshots. Both `rscript` and `rr` execute against those isolated
source directories, so the main checkout is not switched for either runner.

## Reuse Rules

A cached directory is reusable only when the exact hash-derived path exists and `complete.ok`
exists. The driver trusts successful producer layers and does not inspect every payload file.
A missing `complete.ok` means the artifact may be partial and should be regenerated.

For comparison artifacts, `summary.md` must also exist in the cached directory. If `complete.ok`
exists but `summary.md` is missing, the driver fails hard and requires cache cleanup.

The wrapper atomically builds into a temporary sibling directory and promotes it after success.
If a run fails, the previous completed comparison directory should remain intact.

## Runner Modes

The wrapper accepts `--runner auto|rscript|rr` and `COMPARE_GET_CO2_RUNNER`.

- `auto`: Use direct `Rscript` inside the devcontainer and `./rr run` outside it.
- `rscript`: Run directly in the current environment. This is best for devcontainer sessions.
- `rr`: Run through the repository runtime image. This is best when launching from the host and
  uses the same isolated source directories as `rscript`.

Runner mode is not part of the cache key, so matching cached raw runs can be reused across runner
modes.

## Summary And Handoff

The version and external report scripts write `summary.md` directly. The GitHub workflow combines
completed summaries in shell and rewrites plot links relative to the combined GitHub/CML Markdown
file.

The wrapper supports explicit machine handoff via collection and comparison emit files:

```bash
scripts/compare collect base --emit-run-dir <path/to/output-file> ...
scripts/compare collect target --emit-run-dir <path/to/output-file> ...
scripts/compare collect external --emit-run-dir <path/to/output-file> ...
scripts/compare version --emit-comparison-dir <path/to/output-file> ...
scripts/compare external --emit-comparison-dir <path/to/output-file> ...
```

On success, the wrapper writes one line containing the absolute comparison directory path.
The GitHub Actions/CML workflow consumes that file, validates `complete.ok`, derives
`summary.md` as `<comparison_dir>/summary.md`, and uses it for both persistent PR comments and
step summary output.
