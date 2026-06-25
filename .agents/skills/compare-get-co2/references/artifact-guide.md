# Cached get_co2 Comparison Artifacts

The comparison wrapper stores generated artifacts under `.tmp/comparison/`.

## Layout

```text
.tmp/comparison/
  ref_runs/
    <ref_run_hash_prefix>__commit_<short_sha>/
      raw.csv
      complete.ok
    <ref_run_hash_prefix>__worktree_<head_short>_<worktree_hash>/
      raw.csv
      worktree_status.txt
      worktree_diff.patch
      untracked_files.txt
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
```

Committed refs use `commit_<short_sha>` labels and working-copy snapshots use
`worktree_<head_short>_<worktree_hash>` labels. The full cache key is embedded at the front of
the directory name and is the cache identity.

## Hashes

- `collect_script_hash`: SHA-256 of the working-copy `scripts/collect_get_co2.R`.
- `raw_cache_key`: Full hash of the raw-run cache key. Raw-run keys combine source identity
  (commit SHA or working-copy hash), effective collection arguments, and `collect_script_hash`.
- `ref_run_hash_prefix`: Leading 7 hex chars of `raw_cache_key`, used in ref-run directory names.
- `run_hash`: Short hash of `raw_cache_key`, used in logging and summary context.
- `compare_script_hash`: SHA-256 of the working-copy `scripts/compare_get_co2.R`.
- `report_cache_key`: Full hash of the comparison cache key. Comparison keys combine the base
  raw-run key, target raw-run key, and `compare_script_hash`.
- `comparison_hash_prefix`: Leading 7 hex chars of `report_cache_key`, used in comparison
  directory names.
- `report_hash`: Short hash of `report_cache_key`, used in logging and summary context.
- `runner`: Resolved R execution mode used for the artifact run, either `rscript` or `rr`.
- `worktree_hash`: Short hash combining `HEAD`, `git diff HEAD --binary`, and untracked
  unignored file paths and contents.

Changing only the report script should reuse `ref_runs/` and regenerate the comparison report.
Changing collection arguments, `collect_get_co2.R`, the committed source SHA, or worktree source
files should create new ref-run cache directories for affected sources. Changing only the wrapper
driver should not invalidate raw runs.

## Explicit Inputs

The wrapper passes required values explicitly to downstream scripts.

- `scripts/compare_get_co2.R` receives `--raw-base`, `--raw-target`,
  `--base-short-sha`, and `--target-short-sha`.
- `scripts/summarize_compare_get_co2.py` receives explicit summary context
  (`--base-ref`, `--target-ref`, short SHAs, `--date-to`, `--runner`, and hash values).

## Working-Copy Mode

The wrapper uses a working-copy target when:

- no target is provided and tracked files or untracked unignored files differ from `HEAD`;
- one positional base is provided and tracked files or untracked unignored files differ from
  `HEAD`;
- `--worktree` is passed;
- the target is `worktree`, `working-tree`, or `.`.

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

## Summary Helper

Use:

```bash
python3 scripts/summarize_compare_get_co2.py \
  .tmp/comparison/comparisons/<comparison-dir> \
  --base-ref <base-ref> \
  --target-ref <target-ref> \
  --base-short-sha <base-short-sha> \
  --target-short-sha <target-short-sha> \
  --date-to <YYYY-MM-DD> \
  --runner <rscript|rr> \
  --report-hash <report-hash> \
  --base-run-hash <base-run-hash> \
  --target-run-hash <target-run-hash>
```

The helper reads explicit run context arguments and available comparison CSVs, then writes
Markdown to stdout. It should tolerate missing optional CSVs and mention what was unavailable.
The wrapper writes
`summary.md` on every successful comparison.

The wrapper supports explicit machine handoff via:

```bash
scripts/compare_get_co2 --emit-comparison-dir <path/to/output-file> ...
```

On success, the wrapper writes one line containing the absolute comparison directory path.
The GitHub Actions/CML workflow consumes that file, validates `complete.ok`, derives
`summary.md` as `<comparison_dir>/summary.md`, and uses it for both persistent PR comments and
step summary output.
