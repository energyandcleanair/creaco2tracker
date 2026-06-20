# Cached get_co2 Comparison Artifacts

The comparison wrapper stores generated artifacts under `.tmp/comparison/`.

## Layout

```text
.tmp/comparison/
  ref_runs/
    <safe_ref>_<short_sha>_<run_hash>/
      collect_get_co2.R
      metadata.csv
      raw.csv
      complete.ok
    worktree_<head_short>_<worktree_hash>_<run_hash>/
      collect_get_co2.R
      metadata.csv
      raw.csv
      worktree_status.txt
      worktree_diff.patch
      untracked_files.txt
      complete.ok
  comparisons/
    <safe_base>_<base_short>__<safe_target>_<target_short>/
      compare_get_co2.R
      metadata.csv
      comparison_totals_eu.csv
      comparison_totals_eu_component.csv
      comparison_totals_country.csv
      comparison_totals_component.csv
      monthly_eu_component_metrics.csv
      monthly_country_metrics.csv
      monthly_component_metrics.csv
      plots/
      complete.ok
```

`safe_ref` is a lower-case path-safe version of the user-supplied ref. Branch names with `/` are
converted to `-`. The short SHA keeps moved refs from reusing the wrong directory.

## Hashes

- `args_hash`: Hash of effective collection arguments. The default `date_to` is resolved to a
  concrete UTC date before hashing.
- `collect_script_hash`: SHA-256 of the working-copy `scripts/collect_get_co2.R`.
- `driver_script_hash`: SHA-256 of the working-copy `scripts/compare_get_co2` driver.
- `run_hash`: Short hash combining `args_hash`, `collect_script_hash`, and
  `driver_script_hash`.
- `compare_script_hash`: SHA-256 of the working-copy `scripts/compare_get_co2.R`.
- `report_hash`: Short hash combining base/target resolved SHAs, base/target run hashes, and
  `compare_script_hash` and `driver_script_hash`.
- `runner`: Resolved R execution mode used for the artifact, either `rscript` or `rr`.
- `worktree_hash`: Short hash combining `HEAD`, `git diff HEAD --binary`, and untracked
  unignored file paths and contents.

Changing only the report script should reuse `ref_runs/` and regenerate the comparison report.
Changing collection arguments, `collect_get_co2.R`, the driver script, or worktree source files
should create new ref-run cache directories for affected sources.

## Metadata

Every artifact directory has `metadata.csv` with `key,value` rows.

Ref-run metadata includes:

- `artifact_type=ref_run`
- `source_type=commit` for committed refs, or `source_type=worktree` for working-copy runs
- `ref`, `sha`, `short_sha`
- `date_to`
- `args_hash`, `collect_script_hash`, `driver_script_hash`, `run_hash`
- `raw_file=raw.csv`
- `runner`

Worktree ref-run metadata also includes:

- `head_sha`
- `worktree_hash`
- `dirty_tracked`
- `dirty_untracked_unignored`

Comparison metadata includes:

- `artifact_type=comparison`
- `base_ref`, `target_ref`
- `base_source_type`, `target_source_type`
- `base_source_id`, `target_source_id`
- `base_sha`, `target_sha`, `base_short_sha`, `target_short_sha`
- `date_to`
- `raw_base_file`, `raw_target_file`
- `base_run_hash`, `target_run_hash`, `compare_script_hash`, `driver_script_hash`,
  `report_hash`
- `runner`
- `target_worktree_hash` when the target is a working-copy snapshot

`raw_base_file` and `raw_target_file` may be relative to the comparison directory or absolute.

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

A cached directory is reusable only when `complete.ok` exists and metadata matches the expected
hashes. A missing `complete.ok` means the artifact may be partial and should be regenerated.

The wrapper atomically builds into a temporary sibling directory and promotes it after success.
If a run fails, the previous completed comparison directory should remain intact.

## Runner Modes

The wrapper accepts `--runner auto|rscript|rr` and `COMPARE_GET_CO2_RUNNER`.

- `auto`: Use direct `Rscript` inside the devcontainer and `./rr run` outside it.
- `rscript`: Run directly in the current environment. This is best for devcontainer sessions.
- `rr`: Run through the repository runtime image. This is best when launching from the host and
  uses the same isolated source directories as `rscript`.

Runner mode is recorded in metadata but is not part of the cache key, so matching cached raw runs
can be reused across runner modes.

## Summary Helper

Use:

```bash
python3 .agents/skills/compare-get-co2/scripts/summarize_compare_get_co2.py \
  .tmp/comparison/comparisons/<comparison-dir>
```

The helper reads `metadata.csv` and any available comparison CSVs, then writes Markdown to stdout.
It should tolerate missing optional CSVs and mention what was unavailable.
