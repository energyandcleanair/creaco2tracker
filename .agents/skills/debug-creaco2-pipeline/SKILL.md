---
name: debug-creaco2-pipeline
description: 'debug regressions, failures, or unexplained behaviour across the creaco2tracker R package pipelines. Use when investigating whole-pipeline or multi-stage issues in top level or high-level processes. This skill helps choose the right debugging layer: existing artifacts, unit checks, temporary tests, subset integration checks, stage tracing scripts, comparison artifacts, and full end-to-end reruns.'
---

# Debug creaco2tracker pipelines

Use this skill to debug behaviour that spans more than one function or where the failing layer is not yet known. The goal is to find the first meaningful divergence with the cheapest faithful check, then convert the finding into a durable fix and validation.

## Core rule

Do not start with the full pipeline unless the full pipeline is the only faithful reproduction. Work down this ladder, using the smallest layer that can still test the suspected behaviour:

1. Existing outputs, logs, cache metadata, diagnostics, and comparison artifacts
2. A narrow reproducer using existing public or internal functions
3. A unit-level check for one pure or nearly pure function
4. A stage-boundary or subset integration check
5. A temporary trace script that runs only the affected slice
6. A full workflow or branch comparison rerun

If a lower layer cannot reproduce the issue, explain why and move up one layer. Do not treat a passing low-level check as proof that the pipeline is fixed unless it exercises the failing path.

## Start by defining the failure contract

Before changing code, write down:

- The observed bad behaviour and the expected behaviour
- The affected public entry point, if known, such as `get_co2()`, `get_weather_corrected_co2()`, `get_demand_components()`, `get_corrected_demand()`, or `update_all()`
- The smallest known affected slice: dates, countries, fuels, sectors, estimates, source, or workflow option
- Whether the issue is a regression between branches, a failed test, a live-data change, a cache/input issue, or a new feature bug
- Which settings must match for a faithful comparison: date range, cache use, credentials, source data snapshot, branch, and workflow flags

For regressions, separate issues by signature. Different time windows, affected components, or aggregation levels may indicate different causes.

## Choose the debugging layer

### Existing artifacts and logs

Use this first when outputs already exist. Inspect artifacts before rerunning expensive workflows.

Good candidates:

- `.tmp/` comparison directories and logs
- `diagnostics/` outputs
- cached raw source outputs, when relevant and safe to inspect
- existing test snapshots or fixtures
- generated CSVs from comparison or validation scripts

Use this layer to rank what changed and identify the first likely place to inspect. Do not patch production code based only on a final aggregate if an earlier stage can be checked.

### Unit-level checks

Use a unit-level check when the suspect behaviour is in one function or helper, especially code under `R/core_*`, `R/model_*`, `R/access_*`, or `R/client_*`.

Options:

- Add a real `tests/testthat/test-*.R` case when the behaviour should become permanent coverage.
- Use a temporary `testthat` check under `.tmp/` when exploring and not yet sure the expectation is correct.
- Prefer tiny hand-built inputs over live data when testing split, join, aggregation, filtering, date, masking, or request-building logic.

Unit tests are strongest for deterministic transformations and API request construction. They are weak evidence for full-pipeline correctness if the bug depends on source coverage, cache state, orchestration order, or branch-specific inputs.

### Stage-boundary integration checks

Use a stage-boundary check when the individual functions pass but the problem appears between layers.

Typical boundaries in this repo:

- `R/access_*` source loaders to `R/model_*` transformations
- API clients in `R/client_*` to source-specific loaders
- `model_*` stage output to `quality_*` validation
- `model_*` transformations to `workflow_*` orchestration
- pre-projection to post-projection outputs
- pre-aggregation to post-aggregation outputs

A good integration check keeps the real boundary but narrows the input. For example, run one source, one date range, one country set, or one model stage rather than the whole public workflow.

### Temporary subset scripts

Create a temporary script under `.tmp/` when you need to run part of the pipeline repeatedly, inspect intermediate data, or compare stages across branches.

Use a temporary script when:

- The command is too long or fragile for a one-liner
- You need multiple intermediate outputs
- You need to run the same diagnostic on two branches or after a fix
- You need to preserve logs or CSV artifacts for comparison

Do not use a temporary script as a substitute for understanding the pipeline. Keep the script narrow and name the outputs clearly. Smoke-test it on a tiny slice before using it for a longer run.

Promote the script into a committed diagnostic or test only if it is likely to be reused. Otherwise remove it before finishing the PR.

### Full workflow checks

Use full workflow checks only after the failing slice is understood or when a narrower check cannot be faithful.

Before running a slow full workflow, confirm:

- The exact command and working directory
- Whether credentials and network access are required
- Whether cache should be used or bypassed
- Where stdout/stderr logs will be written
- Which output artifacts will prove success or failure

For branch comparisons, preserve known-good base artifacts when inputs and collection settings are unchanged. Recollect only the target branch when validating a target-branch fix, then rerun the comparison against the preserved base. If source data, cache settings, credentials, or collection logic changed, rerun both sides.

When validating an uncommitted fix, use the comparison wrapper's working-copy mode instead of
committing only for diagnostics. `scripts/compare_get_co2` automatically compares against the
working copy when tracked files or untracked unignored source files are dirty; `--worktree` or
target `worktree` can force this mode. Ignored derived files remain excluded.

## Temporary tests and diagnostics

Use this decision rule:

- **Temporary unit check**: use when validating a hypothesis about one function.
- **Real unit test**: use when the expected behaviour is now known and should be guarded permanently.
- **Temporary integration check**: use when checking a boundary or a small real-data slice.
- **Temporary trace script**: use when inspecting stage-by-stage state or comparing branches.
- **Full comparison**: use when validating the final user-visible output after narrowing or fixing the cause.

Keep temporary diagnostics under `.tmp/`. Do not assume existing `.tmp` scripts are current; inspect them before running them.

## Running commands

When running R code, use the repository's R-code skill. For package test commands, prefer the repository's R-test skill if available.

## Interpreting results

After each check, state one of:

- **Confirmed**: the check reproduced the failure or verified the fix.
- **Ruled out**: the check exercised the suspected path and did not reproduce the issue.
- **Inconclusive**: the check did not exercise the failing path or depended on unstable inputs.

Do not call a cause ruled out unless the check actually covered that path. For long debugging sessions, keep short working notes on ruled-out causes that affect next steps.

## Reporting format

When giving the user a debugging plan or progress summary, use this structure:

1. **Failure contract**: what is being debugged and the smallest known affected slice
2. **Current evidence**: what artifacts, logs, tests, or scripts show
3. **Chosen layer**: why the next check is unit, integration, subset script, or full workflow
4. **Next command or change**: the concrete next step
5. **Success criterion**: what result would confirm, rule out, or narrow the hypothesis
6. **Cleanup or promotion**: whether temporary diagnostics should be removed or turned into tests/scripts
