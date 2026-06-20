# AGENTS.md

This repository is the `creaco2tracker` R package for CREA's EU CO2 tracker. Work should stay focused on the package code under `R/`, the tests under `tests/testthat/`, and the generated documentation under `man/`.

The provides data to dashboard on a regular basis.

## Project Shape

- Main public entry points include `get_co2()`, `get_weather_corrected_co2()`, `get_demand_components()`, `get_corrected_demand()`, and `update_all()`.
- `R/client_*` contains API clients
- `R/access_*` contains source-specific loaders
- `R/model_*` contains the main transformations and projections
- `R/quality_*` contains validation and diagnostics
- `R/workflow_*` contains orchestration and upload/download helpers
- `R/viz_*` contains plotting helpers.
- `R/core_*` contains shared utilities, logging, masking, cache, and helper functions.
- `tests/testthat/` contains the test suite, fixtures, and architecture-boundary checks.

## Development Setup

- Open the repo in the provided dev container when possible. The container is configured to use the workspace library in `.r-lib/` and temp files in `.tmp/`.
- Keep secrets and service credentials out of the repo. Tests and workflows may rely on environment variables such as `CREA_DB_PRODUCTION`, `AGSI_API_KEY`, `API_KEY`, `EMBER_KEY`, `EIA_API_KEY`, and `GITHUB_PAT`.
- Treat `cache/`, `diagnostics/`, `reports/`, and similar output folders as derived artifacts unless a task explicitly says otherwise.

## Editing Rules

- Use roxygen2 for documentation. Do not hand-edit generated files in `man/` unless you are deliberately fixing generated output after regenerating it.
- Keep changes consistent with the existing style.
- The repo's lint rules prefer 100-character lines, snake_case or SNAKE_CASE object names, and `%>%` pipe consistency.
- Avoid changing unrelated generated data or cache outputs when editing code.
- Only change the README (unless otherwise requested) with critical information. Keep the rest of the documentation closer to the code.

## Validation

- Use the repository's R test skill for exact validation commands and workflow-specific checks.
- Prefer targeted `testthat` filters for focused changes when possible.
- If a direct `testthat::test_local()` run behaves differently from source-tree loading, confirm with `devtools::test()` before treating the result as a regression.
- For documentation-only edits, `git diff --check` is usually sufficient.

## Debugging and Validation Notes

* For slow or expensive workflows, start with the smallest check that still exercises the relevant behaviour.
* For behavioural regressions, inspect existing outputs or logs to locate the first likely divergence before changing code.
* Narrow investigations by the smallest affected input set, date range, or workflow stage where possible.
* For long debugging sessions, briefly record ruled-out causes when they affect the next steps.
* For API-client changes, where relevant, check the actual request parameters, pagination, cache behaviour, and returned row coverage.
* Store temporary diagnostics and long-running command logs under .tmp/.
* Promote reusable diagnostics into tests or scripts, otherwise remove them before finishing.

## Long-Running Commands

Many operations in this repo include long 

- Prefer bounded foreground runs with logs over background jobs. Use `.tmp/*.log` and `timeout` where appropriate.
- Do not start background jobs and repeatedly poll them by default. Use background execution only when explicitly requested or when an attached foreground run is not practical.

## Working Notes

- The package is organized around the CO2 pipeline, weather correction, demand decomposition, data masking, and publication workflows. When in doubt, follow the data flow from source loaders into model and workflow functions rather than editing surface wrappers first.
- If a change touches a public workflow, check the corresponding test file in `tests/testthat/` and update or add coverage nearby.
