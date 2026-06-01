---
name: run-r-tests
description: 'Run R package tests in this repository. Use when you need to execute the full testthat suite, rerun a specific test file, validate after code changes, or troubleshoot test failures in creaco2tracker. Prefer devtools::test() for authoritative package-level runs; use TEST_LOCAL_API=TRUE only for the weather local API test.'
argument-hint: 'full suite | single test file | local API test'
user-invocable: true
---

# Run R Tests

## When to Use

- Run the full test suite for this repository.
- Rerun a specific test file while debugging a failure.
- Validate package behavior after editing R code or tests.
- Check whether a failure is a real package failure or a test-runner artifact.

## Default Workflow

Use the package-aware runner from the repository root:

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (isFALSE(requireNamespace('devtools', quietly = TRUE))) install.packages('devtools', repos = 'https://cloud.r-project.org'); devtools::test(reporter = 'summary')"
```

This is the preferred command for this repo because it loads the package source tree correctly.

## Why Not Start With test_dir

Some test files in this repository rely on package attachments or source-tree loading behavior that are present under `devtools::test()` but not always present under a direct `testthat::test_dir()` run.

If `test_dir()` reports failures such as missing `tibble()`, `tribble()`, or dplyr verbs, confirm with `devtools::test()` before treating them as package regressions.

## Run One Test File

Use this when you already know the failing file:

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (isFALSE(requireNamespace('devtools', quietly = TRUE))) install.packages('devtools', repos = 'https://cloud.r-project.org'); devtools::test(filter = 'data-masking', reporter = 'summary')"
```

Replace the filter string with a substring of the test *file* name (without the leading `test-` prefix or `.R` suffix), e.g. for `test-helpers_co2.R` use `filter = "helpers_co2"`.

## Local API Test

One weather test is intentionally skipped unless the local API flag is enabled.

Run with:

```sh
cd /workspaces/creaco2tracker && TEST_LOCAL_API=TRUE Rscript -e "if (isFALSE(requireNamespace('devtools', quietly = TRUE))) install.packages('devtools', repos = 'https://cloud.r-project.org'); devtools::test(filter = 'weather', reporter = 'summary')"
```

Only use this when you explicitly want to exercise the local API path.

## Interpreting Results

- `Skipped` entries can be expected, especially for the local weather API test.
- `Warnings` do not necessarily indicate test failure; review them separately from failed expectations.
- If `Rscript tests/testthat.R` fails because `creaco2tracker` is not installed, use `devtools::test()` instead.

## Quick Checks After Environment Changes

After rebuilding the devcontainer or reinstalling dependencies, first verify the key native-library-sensitive packages still load:

```sh
cd /workspaces/creaco2tracker && Rscript -e "pkgs <- c('creahelpers','rcrea','sf','terra','magick','units'); ok <- vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)); print(data.frame(package = pkgs, available = ok)); if (any(ok == FALSE)) quit(status = 1)"
```

Then run the full suite with `devtools::test()`. Always run the quick-check command via `Rscript` on the terminal, never paste it into an interactive R console, because `quit()` will close the session.
