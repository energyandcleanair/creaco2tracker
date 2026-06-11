---
name: run-r-code
description: 'Run R code in this repository. Use when you need to execute non-interactive R checks, inspect data, run one-off scripts, or validate package behavior from the command line.'
argument-hint: 'Rscript expression or workflow'
user-invocable: true
---

# Run R Code

## When to Use

- If Rscript exits with a non-zero status or prints errors to stderr, report the full error output to the user and do not silently retry or suppress it.
- Validate a small behavior without opening an interactive R session.
- Execute ad hoc checks against repository code or data.
- Run scripts from the repository root.

## Default Rule

Prefer `Rscript` for non-interactive checks.

Run commands from the repository root:

```sh
cd /workspaces/creaco2tracker && Rscript -e "sessionInfo()"
```

If the repository root differs from `/workspaces/creaco2tracker`, substitute the actual absolute path to the repository root. You can discover it with `git rev-parse --show-toplevel`.

## Examples

Evaluate a quick expression:

```sh
cd /workspaces/creaco2tracker && Rscript -e "print(Sys.Date())"
```

Run a repository script:

```sh
cd /workspaces/creaco2tracker && Rscript path/to/script.R
```

Load a package namespace safely:

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (requireNamespace('creaco2tracker', quietly = TRUE)) print('available')"
```

## Notes

- Prefer `Rscript -e` for single-expression checks that fit on one line; write a temporary `.R` script file for multi-expression checks.
- Prefer repository-root execution so local `.r-lib` and project paths resolve consistently.
- For package tests, use the dedicated [run-r-tests](../run-r-tests/SKILL.md) skill instead.
