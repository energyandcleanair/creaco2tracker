---
name: run-r-code
description: 'Run R code in this repository. Use when you need to execute non-interactive R checks, inspect data, run one-off scripts, or validate package behavior from the command line.'
argument-hint: 'Rscript expression or workflow'
user-invocable: true
---

# Run R Code

## When to Use

* Validate a small behaviour without opening an interactive R session.
* Execute ad hoc checks against repository code or data.
* Run scripts from the repository root.
* Inspect data or package behaviour from the command line.

## Default Rule

Prefer `Rscript` for non-interactive checks.

Run commands from the repository root:

```sh
cd /workspaces/creaco2tracker && Rscript - <<'EOF'
sessionInfo()
EOF
```

If the repository root differs from `/workspaces/creaco2tracker`, substitute the actual absolute path to the repository root. You can discover it with `git rev-parse --show-toplevel`.

## Examples

### One-off R code

For one-off R code, prefer piping a quoted heredoc into `Rscript -`. Use `<<'EOF'` so the shell does not expand `$`, backticks, embedded double quotes, or other shell-sensitive characters before R receives the code:

```sh
cd /workspaces/creaco2tracker && Rscript - <<'EOF'
library(dplyr)

result <- mtcars %>%
  filter(!is.na(mpg)) %>%
  summarise(mean_mpg = mean(mpg))

print(result)
EOF
```

### Multiline reusable R code

Write a temporary `.R` script under `.tmp/` only when the code is substantial, reused across commands, or useful as a persistent debugging artifact:

You may create the script by whatever means you prefer. For example, we use a heredoc to create a temporary script below:

```sh
cd /workspaces/creaco2tracker && mkdir -p .tmp
cat > .tmp/check_something.R <<'EOF'
library(dplyr)

result <- mtcars %>%
  filter(!is.na(mpg))

print(result)
EOF
```

Then run it with:

```sh
Rscript .tmp/check_something.R
```

If potentially useful artifacts already exist in `.tmp/`, do not assume that they are up to date. Make sure you understand what they do before running them.

### Run a repository script

```sh
cd /workspaces/creaco2tracker && Rscript path/to/script.R
```

### Load a package namespace safely

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (requireNamespace('creaco2tracker', quietly = TRUE)) print('available')"
```

## Failure Handling

* If `Rscript` exits with a non-zero status or prints errors to stderr, report the full error output to the user.
* Do not silently retry, suppress, or work around errors unless the failure is clearly caused by shell quoting or command construction. If retrying for that reason, explain the command-shape change.

## Notes

* Prefer repository-root execution so local `.r-lib`, `.Rprofile`, `.Renviron`, and project paths resolve consistently.
* Use plain `Rscript` by default. Use `Rscript --vanilla` only when deliberately testing behaviour without startup files or restored workspace state.
* Prefer `Rscript -e 'expr'` for short single-expression checks that fit on one line.
* For multiline one-off R code, pipe a quoted heredoc into `Rscript -`.
* Write a temporary `.R` script under `.tmp/` only when the code is substantial, reused across commands, or useful as a persistent debugging artifact.
* Avoid long multiline `Rscript -e "..."` commands, especially when the R code contains `$`, backticks, nested quotes, pipes, or other shell-sensitive syntax.
* For package tests, use the dedicated [run-r-tests](../run-r-tests/SKILL.md) skill instead.
