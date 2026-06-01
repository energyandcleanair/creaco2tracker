---
name: install-r-packages
description: 'Install R packages for this repository. Use when adding or refreshing package dependencies, installing one-off R packages into the project library, or reconciling package requirements after a container rebuild. Prefer pak::pkg_install() and never install packages globally.'
argument-hint: 'package names or dependency workflow'
user-invocable: true
---

# Install R Packages

## When to Use

- Install new R dependencies for this repository.
- Refresh packages after rebuilding the devcontainer.
- Install a specific package into the project library.
- Reconcile package requirements declared by the project.

## Default Rule

For R package installation, use `pak::pkg_install()`, not `install.packages()`.
If pak::pkg_install() fails with a system dependency error, report the exact
error message to the user and suggest installing the required system library via
apt before retrying.

Use the repository configured in `Rprofile.site` and do not install packages
globally. By default, the devcontainer sets up a project library under `.r-lib/`
and configures R to use it as the primary library path. This keeps package
installations isolated to the workspace and avoids polluting the global R
environment.

## Examples

Install one package:

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (isFALSE(requireNamespace('pak', quietly = TRUE))) install.packages('pak', repos = 'https://cloud.r-project.org'); pak::pkg_install('desc')"
```

Install several packages:

```sh
cd /workspaces/creaco2tracker && Rscript -e "if (isFALSE(requireNamespace('pak', quietly = TRUE))) install.packages('pak', repos = 'https://cloud.r-project.org'); pak::pkg_install(c('devtools', 'testthat'))"
```

## Notes

- Keep installs inside the project library.
- Prefer the configured repository settings already present in the devcontainer.
- For full dependency synchronization from DESCRIPTION, run: `cd /workspaces/creaco2tracker && Rscript -e "pak::local_install_deps()"`
