# syntax=docker/dockerfile:1.6

# Multi-arch (amd64 + arm64), so the image runs natively on Apple silicon rather than
# under x86 emulation, where arrow crashes on load. Pinned by digest: Ubuntu 26.04
# (resolute), R 4.6.2. Rocker's version tags carry no OS suffix and move to new Ubuntu
# releases, so update the digest deliberately.
FROM docker.io/rocker/tidyverse:4.6@sha256:9241ac461c924334777dd3fb047c6e5e0fd1435a85acc171f1a32836861d0a69

RUN apt-get update && apt-get install -y --no-install-recommends \
    bubblewrap \
    socat \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-0 \
    libudunits2-dev \
    libmagick++-dev \
    libproj25 \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /workspace /cache/pak/packages /cache/pak/metadata \
    && chown -R rstudio:rstudio /workspace /cache

# The base image sets CRAN to the Posit Package Manager binary repository for its own
# Ubuntu release. Binaries built for another release link against missing system
# libraries, so stop the build if CRAN and /etc/os-release disagree.
RUN . /etc/os-release \
    && case "${CRAN}" in \
        */__linux__/"${VERSION_CODENAME}"/*) ;; \
        *) echo "CRAN=${CRAN} does not match Ubuntu ${VERSION_CODENAME}" >&2; exit 1 ;; \
    esac

RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN"))); install.packages(c("remotes"))'

WORKDIR /workspace
COPY DESCRIPTION /workspace/DESCRIPTION

# install_deps only warns when a package fails to install, so check that every
# DESCRIPTION dependency (including creahelpers and rcrea) loads. The check runs in
# the same layer so a failed install is never cached.
RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat)" && \
    Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN")), Ncpus = 1); remotes::install_deps("/workspace", dependencies = TRUE, upgrade = "never", build = FALSE)' && \
    Rscript -e ' \
    pkgs <- setdiff(remotes::local_package_deps("/workspace", dependencies = TRUE), "R"); \
    ok <- vapply(pkgs, function(pkg) tryCatch({ loadNamespace(pkg); TRUE }, \
        error = function(e) { message(pkg, ": ", conditionMessage(e)); FALSE }), logical(1)); \
    if (!all(ok)) { \
        message("Packages missing or failing to load: ", paste(pkgs[!ok], collapse = ", ")); \
        quit(status = 1) \
    }; \
    message("All ", length(pkgs), " DESCRIPTION dependencies load.")'

RUN chown -R rstudio:rstudio /usr/local/lib/R/site-library /workspace

USER rstudio
