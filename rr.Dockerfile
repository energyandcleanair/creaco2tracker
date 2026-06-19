# syntax=docker/dockerfile:1.6

FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

# For ubuntu 24.04 (noble) and R 4.4.x, use the following PPM URL:
ARG PPM_DISTRO=noble
ARG PPM_URL=https://packagemanager.posit.co/cran/__linux__/${PPM_DISTRO}/latest

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

ENV CRAN=${PPM_URL}

RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN"))); install.packages(c("remotes"))'

WORKDIR /workspace
COPY DESCRIPTION /workspace/DESCRIPTION

RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat)" && \
    Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN")), Ncpus = 1); remotes::install_deps("/workspace", dependencies = TRUE, upgrade = "never", build = FALSE)'

RUN chown -R rstudio:rstudio /usr/local/lib/R/site-library /workspace

USER rstudio
