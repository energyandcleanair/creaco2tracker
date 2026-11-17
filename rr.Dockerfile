# syntax=docker/dockerfile:1.6

FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.5@sha256:289c5d02d8115aa209f4a8a49ee9378dccbf623897eed9cc46c87dfbbca9015b

ARG PPM_DISTRO=noble
ARG PPM_URL=https://packagemanager.posit.co/cran/__linux__/${PPM_DISTRO}/latest

# Posit binaries must target the same distribution as the base image.
RUN . /etc/os-release && \
    if [ "$VERSION_CODENAME" != "$PPM_DISTRO" ]; then \
        echo "Base image is $VERSION_CODENAME but PPM_DISTRO is $PPM_DISTRO" >&2; \
        exit 1; \
    fi

RUN apt-get update && apt-get install -y --no-install-recommends \
    bubblewrap \
    socat \
    cmake \
    gdal-bin \
    git \
    gsfonts \
    libabsl-dev \
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
    libgit2-dev \
    libicu-dev \
    libpq-dev \
    libsqlite3-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libuv1-dev \
    libwebp-dev \
    libx11-dev \
    make \
    pandoc \
    zlib1g-dev \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /workspace /cache/pak/packages /cache/pak/metadata \
    && chown -R rstudio:rstudio /workspace /cache

ENV CRAN=${PPM_URL}
ENV PKG_SYSREQS=false

# Bootstrap pak; namespace loading makes bootstrap installation errors fatal.
RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN"))); install.packages("pak"); loadNamespace("pak")'

WORKDIR /workspace
COPY DESCRIPTION scripts/install-image-deps.R /workspace/

RUN --mount=type=secret,id=github_pat \
    export GITHUB_PAT="$(cat /run/secrets/github_pat)" && \
    Rscript /workspace/install-image-deps.R

RUN chown -R rstudio:rstudio /usr/local/lib/R/site-library /workspace

USER rstudio
