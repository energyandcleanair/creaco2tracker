FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.5@sha256:289c5d02d8115aa209f4a8a49ee9378dccbf623897eed9cc46c87dfbbca9015b

ARG PPM_DISTRO=noble
ARG PPM_URL=https://packagemanager.posit.co/cran/__linux__/${PPM_DISTRO}/latest

ENV CRAN=${PPM_URL}
ENV PKG_CRAN_MIRROR=${PPM_URL}
ENV PKG_SYSREQS=false
ENV PKG_SYSREQS_RSPM_URL=https://packagemanager.posit.co
ENV PKG_CACHE_DIR=/workspaces/.cache/pak
ENV PKG_PACKAGE_CACHE_DIR=/workspaces/.cache/pak/packages
ENV PKG_METADATA_CACHE_DIR=/workspaces/.cache/pak/metadata

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    gdal-bin \
    git \
    gsfonts \
    libabsl-dev \
    libgit2-dev \
    libicu-dev \
    libpq-dev \
    libsqlite3-dev \
    libuv1-dev \
    libwebp-dev \
    libx11-dev \
    make \
    pandoc \
    zlib1g-dev \
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

RUN mkdir /workspaces && \
    chown rstudio:rstudio /workspaces && \
    chmod 755 /workspaces

USER rstudio

COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site
COPY Renviron.site /usr/local/lib/R/etc/Renviron.site

RUN mkdir -p /home/rstudio/.local/share \
    && python3 -m venv /home/rstudio/.local/share/datawrangler-venv \
    && /home/rstudio/.local/share/datawrangler-venv/bin/python -m pip install --upgrade pip \
    && /home/rstudio/.local/share/datawrangler-venv/bin/python -m pip install \
        pandas \
        ipykernel \
        openpyxl \
        pyarrow

RUN mkdir -p /workspaces/.cache/pak/packages /workspaces/.cache/pak/metadata \
    && Rscript -e 'install.packages("pak")' \
    && Rscript -e 'pak::pkg_install(c("languageserver", "lintr", "testthat", "httpgd", "DBI", "RPostgres", "httr2", "jsonlite"))'
