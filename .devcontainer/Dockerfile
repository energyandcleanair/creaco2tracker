# Pinned by digest: Ubuntu 26.04 (resolute), R 4.6.2, matching rr.Dockerfile.
FROM ghcr.io/rocker-org/devcontainer/tidyverse:4.6@sha256:19a7800a60cfb154158886794ea334b976ffb6a3764fca4d37935a5b4a42ddd6

# The base image sets CRAN to the Posit Package Manager binary repository for its own
# Ubuntu release. Stop the build if CRAN and /etc/os-release disagree.
RUN . /etc/os-release \
    && case "${CRAN}" in \
        */__linux__/"${VERSION_CODENAME}"/*) ;; \
        *) echo "CRAN=${CRAN} does not match Ubuntu ${VERSION_CODENAME}" >&2; exit 1 ;; \
    esac

ENV PKG_CRAN_MIRROR=${CRAN}
ENV PKG_SYSREQS=true
ENV PKG_SYSREQS_SUDO=true
ENV PKG_SYSREQS_RSPM_URL=https://packagemanager.posit.co
ENV PKG_CACHE_DIR=/workspaces/.cache/pak
ENV PKG_PACKAGE_CACHE_DIR=/workspaces/.cache/pak/packages
ENV PKG_METADATA_CACHE_DIR=/workspaces/.cache/pak/metadata

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
