# syntax=docker/dockerfile:1.6

FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

ARG PPM_DISTRO=noble
ARG PPM_URL=https://packagemanager.posit.co/cran/__linux__/${PPM_DISTRO}/latest

ENV CRAN=${PPM_URL}
ENV TZ=UTC
ENV R_KEEP_PKG_SOURCE=yes

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1 \
    libuv1-dev \
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
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN"))); install.packages(c("remotes", "devtools", "plotly"))'

COPY DESCRIPTION /app/DESCRIPTION

RUN --mount=type=secret,id=github_pat,required=false \
    if [ -f /run/secrets/github_pat ]; then export GITHUB_PAT="$(cat /run/secrets/github_pat)"; fi && \
    Rscript -e 'options(repos = c(CRAN = Sys.getenv("CRAN")), Ncpus = 1); remotes::install_deps("/app", dependencies = TRUE, upgrade = "never", build = FALSE)'

COPY . /app

RUN R CMD INSTALL /app && \
    chown -R rstudio:rstudio /app /usr/local/lib/R/site-library

USER rstudio

ENTRYPOINT ["Rscript", "-e"]
CMD ["creaco2tracker::update_all(diagnostics_folder=NULL)"]
