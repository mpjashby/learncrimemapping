# Build from the repository root or the installed-package helper
# build_checker_image(), which supplies a temporary context of trusted files.
# R only: this multi-platform base contains neither RStudio Server nor TeX.
FROM rocker/r-ver:4.5.3@sha256:c3f39b365d1077fe24f8e9ab2742e352b6d3950897f51af1624a5bb5550c21c0
USER root
COPY inst/container/install-packages.R /opt/learncrimemapping/install-packages.R
# Keep compilation tools because student pacman::p_load() calls can install
# source packages on ARM. Spatial headers support sf/s2/units (and terra/raster).
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran cmake pkg-config ca-certificates \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev libabsl-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    && Rscript --vanilla /opt/learncrimemapping/install-packages.R \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
# Optional assignment-specific packages; these are otherwise installed privately
# by student code. Changing this argument reuses the core checker layer.
ARG EXERCISE_PACKAGES=""
ARG EXERCISE_SYSTEM_PACKAGES=""
RUN if [ -n "$EXERCISE_SYSTEM_PACKAGES" ]; then \
      apt-get update && apt-get install -y --no-install-recommends $EXERCISE_SYSTEM_PACKAGES; \
    fi \
    && if [ -n "$EXERCISE_PACKAGES" ]; then \
      EXERCISE_PACKAGES="$EXERCISE_PACKAGES" Rscript --vanilla /opt/learncrimemapping/install-packages.R; \
    fi \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
# Use the same helpers as the source-only checker tests. Installing the course
# metapackage would pull in every course dependency, even for a small assignment.
COPY R/check_code.R R/code_feedback.R R/spatial_feedback.R /opt/learncrimemapping/R/
COPY inst/container/runner.R /opt/learncrimemapping-runner.R
RUN chmod -R a+rX /opt/learncrimemapping \
    && chmod a+r /opt/learncrimemapping-runner.R \
    && chmod -R a-w /usr/local/lib/R/site-library /usr/local/lib/R/library
USER 1000:1000
WORKDIR /workspace
ENTRYPOINT ["Rscript", "--vanilla", "/opt/learncrimemapping-runner.R"]
