# Only marking/runtime dependencies; do not install the full course metapackage
# or package Suggests (which can pull in document renderers and large toolchains).
options(repos = c(CRAN = "https://cloud.r-project.org"))
required <- c(callr = "3.7.0", evaluate = "1.0.0", lintr = "3.4.0",
              cyclocomp = "0", cli = "0", rlang = "0", rprojroot = "2.0.4",
              jsonlite = "0", sf = "0", pacman = "0", here = "0")
extra <- trimws(strsplit(Sys.getenv("EXERCISE_PACKAGES"), ",", fixed = TRUE)[[1]])
extra <- extra[nzchar(extra)]
if (any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", extra))) {
  stop("EXERCISE_PACKAGES must contain comma-separated R package names.")
}
packages <- unique(c(names(required), extra))
available <- vapply(packages, function(package) {
  requireNamespace(package, quietly = TRUE) &&
    (!package %in% names(required) || utils::packageVersion(package) >= required[[package]])
}, logical(1))
if (any(!available)) {
  install.packages(packages[!available],
                   dependencies = c("Depends", "Imports", "LinkingTo"), Ncpus = 1L)
}
# install.packages() can warn instead of failing when compilation fails.
# Make those failures stop the image build, rather than surface during marking.
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Could not install required package: ", package)
  }
}
for (package in names(required)) {
  if (utils::packageVersion(package) < required[[package]]) {
    stop("Installed ", package, " is older than ", required[[package]])
  }
}
