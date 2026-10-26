# Trusted helper sources are installed in the image, outside the student's
# writable filesystem. This intentionally avoids the full course metapackage.
options(repos = c(CRAN = "https://cloud.r-project.org"))
# Put the private writable student library before the preinstalled libraries.
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
# Source helpers into one environment so they can resolve each other by name.
checker <- new.env(parent = globalenv())
for (module in c("check_code.R", "code_feedback.R", "spatial_feedback.R")) {
  sys.source(file.path("/opt/learncrimemapping/R", module), envir = checker)
}
# The host writes this request before launch and reads result.rds after exit.
request <- readRDS("/workspace/.sandbox/request.rds")
result <- checker$check_code_backend(
  request$file,
  request$reprex,
  request$style,
  "/workspace",
  request$timeout,
  request$profile,
  "/workspace/checker",
  allow_package_install = TRUE
)
saveRDS(result, "/workspace/.sandbox/result.rds")
