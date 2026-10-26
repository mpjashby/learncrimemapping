# Run against a source archive, just as a user installing a release would.
dir.create("check", showWarnings = FALSE)
archive <- pkgbuild::build(".", dest_path = "check", manual = FALSE)
result <- rcmdcheck::rcmdcheck(
  archive,
  args = c("--no-manual"),
  check_dir = "check",
  error_on = "never"
)
saveRDS(result, "check/results.rds")

# This package deliberately installs course packages through Imports. Permit
# only that dependency note; new notes, warnings and errors need investigation.
expected_note <- startsWith(
  result$notes,
  "checking dependencies in R code ... NOTE\nNamespaces in Imports field not imported from:"
) &
  endsWith(trimws(result$notes), "All declared Imports should be used.")
expected_unused <- c(
  "crsuggest",
  "cyclocomp",
  "gganimate",
  "ggauto",
  "ggrepel",
  "ggridges",
  "ggspatial",
  "ggthemes",
  "gifski",
  "gt",
  "janitor",
  "leaflet",
  "osmdata",
  "pacman",
  "paletteer",
  "patchwork",
  "prettymapr",
  "raster",
  "remotes",
  "rnaturalearth",
  "sfhotspot",
  "slider",
  "tidygeocoder",
  "tidyverse",
  "usethis"
)
for (i in which(expected_note)) {
  # R uses straight or typographic quotation marks depending on its locale.
  quoted <- regmatches(
    result$notes[i],
    gregexpr("['\u2018][A-Za-z][A-Za-z0-9.]*['\u2019]", result$notes[i])
  )[[1]]
  unused <- substring(quoted, 2L, nchar(quoted) - 1L)
  expected_note[i] <- setequal(unused, expected_unused)
}
unexpected_notes <- result$notes[!expected_note]
if (
  length(result$errors) || length(result$warnings) || length(unexpected_notes)
) {
  stop(paste(
    c(result$errors, result$warnings, unexpected_notes),
    collapse = "\n\n"
  ))
}

# Use a fresh package library and a fresh R process, retaining the dependency
# libraries supplied by CI. This also verifies configure/configure.win bundling.
library_path <- tempfile("installed-package-")
dir.create(library_path)
invisible(processx::run(
  file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "R.exe" else "R"
  ),
  c("CMD", "INSTALL", paste0("--library=", library_path), archive),
  echo = TRUE,
  error_on_status = TRUE
))
callr::r(
  function(library_path) {
    .libPaths(c(library_path, .libPaths()))
    library(learncrimemapping)
    actual_library <- normalizePath(
      dirname(find.package("learncrimemapping")),
      winslash = "/",
      mustWork = TRUE
    )
    expected_library <- normalizePath(
      library_path,
      winslash = "/",
      mustWork = TRUE
    )
    if (!identical(actual_library, expected_library)) {
      stop(sprintf(
        "Package loaded from %s; expected %s",
        actual_library,
        expected_library
      ))
    }
    root <- tempfile()
    dir.create(root)
    workspace <- file.path(root, "crime_mapping")
    dir.create(workspace)
    on.exit(unlink(root, recursive = TRUE))
    setwd(workspace)
    paths <- learncrimemapping::create_dirs()
    stopifnot(length(paths) == 5L, all(dir.exists(paths)))
    writeLines("x <- 1 + 1", "R/example.R")
    feedback <- learncrimemapping::check_code("R/example.R", style = FALSE)
    stopifnot(identical(feedback$execution$status, "success"))
    # Avoid editor interaction and live version lookups in this smoke test.
    stopifnot(is.function(learncrimemapping::check_workspace))
    resources <- system.file("container", package = "learncrimemapping")
    context <- tempfile()
    dir.create(context)
    on.exit(unlink(context, recursive = TRUE), add = TRUE)
    learncrimemapping:::prepare_checker_image_context(resources, context)
    stopifnot(all(file.exists(file.path(
      context,
      "R",
      c("check_code.R", "code_feedback.R", "spatial_feedback.R")
    ))))
  },
  args = list(library_path = library_path)
)
unlink(library_path, recursive = TRUE)
