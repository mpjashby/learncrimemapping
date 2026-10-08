# The checker compatibility workflow sources the checker without installing
# all course packages. Load its new modules in that source-only test context.
# Normal package tests already have these functions in the package namespace.
if (!exists("check_code_backend", mode = "function", inherits = TRUE)) {
  for (module in c("code_feedback.R", "check_submissions.R", "submission_container.R",
                   "write_code_feedback.R", "spatial_feedback.R",
                   "package_moodle_feedback.R")) {
    source(testthat::test_path("..", "..", "R", module), local = .GlobalEnv)
  }
}

feedback_fixture <- function(lines, extension = ".R") {
  file <- tempfile(fileext = extension)
  writeLines(lines, file)
  file
}

feedback_zip <- function(submissions) {
  testthat::skip_if(Sys.which("zip") == "", "ZIP fixture creation requires zip")
  root <- tempfile("zip-fixture-")
  dir.create(root)
  for (path in names(submissions)) {
    dir.create(dirname(file.path(root, path)), recursive = TRUE, showWarnings = FALSE)
    if (is.raw(submissions[[path]])) {
      writeBin(submissions[[path]], file.path(root, path))
    } else {
      writeLines(submissions[[path]], file.path(root, path))
    }
  }
  zip <- tempfile(fileext = ".zip")
  withr::with_dir(root, utils::zip(zip, files = names(submissions), flags = "-q"))
  zip
}

