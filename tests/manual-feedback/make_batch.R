# Create a Moodle-shaped archive using synthetic participant identifiers
make_manual_feedback_zip <- function(
    examples_dir = "tests/manual-feedback",
    destination = tempfile("manual-feedback-", fileext = ".zip")) {
  if (!nzchar(Sys.which("zip"))) stop("Creating this archive requires zip.")
  examples_dir <- normalizePath(examples_dir, mustWork = TRUE)
  destination <- file.path(normalizePath(dirname(destination), mustWork = TRUE),
                           basename(destination))
  if (file.exists(destination)) stop("Choose a new ZIP destination.")
  files <- sort(c(list.files(file.path(examples_dir, "r"), pattern = "\\.[Rr]$",
                           full.names = TRUE),
                  list.files(file.path(examples_dir, "quarto"), pattern = "\\.[Qq][Mm][Dd]$",
                             full.names = TRUE)))
  staging <- tempfile("manual-feedback-staging-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  archive_paths <- character(length(files))
  for (i in seq_along(files)) {
    folder <- paste0("Participant_", 1000L + i, "_assignsubmission_file")
    dir.create(file.path(staging, folder))
    archive_paths[i] <- file.path(folder, basename(files[i]))
    if (!file.copy(files[i], file.path(staging, archive_paths[i]))) {
      stop("Could not stage an example file.")
    }
  }
  withr::with_dir(staging, utils::zip(destination, archive_paths, flags = "-q"))
  if (!file.exists(destination)) stop("The example ZIP could not be created.")
  invisible(destination)
}
