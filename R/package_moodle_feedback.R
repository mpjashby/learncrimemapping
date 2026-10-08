#' Package reviewed reports for Moodle feedback upload
#'
#' Matches reports to the original Moodle submission folders by participant ID,
#' including empty submission folders. Refuses incomplete batches and existing
#' output paths, and verifies the resulting ZIP contains one nonempty report
#' per submission folder. Run after reviewing the batch reports.
#'
#' @param submission_zip Original ZIP downloaded from Moodle.
#' @param run_dir Results directory produced by [check_submissions()].
#' @param upload_dir New staging directory for reports in Moodle folders.
#' @param feedback_zip Path to a new feedback ZIP for uploading to Moodle.
#' @return Invisibly, the verified ZIP file listing from [utils::unzip()].
#' @keywords internal
package_moodle_feedback <- function(
  submission_zip,
  run_dir,
  upload_dir,
  feedback_zip
) {
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  manifest <- utils::read.csv(
    file.path(run_dir, "manifest.csv"), stringsAsFactors = FALSE
  )

  paths <- utils::unzip(submission_zip, list = TRUE)$Name
  paths <- paths[!grepl("(^|/)(__MACOSX|\\.DS_Store)(/|$)", paths)]
  folders <- unique(sub("/.*$", "", paths))
  stopifnot(
    length(folders) > 0L,
    all(grepl("^.+_[0-9]+_assignsubmission_file$", folders))
  )
  participants <- paste0(
    "Participant_",
    sub("^.*_([0-9]+)_assignsubmission_file$", "\\1", folders)
  )
  stopifnot(
    !anyDuplicated(participants),
    !anyDuplicated(manifest$participant),
    nrow(manifest) == length(participants),
    setequal(manifest$participant, participants)
  )
  # Match by ID, never by the row order of two lists.
  manifest <- manifest[match(participants, manifest$participant), ]
  reports <- file.path(run_dir, manifest$report)
  stopifnot(all(file.exists(reports)), all(file.info(reports)$size > 0))

  upload_dir <- path.expand(upload_dir)
  feedback_zip <- path.expand(feedback_zip)
  if (file.exists(upload_dir) || file.exists(feedback_zip)) {
    stop("Choose new staging and ZIP paths; existing output is not overwritten.")
  }
  dir.create(upload_dir, recursive = TRUE)
  upload_dir <- normalizePath(upload_dir, winslash = "/", mustWork = TRUE)
  dir.create(dirname(feedback_zip), recursive = TRUE, showWarnings = FALSE)
  feedback_zip <- file.path(
    normalizePath(dirname(feedback_zip), winslash = "/", mustWork = TRUE),
    basename(feedback_zip)
  )
  relative_files <- file.path(folders, "feedback.html")
  for (i in seq_along(folders)) {
    dir.create(file.path(upload_dir, folders[i]))
    if (!file.copy(reports[i], file.path(upload_dir, relative_files[i]))) {
      stop("Failed to copy report for ", participants[i])
    }
  }

  # Zip only the listed reports, from inside the staging directory.
  # -X excludes extra file attributes; no macOS metadata files are selected.
  previous_dir <- setwd(upload_dir)
  on.exit(setwd(previous_dir), add = TRUE)
  status <- utils::zip(feedback_zip, files = relative_files, flags = "-9X")
  if (status != 0L || !file.exists(feedback_zip)) stop("ZIP creation failed.")

  listing <- utils::unzip(feedback_zip, list = TRUE)
  expected <- gsub("\\\\", "/", relative_files)
  stopifnot(
    nrow(listing) == length(expected),
    setequal(listing$Name, expected),
    all(listing$Length > 0)
  )
  message("Ready for Moodle: ", feedback_zip)
  invisible(listing)
}
