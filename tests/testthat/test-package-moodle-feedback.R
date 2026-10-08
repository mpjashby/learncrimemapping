test_that("Moodle packaging preserves folder names and matches reports by ID", {
  skip_if(Sys.which("zip") == "", "ZIP creation requires zip")
  root <- withr::local_tempdir()
  original <- file.path(root, "original")
  dir.create(original)
  folders <- c("Jane Smith_12345_assignsubmission_file",
               "Participant_67890_assignsubmission_file")
  for (folder in folders) dir.create(file.path(original, folder))
  writeLines("x <- 1", file.path(original, folders[1], "exercise.R"))
  # The second submission folder is empty but still needs feedback.
  submission_zip <- file.path(root, "submissions.zip")
  withr::with_dir(original, utils::zip(submission_zip, folders, flags = "-qr"))
  run_dir <- file.path(root, "run")
  dir.create(file.path(run_dir, "feedback"), recursive = TRUE)
  participants <- c("Participant_67890", "Participant_12345")
  reports <- paste0("feedback/", participants, ".html")
  for (i in seq_along(reports)) {
    writeLines(paste0("<p>", participants[i], "</p>"), file.path(run_dir, reports[i]))
  }
  manifest <- data.frame(participant = participants, report = reports)
  utils::write.csv(manifest, file.path(run_dir, "manifest.csv"), row.names = FALSE)
  upload_dir <- file.path(root, "upload")
  feedback_zip <- file.path(root, "feedback.zip")
  previous_dir <- getwd()
  expect_message(
    listing <- package_moodle_feedback(submission_zip, run_dir, upload_dir, feedback_zip),
    "Ready for Moodle"
  )
  expect_identical(getwd(), previous_dir)
  expect_setequal(listing$Name, paste0(folders, "/feedback.html"))
  extracted <- file.path(root, "extracted")
  utils::unzip(feedback_zip, exdir = extracted)
  expect_identical(readLines(file.path(extracted, folders[1], "feedback.html")),
                   "<p>Participant_12345</p>")
  expect_identical(readLines(file.path(extracted, folders[2], "feedback.html")),
                   "<p>Participant_67890</p>")
  expect_error(package_moodle_feedback(submission_zip, run_dir, upload_dir,
                                      file.path(root, "new.zip")), "not overwritten")
  expect_error(package_moodle_feedback(submission_zip, run_dir,
                                      file.path(root, "new-upload"), feedback_zip),
               "not overwritten")
  utils::write.csv(manifest[1, ], file.path(run_dir, "manifest.csv"), row.names = FALSE)
  expect_error(package_moodle_feedback(submission_zip, run_dir,
                                      file.path(root, "incomplete"),
                                      file.path(root, "incomplete.zip")))
  expect_false(dir.exists(file.path(root, "incomplete")))
})
