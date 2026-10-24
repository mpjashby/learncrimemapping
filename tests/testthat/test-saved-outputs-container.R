test_that("Docker saved files are embedded after the container stops", {
  skip_if(Sys.getenv("LCM_TEST_DOCKER") != "true",
          "Set LCM_TEST_DOCKER=true after building the image")
  zip <- feedback_zip(list("Participant_125_assignsubmission_file/map.R" = c(
    'map <- function() plot(1:3)',
    'png("outputs/map.png"); map(); dev.off()',
    'pdf("outputs/map.pdf"); map(); dev.off()',
    'stop("later error")'
  )))
  batch <- check_submissions(zip, tempfile(), style = FALSE,
                             saved_files = "outputs/*", tile_cache = NULL)
  result <- batch$results[[1]]
  expect_identical(result$execution$status, "error")
  expect_length(result$saved_files, 2)
  expect_identical(result$saved_files[[2]]$status, "included")
  if (nzchar(Sys.which("pdftoppm"))) {
    expect_identical(result$saved_files[[1]]$status, "included")
  } else {
    expect_match(result$saved_files[[1]]$note, "Poppler")
  }
  expect_false(dir.exists(result$execution_dir))
  html <- paste(readLines(result$html_report), collapse = "\n")
  expect_match(html, "data:image/png;base64,")
  expect_match(html, "outputs/map.pdf")
})
