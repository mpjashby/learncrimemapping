test_that("saved output patterns stay within visible exercise paths", {
  for (pattern in c("../map.png", "/tmp/map.png", "output/../map.png",
                    ".sandbox/*", "checker/*", "output\\map.png")) {
    expect_error(validate_saved_files(pattern), "relative")
  }
  expect_identical(validate_saved_files(character()), character())
})

test_that("batch feedback retains saved maps after cleanup and packaging", {
  zip <- feedback_zip(list("Participant_123_assignsubmission_file/map.R" = c(
    'map <- function() plot(1:3)',
    'png("outputs/map.png"); map(); dev.off()',
    'stop("later error")'
  )))
  run <- tempfile()
  batch <- suppressWarnings(check_submissions(zip, run, backend = "local",
    style = FALSE, saved_files = c("outputs/*.png", "outputs/missing.pdf")))
  entry <- batch$results[[1]]$saved_files[[1]]
  expect_identical(entry$status, "included")
  expect_identical(entry$file, "outputs/map.png")
  expect_identical(batch$results[[1]]$execution$status, "error")
  expect_false(dir.exists(batch$results[[1]]$execution_dir))
  html <- paste(readLines(batch$results[[1]]$html_report), collapse = "\n")
  expect_match(html, "Saved files produced by your code")
  expect_match(html, "data:image/png;base64,")
  expect_match(html, "outputs/missing.pdf")
  packaged <- tempfile(fileext = ".zip")
  package_moodle_feedback(zip, run, tempfile(), packaged)
  dest <- tempfile(); dir.create(dest)
  utils::unzip(packaged, exdir = dest)
  expect_identical(readLines(file.path(dest,
    "Participant_123_assignsubmission_file/feedback.html")),
    readLines(batch$results[[1]]$html_report))
})

test_that("template images are excluded but overwritten images are collected", {
  root <- tempfile(); dir.create(root)
  file <- file.path(root, "map.png")
  grDevices::png(file); plot(1); grDevices::dev.off()
  before <- submission_output_snapshot(root, "*.png")
  result <- list(execution = list(status = "success"))
  expect_length(collect_submission_outputs(result, root, "*.png", before)$saved_files, 0)
  grDevices::png(file); plot(1:10); grDevices::dev.off()
  expect_length(collect_submission_outputs(result, root, "*.png", before)$saved_files, 1)
})

test_that("PDF previews are portable and multi-page output is bounded", {
  skip_if(Sys.which("pdftoppm") == "")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  for (i in 1:7) plot(i, main = paste("Map", i))
  grDevices::dev.off()
  preview <- saved_file_previews(file)
  expect_length(preview$images, 5)
  expect_true(all(startsWith(preview$images, "data:image/png;base64,")))
  expect_match(preview$note, "first five")
})

test_that("invalid and oversized outputs do not discard checking results", {
  root <- tempfile(); dir.create(root)
  writeLines("not an image", file.path(root, "bad.png"))
  writeLines("<svg/>", file.path(root, "map.svg"))
  result <- collect_submission_outputs(list(execution = list(status = "success")),
    root, "*", setNames(character(), character()))
  expect_identical(result$execution$status, "success")
  expect_true(all(vapply(result$saved_files, function(x) x$status == "unavailable", logical(1))))
  expect_match(result$saved_files[[1]]$note, "signature")
  expect_match(result$saved_files[[2]]$note, "unsupported")
  writeBin(as.raw(rep(0, 10 * 1024^2 + 1)), file.path(root, "large.png"))
  result <- collect_submission_outputs(list(execution = list(status = "timeout")),
    root, "large.png", setNames(character(), character()))
  expect_match(result$saved_files[[1]]$note, "limit")
})

test_that("saved file collection can be disabled and rejects escaped links", {
  root <- tempfile(); dir.create(root)
  result <- list(execution = list(status = "success"))
  expect_length(collect_submission_outputs(result, root, character(), character())$saved_files, 0)
  target <- tempfile(); writeLines("private", target)
  skip_on_os("windows")
  expect_true(file.symlink(target, file.path(root, "map.png")))
  expect_error(submission_output_snapshot(root, "*.png"), "Unsafe")
})

test_that("missing PDF converters produce actionable feedback", {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file); plot(1); grDevices::dev.off()
  withr::local_envvar(PATH = "")
  expect_error(saved_file_previews(file), "install Poppler")
})

test_that("JPEG headers and default discovery support saved ggplot objects", {
  skip_if_not_installed("ggplot2")
  zip <- feedback_zip(list("Participant_124_assignsubmission_file/map.R" = c(
    'map <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()',
    'ggplot2::ggsave("outputs/map.jpg", plot = map, width = 4, height = 3)'
  )))
  batch <- suppressWarnings(check_submissions(zip, tempfile(), backend = "local", style = FALSE))
  result <- batch$results[[1]]
  expect_identical(result$execution$status, "success")
  expect_length(result$saved_files, 1)
  expect_identical(result$saved_files[[1]]$status, "included")
  expect_match(result$saved_files[[1]]$previews, "^data:image/jpeg;base64,")
  expect_length(result$saved_file_notes, 0)
})

test_that("saved files survive an execution timeout", {
  zip <- feedback_zip(list("Participant_126_assignsubmission_file/map.R" = c(
    'png("outputs/map.png"); plot(1:3); dev.off()', 'Sys.sleep(60)'
  )))
  batch <- suppressWarnings(check_submissions(zip, tempfile(), backend = "local",
    style = FALSE, timeout = 5))
  result <- batch$results[[1]]
  expect_identical(result$execution$status, "timeout")
  expect_length(result$saved_files, 1)
  expect_identical(result$saved_files[[1]]$status, "included")
})

test_that("image dimension limits and truncated images are rejected", {
  file <- tempfile(fileext = ".png")
  grDevices::png(file); plot(1); grDevices::dev.off()
  bytes <- readBin(file, "raw", n = file.info(file)$size)
  bytes[17:20] <- as.raw(c(0, 1, 0, 0))
  writeBin(bytes, file)
  expect_error(saved_file_previews(file), "pixel")
  writeBin(head(bytes, 30L), file)
  expect_error(saved_file_previews(file), "incomplete")
})
