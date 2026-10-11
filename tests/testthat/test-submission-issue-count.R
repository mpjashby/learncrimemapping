test_that("batch counts unique issue types while reports keep each occurrence", {
  zip <- feedback_zip(list(
    "Participant_1_assignsubmission_file/exercise.R" = c(
      paste0('# ', strrep('a', 90)), paste0('# ', strrep('b', 90))),
    "Participant_2_assignsubmission_file/exercise.R" = "x <- 1"
  ))
  output <- tempfile("unique-issues-")
  withr::defer(unlink(output, recursive = TRUE))
  batch <- check_submissions(zip, output, backend = "local", reprex = FALSE)
  expect_true(file.exists(batch$issue_summary))
  result <- batch$results$Participant_1
  ids <- vapply(result$issues, `[[`, character(1), "id")
  expect_gte(sum(ids == "lint.line_length_linter"), 2L)
  expect_equal(batch$manifest$issues[batch$manifest$participant == "Participant_1"],
               length(unique(ids)))
  expect_lt(length(unique(ids)), length(ids))
  html <- paste(readLines(file.path(output, "feedback", "Participant_1.html")), collapse = "\n")
  for (issue in Filter(function(x) x$id == "lint.line_length_linter", result$issues)) {
    expect_match(html, paste0("Line ", issue$line), fixed = TRUE)
  }
  index <- paste(readLines(batch$index), collapse = "\n")
  expect_match(index, paste0("<td>", length(unique(ids)), "</td>"), fixed = TRUE)
  expect_equal(batch$manifest$issues[batch$manifest$participant == "Participant_2"], 0L)
})

test_that("package startup and response messages do not increase script issue counts", {
  zip <- feedback_zip(list("Participant_1_assignsubmission_file/exercise.R" = c(
    'packageStartupMessage("Routine package startup")',
    'message("Delimiter: ,")',
    'cat("<httr2_response>\\nStatus: 200 OK")')))
  output <- tempfile("expected-messages-")
  withr::defer(unlink(output, recursive = TRUE))
  expect_warning(batch <- check_submissions(zip, output, backend = "local", style = FALSE),
    "Local checking is unrestricted")
  expect_equal(batch$manifest$issues, 0L)
  expect_true(any(vapply(batch$results$Participant_1$execution$events,
    function(x) "packageStartupMessage" %in% x$condition_classes, logical(1))))
  expect_match(paste(readLines(batch$index), collapse = "\n"), "<td>0</td>", fixed = TRUE)
})
