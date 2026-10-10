test_that("issue summary counts submissions, ranks messages and retains sources", {
  issue <- function(message, evidence = "x < 1")
    code_issue("runtime.warning", "execution", message, "Review", 1L, evidence = evidence)
  results <- list(
    Participant_1 = list(issues = list(issue("Common <warning>"),
      issue("Common <warning>"), issue("Rare"))),
    Participant_2 = list(issues = list(issue("Common <warning>"))),
    Participant_99 = list(issues = list(issue("Stale"))))
  manifest <- data.frame(participant = c("Participant_1", "Participant_2", "Participant_3"),
    report = paste0("feedback/Participant_", 1:3, ".html"))
  file <- tempfile(fileext = ".html")
  withr::defer(unlink(file))
  write_submission_issue_summary(manifest, results, file, list(name = "Exercise"))
  html <- paste(readLines(file), collapse = "\n")
  expect_match(html, "<strong>2 submissions</strong>", fixed = TRUE)
  expect_match(html, "Common &lt;warning&gt;", fixed = TRUE)
  expect_match(html, "x &lt; 1", fixed = TRUE)
  expect_match(html, "feedback/Participant_1.html#code-line-1", fixed = TRUE)
  expect_lt(regexpr("Common", html)[1], regexpr("Rare", html)[1])
  expect_false(grepl("Stale", html, fixed = TRUE))
  expect_match(html, "unavailable for 1 submissions", fixed = TRUE)
})

test_that("summary handles empty results and code fallback", {
  file <- tempfile(fileext = ".html")
  withr::defer(unlink(file))
  manifest <- data.frame(participant = "Participant_1", report = "feedback/Participant_1.html")
  result <- list(issues = list(code_issue("runtime.text_output", "output", "Printed text",
    "Review", 1L, evidence = "captured output")), submitted_code = "print(1)")
  write_submission_issue_summary(manifest, list(Participant_1 = result), file, list(name = "Exercise"))
  expect_match(paste(readLines(file), collapse = "\n"), "print(1)", fixed = TRUE)
  result$issues <- list()
  write_submission_issue_summary(manifest, list(Participant_1 = result), file, list(name = "Exercise"))
  expect_match(paste(readLines(file), collapse = "\n"), "No issues were recorded", fixed = TRUE)
})

test_that("informational messages from the captured log are included", {
  file <- tempfile(fileext = ".html")
  withr::defer(unlink(file))
  result <- list(issues = list(), execution = list(events = list(
    list(kind = "source", text = "library(sf)", line = 1L),
    list(kind = "message", text = "Linking to GEOS", line = 1L))))
  manifest <- data.frame(participant = "Participant_1", report = "feedback/Participant_1.html")
  write_submission_issue_summary(manifest, list(Participant_1 = result), file, list(name = "Exercise"))
  html <- paste(readLines(file), collapse = "\n")
  expect_match(html, "Linking to GEOS", fixed = TRUE)
  expect_match(html, "library(sf)", fixed = TRUE)
})
