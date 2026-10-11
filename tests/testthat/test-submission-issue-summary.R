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

test_that("expected script messages remain in logs but are excluded from marking issues", {
  messages <- c("Rows: 10 Columns: 2", "Delimiter: ,", "Column specification",
    "Use `spec()` to retrieve the full column specification for this data.",
    "Specify the column types or set `show_col_types = FALSE` to quiet this message.",
    "The following objects are masked from package:base:\n    union",
    "A package-specific startup notice", "<httr2_response>\nStatus: 200 OK")
  for (extension in c(".R", ".qmd")) {
    result <- check_code_backend(feedback_fixture(if (extension == ".R") "x <- 1" else
      c("```{r}", "x <- 1", "```"), extension), reprex = FALSE, style = FALSE)
    result$execution$events <- lapply(seq_along(messages), function(i)
      list(kind = if (i == length(messages)) "output" else "message",
        text = messages[i], line = 1L,
        condition_classes = if (i == 7L) c("packageStartupMessage", "message") else "message"))
    result$issues <- interpret_code_feedback(result)
    report <- tempfile(fileext = ".html")
    summary <- tempfile(fileext = ".html")
    withr::defer(unlink(c(report, summary)))
    write_code_feedback(result, report)
    manifest <- data.frame(participant = "Participant_1", report = "feedback/report.html")
    write_submission_issue_summary(manifest, list(Participant_1 = result), summary,
      result$profile)
    html <- paste(readLines(report), collapse = "\n")
    issues_html <- paste(readLines(summary), collapse = "\n")
    log_start <- regexpr("<details><summary>Complete captured text log</summary>", html, fixed = TRUE)[1]
    expect_gt(log_start, 0L)
    issue_html <- substr(html, 1L, log_start - 1L)
    log_html <- substring(html, log_start)
    for (message in messages) expect_match(log_html, feedback_html_escape(message), fixed = TRUE)
    if (extension == ".R") {
      expect_length(result$issues, 0L)
      for (message in messages) {
        expect_false(grepl(feedback_html_escape(message), issue_html, fixed = TRUE))
        expect_false(grepl(feedback_html_escape(message), issues_html, fixed = TRUE))
      }
    } else {
      expect_equal(sum(vapply(result$issues, function(x) x$id == "runtime.message",
        logical(1))), length(messages) - 1L)
      for (message in messages) expect_match(html, feedback_html_escape(message), fixed = TRUE)
      for (message in head(messages, -1L)) expect_match(issues_html,
        feedback_html_escape(message), fixed = TRUE)
    }
    result$execution$events <- list(list(kind = "message", text = "Unexpected diagnostic",
      line = 1L), list(kind = "warning", text = "Review this warning", line = 1L))
    expect_length(interpret_code_feedback(result), 2L)
  }
})
