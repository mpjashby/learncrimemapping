test_that("inspection calls are flagged only when their results are unused", {
  code <- c("head(x)", "readxl::excel_sheets(path)", "x |> head()", "x %>% tail()",
            "y <- head(x)", "head(x) |> nrow()", "x |> head() |> nrow()",
            "nrow(head(x))", "f <- function(x) head(x)", "head(x) -> y",
            "# head(x)", 'text <- "head(x)"', "{", "  tail(x)", "}", "(head(x))", "for (i in 1:2) head(x)")
  issues <- code_course_issues(feedback_fixture(code), code_feedback_profile())
  inspection <- Filter(function(x) x$id == "course.inspection_in_script", issues)
  expect_equal(vapply(inspection, `[[`, integer(1), "line"), c(1L, 2L, 3L, 4L, 14L, 16L, 17L))
})

test_that("conditions are presented directly and R-only warnings remain in the log", {
  code <- c('warning("st_point_on_surface may not give correct results for longitude/latitude data")',
            'message("The following objects are masked from package:base: intersect")')
  r <- check_code_backend(feedback_fixture(code), style = FALSE)
  q <- check_code_backend(feedback_fixture(c("```{r}", code, "```"), ".qmd"), style = FALSE)
  expect_length(r$issues, 0L)
  expect_identical(code_feedback_status(r), "success")
  expect_identical(code_feedback_status(q), "warning")
  expect_length(Filter(function(x) x$kind == "warning", r$execution$events), 1L)
  html <- tempfile(fileext = ".html")
  write_code_feedback(q, html)
  text <- paste(readLines(html), collapse = "\n")
  expect_match(text, "<span class='condition-warning'>WARNING: </span>st_point_on_surface", fixed = TRUE)
  expect_match(text, "Message: The following objects", fixed = TRUE)
  expect_false(grepl("<ol>", text, fixed = TRUE))
  expect_false(grepl("This expression produced a message", text, fixed = TRUE))
  expect_false(grepl("initial-fail{", text, fixed = TRUE))
})

test_that("missing arguments show the enclosing call and give comma guidance", {
  r <- check_code_backend(feedback_fixture(c("x <- paste(", '  "a",', ")")), reprex = FALSE)
  issues <- Filter(function(i) grepl("Missing argument", i$message), r$issues)
  expect_length(issues, 1L)
  expect_match(issues[[1]]$action, "extra comma after the last argument", fixed = TRUE)
  expect_match(issues[[1]]$evidence, 'paste(\n  "a",\n)', fixed = TRUE)
  expect_equal(issues[[1]]$line, 1L)
})

test_that("index statuses prioritise errors, warnings, messages and style", {
  r <- check_code_backend(feedback_fixture("x <- 1"), style = FALSE)
  expect_identical(code_feedback_status(r), "success")
  for (pair in list(c("lint.example", "style issues"), c("runtime.message", "message"),
                   c("runtime.warning", "warning"), c("runtime.error", "error"))) {
    r$issues <- c(list(code_issue(pair[1], "execution", "condition", "action")), r$issues)
    expect_identical(code_feedback_status(r), pair[2])
  }
})


test_that("assessor status labels are displayed with the requested colours", {
  manifest <- data.frame(participant = paste0("Participant_", 1:6),
    report = paste0(1:6, ".html"), execution = "success", issues = 0L,
    incomplete = FALSE, status = c("error", "warning", "message", "style issues", "success", "skipped"))
  file <- tempfile(fileext = ".html")
  write_submission_index(manifest, file, code_feedback_profile())
  html <- paste(readLines(file), collapse = "\n")
  expect_match(html, "<th>Status</th>", fixed = TRUE)
  expect_match(html, "<span class='status-error'>error</span>", fixed = TRUE)
  expect_match(html, "<span class='status-warning'>warning</span>", fixed = TRUE)
})
