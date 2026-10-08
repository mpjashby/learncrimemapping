test_that("shared feedback distinguishes output from routine setup messages", {
  file <- feedback_fixture(c(
    'message("Rows: 10 Columns: 2")', 'print("inspect this")',
    'message("Please review this")', 'warning("careful")', 'stop("broken")'
  ))
  result <- check_code_backend(file, style = FALSE)
  ids <- vapply(result$issues, `[[`, character(1), "id")
  expect_equal(sum(ids == "runtime.message"), 1L)
  expect_equal(sum(ids == "runtime.text_output"), 1L)
  expect_true(all(c("runtime.warning", "runtime.error") %in% ids))
  expect_identical(result$execution$status, "error")
  expect_equal(Filter(function(x) x$id == "runtime.error", result$issues)[[1]]$line, 5L)
})

test_that("Quarto text and disabled chunks do not attract script output feedback", {
  file <- feedback_fixture(c(
    "```{r}", "#| eval: false", "install.packages('no')", "```",
    "```{r}", 'print("required table")', "```"
  ), ".qmd")
  result <- check_code_backend(file)
  ids <- vapply(result$issues, `[[`, character(1), "id")
  expect_identical(result$execution$status, "success")
  expect_false("runtime.text_output" %in% ids)
  expect_false("course.package_install" %in% ids)
})

test_that("package installation preflight prevents side effects even without style checks", {
  marker <- tempfile()
  file <- feedback_fixture(c(sprintf('file.create("%s")', marker),
                              'utils::install.packages("nonexistent")'))
  result <- check_code_backend(file, style = FALSE)
  expect_identical(result$execution$status, "skipped")
  expect_false(file.exists(marker))
  expect_true(any(vapply(result$issues, function(x)
    x$id == "course.package_install", logical(1))))
  harmless <- feedback_fixture(c('# install.packages("x")',
                                  'x <- "install.packages()"'))
  expect_identical(check_code_backend(harmless, style = FALSE)$execution$status, "success")
})

test_that("profiles reject typos and declarations must be comments", {
  expect_error(code_feedback_profile(list(typo = TRUE)), "Unknown")
  expect_error(code_feedback_profile(list(text_output = "no")), "text_output")
  profile <- list(required_declaration = "AI declaration")
  result <- check_code_backend(feedback_fixture('x <- "AI declaration"'),
                               reprex = FALSE, profile = profile)
  expect_true("course.declaration_missing" %in%
                vapply(result$issues, `[[`, character(1), "id"))
  result <- check_code_backend(feedback_fixture("# AI declaration"),
                               reprex = FALSE, profile = profile)
  expect_length(result$issues, 0)
})

test_that("routine download receipts and network failures receive appropriate feedback", {
  result <- check_code_backend(feedback_fixture("x <- 1"), reprex = FALSE)
  result$execution$events <- list(
    list(kind = "source", text = 'req_perform(path = "data/raw/file.csv")', line = 1L),
    list(kind = "output", text = "<httr2_response>\nStatus: 200 OK", line = 1L),
    list(kind = "error", text = "Could not resolve host: example.org", line = 2L))
  issues <- interpret_code_feedback(result)
  expect_length(issues, 1L)
  expect_identical(issues[[1L]]$id, "runtime.environment")
  expect_identical(issues[[1L]]$category, "environment")
})

test_that("timeouts preserve prior output and static checks still run", {
  file <- feedback_fixture(c('cat("before")', "Sys.sleep(30)", "x=1"))
  result <- check_code_backend(file, timeout = 3)
  expect_identical(result$execution$status, "timeout")
  expect_true("runtime.timeout" %in% vapply(result$issues, `[[`, character(1), "id"))
  expect_true("assignment_linter" %in% result$lints$linter)
  expect_true(any(vapply(result$execution$events, function(x)
    x$kind == "output" && grepl("before", x$text), logical(1))))
})

test_that("disabled checks retain the compatible return schema without extraction", {
  file <- feedback_fixture("not valid R", ".qmd")
  expect_warning(result <- check_code(file, reprex = FALSE, style = FALSE), "disabled")
  expect_identical(result$execution$status, "skipped")
  expect_null(result$report)
  expect_length(result$issues, 0L)
})

test_that("HTML escapes student text and embeds plots without external dependencies", {
  result <- check_code_backend(feedback_fixture(c(
    'cat("<script>alert(1)</script>")', 'plot(1:3)'
  )), style = FALSE)
  file <- tempfile(fileext = ".html")
  write_code_feedback(result, file, participant = "Participant_1")
  html <- paste(readLines(file), collapse = "\n")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_match(html, "Content-Security-Policy", fixed = TRUE)
  expect_match(html, "SECU0005 Crime Mapping", fixed = TRUE)
  expect_match(html, "Code feedback: Participant 1", fixed = TRUE)
  expect_match(html, "Submitted file name:", fixed = TRUE)
  expect_match(html, "Submitted file type: R script", fixed = TRUE)
  expect_false(grepl("Execution: <strong>", html, fixed = TRUE))
  expect_match(html, "Code free of syntax errors: <span role='img' aria-label='Passed'>✅", fixed = TRUE)
  expect_match(html, "Code free of runtime errors: <span role='img' aria-label='Passed'>✅", fixed = TRUE)
  expect_false(grepl("Submission requirement problem", html, fixed = TRUE))
  expect_match(html, "syntax-function", fixed = TRUE)
  expect_match(html, "Visualisations produced by your code", fixed = TRUE)
  expect_lt(regexpr("<h2>Submitted code", html, fixed = TRUE)[1],
             regexpr("<h2>Visualisations", html, fixed = TRUE)[1])
})

test_that("initial checks distinguish parse failures, runtime errors and skipped execution", {
  invalid <- check_code_backend(feedback_fixture("x <- ("))
  expect_identical(invalid$syntax$status, "error")
  html <- feedback_initial_checks(invalid)
  expect_match(html, "Code free of syntax errors</a>: <span role='img' aria-label='Problem found'>❌", fixed = TRUE)
  expect_match(html, "Code free of runtime errors: <span role='img' aria-label='Check incomplete'>⚪</span> Not checked (syntax error)", fixed = TRUE)
  runtime <- check_code_backend(feedback_fixture('stop("broken")'), style = FALSE)
  expect_match(feedback_initial_checks(runtime), "Code free of syntax errors: <span role='img' aria-label='Passed'>✅", fixed = TRUE)
  expect_match(feedback_initial_checks(runtime), "Code free of runtime errors: <span role='img' aria-label='Problem found'>❌", fixed = TRUE)
  skipped <- check_code_backend(feedback_fixture("x <- 1"), reprex = FALSE)
  expect_match(feedback_initial_checks(skipped), "Code free of runtime errors: <span role='img' aria-label='Check incomplete'>⚪</span> Full check not possible – check for errors below", fixed = TRUE)
})

test_that("submitted code preserves original content and safely handles invalid syntax", {
  file <- feedback_fixture(c("# comment", "x <- 1", "\tprint(x)"))
  result <- check_code_backend(file, reprex = FALSE)
  writeLines("CHANGED AFTER CHECKING", file)
  html <- feedback_submitted_code(result)
  expect_match(html, "syntax-comment", fixed = TRUE)
  expect_match(html, "syntax-number", fixed = TRUE)
  expect_match(html, "\tprint(x)", fixed = TRUE)
  expect_false(grepl("CHANGED AFTER CHECKING", html, fixed = TRUE))
  invalid <- check_code_backend(feedback_fixture('x <- "<script>'), reprex = FALSE)
  expect_match(feedback_submitted_code(invalid), "&lt;script&gt;", fixed = TRUE)
  quarto <- check_code_backend(feedback_fixture(c(
    "# My report", "```{r}", "x <- 1", "```"), ".qmd"), reprex = FALSE)
  expect_match(feedback_submitted_code(quarto), "# My report", fixed = TRUE)
  expect_match(feedback_submitted_code(quarto), "syntax-number", fixed = TRUE)
  expect_identical(feedback_file_type(quarto$file), "Quarto file")
})

test_that("batch workspace roots are isolated and feedback matches the backend", {
  zip <- feedback_zip(list(
    "Participant_1_assignsubmission_file/exercise.R" = c(
      'root <- here::here()', 'writeLines(root, "outputs/root.txt")', 'x=1'),
    "Participant_2_assignsubmission_file/exercise.R" = 'stop("student error")'
  ))
  output <- tempfile("batch-")
  batch <- suppressWarnings(check_submissions(zip, output, backend = "local"))
  expect_equal(nrow(batch$manifest), 2L)
  expect_true(file.exists(batch$index))
  expect_true(all(file.exists(file.path(output, batch$manifest$report))))
  expect_identical(batch$results$Participant_1$execution$status, "success")
  expect_identical(batch$results$Participant_2$execution$status, "error")
  workspace <- file.path(output, "workspaces", "Participant_1")
  expect_identical(readLines(file.path(workspace, "outputs", "root.txt")),
                   normalizePath(workspace, winslash = "/"))
  result <- batch$results$Participant_1
  direct <- check_code_backend(result$file, execution_dir = workspace)
  expect_identical(result$issues, direct$issues)
  student <- NULL
  suppressMessages(capture.output(student <- check_code(
    result$file, execution_dir = workspace)))
  expect_identical(result$issues, student$issues)
  expect_error(suppressWarnings(check_submissions(zip, output, backend = "local")), "new or empty")
})

test_that("invalid submissions receive reports while valid submissions continue", {
  zip <- feedback_zip(list(
    "Participant_1_assignsubmission_file/a.R" = "x <- 1",
    "Participant_1_assignsubmission_file/b.R" = "x <- 2",
    "Participant_2_assignsubmission_file/a.txt" = "hello",
    "Participant_3_assignsubmission_file/a.R" = character(),
    "Participant_4_assignsubmission_file/a.R" = "x <- 1"
  ))
  output <- tempfile("batch-invalid-")
  batch <- check_submissions(zip, output, backend = "local", reprex = FALSE)
  expect_equal(nrow(batch$manifest), 4L)
  expect_equal(sum(vapply(batch$results, function(x)
    any(vapply(x$issues, function(i) i$id == "submission.invalid", logical(1))),
    logical(1))), 3L)
  expect_length(batch$results$Participant_4$issues, 0)
  expect_true(all(file.exists(file.path(output, batch$manifest$report))))
})

test_that("nested archives are rejected before output creation", {
  zip <- feedback_zip(list("Participant_1_assignsubmission_file/nested/code.R" = "1"))
  output <- tempfile()
  expect_error(suppressWarnings(check_submissions(zip, output, backend = "local")), "Nested")
  expect_false(dir.exists(output))
  template <- tempfile()
  dir.create(template)
  writeLines("stop('startup')", file.path(template, ".Rprofile"))
  expect_error(validate_submission_template(template), "startup")
})

test_that("binary ZIP submissions get an explicit error report without aborting the batch", {
  zip <- feedback_zip(list(
    "Participant_1_assignsubmission_file/submission.zip" = as.raw(0:255),
    "Participant_2_assignsubmission_file/submission.R" = "x <- 1"
  ))
  batch <- check_submissions(zip, tempfile("binary-batch-"), backend = "local", reprex = FALSE)
  expect_equal(nrow(batch$manifest), 2L)
  invalid <- batch$results$Participant_1
  expect_identical(invalid$execution$status, "skipped")
  expect_match(invalid$issues[[1]]$message,
               "The submitted file could not be checked because it is a ZIP archive.", fixed = TRUE)
  html <- paste(readLines(invalid$html_report), collapse = "\n")
  expect_match(html, "The submitted file could not be checked", fixed = TRUE)
  expect_false(grepl("aria-label='Passed'", feedback_initial_checks(invalid), fixed = TRUE))
  expect_length(batch$results$Participant_2$issues, 0L)
})

test_that("course package loading requires one p_load call or namespace-qualified functions", {
  ids <- function(code) vapply(check_code_backend(feedback_fixture(code),
    reprex = FALSE)$issues, `[[`, character(1), "id")
  expect_false("course.package_loading" %in% ids("pacman::p_load(sf, raster)"))
  expect_false("course.package_loading" %in% ids("x <- base::sum(1, 2)"))
  expect_true("course.package_loading" %in% ids("library(raster)"))
  mixed <- check_code_backend(feedback_fixture(c(
    "pacman::p_load(sf)", "library(raster)")), reprex = FALSE)
  loading <- Filter(function(i) i$id == "course.package_loading", mixed$issues)
  expect_length(loading, 1L)
  expect_identical(loading[[1]]$line, 2L)
  expect_false("library_call_linter" %in% mixed$lints$linter)
  expect_true("course.package_loading" %in% ids(c(
    "pacman::p_load(sf)", "pacman::p_load(raster)")))
  expect_false("course.package_loading" %in% ids(c(
    "# library(raster)", 'x <- "library(raster)"')))
})
