make_submission <- function(code, extension = ".R", directory = tempdir(),
                            name = NULL) {
  file <- if (is.null(name)) tempfile(fileext = extension, tmpdir = directory) else
    file.path(directory, paste0(name, extension))
  writeLines(code, file)
  file
}

quiet_check <- function(...) {
  result <- NULL
  suppressMessages(capture.output(result <- check_code(...)))
  result
}

event_text <- function(result, kind) {
  events <- Filter(function(x) x$kind == kind, result$execution$events)
  vapply(events, function(x) x$text, character(1))
}

test_that("execution captures warnings, preserves case, and stops at the first error", {
  file <- make_submission(c(
    'warning("One")', 'warning("Two")', 'warning("Three")', 'warning("Four")',
    'stop("MyObject failed")', 'cat("SHOULD NOT RUN")'
  ))
  result <- quiet_check(file, style = FALSE)
  expect_identical(result$execution$status, "error")
  expect_identical(event_text(result, "warning"), c("One", "Two", "Three", "Four"))
  expect_identical(event_text(result, "error"), "MyObject failed")
  expect_length(event_text(result, "output"), 0)
  expect_true(file.exists(result$report))
  report <- readLines(result$report)
  expect_true(any(grepl("Four", report, fixed = TRUE)))
  expect_false(any(grepl("SHOULD NOT RUN", report, fixed = TRUE)))
  # All warnings remain in results, while the console shows only three.
  console <- capture.output(check_code(file, style = FALSE), type = "message")
  expect_true(any(grepl("Three", console, fixed = TRUE)))
  expect_false(any(grepl("Line 4: Four", console, fixed = TRUE)))
})

test_that("printed error-like text is ordinary output", {
  result <- quiet_check(make_submission('cat("#> Error: pretend\\n")'), style = FALSE)
  expect_identical(result$execution$status, "success")
  expect_length(event_text(result, "error"), 0)
  expect_match(event_text(result, "output"), "pretend")
})

test_that("execution uses a separate session and does not change the caller directory", {
  original <- getwd()
  result <- quiet_check(make_submission(c('isolated <- 1', 'setwd(tempdir())', '1 + 1')),
                        style = FALSE)
  expect_identical(result$execution$status, "success")
  expect_identical(getwd(), original)
  expect_false(exists("isolated", envir = .GlobalEnv, inherits = FALSE))
})

test_that("project discovery follows the submitted file and supports overrides", {
  root <- tempfile("course-project-")
  dir.create(root)
  dir.create(file.path(root, "scripts"))
  writeLines("Version: 1.0", file.path(root, "course.Rproj"))
  writeLines("42", file.path(root, "data.txt"))
  file <- make_submission('cat(readLines("data.txt"))', directory = file.path(root, "scripts"))
  result <- quiet_check(file, style = FALSE)
  expect_identical(result$execution_dir, normalizePath(root, winslash = "/"))
  expect_identical(result$execution$status, "success")
  expect_match(event_text(result, "output"), "42")
  standalone <- tempfile("standalone-")
  dir.create(standalone)
  file <- make_submission('1 + 1', directory = standalone)
  expect_identical(quiet_check(file, style = FALSE)$execution_dir,
                   normalizePath(standalone, winslash = "/"))
  expect_identical(quiet_check(file, style = FALSE, execution_dir = root)$execution_dir,
                   normalizePath(root, winslash = "/"))
  unlink(file.path(root, "course.Rproj"))
  writeLines("project:\n  type: default", file.path(root, "_quarto.yml"))
  expect_identical(check_execution_directory(file.path(root, "scripts", "x.R"), NULL),
                   normalizePath(root, winslash = "/"))
})

test_that("disabled document chunks are excluded and source locations are preserved", {
  for (extension in c(".Rmd", ".qmd")) {
    directory <- tempfile("document-")
    dir.create(directory)
    companion <- file.path(directory, "submission.R")
    writeLines("KEEP THIS FILE", companion)
    file <- make_submission(c(
      "Prose", "```{r, eval=FALSE}", 'stop("disabled")', "```",
      "```{r}", "#| eval: false", 'stop("also disabled")', "```",
      "```{r}", 'warning("Enabled")', "x=1", "```"
    ), extension = extension, directory = directory, name = "submission")
    result <- quiet_check(file)
    expect_identical(result$execution$status, "success")
    expect_identical(event_text(result, "warning"), "Enabled")
    warning_event <- Filter(function(x) x$kind == "warning", result$execution$events)[[1]]
    expect_identical(warning_event$line, 10L)
    expect_true(11L %in% result$lints$line_number)
    expect_false(any(result$lints$line_number %in% c(3L, 7L)))
    expect_identical(readLines(companion), "KEEP THIS FILE")
  }
})

test_that("timeouts preserve output and do not prevent static checks", {
  file <- make_submission(c('cat("BEFORE\\n")', "Sys.sleep(30)", 'cat("AFTER")'))
  result <- quiet_check(file, timeout = 5)
  expect_identical(result$execution$status, "timeout")
  expect_true(any(grepl("BEFORE", event_text(result, "output"), fixed = TRUE)))
  expect_false(any(grepl("AFTER", event_text(result, "output"), fixed = TRUE)))
  expect_null(result$failures$style)
  expect_true(file.exists(result$report))
})

test_that("static checking distinguishes syntax, environment, and style findings", {
  result <- quiet_check(make_submission("x <- ("), reprex = FALSE)
  expect_true("error" %in% result$lints$type)
  expect_true("correctness" %in% result$lints$category)
  result <- quiet_check(make_submission(c("x=1", "noSuchCoursePackageXYZ::foo()")), reprex = FALSE)
  expect_true("environment" %in% result$lints$category)
  expect_true("style" %in% result$lints$category)
  expect_identical(result$execution$status, "skipped")
})

test_that("local configuration does not suppress course checks", {
  directory <- tempfile("local-lintr-")
  dir.create(directory)
  writeLines('linters: list()\nexclusions: list("submission.R")', file.path(directory, ".lintr"))
  result <- quiet_check(make_submission("x=1", directory = directory, name = "submission"),
                        reprex = FALSE)
  expect_true("assignment_linter" %in% result$lints$linter)
})

test_that("clean code and empty submissions have a stable empty lint schema", {
  for (code in list("x <- 1", character())) {
    result <- quiet_check(make_submission(code))
    expect_identical(result$execution$status, "success")
    expect_equal(nrow(result$lints), 0)
    expect_true(all(c("type", "linter", "category") %in% names(result$lints)))
  }
})

test_that("arguments and noninteractive file selection fail clearly", {
  file <- make_submission("1")
  expect_error(check_code(file, reprex = NA), "TRUE or FALSE")
  expect_error(check_code(file, style = NA), "TRUE or FALSE")
  expect_error(check_code(NA_character_), "nonempty")
  expect_error(check_code(character()), "nonempty")
  expect_error(check_code(file, timeout = Inf), "finite")
  expect_error(check_code(file, timeout = 0), "positive")
  expect_error(check_code(tempdir()), "regular file")
  expect_error(check_code(file, execution_dir = tempfile()), "does not exist")
  if (!interactive()) expect_error(check_code(), "noninteractively")
})

test_that("disabled checks return without extraction or report generation", {
  file <- make_submission("not even valid code", extension = ".qmd")
  expect_warning(result <- quiet_check(file, reprex = FALSE, style = FALSE), "disabled")
  expect_identical(result$execution$status, "skipped")
  expect_null(result$report)
  expect_length(result$failures, 0)
})

test_that("filenames and condition messages containing braces are safe", {
  directory <- tempfile("braces-")
  dir.create(directory)
  file <- make_submission('warning("Object {MyCase} matters")',
                          directory = directory, name = "file{a}")
  expect_no_error(result <- quiet_check(file, style = FALSE))
  expect_identical(event_text(result, "warning"), "Object {MyCase} matters")
})

test_that("plots are retained beside the report", {
  result <- quiet_check(make_submission("plot(1:3)"), style = FALSE)
  expect_identical(result$execution$status, "success")
  plots <- Filter(function(x) x$kind == "plot", result$execution$events)
  expect_length(plots, 1)
  expect_true(file.exists(plots[[1]]$plot))
  expect_gt(file.info(plots[[1]]$plot)$size, 0)
})

test_that("checker failures do not masquerade as student errors", {
  # An abruptly terminated child session is a runner failure, not a code condition.
  result <- quiet_check(make_submission(c('quit(save = "no", status = 42)', "x=1")))
  expect_identical(result$execution$status, "failed")
  expect_type(result$failures$execution, "character")
  expect_true("assignment_linter" %in% result$lints$linter)
  expect_length(event_text(result, "error"), 0)
})


test_that("complexity checking remains part of the course checks", {
  file <- make_submission(c(
    "complex <- function(x) {",
    rep("  if (x > 0) x <- x - 1", 16),
    "  x", "}"
  ))
  result <- quiet_check(file, reprex = FALSE)
  expect_true("cyclocomp_linter" %in% result$lints$linter)
})
