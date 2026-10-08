test_that("prose comment spacing and case exclude strings and technical markers", {
  file <- feedback_fixture(c(
    "#load data", "# Load Data From The File", "# LOAD DATA", "# Load the data",
    "# and convert its coordinates.", 'x <- "#not a comment"',
    "#| echo: false", "#' roxygen documentation", "#!/usr/bin/Rscript",
    "# nolint start", "# st_as_sf() preserves identifiers", "# EPSG and CRS identifiers",
    "# New York City", "# ----", "#  Extra space", "x <- 1 # inspect data", "# Use my_Data column", "# Use `Data` as an identifier"))
  issues <- code_comment_issues(file)
  lines <- function(id) vapply(Filter(function(x) x$id == id, issues), `[[`, integer(1), "line")
  expect_equal(lines("course.comment_spacing"), c(1L, 15L))
  expect_equal(lines("course.comment_sentence_case"), c(1L, 2L, 3L, 16L))
  qmd <- feedback_fixture(c("# lowercase prose heading", "```{r}", "#load data", "```",
                            "```{r}", "#| eval: false", "#bad disabled comment", "```"), ".qmd")
  expect_equal(vapply(code_comment_issues(qmd), `[[`, integer(1), "line"), c(3L, 3L))
  broken <- feedback_fixture(c("#load data", ")"))
  expect_length(code_comment_issues(broken), 2L)
})

test_that("object naming notes list distinct names and collapse with all locations", {
  result <- check_code_backend(feedback_fixture(c("badName <- 1", "other.name <- 2",
    "badName <- 3", "`bad-name` <- 4")), reprex = FALSE)
  lints <- result$lints[result$lints$linter == "object_name_linter", ]
  expect_length(unique(lints$message), 1L)
  expect_match(lints$message[1], "badName, other.name, bad-name", fixed = TRUE)
  expect_false(grepl("and function|or symbols", lints$message[1]))
  target <- tempfile(fileext = ".html")
  write_code_feedback(result, target)
  html <- paste(readLines(target), collapse = "\n")
  expect_equal(lengths(regmatches(html, gregexpr("Rule: lint.object_name_linter", html, fixed = TRUE))), 1L)
  for (line in 1:4) expect_match(html, paste0("Line ", line, ", column 1"), fixed = TRUE)
})

test_that("indentation variants collapse while retaining their detailed diagnoses", {
  result <- check_code_backend(feedback_fixture("x <- 1"), reprex = FALSE)
  result$lints <- data.frame(linter = "indentation_linter", category = "style",
    message = c("Indentation should be 2 spaces, not 4 spaces.", "Indentation should be 2 spaces, not 6 spaces."),
    line_number = c(2L, 4L), column_number = 1L, line = c("    x", "      y"))
  result$issues <- interpret_code_feedback(result)
  target <- tempfile(fileext = ".html")
  write_code_feedback(result, target)
  html <- paste(readLines(target), collapse = "\n")
  expect_equal(lengths(regmatches(html, gregexpr("Rule: lint.indentation_linter", html, fixed = TRUE))), 1L)
  expect_match(html, "Line 2, column 1; Line 4, column 1", fixed = TRUE)
  expect_match(html, "not 4 spaces", fixed = TRUE)
  expect_match(html, "not 6 spaces", fixed = TRUE)
})
