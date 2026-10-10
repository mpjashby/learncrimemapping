marking_ids <- function(lines, extension = ".R") {
  vapply(marking_style_issues(feedback_fixture(lines, extension)),
         function(x) x$id, character(1))
}

chain_code <- c('robbery1 <- read_csv("data.csv")',
                'robbery2 <- select(robbery1, date_time)',
                'robbery3 <- filter(robbery2, date_time > 0)')

test_that("intermediate suggestions target complete single-use chains", {
  findings <- marking_style_issues(feedback_fixture(chain_code))
  expect_length(findings, 1L)
  expect_identical(findings[[1L]]$id, "course.intermediate_objects")
  expect_identical(findings[[1L]]$line, 1L)
  expect_match(findings[[1L]]$action, "Keep separate objects")
  expect_length(marking_ids(chain_code[1:2]), 0L)
  expect_length(marking_ids(c(chain_code, 'print(robbery1)')), 0L)
  expect_length(marking_ids(c(chain_code, 'get("robbery1")')), 0L)
  expect_length(marking_ids(paste0('f <- function() {', paste(chain_code, collapse = ';'), '}')), 0L)
  expect_length(marking_ids(c('settings <- list(a = 1)', 'plot <- draw(settings)')), 0L)
  expect_length(marking_ids(c(chain_code, 'broken <- (')), 0L)
  expect_identical(marking_ids(sub('select', 'dplyr::select', chain_code)),
                   "course.intermediate_objects")
})

spacing_code <- c('x <- read_csv(', '  "data.csv"', ')',
                  '# Plot the data', 'plot(x)')

test_that("section spacing uses top-level comment boundaries", {
  findings <- marking_style_issues(feedback_fixture(spacing_code))
  expect_length(findings, 1L)
  expect_identical(findings[[1L]]$id, "course.section_spacing")
  expect_identical(findings[[1L]]$line, 4L)
  expect_length(marking_ids(append(spacing_code, '', after = 3)), 0L)
  expect_length(marking_ids(c('x <- 1', '# Related setting', 'y <- 2')), 0L)
  expect_length(marking_ids(c('x <- read_csv(', '# File to read', '"data.csv")')), 0L)
  expect_length(marking_ids(c('f <- function() {', spacing_code, '}')), 0L)
  expect_length(marking_ids(sub('# Plot the data', '#| echo: false', spacing_code)), 0L)
})

test_that("document extraction preserves locations and skips disabled chunks", {
  code <- c('---', 'title: Example', '---', '```{r}', chain_code, '```',
            '```{r, eval=FALSE}', spacing_code, '```')
  findings <- marking_style_issues(feedback_fixture(code, '.Rmd'))
  expect_length(findings, 1L)
  expect_identical(findings[[1L]]$line, 5L)
})

test_that("staff batches add suggestions while student checks do not", {
  file <- feedback_fixture(c(chain_code, '', spacing_code))
  student <- suppressMessages(check_code(file, reprex = FALSE))
  expect_false(any(vapply(student$issues, function(x)
    x$id %in% c('course.intermediate_objects', 'course.section_spacing'), logical(1))))
  zip <- feedback_zip(list('Participant_123_assignsubmission_file/code.R' =
                            c(chain_code, '', spacing_code)))
  staff <- check_submissions(zip, tempfile(), reprex = FALSE, backend = 'local')
  result <- staff$results[[1L]]
  ids <- vapply(result$issues, function(x) x$id, character(1))
  expect_true(all(c('course.intermediate_objects', 'course.section_spacing') %in% ids))
  expect_match(paste(readLines(result$html_report), collapse = '\n'), 'course.intermediate_objects')
  expect_match(paste(readLines(result$report), collapse = '\n'), 'course.section_spacing')
  disabled <- check_submissions(zip, tempfile(), reprex = FALSE, style = FALSE, backend = 'local')
  expect_length(disabled$results[[1L]]$issues, 0L)
})
