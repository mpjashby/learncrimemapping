test_that("file-name checks allow portable snake case and the extension separator", {
  for (name in c("exercise_01.R", "exercise_01.r", "week_10_submission.qmd",
                 "01_exercise.R", "exercise.R", "exercise_01.QMD")) {
    expect_null(code_file_name_issue(file.path("Directory With Spaces", name)), info = name)
  }
  for (name in c("Exercise_01.R", "exercise 01.R", "exercise.01.R",
                 "exercise-01.qmd", "exercise_01.R(1).R", "_exercise.R",
                 "exercise_.R", "exercise__01.R", "éxercise.R")) {
    issue <- code_file_name_issue(name)
    expect_identical(issue$id, "course.file_name", info = name)
    expect_identical(issue$category, "style")
    expect_identical(issue$evidence, name)
    expect_match(issue$action, "period before the", fixed = TRUE)
  }
})

test_that("file-name issues are shared, survive syntax errors and retain original names", {
  directory <- tempfile("submission-")
  dir.create(directory)
  for (name in c("exercise.01.R", "exercise.01.qmd")) {
    file <- file.path(directory, name)
    writeLines(if (tools::file_ext(file) == "R") ")" else c("```{r}", ")", "```"), file)
    result <- check_code_backend(file, reprex = FALSE)
    expect_true("course.file_name" %in% vapply(result$issues, `[[`, character(1), "id"))
    expect_identical(result$issues[[1]]$id, "syntax.error")
    expect_identical(basename(result$file), name)
    expect_true(file.exists(file))
  }
  zip <- feedback_zip(list("Participant_123_assignsubmission_file/Exercise 01.R" = "x <- 1"))
  batch <- check_submissions(zip, tempfile(), backend = "local", reprex = FALSE)
  issue <- Filter(function(i) i$id == "course.file_name", batch$results[[1]]$issues)
  expect_length(issue, 1L)
  expect_identical(issue[[1]]$evidence, "Exercise 01.R")
})
