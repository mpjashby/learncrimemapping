test_that("create_dirs creates the complete workspace and refuses repeats", {
  folder <- tempfile()
  dir.create(folder)
  withr::defer(unlink(folder, recursive = TRUE))
  workspace <- file.path(folder, "crime_mapping")
  dir.create(workspace)
  withr::local_dir(workspace)
  expect_message(paths <- create_dirs(), "Created")
  expect_true(all(dir.exists(file.path(workspace,
    c("data", "data/raw", "data/processed", "R", "output")))))
  expect_length(paths, 5L)
  writeLines("keep", "data/raw/student.csv")
  expect_error(create_dirs(), "already exist")
  expect_identical(readLines("data/raw/student.csv"), "keep")
})

test_that("create_dirs checks the folder name and all existing paths before writing", {
  folder <- tempfile()
  dir.create(folder)
  withr::defer(unlink(folder, recursive = TRUE))
  withr::local_dir(folder)
  expect_error(create_dirs(), "must be named")
  expect_length(list.files(folder), 0L)
  workspace <- file.path(folder, "crime_mapping")
  dir.create(workspace)
  withr::local_dir(workspace)
  for (path in c("data", "R", "output")) {
    writeLines("preserve", path)
    expect_error(create_dirs(), "already exist")
    expect_identical(list.files(workspace), path)
    unlink(path)
    dir.create(path)
    expect_error(create_dirs(), "already exist")
    expect_identical(list.files(workspace), path)
    unlink(path, recursive = TRUE)
  }
})

