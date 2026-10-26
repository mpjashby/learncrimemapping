test_that("submission workspaces create the course defaults", {
  workspace <- tempfile()
  dir.create(workspace)
  prepare_submission_workspace(workspace, NULL)
  expect_true(all(dir.exists(file.path(
    workspace,
    c("data/raw", "data/processed", "output")
  ))))
  expect_false(dir.exists(file.path(workspace, "outputs")))
  expect_false(dir.exists(file.path(workspace, "scripts")))
})

test_that("workspace directories can replace or disable defaults", {
  for (directories in list(c("figures/maps", "tables"), character())) {
    workspace <- tempfile()
    dir.create(workspace)
    prepare_submission_workspace(workspace, NULL, directories)
    expect_true(all(dir.exists(file.path(workspace, directories))))
    expect_false(dir.exists(file.path(workspace, "output")))
    expect_true(file.exists(file.path(workspace, ".here")))
  }
})

test_that("template directories and contents survive preparation", {
  template <- tempfile()
  dir.create(file.path(template, "output"), recursive = TRUE)
  writeLines("resource", file.path(template, "output", "resource.txt"))
  workspace <- tempfile()
  dir.create(workspace)
  prepare_submission_workspace(workspace, template)
  expect_identical(
    readLines(file.path(workspace, "output", "resource.txt")),
    "resource"
  )
  prepare_submission_workspace(workspace, NULL)
  expect_true(dir.exists(file.path(workspace, "output")))
})

test_that("workspace directory validation rejects unsafe settings", {
  for (directories in list(
    NULL,
    1,
    NA_character_,
    "",
    "/tmp/map",
    "C:/map",
    "../map",
    "output/../map",
    "output\\map",
    ".sandbox",
    "checker/maps",
    "rosm.cache/maps",
    "output/.hidden",
    "a\nb"
  )) {
    expect_error(
      validate_submission_workspace_dirs(directories),
      "workspace_dirs"
    )
  }
  expect_identical(validate_submission_workspace_dirs(character()), character())
})

test_that("files blocking a requested directory fail workspace setup", {
  workspace <- tempfile()
  dir.create(workspace)
  writeLines("keep", file.path(workspace, "output"))
  expect_error(
    prepare_submission_workspace(workspace, NULL),
    "Could not create workspace directory: output"
  )
  expect_identical(readLines(file.path(workspace, "output")), "keep")
})

test_that("batch checking passes custom workspace directories to execution", {
  zip <- feedback_zip(list(
    "Participant_123_assignsubmission_file/map.R" = 'png("figures/maps/map.png"); plot(1:3); dev.off()'
  ))
  batch <- suppressWarnings(check_submissions(
    zip,
    tempfile(),
    backend = "local",
    style = FALSE,
    workspace_dirs = "figures/maps",
    saved_files = "figures/maps/*.png"
  ))
  expect_identical(batch$results[[1]]$execution$status, "success")
  expect_identical(batch$results[[1]]$saved_files[[1]]$status, "included")
})
