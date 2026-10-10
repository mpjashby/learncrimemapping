test_that("Docker build context contains only bundled checker resources", {
  resources <- tempfile("resources-")
  context <- tempfile("context-")
  dir.create(resources); dir.create(context)
  on.exit(unlink(c(resources, context), recursive = TRUE))
  dir.create(file.path(resources, "R"))
  files <- c("Dockerfile", "install-packages.R", "runner.R",
             "R/check_code.R", "R/code_feedback.R", "R/spatial_feedback.R")
  for (file in files) writeLines(file, file.path(resources, file))
  writeLines("private", file.path(resources, "private-records.txt"))
  prepare_checker_image_context(resources, context)
  expect_setequal(list.files(context, recursive = TRUE),
                  c(paste0("inst/container/", files[1:3]), files[4:6]))
  expect_identical(readLines(file.path(context, "R/check_code.R")), "R/check_code.R")
})

test_that("incomplete installed Docker resources fail clearly", {
  context <- tempfile("context-")
  dir.create(context)
  on.exit(unlink(context, recursive = TRUE))
  expect_error(prepare_checker_image_context("", context), "Reinstall")
})

test_that("invalid build arguments are rejected before running Docker", {
  expect_error(build_checker_image(image = "--bad"), "image")
  expect_error(build_checker_image(platform = "linux/unknown"), "platform")
  expect_error(build_checker_image(packages = "a,b"), "package names")
  expect_error(build_checker_image(packages = character(), system_packages = "--bad"),
               "Debian package names")
})
