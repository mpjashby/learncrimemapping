# Rebind the source environment in lightweight CI and the namespace otherwise.
local_code_dialog_bindings <- function(..., .env = parent.frame()) {
  target <- environment(check_code)
  if (isNamespace(target)) {
    testthat::local_mocked_bindings(
      ...,
      .package = "learncrimemapping",
      .env = .env
    )
  } else {
    rlang::local_bindings(..., .env = target, .frame = .env)
  }
}

test_that("Positron selection preserves paths and handles dialog errors", {
  withr::local_envvar(POSITRON = "1")
  # Model a desktop OS so the test does not depend on Linux GUI tools.
  local_code_dialog_bindings(
    Sys.info = function() c(sysname = "Darwin"),
    open_check_code_dialog = function() "folder with spaces/café.qmd",
    file.choose = function() stop("Unexpected base chooser")
  )
  expect_identical(choose_check_code_file(), "folder with spaces/café.qmd")
  local_code_dialog_bindings(
    open_check_code_dialog = function() stop("Dialog unavailable")
  )
  expect_error(choose_check_code_file(), "Could not open")
})

test_that("other editors retain the existing chooser and error", {
  withr::local_envvar(POSITRON = "")
  local_code_dialog_bindings(
    file.choose = function() "script.R",
    open_check_code_dialog = function() stop("Unexpected native chooser")
  )
  expect_identical(choose_check_code_file(), "script.R")
  local_code_dialog_bindings(file.choose = function() stop("Cancelled"))
  expect_error(choose_check_code_file(), "No file was selected")
})

test_that("Linux without a dialog tool gives actionable guidance", {
  withr::local_envvar(POSITRON = "1")
  local_code_dialog_bindings(
    Sys.info = function() c(sysname = "Linux"),
    Sys.which = function(cmd) setNames(rep("", length(cmd)), cmd),
    open_check_code_dialog = function() stop("Unexpected GUI call")
  )
  expect_error(choose_check_code_file(), "Install yad or zenity")
  # Either supported executable should allow native selection.
  local_code_dialog_bindings(
    Sys.which = function(cmd) c(yad = "", zenity = "/usr/bin/zenity"),
    open_check_code_dialog = function() "script.R"
  )
  expect_identical(choose_check_code_file(), "script.R")
})

test_that("cancellation returns invisible NULL before checking", {
  local_code_dialog_bindings(interactive = function() TRUE)
  # Support both cancellation representations described upstream.
  for (cancelled in list(character(), "")) {
    local_code_dialog_bindings(choose_check_code_file = function() cancelled)
    result <- withVisible(check_code())
    expect_null(result$value)
    expect_false(result$visible)
  }
})

test_that("explicit paths and noninteractive calls bypass the chooser", {
  local_code_dialog_bindings(
    interactive = function() FALSE,
    choose_check_code_file = function() stop("Unexpected chooser")
  )
  expect_error(check_code(), "noninteractively")
  # A missing explicit path must reach file validation, not open a dialog.
  expect_error(check_code(tempfile(fileext = ".R")), "readable regular file")
})
