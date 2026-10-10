# The CI workflow also sources these functions without installing the large
# course metapackage. In that mode rebind their source environment directly;
# installed-package tests use testthat to handle locked namespace bindings.
local_workspace_bindings <- function(..., .package = "learncrimemapping",
                                     .env = parent.frame()) {
  target <- environment(check_workspace)
  if (.package != "learncrimemapping" || isNamespace(target)) {
    testthat::local_mocked_bindings(..., .package = .package, .env = .env)
  } else {
    rlang::local_bindings(..., .env = target, .frame = .env)
  }
}

# All editor, tool, package, network and secret state used by the report is
# controlled here. Fixtures never read a real student's (or developer's) key.
workspace_fixture <- function(name = "crime_mapping") {
  parent <- tempfile("workspace with spaces-quoted-'é-")
  dir.create(parent)
  folder <- file.path(parent, name)
  dir.create(folder)
  for (path in c("data/raw", "data/processed", "output", "R", ".vscode")) {
    dir.create(file.path(folder, path), recursive = TRUE)
  }
  file.create(file.path(folder, "air.toml"))
  writeLines(c('{ // workspace settings', '"[r]": {',
               '"editor.formatOnSave": true, /* save */',
               '"editor.defaultFormatter": "Posit.air-vscode",', '},', '}'),
             file.path(folder, ".vscode", "settings.json"))
  folder
}
workspace_test_desc <- function() list(
  Depends = "R (>= 4.1.0)",
  Imports = "cli, here, usethis (>= 3.2.0), examplegithub (>= 1.0.0)",
  Remotes = "example/examplegithub"
)
local_workspace_mocks <- function(folder, ..., .env = parent.frame()) {
  local_workspace_bindings(
    workspace_description = workspace_test_desc,
    workspace_os = function() "Darwin",
    workspace_editor = function() list(detected = TRUE, workspace = folder, version = NULL),
    workspace_package_version = function(package) "99.0.0",
    workspace_here = function() folder,
    workspace_key_present = function() TRUE,
    workspace_saved_key = function(start_folder) list(state = "present", path = file.path(folder, ".Renviron"), source = "user"),
    workspace_latest_r = function() as.character(getRversion()),
    workspace_rtools = function() stop("Tools should not be probed on a Mac"),
    .package = "learncrimemapping", .env = .env
  )
  if (length(list(...))) {
    local_workspace_bindings(..., .package = "learncrimemapping", .env = .env)
  }
}
workspace_report <- function(...) {
  visible <- NULL
  output <- capture.output(visible <- withVisible(check_workspace(...)), type = "message")
  list(result = visible$value, visible = visible$visible, output = paste(output, collapse = "\n"))
}
check_status <- function(report, id) report$result$checks[[id]]$status

test_that("a correct Positron workspace needs no Rproj and returns invisible stable results", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder)
  report <- workspace_report()
  expect_false(report$visible)
  expect_true(report$result$workspace_confident)
  expect_identical(report$result$workspace_source, "positron")
  expect_identical(report$result$counts[["problem"]], 0L)
  expect_identical(check_status(report, "workspace.here"), "passed")
  for (id in c("workspace.active", "workspace.working_directory", "air.file", "air.settings", "air.format_on_save", "air.formatter", "carto.session", "carto.saved")) {
    expect_identical(check_status(report, id), "passed")
  }
  expect_false("air.behaviour" %in% names(report$result$checks))
  expect_identical(check_status(report, "rtools"), "not_applicable")
  expect_false(any(grepl("Rproj$", list.files(folder, all.files = TRUE))))
  expect_match(report$output, "PASS:.*here::here")
  expect_match(report$output, "checks requiring manual verification")
  expect_false(grepl("everything is", report$output, ignore.case = TRUE))
  expect_identical(names(report$result$checks), unname(vapply(report$result$checks, `[[`, character(1), "id")))
})

test_that("missing directories get safely quoted code; blocking files are preserved", {
  folder <- workspace_fixture()
  unlink(file.path(folder, "output"), recursive = TRUE)
  unlink(file.path(folder, "data/raw"), recursive = TRUE)
  writeLines("keep me", file.path(folder, "data/raw"))
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "directory.output"), "problem")
  action <- report$result$checks[["directory.output"]]$actions[[2]]
  expect_identical(eval(parse(text = action)[[1]][[2]]), file.path(workspace_path(folder), "output"))
  expect_match(report$output, "Do not overwrite or delete")
  expect_identical(readLines(file.path(folder, "data/raw")), "keep me")
  expect_identical(check_status(report, "directory.data/raw"), "problem")
  expect_identical(check_status(report, "carto.session"), "passed")
})

test_that("parent failures prevent dependent checks", {
  folder <- workspace_fixture()
  unlink(file.path(folder, "data"), recursive = TRUE)
  writeLines("keep me", file.path(folder, "data"))
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "directory.data"), "problem")
  expect_identical(check_status(report, "directory.data/raw"), "not_checked")
  expect_identical(check_status(report, "directory.data/processed"), "not_checked")
  expect_identical(check_status(report, "directory.output"), "passed")
})

test_that("explicit, active, working and remembered here folders are independent", {
  folder <- workspace_fixture()
  wrong <- workspace_fixture("other_course")
  local_workspace_mocks(folder,
    workspace_editor = function() list(detected = TRUE, workspace = wrong, version = NULL),
    workspace_here = function() wrong)
  withr::local_dir(wrong)
  report <- workspace_report(folder)
  expect_identical(report$result$workspace_source, "supplied")
  for (id in c("workspace.active", "workspace.working_directory", "workspace.here")) expect_identical(check_status(report, id), "problem")
  expect_match(report$output, "here\\(\\) remembers")
  expect_match(report$output, "File > Open Folder")
  expect_false(grepl("setwd\\(", report$output))
  expect_identical(check_status(workspace_report(), "workspace.name"), "problem")
})

test_that("unconfident working-directory fallback never suggests directory creation", {
  folder <- workspace_fixture("unrelated")
  unlink(file.path(folder, "output"), recursive = TRUE)
  withr::local_dir(folder)
  local_workspace_mocks(folder,
    workspace_editor = function() list(detected = FALSE, workspace = NULL, version = NULL))
  report <- workspace_report()
  expect_false(report$result$workspace_confident)
  expect_identical(check_status(report, "workspace.folder"), "manual")
  expect_identical(check_status(report, "workspace.active"), "manual")
  expect_false(grepl("dir.create", report$output, fixed = TRUE))
  expect_match(gsub("[[:space:]]+", " ", report$output), "does not mean it is not installed")
})

test_that("here and vscode markers establish fallback without an Rproj", {
  for (marker in c(".here", ".vscode/settings.json")) {
    folder <- workspace_fixture()
    if (marker == ".here") {
      unlink(file.path(folder, ".vscode"), recursive = TRUE)
      file.create(file.path(folder, ".here"))
    }
    local_workspace_mocks(folder,
      workspace_editor = function() list(detected = FALSE, workspace = NULL, version = NULL))
    withr::with_dir(folder, {
      report <- workspace_report()
      expect_true(report$result$workspace_confident)
      expect_identical(check_status(report, "workspace.active"), "manual")
    })
  }
})

test_that("capitalisation is checked by directory entries even on insensitive disks", {
  folder <- workspace_fixture()
  expect_true(file.rename(file.path(folder, "data"), file.path(folder, "temporary_data")))
  expect_true(file.rename(file.path(folder, "temporary_data"), file.path(folder, "Data")))
  expect_true(file.rename(file.path(folder, "R"), file.path(folder, "temporary_scripts")))
  expect_true(file.rename(file.path(folder, "temporary_scripts"), file.path(folder, "r")))
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_identical(report$result$checks[["directory.data"]]$details$kind, "case")
  expect_identical(report$result$checks[["directory.R"]]$details$kind, "case")
  expect_identical(check_status(report, "directory.data/raw"), "not_checked")
})

test_that("all authoritative dependencies and version constraints are retained", {
  desc <- workspace_test_desc()
  requirements <- workspace_requirements(desc)
  expect_identical(requirements$name, c("R", "cli", "here", "usethis", "examplegithub"))
  expect_true(workspace_version_ok("3.2.0", ">=", "3.2.0"))
  expect_false(workspace_version_ok("3.1.0", ">=", "3.2.0"))
  folder <- workspace_fixture()
  local_workspace_mocks(folder,
    workspace_package_version = function(package) {
      if (package == "here") stop("not installed")
      if (package == "usethis") return("3.1.0")
      "99.0.0"
    })
  report <- workspace_report(folder)
  expect_identical(check_status(report, "package.here"), "problem")
  expect_identical(check_status(report, "package.usethis"), "problem")
  expect_identical(check_status(report, "package.cli"), "passed")
  expect_match(report$output, 'install.packages\\("here"\\)')
  github_action <- workspace_install_action("examplegithub", desc)
  expect_true(any(grepl('remotes::install_github("example/examplegithub")', github_action, fixed = TRUE)))
})

test_that("failed default online lookup warns and later checks continue", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder, workspace_latest_r = function() stop("connection error"))
  expect_warning(report <- workspace_report(folder), "online lookup failed or timed out",
                 class = "learncrimemapping_update_warning")
  expect_identical(check_status(report, "r.current"), "not_checked")
  expect_match(report$output, "not a setup failure")
  expect_identical(check_status(report, "directory.R"), "passed")
  expect_identical(check_status(report, "carto.session"), "passed")
})

test_that("bounded release lookup handles current and newer releases", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder, workspace_latest_r = function() "99.0.0")
  expect_identical(check_status(workspace_report(folder, check_updates = TRUE), "r.current"), "problem")
  local_workspace_bindings(workspace_latest_r = function() "1.0.0", .package = "learncrimemapping")
  expect_identical(check_status(workspace_report(folder, check_updates = TRUE), "r.current"), "passed")
})

test_that("Windows tools checks distinguish missing, detected and unavailable tools", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder, workspace_os = function() "Windows", workspace_rtools = function() FALSE)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "rtools"), "problem")
  expect_false("windows.redistributable" %in% names(report$result$checks))
  expect_false(grepl("Visual C++", report$output, fixed = TRUE))
  expect_match(report$output, "matching your running R version")
  local_workspace_bindings(workspace_rtools = function() TRUE, .package = "learncrimemapping")
  expect_identical(check_status(workspace_report(folder), "rtools"), "passed")
  local_workspace_bindings(workspace_rtools = function() stop("unavailable"), .package = "learncrimemapping")
  expect_identical(check_status(workspace_report(folder), "rtools"), "manual")
})

test_that("Air accepts empty toml and JSONC without interpreting string contents", {
  folder <- workspace_fixture()
  file <- file.path(folder, ".vscode", "settings.json")
  settings <- workspace_read_settings(file)
  expect_identical(settings[["[r]"]][["editor.formatOnSave"]], TRUE)
  writeLines('{"example": "https://example.org/a,}", "array": [1, 2,],}', file)
  settings <- workspace_read_settings(file)
  expect_identical(settings$example, "https://example.org/a,}")
  expect_equal(unlist(settings$array), c(1, 2))
  writeLines('{}', file)
  expect_length(workspace_read_settings(file), 0L)
  writeLines('{/* unclosed', file)
  expect_error(workspace_read_settings(file), "Unclosed comment")
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "air.file"), "passed")
  expect_identical(check_status(report, "air.settings"), "manual")
})

test_that("incorrect R-specific Air settings are problems but inheritance is manual", {
  folder <- workspace_fixture()
  file <- file.path(folder, ".vscode", "settings.json")
  writeLines('{"editor.formatOnSave": true, "[r]": {"editor.formatOnSave": false, "editor.defaultFormatter": "other"}}', file)
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "air.format_on_save"), "problem")
  expect_identical(check_status(report, "air.formatter"), "problem")
  expect_match(report$output, "Air: Initialize Workspace Folder")
  writeLines('{"editor.formatOnSave": false, "editor.defaultFormatter": "other"}', file)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "air.format_on_save"), "manual")
  expect_identical(check_status(report, "air.formatter"), "manual")
  writeLines('{"[r][quarto]": {"editor.formatOnSave": false}, "[r]": {"editor.formatOnSave": true}}', file)
  expect_identical(check_status(workspace_report(folder), "air.format_on_save"), "manual")
})

test_that("missing Air files, unobservable settings and missing workspace are explicit", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder)
  unlink(file.path(folder, "air.toml"))
  unlink(file.path(folder, ".vscode"), recursive = TRUE)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "air.file"), "problem")
  expect_identical(check_status(report, "air.settings"), "manual")
  report <- workspace_report(file.path(folder, "missing"))
  expect_identical(check_status(report, "workspace.folder"), "problem")
  expect_identical(check_status(report, "air.file"), "not_checked")
  expect_identical(check_status(report, "air.settings"), "not_checked")
})

test_that("CARTO session and saved presence have distinct statuses", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder, workspace_key_present = function() FALSE,
    workspace_saved_key = function(start_folder) list(state = "absent", path = file.path(folder, ".Renviron"), source = "user"))
  report <- workspace_report(folder)
  expect_identical(check_status(report, "carto.session"), "problem")
  expect_identical(check_status(report, "carto.saved"), "problem")
  expect_match(report$output, 'edit_r_environ\\(scope = "user"\\)')
  local_workspace_bindings(workspace_key_present = function() TRUE, .package = "learncrimemapping")
  expect_match(workspace_report(folder)$output, "may not survive an R restart")
  local_workspace_bindings(workspace_key_present = function() FALSE,
    workspace_saved_key = function(start_folder) list(state = "present", path = file.path(folder, ".Renviron"), source = "workspace"), .package = "learncrimemapping")
  report <- workspace_report(folder)
  expect_identical(check_status(report, "carto.saved"), "problem")
  expect_match(report$output, "restart may still be needed")
  expect_match(report$output, "instead of the user .Renviron")
})

test_that("actual CARTO file inspection obeys startup selection and redacts secrets", {
  folder <- workspace_fixture()
  home <- tempfile("fake-home-")
  dir.create(home)
  local_workspace_bindings(workspace_home = function() home, .package = "learncrimemapping")
  withr::local_envvar(c(R_ENVIRON_USER = NA, CARTO_API_KEY = NA))
  secret <- "secret-not-to-be-displayed-0123456789"
  user_file <- file.path(home, ".Renviron")
  writeLines(c('OTHER_SECRET="do-not-display-either"', paste0('CARTO_API_KEY="', secret, '"')), user_file)
  saved <- workspace_saved_key(folder)
  expect_identical(saved$state, "present")
  expect_identical(saved$source, "user")
  expect_false(grepl(secret, paste(capture.output(str(saved)), collapse = ""), fixed = TRUE))
  local_file <- file.path(folder, ".Renviron")
  writeLines('CARTO_API_KEY=""', local_file)
  expect_identical(workspace_saved_key(folder)$state, "absent")
  expect_identical(workspace_saved_key(folder)$source, "workspace")
  override <- file.path(home, "override.env")
  writeLines('CARTO_API_KEY="${OTHER_KEY}"', override)
  withr::with_envvar(c(R_ENVIRON_USER = override), {
    expect_identical(workspace_saved_key(folder)$source, "R_ENVIRON_USER")
    expect_identical(workspace_saved_key(folder)$state, "unknown")
  })
  withr::with_envvar(c(R_ENVIRON_USER = ""), expect_identical(workspace_saved_key(folder)$state, "unknown"))
  # Use the real saved-key parser and real presence check for the full report.
  saved_parser <- workspace_saved_key
  presence <- workspace_key_present
  local_workspace_mocks(folder, workspace_home = function() home,
    workspace_saved_key = saved_parser, workspace_key_present = presence)
  withr::with_envvar(c(CARTO_API_KEY = secret), {
    report <- workspace_report(folder)
    all_text <- paste(report$output, paste(capture.output(str(report$result)), collapse = "\n"))
    expect_false(grepl(secret, all_text, fixed = TRUE))
    expect_false(grepl("do-not-display-either", all_text, fixed = TRUE))
    expect_identical(check_status(report, "carto.session"), "passed")
  })
})

test_that("literal startup assignments handle empty, repeated and uncertain values", {
  folder <- workspace_fixture()
  file <- file.path(folder, ".Renviron")
  withr::local_envvar(c(R_ENVIRON_USER = file))
  for (value in c('""', "''", "   ")) {
    writeLines(paste0("CARTO_API_KEY=", value), file)
    expect_identical(workspace_saved_key(folder)$state, "absent")
  }
  writeLines(c('CARTO_API_KEY="one"', 'CARTO_API_KEY=""'), file)
  expect_identical(workspace_saved_key(folder)$state, "absent")
  writeLines('CARTO_API_KEY="unclosed', file)
  expect_identical(workspace_saved_key(folder)$state, "unknown")
})

test_that("tooling failures do not stop later checks or leak error text", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder,
    workspace_editor = function() stop("secret-editor-error"),
    workspace_here = function() stop("secret-here-error"),
    workspace_read_settings = function(path) stop("secret-settings-error"),
    workspace_saved_key = function(path) stop("secret-environ-error"))
  report <- workspace_report(folder)
  expect_identical(check_status(report, "workspace.here"), "not_checked")
  expect_identical(check_status(report, "air.settings"), "manual")
  expect_identical(check_status(report, "carto.saved"), "not_checked")
  expect_false(grepl("secret-", report$output, fixed = TRUE))
  expect_gt(report$result$counts[["passed"]], 0)
})

test_that("report leaves files, directories, working directory, environment and settings unchanged", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder)
  file.create(file.path(folder, "extra-student-file.txt"))
  files <- list.files(folder, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  env <- Sys.getenv()
  wd <- getwd()
  report <- workspace_report()
  expect_identical(getwd(), wd)
  expect_identical(Sys.getenv(), env)
  expect_identical(list.files(folder, recursive = TRUE, all.files = TRUE, full.names = TRUE), files)
  expect_identical(tools::md5sum(files), before)
  expect_identical(report$result$counts[["problem"]], 0L)
})

test_that("invalid inputs fail before any session checks", {
  expect_error(check_workspace(NA_character_), "nonempty")
  expect_error(check_workspace(character()), "nonempty")
  expect_error(check_workspace(check_updates = NA), "TRUE or FALSE")
})

test_that("release transport has a five-second bound and validates responses", {
  timeout <- NULL
  local_workspace_bindings(
    req_perform = function(req, ...) {
      timeout <<- req$options$timeout_ms
      list()
    },
    resp_body_json = function(resp, ...) list(version = "4.6.1"),
    .package = "httr2"
  )
  expect_identical(workspace_latest_r(), "4.6.1")
  expect_equal(timeout, 5000)
  local_workspace_bindings(resp_body_json = function(resp, ...) list(version = "invalid"), .package = "httr2")
  expect_error(workspace_latest_r(), "Unrecognised release")
})

test_that("Positron detection uses documented indicators and defensive API calls", {
  folder <- workspace_fixture()
  local_workspace_bindings(isAvailable = function(...) TRUE,
    hasFun = function(...) TRUE, getActiveProject = function() folder,
    .package = "rstudioapi")
  withr::with_envvar(c(POSITRON = "1"), {
    editor <- workspace_editor()
    expect_true(editor$detected)
    expect_identical(editor$workspace, folder)
    expect_null(editor$version)
  })
  withr::with_envvar(c(POSITRON = NA), {
    editor <- workspace_editor()
    expect_false(editor$detected)
    expect_null(editor$workspace)
  })
  local_workspace_bindings(hasFun = function(...) FALSE, .package = "rstudioapi")
  withr::with_envvar(c(POSITRON = "1"), expect_null(workspace_editor()$workspace))
  local_workspace_bindings(hasFun = function(...) TRUE,
    getActiveProject = function() stop("API unavailable"), .package = "rstudioapi")
  withr::with_envvar(c(POSITRON = "1"), expect_null(workspace_editor()$workspace))
})

test_that("an actual cached here folder is observed without resetting it", {
  # Load here before checking another folder, but never alter its real cache.
  # The established value must still be the same after the diagnostic.
  original <- here::here()
  folder <- workspace_fixture()
  current_here <- workspace_here
  local_workspace_mocks(folder, workspace_here = current_here)
  report <- workspace_report(folder)
  expect_identical(check_status(report, "workspace.here"), "problem")
  expect_identical(report$result$checks[["workspace.here"]]$details$actual, workspace_path(original))
  expect_identical(here::here(), original)
})

test_that("directory access flags are checked without probing writes", {
  folder <- workspace_fixture()
  # Local override makes this reliable even under privileged CI accounts.
  # Keep base access results except the write permission for output.
  real_access <- base::file.access
  local_workspace_bindings(
    workspace_access = function(names, mode = 0) {
      if (identical(names, file.path(folder, "output")) && mode == 2) return(-1L)
      real_access(names, mode)
    }, .package = "learncrimemapping")
  expect_identical(workspace_directory(folder, "output")$kind, "permission")
  expect_identical(workspace_directory(folder, "data/raw")$kind, "ok")
})


test_that("relative R_ENVIRON_USER paths are resolved from the starting folder", {
  folder <- workspace_fixture()
  file <- file.path(folder, "custom.env")
  writeLines('CARTO_API_KEY="synthetic-key"', file)
  withr::local_envvar(c(R_ENVIRON_USER = "custom.env"))
  saved <- workspace_saved_key(folder)
  expect_identical(saved$path, workspace_path(file))
  expect_identical(saved$source, "R_ENVIRON_USER")
  expect_identical(saved$state, "present")
})


test_that("repair code quotes spaces, quotes and Unicode without evaluating paths", {
  path <- 'folder with spaces/"quotes"/é'
  expect_identical(eval(parse(text = workspace_quote(path))), path)
})

test_that("online comparison is enabled by default and can be explicitly skipped", {
  folder <- workspace_fixture()
  calls <- 0L
  local_workspace_mocks(folder, workspace_latest_r = function() {
    calls <<- calls + 1L
    as.character(getRversion())
  })
  report <- workspace_report(folder)
  expect_identical(calls, 1L)
  expect_identical(check_status(report, "r.current"), "passed")
  report <- workspace_report(folder, check_updates = FALSE)
  expect_identical(calls, 1L)
  expect_identical(check_status(report, "r.current"), "not_checked")
  expect_match(report$output, "check_updates = FALSE", fixed = TRUE)
})

test_that("timeouts warn without treating a failed lookup as a setup problem", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder, workspace_latest_r = function() stop("request timed out"))
  expect_warning(report <- workspace_report(folder), "online lookup failed or timed out",
                 class = "learncrimemapping_update_warning")
  expect_identical(check_status(report, "r.current"), "not_checked")
  expect_identical(report$result$counts[["problem"]], 1L) # cwd differs from supplied folder
  expect_false("r.current" %in% names(report$result$problems))
  expect_identical(check_status(report, "carto.session"), "passed")
})

test_that("requested manual checks and student-facing CARTO caveats are omitted", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_false(any(c("positron.current", "positron.version", "system.compatibility", "air.behaviour", "carto.startup") %in% names(report$result$checks)))
  for (text in c("Check for Updates", "About Positron", "Help > About", "type x=1", "Only presence was checked",
                 "Its value is never displayed or tested", "earlier start read")) {
    expect_false(grepl(text, report$output, fixed = TRUE))
  }
  local_workspace_bindings(workspace_saved_key = function(path) stop("unreadable"))
  report <- workspace_report(folder)
  expect_identical(check_status(report, "carto.saved"), "not_checked")
  expect_length(report$result$checks[["carto.saved"]]$actions, 0L)
})

test_that("problems and repair code are repeated at the end without changing counts", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder)
  unlink(file.path(folder, "output"), recursive = TRUE)
  unlink(file.path(folder, "R"), recursive = TRUE)
  report <- workspace_report()
  problems <- report$result$problems
  expect_identical(names(problems), c("directory.output", "directory.R"))
  expect_identical(report$result$counts[["problem"]], length(problems))
  expect_identical(problems, report$result$checks[names(problems)])
  # cli wraps prose to the console width; compare the text independently of
  # those display line breaks, leaving copyable repair code intact.
  prose <- gsub("[[:space:]]+", " ", report$output)
  summary <- strsplit(prose, "Problems to fix", fixed = TRUE)[[1]][[2]]
  for (problem in problems) {
    occurrences <- gregexpr(problem$message, prose, fixed = TRUE)[[1]]
    expect_length(occurrences, 2L)
    expect_true(all(occurrences > 0L))
    expect_true(grepl(problem$message, summary, fixed = TRUE))
    expect_true(grepl(problem$actions[[2]], summary, fixed = TRUE))
  }
})

test_that("no-problems summary does not claim unverified checks passed", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder,
    workspace_editor = function() list(detected = TRUE, workspace = NULL, version = NULL))
  report <- workspace_report()
  expect_length(report$result$problems, 0L)
  expect_match(report$output, "No problems were found by the checks that ran.", fixed = TRUE)
  expect_gt(report$result$counts[["manual"]], 0L)
})

test_that("status labels use the requested weight and colours with plain-text fallback", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder,
    workspace_editor = function() list(detected = TRUE, workspace = NULL, version = NULL))
  unlink(file.path(folder, "output"), recursive = TRUE)
  withr::local_options(cli.num_colors = 256)
  report <- workspace_report()
  green_pass <- cli::make_ansi_style("green")("PASS")
  red_problem <- cli::make_ansi_style("bold")(cli::make_ansi_style("#8B0000")("PROBLEM"))
  orange_manual <- cli::make_ansi_style("orange")("MANUAL CHECK")
  expect_true(grepl(green_pass, report$output, fixed = TRUE))
  expect_true(grepl(red_problem, report$output, fixed = TRUE))
  expect_true(grepl(orange_manual, report$output, fixed = TRUE))
  expect_false(grepl(paste0("\033[1m", orange_manual), report$output, fixed = TRUE))
  expect_false(grepl(paste0("\033[1m", green_pass), report$output, fixed = TRUE))
  expect_false(cli::ansi_has_any(paste(capture.output(str(report$result)), collapse = "\n")))
  withr::local_options(cli.num_colors = 1)
  plain <- workspace_report()
  expect_false(cli::ansi_has_any(plain$output))
  expect_match(plain$output, "PASS:", fixed = TRUE)
  expect_match(plain$output, "PROBLEM:", fixed = TRUE)
  expect_match(plain$output, "MANUAL CHECK:", fixed = TRUE)
})

test_that("Posit Cloud's Project label is accepted without weakening local folder checks", {
  expect_true(workspace_cloud_folder("/cloud/project"))
  expect_false(workspace_cloud_folder("/cloud/project/other"))
  expect_false(workspace_cloud_folder(file.path(tempdir(), "Project")))
  folder <- workspace_fixture("Project")
  withr::local_dir(folder)
  local_workspace_mocks(folder, workspace_cloud_folder = function(path) workspace_same_path(path, folder))
  report <- workspace_report()
  expect_identical(check_status(report, "workspace.name"), "passed")
  expect_match(gsub("[[:space:]]+", " ", report$output), "may display it as Project", fixed = TRUE)
  expect_false(grepl("rename it to crime_mapping", report$output, fixed = TRUE))
  unlink(file.path(folder, "output"), recursive = TRUE)
  report <- workspace_report()
  expect_true(any(grepl("dir.create", report$result$checks[["directory.output"]]$actions, fixed = TRUE)))
  # Cloud fallback is also identifiable when the active-workspace API and
  # local editor markers are unavailable; getwd is not presented as API proof.
  unlink(file.path(folder, ".vscode"), recursive = TRUE)
  local_workspace_bindings(workspace_editor = function() list(detected = TRUE, workspace = NULL, version = NULL))
  report <- workspace_report()
  expect_true(report$result$workspace_confident)
  expect_identical(check_status(report, "workspace.active"), "manual")
  local_workspace_bindings(workspace_cloud_folder = function(path) FALSE)
  report <- workspace_report()
  expect_identical(check_status(report, "workspace.name"), "problem")
})


test_that("Positron versions are reported only when available, with no manual lookup", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  expect_false("positron.version" %in% names(report$result$checks))
  expect_false(grepl("About Positron", report$output, fixed = TRUE))
  expect_false(grepl("Help > About", report$output, fixed = TRUE))
  local_workspace_bindings(workspace_editor = function() {
    list(detected = TRUE, workspace = folder, version = "2026.09.1")
  })
  report <- workspace_report(folder)
  expect_identical(check_status(report, "positron.version"), "passed")
  expect_length(report$result$checks[["positron.version"]]$actions, 0L)
})

test_that("final section repeats all manual checks after all problems without recounting", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder,
    workspace_editor = function() list(detected = TRUE, workspace = NULL, version = NULL))
  writeLines('{}', file.path(folder, ".vscode", "settings.json"))
  unlink(file.path(folder, "output"), recursive = TRUE)
  report <- workspace_report()
  manual <- Filter(function(check) check$status == "manual", report$result$checks)
  expect_identical(names(manual), c("workspace.active", "air.format_on_save", "air.formatter"))
  expect_identical(report$result$counts[["manual"]], 3L)
  expect_identical(report$result$counts[["problem"]], 1L)
  prose <- gsub("[[:space:]]+", " ", report$output)
  summary <- strsplit(prose, "Problems to fix", fixed = TRUE)[[1]][[2]]
  problem_position <- regexpr("PROBLEM:", summary, fixed = TRUE)[[1]]
  manual_position <- regexpr("MANUAL CHECK:", summary, fixed = TRUE)[[1]]
  expect_gt(problem_position, 0L)
  expect_gt(manual_position, problem_position)
  for (check in manual) {
    positions <- gregexpr(check$message, prose, fixed = TRUE)[[1]]
    expect_length(positions, 2L)
    expect_true(all(positions > 0L))
    expect_true(grepl(check$message, summary, fixed = TRUE))
    for (action in check$actions) expect_true(grepl(action, summary, fixed = TRUE))
  }
})

test_that("introduction explains the course purpose and links to unversioned chapters", {
  folder <- workspace_fixture()
  local_workspace_mocks(folder)
  report <- workspace_report(folder)
  introduction <- strsplit(report$output, "R and Positron", fixed = TRUE)[[1]][[1]]
  prose <- gsub("[[:space:]]+", " ", introduction)
  expect_match(prose, "your computer is set up correctly for the Crime Mapping course", fixed = TRUE)
  expect_match(prose, "https://books.lesscrime.info/learncrimemapping/setup.html", fixed = TRUE)
  expect_match(prose, "https://books.lesscrime.info/learncrimemapping/01_getting_started/", fixed = TRUE)
  expect_false(grepl("/learncrimemapping/[0-9]{4}/", introduction))
})

test_that("workspace checker suggests create_dirs only for a fresh current workspace", {
  folder <- workspace_fixture()
  withr::local_dir(folder)
  local_workspace_mocks(folder)
  unlink(file.path(folder, c("data", "R", "output")), recursive = TRUE)
  report <- workspace_report()
  expect_match(report$result$checks[["directory.data"]]$actions[[2]],
               "learncrimemapping::create_dirs()", fixed = TRUE)
  dir.create(file.path(folder, "R"))
  report <- workspace_report()
  expect_match(report$result$checks[["directory.data"]]$actions[[2]],
               "dir.create", fixed = TRUE)
})
