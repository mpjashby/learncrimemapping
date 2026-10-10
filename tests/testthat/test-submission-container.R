test_that("container launch exposes only the student workspace and drops privileges", {
  config <- list(image = "sha256:example", memory = "4g", cpus = 2, user = "1000:1000", platform = "linux/amd64")
  args <- submission_container_args("/tmp/student workspace", "test-student", config)
  expect_true(all(c("--read-only", "--cap-drop", "ALL", "no-new-privileges:true") %in% args))
  expect_equal(sum(args == "--mount"), 1L)
  expect_true("type=bind,source=/tmp/student workspace,target=/workspace" %in% args)
  expect_true("R_LIBS_USER=/workspace/.sandbox/library" %in% args)
  expect_true("TMPDIR=/workspace/.sandbox/tmp" %in% args)
  expect_true("bridge" %in% args)
  expect_identical(args[which(args == "--platform") + 1L], "linux/amd64")
  expect_false(any(grepl("docker.sock|--privileged|--publish", args)))
  expect_true(all(c("max-size=5m", "max-file=1", "compress=false") %in% args))
  expect_error(submission_container_args("/tmp/a,b", "test", config), "commas")
})

test_that("Docker command failures retain stderr for batch feedback", {
  skip_on_os("windows")
  expect_error(submission_docker("/bin/sh", c("-c", "echo 'daemon launch detail' >&2; exit 1")),
               "daemon launch detail")
})

test_that("CARTO credentials are forwarded by name without exposing their value", {
  withr::local_envvar(c(CARTO_API_KEY = "fixture-secret", OTHER_API_KEY = "unrelated-secret"))
  config <- list(image = "sha256:example", memory = "4g", cpus = 2,
                 user = "1000:1000", platform = "linux/arm64")
  args <- submission_container_args("/tmp/workspace", "test-student", config)
  variables <- args[which(args == "--env") + 1L]
  expect_true("CARTO_API_KEY" %in% variables)
  expect_false(any(grepl("fixture-secret|unrelated-secret|OTHER_API_KEY", args)))
})

test_that("container configuration rejects unsafe or invalid settings", {
  expect_error(submission_container_config("--privileged", "4g", 2), "image")
  expect_error(submission_container_config("image", "bad", 2), "memory")
  expect_error(submission_container_config("image", "4g", 0), "cpus")
})

test_that("workspace output links cannot redirect host report reads", {
  skip_on_os("windows")
  workspace <- tempfile()
  dir.create(workspace)
  workspace <- normalizePath(workspace, winslash = "/")
  outside <- tempfile()
  writeLines("outside", outside)
  expect_true(file.symlink(outside, file.path(workspace, "link")))
  expect_error(assert_submission_workspace_files(workspace), "symbolic links")
  unlink(file.path(workspace, "link"))
  expect_silent(assert_submission_workspace_files(workspace))
})

test_that("package installation policy can be relaxed only for the isolated backend", {
  # Use a locally created package to avoid network or changing the host library.
  code <- feedback_fixture('install.packages(character(), lib = tempdir())')
  local <- check_code_backend(code, style = FALSE)
  isolated <- suppressWarnings(check_code_backend(code, style = FALSE,
                                                  allow_package_install = TRUE))
  expect_identical(local$execution$status, "skipped")
  expect_identical(isolated$execution$status, "success")
  expect_false(any(vapply(isolated$issues, function(x)
    x$id == "course.package_install", logical(1))))
})

# Opt-in integration tests use the actual image and daemon. No Docker dependency
# is required for the ordinary R test suite.
test_that("Docker isolates host files, students, package installs and system writes", {
  skip_if(Sys.getenv("LCM_TEST_DOCKER") != "true", "Set LCM_TEST_DOCKER=true after building the image")
  host_file <- tempfile()
  writeLines("host sentinel", host_file)
  template <- tempfile()
  dir.create(template)
  pkg <- file.path(template, "tinyfixture")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(c("Package: lcmisolationfixture", "Version: 0.0.1",
               "Title: Isolation Test", "Description: A package used in isolation tests.",
               "Authors@R: person('Test', 'Author', email='test@example.com', role=c('aut', 'cre'))",
               "License: MIT"), file.path(pkg, "DESCRIPTION"))
  writeLines("export(answer)", file.path(pkg, "NAMESPACE"))
  writeLines("answer <- function() 42", file.path(pkg, "R", "answer.R"))
  output <- tempfile()
  code <- c(
    sprintf('stopifnot(!file.exists(%s))', encodeString(host_file, quote = '"')),
    'stopifnot(!file.exists("/workspace/../Participant_2"))',
    'stopifnot(file.access(R.home("library"), 2) != 0)',
    'stopifnot(all(file.access(.libPaths()[-1], 2) != 0))',
    'stopifnot(!file.create("/opt/forbidden-student-file"))',
    'install.packages("tinyfixture", repos = NULL, type = "source")',
    'pacman::p_load(lcmisolationfixture)',
    'stopifnot(lcmisolationfixture::answer() == 42)',
    'stopifnot(startsWith(find.package("lcmisolationfixture"), "/workspace/.sandbox/library/"))',
    'stopifnot(here::here() == "/workspace")',
    'download.file("https://cloud.r-project.org/robots.txt", "outputs/web.txt", quiet = TRUE)',
    'writeLines("one", "outputs/student-one.txt")',
    'plot(1:3)'
  )
  zip <- feedback_zip(list(
    "Participant_1_assignsubmission_file/exercise.R" = code,
    "Participant_2_assignsubmission_file/exercise.R" = c(
      'stopifnot(!requireNamespace("lcmisolationfixture", quietly = TRUE))',
      'stopifnot(!file.exists("outputs/student-one.txt"))',
      'stopifnot(!file.exists("/opt/forbidden-student-file"))',
      'writeLines("two", "outputs/student-two.txt")'
    )))
  batch <- check_submissions(zip, output, workspace_template = template, style = FALSE,
                            container_image = Sys.getenv("LCM_TEST_IMAGE", "learncrimemapping-checker:local"))
  expect_identical(batch$manifest$execution, c("success", "success"))
  expect_identical(readLines(host_file), "host sentinel")
  expect_false(dir.exists(batch$results$Participant_1$execution_dir))
  plots <- Filter(function(e) e$kind == "plot", batch$results$Participant_1$execution$events)
  expect_length(plots, 1L)
  expect_false(file.exists(plots[[1]]$plot))
  expect_false(dir.exists(file.path(.libPaths()[1], "lcmisolationfixture")))
  expect_match(paste(readLines(batch$results$Participant_1$html_report), collapse = ""), "data:image/png")
})

test_that("Docker timeout terminates the student container", {
  skip_if(Sys.getenv("LCM_TEST_DOCKER") != "true", "Set LCM_TEST_DOCKER=true after building the image")
  zip <- feedback_zip(list("Participant_1_assignsubmission_file/code.R" = 'Sys.sleep(120)'))
  batch <- check_submissions(zip, tempfile(), style = FALSE, timeout = 2,
                            container_image = Sys.getenv("LCM_TEST_IMAGE", "learncrimemapping-checker:local"))
  expect_identical(batch$results$Participant_1$execution$status, "timeout")
  containers <- processx::run(Sys.which("docker"), c("ps", "-a", "--format", "{{.Names}}"))$stdout
  expect_false(grepl(paste0("lcm-", Sys.getpid(), "-"), containers, fixed = TRUE))
})

test_that("Docker startup failure aborts before extraction without local fallback", {
  zip <- feedback_zip(list("Participant_1_assignsubmission_file/code.R" = "1"))
  output <- tempfile()
  isolated <- check_submissions
  environment(isolated) <- new.env(parent = environment(check_submissions))
  environment(isolated)$submission_container_config <- function(...) stop("Docker unavailable")
  expect_error(isolated(zip, output), "Docker unavailable")
  expect_false(dir.exists(output))
})

test_that("container results translate plot paths and cleanup on failed monitoring", {
  file <- feedback_fixture("1")
  workspace <- tempfile()
  dir.create(workspace)
  report_dir <- file.path(workspace, "checker")
  result <- check_code_backend(file, reprex = FALSE, style = FALSE,
                               execution_dir = workspace, report_dir = report_dir)
  result$execution$events <- list(list(kind = "plot", plot = "/workspace/checker/plot-00000001.png"))
  calls <- list()
  runner <- check_submission_container
  environment(runner) <- new.env(parent = environment(check_submission_container))
  environment(runner)$submission_docker <- function(docker, args, timeout = 30000) {
    calls[[length(calls) + 1L]] <<- args
    if (args[1] == "wait") {
      saveRDS(result, file.path(workspace, ".sandbox", "result.rds"))
      return(list(stdout = "0\n"))
    }
    list(stdout = "container")
  }
  config <- list(docker = "fixture", image = "sha256:fixture", memory = "4g",
                 cpus = 2, user = "1000:1000", platform = "linux/amd64")
  checked <- runner(file, FALSE, FALSE, workspace, 10, code_feedback_profile(), report_dir, config)
  expect_identical(checked$file, file)
  expect_identical(checked$execution$events[[1]]$plot,
                   file.path(report_dir, "plot-00000001.png"))
  expect_identical(unname(checked$versions[["container_image"]]), "sha256:fixture")
  expect_equal(sum(vapply(calls, function(x) x[1] == "rm", logical(1))), 2L)
  unlink(file.path(workspace, ".sandbox"), recursive = TRUE)
  calls <- list()
  environment(runner)$submission_docker <- function(docker, args, timeout = 30000) {
    calls[[length(calls) + 1L]] <<- args
    if (args[1] == "wait") {
      err <- simpleError("timed out")
      class(err) <- c("system_command_timeout_error", class(err))
      stop(err)
    }
    list(stdout = "container")
  }
  checked <- runner(file, TRUE, FALSE, workspace, 1, code_feedback_profile(), report_dir, config)
  expect_identical(checked$execution$status, "timeout")
  expect_equal(sum(vapply(calls, function(x) x[1] == "rm", logical(1))), 2L)
})

test_that("the runtime platform follows the image rather than the host CPU", {
  inspect <- submission_container_config
  environment(inspect) <- new.env(parent = environment(submission_container_config))
  environment(inspect)$Sys.which <- function(...) c(docker = "fixture-docker")
  environment(inspect)$Sys.info <- function() c(sysname = "Darwin")
  architecture <- "amd64"
  environment(inspect)$submission_docker <- function(docker, args, ...) {
    expect_match(args[which(args == "--format") + 1L], "Architecture", fixed = TRUE)
    list(stdout = paste("sha256:fixture linux", architecture))
  }
  expect_identical(inspect("fixture", "4g", 2)$platform, "linux/amd64")
  architecture <- "arm64"
  expect_identical(inspect("fixture", "4g", 2)$platform, "linux/arm64")
})
