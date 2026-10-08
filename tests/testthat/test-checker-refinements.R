test_that("named and anonymous Moodle exports preserve IDs and reject ambiguous IDs", {
  zip <- feedback_zip(list(
    "A Student_With_Underscores_123_assignsubmission_file/code.R" = "x <- 1",
    "Participant_456_assignsubmission_file/code.R" = "x <- 2"
  ))
  batch <- check_submissions(zip, tempfile(), backend = "local", reprex = FALSE)
  expect_identical(names(batch$results), c("Participant_123", "Participant_456"))
  expect_match(batch$manifest$archive_path[1], "A Student_With_Underscores", fixed = TRUE)
  duplicate <- feedback_zip(list(
    "First Student_123_assignsubmission_file/code.R" = "1",
    "Second Student_123_assignsubmission_file/code.R" = "2"
  ))
  out <- tempfile()
  expect_error(check_submissions(duplicate, out, backend = "local"), "same participant ID")
  expect_false(dir.exists(out))
})

test_that("syntax failures prevent side effects and report the original document line", {
  marker <- tempfile()
  code <- c(sprintf('file.create("%s")', marker), ")")
  result <- check_code_backend(feedback_fixture(code))
  expect_false(file.exists(marker))
  expect_match(result$issues[[1]]$action, "none of the code could be run or checked for runtime issues", fixed = TRUE)
  expect_identical(result$execution$status, "skipped")
  expect_length(result$execution$events, 0L)
  expect_identical(result$issues[[1]]$id, "syntax.error")
  expect_identical(result$issues[[1]]$line, 2L)
  expect_match(result$issues[[1]]$message, "unexpected ')'", fixed = TRUE)
  expect_false(any(vapply(result$issues, function(i) i$id == "runtime.error", logical(1))))
  expect_match(feedback_initial_checks(result), "Not checked (syntax error)", fixed = TRUE)
  qmd <- check_code_backend(feedback_fixture(c("# Title", "```{r}", code, "```"), ".qmd"))
  expect_identical(qmd$issues[[1]]$line, 4L)
  expect_false(file.exists(marker))
})

test_that("long URL literals are exempt without exempting long comments or expressions", {
  url <- 'attacks <- read_csv("https://mpjashby.github.io/crimemappingdata/london_attacks.csv")'
  result <- check_code_backend(feedback_fixture(c(url, paste0("# ", strrep("a", 90)))), reprex = FALSE)
  long <- result$lints[result$lints$linter == "line_length_linter", ]
  expect_identical(long$line_number, 2L)
  expression <- paste0("x <- paste(", paste(rep('"x"', 20), collapse = ", "), ")")
  result <- check_code_backend(feedback_fixture(expression), reprex = FALSE)
  expect_true("line_length_linter" %in% result$lints$linter)
  qmd <- check_code_backend(feedback_fixture(c("```{r}", url, "```"), ".qmd"), reprex = FALSE)
  expect_false("line_length_linter" %in% qmd$lints$linter)
  invalid <- check_code_backend(feedback_fixture(c(url, ")")), reprex = FALSE)
  expect_false("line_length_linter" %in% invalid$lints$linter)
})

test_that("routine R setup messages are retained in Quarto feedback and raw logs", {
  code <- c('message("Loading required namespace: raster")',
            'message("The following object is masked from package:dplyr: select")',
            'message("Attaching package: sf")',
            'message("ℹ Use `spec()` to retrieve the full column specification for this data.")')
  r <- check_code_backend(feedback_fixture(code), style = FALSE)
  qmd <- check_code_backend(feedback_fixture(c("```{r}", code, "```"), ".qmd"), style = FALSE)
  messages <- function(x) Filter(function(i) i$id == "runtime.message", x$issues)
  expect_length(messages(r), 0L)
  expect_length(messages(qmd), 4L)
  expect_match(messages(qmd)[[1]]$action, "rendered document", fixed = TRUE)
  expect_equal(sum(vapply(r$execution$events, function(e) e$kind == "message", logical(1))), 4L)
  meaningful <- check_code_backend(feedback_fixture('message("Conflicts found in the input data")'),
                                   style = FALSE)
  expect_length(messages(meaningful), 1L)
})

test_that("embedded checker calls receive specific feedback before they can execute", {
  for (call in c("learncrimemapping::check_code()", "check_code(\"script.R\")")) {
    marker <- tempfile()
    result <- check_code_backend(feedback_fixture(c(sprintf('file.create("%s")', marker), call)),
                                 style = FALSE)
    expect_false(file.exists(marker))
    expect_identical(result$execution$status, "skipped")
    expect_identical(result$issues[[1]]$id, "course.checker_in_script")
    expect_identical(result$issues[[1]]$category, "execution")
    expect_match(feedback_initial_checks(result),
      "Code free of runtime errors: <span role='img' aria-label='Problem found'>❌", fixed = TRUE)
    expect_length(result$execution$events, 0L)
    expect_match(result$issues[[1]]$action, "none of the code could be run or checked for runtime issues", fixed = TRUE)
    expect_match(result$issues[[1]]$message, "only be run in the console", fixed = TRUE)
    expect_false(grepl("Supply `file`", result$issues[[1]]$message, fixed = TRUE))
  }
  harmless <- check_code_backend(feedback_fixture(c(
    "# learncrimemapping::check_code()", 'x <- "check_code()"')), style = FALSE)
  expect_identical(harmless$execution$status, "success")
  qmd <- check_code_backend(feedback_fixture(c("```{r}", "check_code()", "```"), ".qmd"))
  expect_identical(qmd$issues[[1]]$id, "course.checker_in_script")
})

test_that("spatial instrumentation retains actual package startup messages for Quarto", {
  skip_if_not_installed("sf")
  script <- check_code_backend(feedback_fixture("library(sf)"), style = FALSE)
  document <- check_code_backend(feedback_fixture(c("```{r}", "library(sf)", "```"), ".qmd"), style = FALSE)
  expect_true(any(vapply(script$execution$events, function(e)
    e$kind == "message" && grepl("Linking to GEOS", e$text, fixed = TRUE), logical(1))))
  expect_false(any(vapply(script$issues, function(i) i$id == "runtime.message", logical(1))))
  expect_true(any(vapply(document$issues, function(i) i$id == "runtime.message", logical(1))))
})

test_that("runtime errors precede earlier warnings in shared and HTML feedback", {
  result <- check_code_backend(feedback_fixture(c('warning("early warning")',
                                                'stop("later error")')), style = FALSE)
  expect_identical(result$issues[[1]]$id, "runtime.error")
  expect_match(result$issues[[1]]$action, "code block beginning on line 2", fixed = TRUE)
  expect_match(result$issues[[1]]$action, "None of the code below this block", fixed = TRUE)
  expect_match(result$issues[[1]]$action, "Static checks may still identify", fixed = TRUE)
  expect_identical(result$issues[[2]]$id, "runtime.warning")
  # The writer also orders structured results created by earlier checker versions.
  result$issues <- rev(result$issues)
  target <- tempfile(fileext = ".html")
  write_code_feedback(result, target)
  html <- paste(readLines(target), collapse = "\n")
  expect_lt(regexpr("later error", html)[1], regexpr("early warning", html)[1])
})

test_that("EPSG area checks use actual geometry, target variables and pipes", {
  skip_if_not_installed("sf")
  code <- c(
    'london <- sf::st_as_sf(data.frame(lon = -0.1, lat = 51.5), coords = c("lon", "lat"), crs = 4326)',
    'atlanta <- sf::st_as_sf(data.frame(lon = -84.4, lat = 33.75), coords = c("lon", "lat"), crs = 4326)',
    'target <- sf::st_crs(26967)',
    'transformer <- sf::st_transform',
    'wrong <- london |> transformer(crs = target)',
    'right <- sf::st_transform(atlanta, 26967)',
    'britain <- sf::st_transform(london, 27700)',
    'world <- sf::st_transform(london, 4326)',
    'partial <- sf::st_transform(rbind(london, atlanta), 26967)',
    'projected <- sf::st_transform(atlanta, 3857)',
    'again <- sf::st_transform(projected, 26967)'
  )
  result <- check_code_backend(feedback_fixture(code), style = FALSE)
  expect_identical(result$execution$status, "success")
  crs <- Filter(function(e) e$kind == "crs", result$execution$events)
  expect_length(crs, 7L) # observer's internal inverse transforms must not appear
  expect_identical(vapply(crs, function(e) e$details$status, character(1)),
    c("outside", "inside", "inside", "inside", "partial", "inside", "inside"))
  issues <- Filter(function(i) i$id == "spatial.crs_area", result$issues)
  expect_length(issues, 2L)
  expect_identical(issues[[1]]$line, 5L)
  expect_match(issues[[1]]$message, "EPSG:26967", fixed = TRUE)
  expect_match(issues[[1]]$evidence, "51.5000", fixed = TRUE)
  expect_match(issues[[2]]$message, "extend beyond", fixed = TRUE)
})

test_that("dateline extents and unavailable CRS metadata do not produce false passes", {
  skip_if_not_installed("sf")
  result <- check_code_backend(feedback_fixture(c(
    'nz <- sf::st_as_sf(data.frame(lon = c(179, -179), lat = c(-40, -40)), coords = c("lon", "lat"), crs = 4326)',
    'out <- sf::st_transform(nz, 3994)',
    'custom <- sf::st_transform(nz, "+proj=aeqd +lat_0=-40 +lon_0=180 +datum=WGS84 +units=m +no_defs")',
    'empty <- sf::st_transform(sf::st_sfc(crs = 4326), 2193)'
  )), style = FALSE)
  expect_identical(result$execution$status, "success")
  crs <- Filter(function(e) e$kind == "crs", result$execution$events)
  expect_identical(crs[[1]]$details$status, "inside")
  expect_identical(crs[[2]]$details$status, "not_checked")
  expect_identical(crs[[3]]$details$status, "not_checked")
  expect_false(any(vapply(result$issues, function(i) i$id == "spatial.crs_area", logical(1))))
  expect_equal(sum(vapply(result$issues, function(i) i$id == "spatial.crs_unchecked", logical(1))), 1L)
})

test_that("repeated transformations in one expression retain evidence without repeated feedback", {
  skip_if_not_installed("sf")
  result <- check_code_backend(feedback_fixture(c(
    'x <- sf::st_as_sf(data.frame(lon = -0.1, lat = 51.5), coords = c("lon", "lat"), crs = 4326)',
    'out <- list(sf::st_transform(x, 26967), sf::st_transform(x, 26967))'
  )), style = FALSE)
  expect_equal(sum(vapply(result$execution$events, function(e) e$kind == "crs", logical(1))), 2L)
  expect_equal(sum(vapply(result$issues, function(i) i$id == "spatial.crs_area", logical(1))), 1L)
})

test_that("Quarto point label explanations never extend to polygons or unrelated warnings", {
  skip_if_not_installed("sf")
  result <- check_code_backend(feedback_fixture(c(
    '```{r}',
    'point <- sf::st_sfc(sf::st_point(c(-0.1, 51.5)), crs = 4326)',
    'p <- sf::st_point_on_surface(point)',
    'stopifnot(identical(sf::st_coordinates(p), sf::st_coordinates(point)))',
    'polygon <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 50), c(1, 50), c(0, 51), c(0, 50)))), crs = 4326)',
    'surface <- sf::st_point_on_surface(polygon)',
    'warning("st_point_on_surface may not give correct results for longitude/latitude data")',
    '```'
  ), ".qmd"), style = FALSE)
  expect_identical(result$execution$status, "success")
  warnings <- Filter(function(i) i$id == "runtime.warning", result$issues)
  expect_length(warnings, 3L)
  expect_match(warnings[[1]]$action, "point locations are unchanged", fixed = TRUE)
  expect_false(grepl("point locations are unchanged", warnings[[2]]$action, fixed = TRUE))
  expect_false(grepl("point locations are unchanged", warnings[[3]]$action, fixed = TRUE))
})
