#' Check code file before submission
#'
#' Check an R script or the enabled R chunks of an R Markdown or Quarto
#' document in a separate R session, then report potential problems and
#' suggestions using a consistent course style configuration.
#'
#' @param file A path to an `.R`, `.Rmd`, or `.qmd` file, or `NULL` to choose
#'   a file interactively.
#' @param reprex Should the code be executed to check for errors and warnings?
#'   This argument is retained for compatibility; execution uses callr and
#'   evaluate rather than reprex.
#' @param style Should the code be checked for style and other static issues?
#' @param execution_dir An optional execution directory. By default, search
#'   upwards from the submitted file for an RStudio or Quarto project, using
#'   the file's directory if no project is found.
#' @param timeout Maximum execution time in seconds, defaulting to 600 (ten
#'   minutes). Must be a positive finite number.
#' @return Invisibly, a list containing the original file, execution directory,
#'   execution status and captured events, lint results, check failures,
#'   report path, and checker dependency versions. Execution status is one of
#'   `skipped`, `success`, `error`, `timeout`, or `failed`. Reports and plots
#'   are temporary and last for the current R session.
#' @details Execution stops at the first error. Static checks can still run
#'   after execution fails or times out. An isolated R session avoids relying
#'   on objects in the student's workspace, but code can still read and write
#'   files and access the network. Personal startup profiles are not loaded;
#'   the current package library paths are passed to the child session.
#'
#'   For documents, extracted R code is checked, rather than rendering the
#'   document. Inline expressions and chunks explicitly marked `eval = FALSE`
#'   (including Quarto's `#| eval: false`) are excluded from both stages.
#'   Dynamic chunk options, child documents, and document rendering settings
#'   are not reproduced. Locations refer to the original file. Runtime
#'   locations identify the start of the executing expression, rather than
#'   the precise failing statement within a function.
#'
#'   Local lintr configuration is ignored. Findings are suggestions, not a
#'   guarantee of correctness or an assessment grade.
#' @export
check_code <- function(file = NULL, reprex = TRUE, style = TRUE,
                       execution_dir = NULL, timeout = 600) {
  # Validate flags before opening a file dialog or running checks. NA is a
  # logical scalar, but cannot be used as the condition of an if statement.
  check_code_arguments(file, reprex, style, execution_dir, timeout)
  if (is.null(file)) {
    if (!interactive()) {
      cli::cli_abort("Supply {.arg file} when running noninteractively.")
    }
    file <- tryCatch(file.choose(), error = function(cnd) {
      cli::cli_abort("No file was selected.", parent = cnd)
    })
  }
  if (!file.exists(file) || dir.exists(file) || file.access(file, 4) != 0) {
    cli::cli_abort("{.file {file}} must be a readable regular file.")
  }
  # Resolve the original path before the child changes working directory.
  # Forward slashes keep recorded locations consistent across platforms.
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  extension <- tolower(tools::file_ext(file))
  if (!extension %in% c("r", "rmd", "qmd")) {
    cli::cli_abort("{.arg file} must be an R, R Markdown, or Quarto file.")
  }
  execution_dir <- check_execution_directory(file, execution_dir)
  # Initialise the complete return structure even when a stage is disabled.
  # "error" means submitted code failed; "failed" means the checker failed.
  result <- list(
    file = file, execution_dir = execution_dir,
    execution = list(status = "skipped", events = list()),
    lints = empty_check_lints(), failures = list(), report = NULL,
    versions = check_dependency_versions()
  )
  cli::cli_h1("Checking your code")
  if (!reprex && !style) {
    cli::cli_warn("No checking was done because both checks were disabled.")
    return(invisible(result))
  }
  # Use a unique session directory, never a predictable file beside the
  # submission. Keep reports and plots available after this call returns.
  report_dir <- tempfile("check-code-")
  dir.create(report_dir)
  # Event files allow completed output to survive an execution timeout.
  # Event files are checkpoints, removed once the report has been produced.
  # Do not remove the entire directory, which also contains retained plots.
  on.exit(unlink(list.files(report_dir, pattern = "^event-", full.names = TRUE)),
          add = TRUE)

  if (extension != "r") {
    cli::cli_text(
      "Checking enabled R chunks in the original document. Inline R, disabled ",
      "chunks, and full rendering behaviour are not checked."
    )
  }
  if (reprex) {
    cli::cli_text("Execution directory: {.file {execution_dir}}")
    cli::cli_text("Execution will stop on the first error or after {timeout} seconds.")
    # Failures here belong to the checker; submitted code conditions are
    # captured inside run_check_code(). Check only this stage's dependencies.
    execution <- tryCatch({
      rlang::check_installed(c("callr", "evaluate", "lintr"),
                             version = c("3.7.0", "1.0.0", "3.4.0"))
      code <- read_check_code(file, extension)
      run_check_code(code, file, execution_dir, timeout, report_dir)
    }, error = function(cnd) {
      list(status = "failed", events = read_check_events(report_dir),
           message = conditionMessage(cnd))
    })
    result$execution <- execution
    if (execution$status == "failed") {
      result$failures$execution <- execution$message
    }
    report_execution(execution, execution_dir)
  }
  # Static feedback is independent of execution and remains useful after
  # a code error, timeout, or runner failure.
  if (style) {
    lints <- tryCatch({
      rlang::check_installed(c("lintr", "cyclocomp"),
                             version = c("3.4.0", NA_character_))
      lint_check_code(file)
    }, error = function(cnd) {
      # The handler has its own frame; update the enclosing result so the
      # returned object and report retain the reason this stage was incomplete.
      result$failures$style <<- conditionMessage(cnd)
      NULL
    })
    if (is.null(lints)) {
      cli::cli_alert_warning("Static checks could not be completed.")
      cli::cli_text("{result$failures$style}")
    } else {
      result$lints <- lints
      report_check_lints(lints, file)
    }
  }
  # Build the full report from results, independently of the abbreviated
  # console presentation. Return those same structured results invisibly.
  result$report <- file.path(report_dir, "report.txt")
  write_check_report(result)
  cli::cli_text("Full report: {.file {result$report}}")
  cli::cli_text("{.emph Run {.fn check_code} again after making changes.}")
  invisible(result)
}

# Validate inputs separately from file discovery and checking stages.
check_code_arguments <- function(file, reprex, style, execution_dir, timeout) {
  valid_path <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  if (!is.null(file) && !valid_path(file)) {
    cli::cli_abort("{.arg file} must be NULL or a single nonempty file path.")
  }
  for (name in c("reprex", "style")) {
    value <- get(name)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort("{.arg {name}} must be a single TRUE or FALSE value.")
    }
  }
  if (!is.null(execution_dir) && !valid_path(execution_dir)) {
    cli::cli_abort("{.arg execution_dir} must be NULL or a single directory path.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) {
    cli::cli_abort("{.arg timeout} must be a positive finite number of seconds.")
  }
}

check_execution_directory <- function(file, execution_dir) {
  if (!is.null(execution_dir)) {
    if (!dir.exists(execution_dir)) {
      cli::cli_abort("Execution directory {.file {execution_dir}} does not exist.")
    }
    return(normalizePath(execution_dir, winslash = "/", mustWork = TRUE))
  }
  # Search from the submission, not getwd(): the caller may have an
  # unrelated project open. Standalone files need no project marker.
  criterion <- rprojroot::is_rstudio_project | rprojroot::is_quarto_project
  tryCatch(
    normalizePath(rprojroot::find_root(criterion, path = dirname(file)),
                  winslash = "/", mustWork = TRUE),
    error = function(cnd) dirname(file)
  )
}

read_check_code <- function(file, extension) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  if (extension != "r") {
    # The public extraction API preserves original line positions and skips
    # explicitly disabled R chunks without evaluating document options.
    lines <- lintr::get_source_expressions(file, lines = lines)$lines
    # Replace excluded prose and chunks with blank lines rather than deleting
    # them, so runtime locations still refer to the original file.
    lines[is.na(lines)] <- ""
  }
  unname(lines)
}

# A child process prevents workspace objects hiding missing setup and lets
# callr terminate execution stuck in a long operation.
run_check_code <- function(code, file, execution_dir, timeout, report_dir) {
  status <- tryCatch(
    callr::r(
      # Keep this function self-contained: it runs in a fresh R session and
      # cannot rely on the parent session's helper functions or local objects.
      function(code, file, report_dir) {
        event_count <- 0L
        next_line <- 1L
        current_line <- 1L
        # Checkpoint each event immediately; a timed-out child cannot return
        # in-memory results. Handler closures share the counters through <<-.
        record <- function(kind, text = NULL, plot = NULL) {
          event_count <<- event_count + 1L
          event <- list(kind = kind, text = text, line = current_line, plot = plot)
          target <- file.path(report_dir, sprintf("event-%08d.rds", event_count))
          # Publish only complete files. If the child is killed during a write,
          # the parent ignores the unfinished .partial checkpoint.
          saveRDS(event, paste0(target, ".partial"))
          if (!file.rename(paste0(target, ".partial"), target)) {
            stop("Could not save execution output")
          }
          invisible(NULL)
        }
        # Capture actual conditions, not text resembling "Error" or "Warning"
        # in printed output. Event order is retained in the full report.
        handler <- evaluate::new_output_handler(
          source = function(x) {
            # Source events partition the input. Counting their newlines locates
            # each expression, including preceding blanks. This identifies its
            # start, not the precise failing statement inside a function.
            current_line <<- next_line
            next_line <<- next_line + lengths(regmatches(x$src, gregexpr("\n", x$src)))
            if (nzchar(trimws(x$src))) record("source", x$src)
          },
          text = function(x) record("output", x),
          warning = function(x) record("warning", conditionMessage(x)),
          message = function(x) record("message", conditionMessage(x)),
          error = function(x) record("error", conditionMessage(x)),
          graphics = function(x) {
            path <- file.path(report_dir, sprintf("plot-%08d.png", event_count + 1L))
            # Save plots in the child rather than returning device-dependent
            # recorded plots. Close the PNG device even if replaying fails.
            grDevices::png(path, width = 1000, height = 800)
            tryCatch(grDevices::replayPlot(x), finally = grDevices::dev.off())
            record("plot", plot = path)
          }
        )
        # Suppression by a user's environment variable would defeat capture.
        Sys.unsetenv("R_EVALUATE_BYPASS_MESSAGES")
        tryCatch({
          output <- evaluate::evaluate(
            code, envir = new.env(parent = globalenv()), filename = file,
            # 1 stops at the first error while retaining earlier output; 0 would
            # continue and 2 would discard the evaluation result.
            stop_on_error = 1L, keep_warning = TRUE, keep_message = TRUE,
            output_handler = handler
          )
          if (any(vapply(output, inherits, logical(1), "error"))) "error" else "success"
        }, error = function(cnd) {
          # Parse failures can precede handler callbacks. Record these as
          # submission errors using the same event format.
          record("error", conditionMessage(cnd))
          "error"
        })
      },
      args = list(code = code, file = file, report_dir = report_dir),
      wd = execution_dir, timeout = timeout,
      # Avoid startup profiles changing checker behaviour. callr passes the
      # parent's library paths by default, preserving installed course packages.
      user_profile = FALSE, system_profile = FALSE
    ),
    error = function(cnd) {
      if (inherits(cnd, c("callr_timeout_error", "system_command_timeout_error"))) {
        return("timeout")
      }
      # Propagate other process failures to the outer checker-failure handler.
      stop(cnd)
    }
  )
  list(status = status, events = read_check_events(report_dir))
}

# Zero-padded filenames sort chronologically. The pattern excludes partial
# checkpoints left by a terminated child process.
read_check_events <- function(report_dir) {
  files <- sort(list.files(report_dir, pattern = "^event-[0-9]+\\.rds$",
                           full.names = TRUE))
  lapply(files, readRDS)
}

# Preserve columns for clean or skipped checks so callers do not need a
# special case for the empty result.
empty_check_lints <- function() {
  data.frame(filename = character(), line_number = integer(),
             column_number = integer(), type = character(), message = character(),
             line = character(), linter = character(), category = character())
}

# Maintain a deliberate course rule list rather than adopting all upstream
# defaults. On lintr upgrades, review renamed rules and changed defaults.
lint_check_code <- function(file) {
  lints <- lintr::lint(
    file,
    linters = list(
      lintr::absolute_path_linter(), # no absolute paths
      lintr::assignment_linter(operator = "<-"), # `<-` is used for assignment
      lintr::brace_linter(), # {} are correctly styled
      lintr::class_equals_linter(), # classes are checked for with `inherits()` not `==`
      lintr::commas_linter(), # commas are followed by spaces
      # cyclocomp is an optional lintr dependency, checked explicitly above.
      lintr::cyclocomp_linter(), # no expressions are very complicated
      lintr::duplicate_argument_linter(), # no duplicate arguments
      lintr::equals_na_linter(), # NAs are checked with `is.na()` not `==`
      lintr::for_loop_index_linter(), # input var is not overwritten as loop-index var
      lintr::function_left_parentheses_linter(), # no space before function ()
      lintr::implicit_assignment_linter(), # no assignment inside function args
      lintr::indentation_linter(), # consistent indentation
      lintr::infix_spaces_linter(), # spaces around operators
      lintr::inner_combine_linter(), # vectorised function not re-used for every element of vector
      # lintr::keyword_quote_linter(), # no unnecessary quoting of obj index
      lintr::length_test_linter(), # no mistakes with usage of `length()`
      lintr::library_call_linter(allow_preamble = FALSE), # packages loaded first
      # Exempt string contents so paths and URLs need not be split. This also
      # exempts other long strings; the remainder of a line is still checked.
      lintr::line_length_linter(length = 80, ignore_string_bodies = TRUE), # lines no more than 80 chrs
      lintr::missing_argument_linter(), # no empty function args
      lintr::missing_package_linter(), # no uninstalled packages
      # Symbol checks load referenced namespaces and depend on installed
      # package versions, so these findings may differ between machines.
      lintr::namespace_linter(), # no uninstalled packages
      lintr::nested_ifelse_linter(), # no nested `ifelse()` calls
      lintr::numeric_leading_zero_linter(), # require leading zeros before `.`
      lintr::object_length_linter(length = 40), # no excessively long object names
      lintr::object_name_linter(), # object names follow style guide
      lintr::paren_body_linter(), # space after function ()
      lintr::paste_linter(), # `paste()` not misused
      lintr::pipe_consistency_linter(pipe = "|>"), # use base pipe
      lintr::pipe_continuation_linter(), # pipe split over lines properly
      lintr::quotes_linter(), # double quotes
      lintr::repeat_linter(), # suggests explicit loop conditions
      lintr::scalar_in_linter(), # no use of `%in%` on single values
      lintr::semicolon_linter(), # no semi-colons
      lintr::seq_linter(), # no problems with `1:length()` etc.
      lintr::sort_linter(), # no common mistakes with vector sorting
      lintr::spaces_inside_linter(), # no spaces inside () or []
      lintr::spaces_left_parentheses_linter(), # space before ( except function calls
      lintr::sprintf_linter(), # no common mistakes with `sprintf()`
      lintr::system_file_linter(), # no use of `file.path()` in `system.file()`
      lintr::T_and_F_symbol_linter(), # `T`/`F` aren't used for `TRUE`/`FALSE`
      lintr::todo_comment_linter(), # no comments saying 'to do' or 'fix me'
      # no use of undesirable functions (except `library()`)
      lintr::undesirable_function_linter(
        fun = lintr::modify_defaults(
          defaults = lintr::default_undesirable_functions,
          # library() is encouraged for explicit setup in student scripts.
          library = NULL
        )
      ),
      lintr::undesirable_operator_linter(), # no use of undesirable operators
      lintr::unnecessary_concatenation_linter(), # no use of `c()` with empty args
      lintr::unnecessary_lambda_linter(), # no unnecessary anonymous functions
      lintr::unnecessary_nesting_linter(), # avoid unnecessary nesting
      lintr::unreachable_code_linter(), # no unreachable code
      lintr::unused_import_linter(), # no un-used packages loaded
      lintr::vector_logic_linter() # no problematic `&` or `|` in `if()` etc.
    ),
    # Recompute each submission and ignore external .lintr settings, including
    # their exclusions, to keep the course configuration consistent.
    cache = FALSE,
    parse_settings = FALSE
  )
  results <- as.data.frame(lints)
  if (!nrow(results)) return(empty_check_lints())
  # Missing packages describe the environment, not a style mistake. Keep
  # lintr's original severity and message alongside our presentation category.
  results$category <- ifelse(
    results$linter %in% c("missing_package_linter", "namespace_linter") &
      grepl("not installed", results$message, fixed = TRUE),
    "environment", ifelse(results$type == "style", "style", "correctness")
  )
  results[order(results$line_number, results$column_number), , drop = FALSE]
}

report_execution <- function(execution, execution_dir) {
  status <- execution$status
  if (status == "timeout") {
    cli::cli_alert_warning("Execution reached the time limit; the check is incomplete.")
    cli::cli_text(
      "Common causes include large spatial datasets, slow spatial operations, ",
      "downloads or geocoding requests, long or infinite loops, and code waiting ",
      "for input. Run sections separately to find where execution slows down."
    )
  } else if (status == "failed") {
    cli::cli_alert_warning("The execution checker could not complete its checks.")
    cli::cli_text("{execution$message}")
  } else if (status == "error") {
    cli::cli_alert_danger("Execution stopped at the first error.")
  } else {
    cli::cli_alert_success("Execution completed without detected errors.")
  }
  # Report warnings independently of errors: earlier warnings remain useful
  # after execution stops. Truncate only the console, never the results.
  errors <- Filter(function(x) x$kind == "error", execution$events)
  warnings <- Filter(function(x) x$kind == "warning", execution$events)
  if (length(errors)) {
    cli::cli_text("Line {errors[[1]]$line}: {errors[[1]]$text}")
    cli::cli_text(
      "If a data file could not be found, check its path relative to ",
      "{.file {execution_dir}}."
    )
  }
  if (length(warnings)) {
    cli::cli_text("{length(warnings)} warning(s) were captured; showing up to three:")
    cli::cli_ul()
    for (event in utils::head(warnings, 3)) {
      # Interpolate messages as data so braces in student output are not
      # interpreted as cli expressions. Use this pattern for paths as well.
      cli::cli_li("Line {event$line}: {event$text}")
    }
    cli::cli_end()
    cli::cli_text("Check whether these warnings require changes to your code.")
  }
}

report_check_lints <- function(lints, file) {
  if (!nrow(lints)) {
    cli::cli_alert_success("No static issues were found.")
    return(invisible(NULL))
  }
  labels <- c(correctness = "Likely errors or correctness problems",
              environment = "Environment problems", style = "Style suggestions")
  for (category in names(labels)) {
    findings <- lints[lints$category == category, , drop = FALSE]
    if (!nrow(findings)) next
    cli::cli_h2("{labels[[category]]}")
    # Group by rule and unchanged message, preserving case-sensitive examples.
    groups <- unique(findings[c("linter", "message")])
    for (i in seq_len(nrow(groups))) {
      selected <- findings[findings$linter == groups$linter[i] &
                             findings$message == groups$message[i], , drop = FALSE]
      lines <- sort(unique(selected$line_number))
      message <- groups$message[i]
      cli::cli_text("Lines {lines}: {message}")
      # One line can contain several findings. Deduplicate exact locations
      # while retaining distinct columns for editor navigation.
      locations <- unique(selected[c("line_number", "column_number")])
      # cli's file markup supports editor links with line and column suffixes.
      for (j in seq_len(min(nrow(locations), 3L))) {
        location <- paste0(file, ":", locations$line_number[j], ":",
                           locations$column_number[j])
        cli::cli_text("  {.file {location}}")
      }
      if (nrow(locations) > 3L) cli::cli_text("  Further locations are in the full report.")
    }
  }
  invisible(NULL)
}

# Plain text is easy to inspect and copy. Preserve every captured event and
# lint, including details omitted from the concise console summary.
write_check_report <- function(result) {
  lines <- c(
    "check_code report", paste("File:", result$file),
    paste("Execution directory:", result$execution_dir),
    paste("Execution status:", result$execution$status),
    "Runtime locations identify the start of the executing expression.", ""
  )
  for (event in result$execution$events) {
    lines <- c(lines, paste0("[", event$kind, ", line ", event$line, "]"),
               if (event$kind == "plot") event$plot else event$text)
  }
  if (nrow(result$lints)) {
    lines <- c(lines, "", "Static findings:", vapply(seq_len(nrow(result$lints)), function(i) {
      x <- result$lints[i, ]
      paste0(x$filename, ":", x$line_number, ":", x$column_number,
             " [", x$category, "/", x$linter, "] ", x$message)
    }, character(1)))
  }
  if (length(result$failures)) {
    lines <- c(lines, "", "Incomplete checks:", unlist(result$failures))
  }
  lines <- c(lines, "", "Checker versions:",
             paste(names(result$versions), result$versions))
  writeLines(lines, result$report, useBytes = TRUE)
}

# Record versions to diagnose machine-specific behaviour. Missing packages
# are recorded without preventing checks that do not require them.
check_dependency_versions <- function() {
  packages <- c("callr", "evaluate", "lintr", "cyclocomp", "cli", "rprojroot")
  versions <- vapply(packages, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      "not installed"
    }
  }, character(1))
  c(R = as.character(getRversion()), versions)
}
