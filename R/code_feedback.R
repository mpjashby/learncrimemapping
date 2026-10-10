# Shared, quiet checking and feedback interpretation for student and batch use.
check_code_backend <- function(file, reprex = TRUE, style = TRUE,
                               execution_dir = NULL, timeout = 600,
                               profile = list(), report_dir = NULL,
                               allow_package_install = FALSE) {
  check_code_arguments(file, reprex, style, execution_dir, timeout)
  if (is.null(file) || !file.exists(file) || dir.exists(file) ||
      file.access(file, 4) != 0) {
    stop("Supply a readable code file.")
  }
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  extension <- tolower(tools::file_ext(file))
  if (!extension %in% c("r", "rmd", "qmd")) {
    stop("The submission must be an R, R Markdown, or Quarto file.")
  }
  profile <- code_feedback_profile(profile)
  result <- list(
    file = file, execution_dir = check_execution_directory(file, execution_dir),
    execution = list(status = "skipped", events = list()),
    lints = empty_check_lints(), failures = list(), report = NULL,
    versions = c(check_dependency_versions(), policy = "6"), profile = profile,
    timeout = timeout,
    issues = list(), checks = list(execution = reprex, style = style),
    scope = if (extension == "r") "R script" else
      paste("Enabled R chunks only; inline R, disabled chunks, child documents",
            "and full rendering are not checked.")
  )
  result$submitted_code <- readLines(file, warn = FALSE, encoding = "UTF-8")
  # Record parsing separately: a parse failure does not mean execution ran.
  result$syntax <- list(status = "not_checked", message = NULL)
  if (reprex || style) {
    extracted <- tryCatch(read_check_code(file, extension), error = function(cnd) NULL)
    if (!is.null(extracted)) {
      result$syntax <- tryCatch({
        parse(text = extracted)
        list(status = "valid", message = NULL)
      }, error = function(cnd) {
        message <- conditionMessage(cnd)
        location <- regmatches(message, regexec("^.*?:([0-9]+):([0-9]+):", message))[[1]]
        list(status = "error", message = message,
             line = if (length(location)) as.integer(location[2]) else NA_integer_,
             column = if (length(location)) as.integer(location[3]) else NA_integer_)
      })
    } else {
      result$failures$source <- "The checker could not extract the R code."
    }
  }
  if (is.null(report_dir)) report_dir <- tempfile("check-code-")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(report_dir)) stop("Could not create the checking directory.")
  on.exit(unlink(list.files(report_dir, pattern = "^event-", full.names = TRUE)),
          add = TRUE)
  if (reprex && result$syntax$status == "valid") {
    result$execution <- tryCatch({
      rlang::check_installed(c("callr", "evaluate", "lintr"),
                             version = c("3.7.0", "1.0.0", "3.4.0"))
      preflight <- code_course_issues(file, profile)
      installs <- if (allow_package_install) list() else
        Filter(function(x) x$id == "course.package_install", preflight)
      diagnostics <- Filter(function(x) x$id == "course.checker_in_script", preflight)
      if (length(installs) || length(diagnostics)) {
        result$course_issues <- c(installs, diagnostics)
        if (length(installs)) result$failures$preflight <- paste(
            "Execution was skipped because the code contains package installation",
            "or update calls. Remove them and check again.")
        list(status = "skipped", events = list(), reason =
          if (length(diagnostics)) "Run check_code() only in the console; remove it from the submitted code before executing the check." else
            result$failures$preflight)
      } else {
        run_check_code(read_check_code(file, extension), file,
                       result$execution_dir, timeout, report_dir)
      }
    }, error = function(cnd) {
      list(status = "failed", events = read_check_events(report_dir),
           message = conditionMessage(cnd))
    })
    if (result$execution$status == "failed") {
      result$failures$execution <- result$execution$message
    }
  }
  if (reprex && result$syntax$status == "error") {
    result$execution$reason <- "Execution was not attempted because the code has a syntax error."
  }
  spatial_failures <- Filter(function(e) e$kind == "spatial_checker", result$execution$events)
  if (length(spatial_failures)) result$failures$spatial <- paste(
    vapply(spatial_failures, function(e) e$text, character(1)), collapse = "\n")
  if (style) {
    tryCatch({
      rlang::check_installed(c("lintr", "cyclocomp"),
                             version = c("3.4.0", NA_character_))
      result$lints <- lint_check_code(file)
      result$course_issues <- code_course_issues(file, profile)
    }, error = function(cnd) {
      result$failures$style <<- conditionMessage(cnd)
    })
  }
  if (allow_package_install) {
    result$course_issues <- Filter(function(x) x$id != "course.package_install",
                                   result$course_issues)
  }
  result$issues <- interpret_code_feedback(result)
  if (reprex || style) {
    result$report <- file.path(report_dir, "report.txt")
    write_check_report(result)
  }
  result
}

# Profiles are data, never callbacks executed as part of checking.
code_feedback_profile <- function(profile = list()) {
  defaults <- list(name = "Course code checks", expected_extension = NULL,
                   required_declaration = NULL, text_output = "review")
  if (!is.list(profile) || (length(profile) &&
      (is.null(names(profile)) || any(!nzchar(names(profile))) ||
       anyDuplicated(names(profile))))) stop("profile must be a named list.")
  if (length(setdiff(names(profile), names(defaults)))) {
    stop("Unknown exercise profile setting.")
  }
  defaults[names(profile)] <- profile
  scalar <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(x)
  if (!scalar(defaults$name) || !scalar(defaults$text_output) ||
      !defaults$text_output %in% c("review", "allow")) {
    stop("Supply a profile name and text_output = 'review' or 'allow'.")
  }
  for (key in c("expected_extension", "required_declaration")) {
    if (!is.null(defaults[[key]]) && !scalar(defaults[[key]])) {
      stop(paste(key, "must be NULL or a nonempty string."))
    }
  }
  if (!is.null(defaults$expected_extension)) {
    defaults$expected_extension <- tolower(sub("^\\.", "",
                                               defaults$expected_extension))
    if (!defaults$expected_extension %in% c("r", "qmd", "rmd")) {
      stop("expected_extension must be R, qmd or Rmd.")
    }
  }
  defaults
}

code_issue <- function(id, category, message, action, line = NA_integer_,
                       column = NA_integer_, evidence = "") {
  if (!length(evidence) || is.na(evidence)) evidence <- ""
  list(id = id, category = category, message = message, action = action,
       line = as.integer(line), column = as.integer(column), evidence = evidence)
}

code_course_issues <- function(file, profile) {
  lines <- read_check_code(file, tolower(tools::file_ext(file)))
  expressions <- tryCatch(parse(text = lines, keep.source = TRUE),
                          error = function(cnd) NULL)
  filename_issue <- code_file_name_issue(file)
  issues <- c(if (is.null(filename_issue)) list() else list(filename_issue),
              code_comment_issues(file))
  # Inspect parsed calls, so strings, comments and disabled chunks do not match.
  if (!is.null(expressions)) {
    pd <- utils::getParseData(expressions)
    if (is.null(pd)) pd <- data.frame(token = character(), text = character(),
                                      line1 = integer(), col1 = integer())
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                  pd$text %in% c("install.packages", "update.packages",
                                 "p_install", "install_github",
                                 "install_version"), , drop = FALSE]
    for (i in seq_len(nrow(calls))) {
      issues[[length(issues) + 1L]] <- code_issue(
        "course.package_install", "correctness",
        "This code contains a package installation or update call.",
        "Install packages from the console before running your submission.",
        calls$line1[i], calls$col1[i], lines[calls$line1[i]])
    }
    loaders <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                    pd$text %in% c("library", "require", "p_load"), , drop = FALSE]
    p_loads <- which(loaders$text == "p_load")
    wrong_method <- which(loaders$text %in% c("library", "require"))
    repeated_load <- if (length(p_loads) > 1L) p_loads[-1L] else integer()
    for (i in c(wrong_method, repeated_load)) {
      issues[[length(issues) + 1L]] <- code_issue(
        "course.package_loading", "style",
        "Load all packages in a single pacman::p_load() call, or use :: to call functions.",
        paste("Combine packages in one pacman::p_load() call at the start of the script,",
              "or call functions as package::function(). Remove library(), require()",
              "and additional p_load() calls."),
        loaders$line1[i], loaders$col1[i], lines[loaders$line1[i]])
    }
    issues <- c(issues, code_inspection_issues(pd, lines))
    diagnostics <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "check_code", , drop = FALSE]
    for (i in seq_len(nrow(diagnostics))) {
      issues[[length(issues) + 1L]] <- code_issue(
        "course.checker_in_script", "execution",
        "check_code() must only be run in the console; it must not be included in a submitted script or document.",
        paste("This diagnostic call prevents execution checks: none of the code could be run or checked for runtime issues.",
              "Static checks may still identify style or formatting issues.",
              "Remove this diagnostic call from the script or document to avoid interactive prompts or recursive checking, then check again."),
        diagnostics$line1[i], diagnostics$col1[i], lines[diagnostics$line1[i]])
    }
  }
  if (!is.null(profile$required_declaration)) {
    # A required declaration must occur in a comment, not an executable string.
    comments <- if (is.null(expressions)) character() else {
      pd <- utils::getParseData(expressions)
      pd$text[pd$token == "COMMENT"]
    }
    if (!any(grepl(profile$required_declaration, comments, fixed = TRUE))) {
      issues[[length(issues) + 1L]] <- code_issue(
        "course.declaration_missing", "requirements",
        "The required declaration was not found in an R comment.",
        paste("Add the declaration required for this exercise:",
              profile$required_declaration))
    }
  }
  code_order_issues(issues)
}

# Show the problem that prevents execution before any earlier warnings.
code_order_issues <- function(issues) {
  if (!length(issues)) return(issues)
  priority <- vapply(issues, function(x) {
    if (x$id == "syntax.error") 0L else
      if (x$id %in% c("runtime.error", "runtime.environment", "course.checker_in_script")) 1L else 2L
  }, integer(1))
  issues[order(priority, seq_along(issues))]
}

interpret_code_feedback <- function(result) {
  issues <- result$course_issues
  if (is.null(issues)) issues <- list()
  add <- function(x) issues[[length(issues) + 1L]] <<- x
  if (identical(result$syntax$status, "error")) {
    add(code_issue("syntax.error", "execution", result$syntax$message,
      paste("Because of this syntax error, none of the code could be run or checked for runtime issues.",
            "Static checks may still identify style or formatting issues. Fix this syntax error, then check again."),
      result$syntax$line, result$syntax$column))
  }
  extension <- tolower(tools::file_ext(result$file))
  expected <- result$profile$expected_extension
  if (!is.null(expected) && extension != expected) {
    add(code_issue("submission.format", "requirements",
                   paste("This exercise requires a", paste0(".", expected), "file."),
                   "Submit the required file format."))
  }
  source <- ""
  tile_failure_line <- NA_integer_
  crs_findings <- character()
  for (event in code_feedback_events(result)) {
    if (event$kind == "source") source <- event$text
    if (event$kind == "crs") {
      issue <- code_crs_issue(event, source)
      # Plotting can transform the same geometry repeatedly. Preserve every
      # observation in the log but give one finding per target/outcome/expression.
      key <- paste(event$line, event$details$epsg, event$details$name,
                   event$details$status, event$details$reason, sep = "\034")
      if (!is.null(issue) && !key %in% crs_findings) {
        add(issue)
        crs_findings <- c(crs_findings, key)
      }
    }
    if (event$kind == "message" && grepl("could not be loaded for type", event$text,
                                          fixed = TRUE)) tile_failure_line <- event$line
    if (event$kind %in% c("error", "warning")) {
      transport_failure <- event$kind == "error" && (grepl(
        "Could not resolve host|Failed to connect|SSL certificate|Timeout was reached",
        event$text, ignore.case = TRUE) || identical(event$line, tile_failure_line))
      add(code_issue(if (transport_failure) "runtime.environment" else
                       paste0("runtime.", event$kind),
                     if (transport_failure) "environment" else "execution",
                     event$text, if (transport_failure)
                       paste("Check network access and the external service, then rerun.",
                             code_runtime_limit(event$line), "This is not automatically a student error.") else
                       if (event$kind == "error")
                       paste(code_runtime_limit(event$line), "Fix this error, then check again.") else
                       if (isTRUE(event$point_geometry) && grepl("st_point_on_surface", event$text, fixed = TRUE))
                         paste("This warning came from point geometry. The point locations are unchanged;",
                               "a CRS change is not needed solely to address this label warning.",
                               "Check the map visually. The original warning is retained in the log.") else
                         "Review this warning and correct its cause where necessary.",
                     event$line, evidence = source))
    }
    # Quarto text can be deliberate document content. It is not automatically
    # flagged, and chunk display options are not reproduced by this backend.
    download_receipt <- event$kind == "output" &&
      startsWith(trimws(event$text), "<httr2_response>") &&
      grepl("req_perform", source, fixed = TRUE) &&
      grepl("\\bpath\\s*=", source)
    if (extension == "r" && result$profile$text_output == "review" &&
        event$kind == "output" && nzchar(trimws(event$text)) && !download_receipt &&
        !any(vapply(issues, function(i) i$id == "course.inspection_in_script" &&
          identical(i$line, event$line), logical(1)))) {
      add(code_issue("runtime.text_output", "output",
                     "This expression printed text. Check whether it is needed in the final script.",
                     paste("Keep diagnostic inspection in the console unless the exercise",
                           "requires this output; retain code that produces required maps or charts."),
                     event$line, evidence = event$text))
    }
    if (event$kind == "message" && nzchar(trimws(event$text))) {
      # Setup/import information is retained in the event log, but not treated
      # as unnecessary output. Unknown messages are explicitly review prompts.
      informational <- grepl(
        paste0("^(\u2139[[:space:]]*|i[[:space:]]+)?(Attaching (package|core tidyverse)|\u2500\u2500|Rows: [0-9]|Columns: [0-9]|",
               "Column specification|Delimiter:|Use `spec\\(|Specify the column types|",
               "Linking to GEOS|Loading required (package|namespace):|Registered S3 method|",
               "The following objects? (is|are) masked (from|by)|here\\(\\) starts at)"),
        trimws(event$text))
      if ((!informational || extension != "r") && result$profile$text_output == "review") {
        add(code_issue("runtime.message", "output",
                       trimws(event$text),
                       if (extension == "r")
                         "Inspect the message before deciding whether any code changes are needed." else
                         paste("Inspect the message before deciding whether any code changes are needed.",
                               "Ensure raw R messages do not appear in the rendered document.",
                               if (extension == "qmd")
                                 "For routine messages, use a suitable quiet option or #| message: false." else
                                 "For routine messages, use a suitable quiet option or the chunk option message = FALSE.",
                               "This check captures R execution; it does not render the document or reproduce chunk display options."),
                       event$line, evidence = source))
      }
    }
  }
  if (result$execution$status == "timeout") {
    last <- Filter(function(e) e$kind == "source", result$execution$events)
    last <- if (length(last)) last[[length(last)]] else list(line = NA_integer_, text = "")
    seconds <- if (is.null(result$timeout)) "the configured" else paste0(result$timeout, "-second")
    add(code_issue("runtime.timeout", "incomplete",
                   paste("Execution reached", seconds, "time limit; the remaining code was not checked."),
                   if (grepl("annotation_map_tile", last$text, fixed = TRUE))
                     paste("The last expression requested map tiles. Check the map extent and zoom level,",
                           "network access and the tile service, then rerun with an appropriate time limit.",
                           "A timeout alone does not establish a mistake in your code.") else
                     "Review the last expression for slow operations or downloads, then rerun with an appropriate time limit.",
                   last$line, evidence = last$text))
  }
  for (stage in names(result$failures)) {
    if (stage == "preflight") {
      add(code_issue("execution.blocked", "incomplete",
        "Execution was blocked because the code contains a package-installation or update call.",
        "Install packages from the console, remove the installation call from your submission, then check again."))
      next
    }
    add(code_issue(paste0("checker.", stage), "incomplete",
                   paste(stage, "checking could not be completed."),
                   "The assessor should investigate the checking environment and rerun.",
                   evidence = result$failures[[stage]]))
  }
  for (i in seq_len(nrow(result$lints))) {
    lint <- result$lints[i, ]
    if (identical(result$syntax$status, "error") && lint$linter == "error") next
    missing_argument <- grepl("Missing argument [0-9]+ in function call", lint$message)
    evidence <- lint$line
    location <- lint$line_number
    if (missing_argument) {
      extract <- code_enclosing_call(result$file, location, lint$column_number)
      if (!is.null(extract)) {
        evidence <- extract$text
        location <- extract$line
      }
    }
    message <- lint$message
    if (lint$linter %in% c("indentation_linter", "line_length_linter")) {
      message <- if (lint$linter == "indentation_linter")
        "Use consistent indentation." else "Keep code lines within 80 characters."
      evidence <- paste0("Line ", location, ": ", lint$message, "\n", evidence)
    }
    add(code_issue(paste0("lint.", lint$linter), lint$category, message,
                   if (lint$category == "environment")
                     "Check the required package installation in the execution environment." else
                     paste("Revise this code using the course style guidance, then check again.",
                           if (missing_argument) "Check the function does not have an extra comma after the last argument." else ""),
                   location, if (missing_argument) NA_integer_ else lint$column_number, evidence))
  }
  code_order_issues(issues)
}

report_code_issues <- function(issues) {
  if (!length(issues)) return(invisible(NULL))
  cli::cli_h2("Feedback to review")
  keys <- vapply(issues, function(i) paste(i$id, i$message, i$action, sep = "\034"), character(1))
  for (key in unique(keys)) {
    group <- issues[keys == key]
    issue <- group[[1L]]
    lines <- sort(unique(vapply(group, `[[`, integer(1), "line")))
    lines <- lines[!is.na(lines)]
    location <- if (!length(lines)) "" else paste0(
      if (length(lines) == 1L) "Line " else "Lines ", paste(lines, collapse = ", "), ": ")
    cli::cli_text("{location}{issue$message}")
    cli::cli_text("{issue$action}")
  }
  invisible(NULL)
}

# Filter presentation only: the complete condition log remains available.
code_feedback_events <- function(result) {
  Filter(function(event) !(tolower(tools::file_ext(result$file)) == "r" &&
    event$kind == "warning" && grepl(
      "st_point_on_surface may not give correct results for longitude/latitude data",
      event$text, fixed = TRUE)), result$execution$events)
}

code_enclosing_call <- function(file, line, column) {
  lines <- read_check_code(file, tolower(tools::file_ext(file)))
  parsed <- tryCatch(parse(text = lines, keep.source = TRUE), error = function(e) NULL)
  pd <- utils::getParseData(parsed)
  if (is.null(pd)) return(NULL)
  calls <- pd[pd$token == "'('", , drop = FALSE]
  spans <- pd[pd$id %in% calls$parent & pd$line1 <= line & pd$line2 >= line, , drop = FALSE]
  spans <- spans[spans$line1 < line | spans$col1 <= column, , drop = FALSE]
  spans <- spans[spans$line2 > line | spans$col2 >= column, , drop = FALSE]
  if (!nrow(spans)) return(NULL)
  span <- spans[order(spans$line2 - spans$line1, spans$col2 - spans$col1), ][1L, ]
  text <- lines[seq.int(span$line1, span$line2)]
  text[length(text)] <- substr(text[length(text)], 1L, span$col2)
  text[1L] <- substring(text[1L], span$col1)
  list(line = span$line1, text = paste(text, collapse = "\n"))
}

code_inspection_issues <- function(pd, lines) {
  inspectors <- c("view", "View", "excel_sheets", "head", "tail", "slice",
                  "slice_head", "slice_tail", "glimpse")
  symbols <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% inspectors, , drop = FALSE]
  issues <- list()
  for (i in seq_len(nrow(symbols))) {
    # A call is the parent of the function-name expression (also for :: calls).
    name <- pd[pd$id == symbols$parent[i], , drop = FALSE]
    call <- pd[pd$id == name$parent, , drop = FALSE]
    if (!nrow(call)) next
    current <- call
    consumed <- FALSE
    while (nrow(current) && current$parent != 0L) {
      parent <- pd[pd$id == current$parent, , drop = FALSE]
      siblings <- pd[pd$parent == parent$id, , drop = FALSE]
      opening <- siblings[siblings$token == "'('", , drop = FALSE]
      # Parenthesised groups and control-flow bodies do not consume a value.
      enclosing_call <- nrow(opening) && any(siblings$token == "expr" &
        (siblings$line2 < opening$line1[1L] | (siblings$line2 == opening$line1[1L] &
          siblings$col2 < opening$col1[1L])))
      if (any(siblings$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN", "FUNCTION")) ||
          enclosing_call) {
        consumed <- TRUE
        break
      }
      pipe <- siblings[siblings$text %in% c("|>", "%>%"), , drop = FALSE]
      # Determine the left operand by its position, independent of parse IDs.
      if (nrow(pipe) && (current$line2 < pipe$line1[1L] ||
          (current$line2 == pipe$line1[1L] && current$col2 < pipe$col1[1L]))) {
        consumed <- TRUE
        break
      }
      current <- parent
    }
    if (!consumed) issues[[length(issues) + 1L]] <- code_issue(
      "course.inspection_in_script", "style",
      paste0(symbols$text[i], "() is used to inspect data without its result being saved or used by another function."),
      "Run temporary data-inspection code in the console. Keep it in the submission only when its result is assigned to an object or passed to another function.",
      call$line1, call$col1, paste(lines[seq.int(call$line1, call$line2)], collapse = "\n"))
  }
  issues
}

code_feedback_status <- function(result) {
  ids <- vapply(result$issues, `[[`, character(1), "id")
  if (any(ids %in% c("syntax.error", "runtime.error", "runtime.environment", "course.checker_in_script")) ||
      result$execution$status == "error") return("error")
  if (!result$execution$status %in% c("success", "error")) return("skipped")
  if (any(ids %in% c("runtime.warning", "spatial.crs_area"))) return("warning")
  if (any(ids %in% c("runtime.message", "runtime.text_output"))) return("message")
  if (length(ids)) return("style issues")
  "success"
}

# Check only the submitted basename: assessor directory names are irrelevant.
# The single separator before the extension is required and exempt.
code_file_name_issue <- function(file) {
  name <- basename(file)
  stem <- tools::file_path_sans_ext(name)
  if (grepl("^[a-z0-9]+(_[a-z0-9]+)*$", stem)) return(NULL)
  extension <- tools::file_ext(name)
  code_issue("course.file_name", "style",
    "Use snake case for the submitted file name.",
    paste0("Use lower-case letters (a-z), numbers and single underscores between words. ",
           "Avoid spaces, extra periods and other punctuation, and do not start or end ",
           "the name with an underscore. Keep the period before the .", extension,
           " extension. For example, exercise_01.", extension,
           ". Rename the file and update any references to it in your code."),
    evidence = name)
}

# A srcfile retains tokens preceding a syntax error. Quarto extraction preserves
# original line numbers and removes prose, disabled chunks and chunk options.
code_feedback_parse_data <- function(file) {
  lines <- read_check_code(file, tolower(tools::file_ext(file)))
  source <- srcfilecopy(file, lines)
  tryCatch(parse(text = lines, srcfile = source, keep.source = TRUE),
           error = function(e) NULL)
  list(lines = lines, tokens = utils::getParseData(source))
}

code_name_lint_messages <- function(lints, file) {
  rows <- which(lints$linter == "object_name_linter")
  if (!length(rows)) return(lints)
  pd <- code_feedback_parse_data(file)$tokens
  names <- vapply(rows, function(i) {
    token <- pd[pd$terminal & pd$line1 == lints$line_number[i] &
                  pd$col1 == lints$column_number[i], , drop = FALSE]
    if (!nrow(token)) return(trimws(substring(lints$line[i], lints$column_number[i])))
    text <- token$text[1L]
    if (token$token[1L] == "STR_CONST") {
      literal <- tryCatch(parse(text = text)[[1L]], error = function(e) NULL)
      if (is.character(literal)) return(literal)
    }
    sub("`$", "", sub("^`", "", text))
  }, character(1))
  names <- unique(names)
  lints$message[rows] <- paste0("Variable names should use snake_case. Rename ",
    if (length(names) == 1L) "this object: " else "these objects: ",
    paste(names, collapse = ", "), ".")
  lints
}

code_comment_issues <- function(file) {
  parsed <- code_feedback_parse_data(file)
  pd <- parsed$tokens
  if (is.null(pd)) return(list())
  comments <- pd[pd$token == "COMMENT", , drop = FALSE]
  comments <- comments[order(comments$line1, comments$col1), , drop = FALSE]
  issues <- list()
  previous <- NULL
  for (i in seq_len(nrow(comments))) {
    comment <- comments[i, ]
    text <- comment$text
    # These markers have executable/documentation meanings, not prose syntax.
    if (grepl("^#([!'|]|[[:space:]]*nolint\\b)", text, perl = TRUE)) {
      previous <- NULL
      next
    }
    content <- trimws(sub("^#+", "", text))
    if (!nzchar(content)) { previous <- NULL; next }
    if (!grepl("^# [^[:space:]]", text)) {
      issues[[length(issues) + 1L]] <- code_issue("course.comment_spacing", "style",
        "Put a single space after # in code comments.",
        "Begin each prose comment with # followed by one space, for example # Load the data.",
        comment$line1, comment$col1, parsed$lines[comment$line1])
    }
    standalone <- !nzchar(trimws(substr(parsed$lines[comment$line1], 1L, comment$col1 - 1L)))
    continuation <- standalone && !is.null(previous) && previous$standalone &&
      comment$line1 == previous$line + 1L &&
      !grepl("[.!?:]$|[-=]{3,}$", previous$content)
    # Case checks concern prose, not URLs or identifiers such as st_as_sf().
    technical <- grepl("^(https?://|[[:alnum:]_.]+(::|_|\\(|\\$))", content)
    lower_start <- !continuation && !technical && grepl("^[[:lower:]]", content)
    prose <- gsub("`[^`]*`|https?://[^[:space:]]+|[[:alnum:].]*[_:][[:alnum:]_:]*|[[:alnum:]_.]+\\([^)]*\\)", "", content)
    words <- regmatches(prose, gregexpr("[[:alpha:]]+", prose))[[1L]]
    common <- c("a", "an", "and", "as", "at", "by", "convert", "create", "data",
                "download", "for", "from", "in", "is", "load", "of", "on", "plot",
                "the", "this", "to", "transform", "using", "with")
    # Preserve proper nouns and acronyms; flag clear prose in title/all caps.
    title_words <- if (length(words) > 1L) words[-1L] else character()
    title_case <- any(title_words %in% paste0(toupper(substr(common, 1L, 1L)), substring(common, 2L)))
    shouting <- length(words) >= 2L && any(tolower(words) %in% common) &&
      all(words == toupper(words))
    if (lower_start || title_case || shouting) {
      issues[[length(issues) + 1L]] <- code_issue("course.comment_sentence_case", "style",
        "Use sentence case in code comments.",
        paste("Start a prose sentence with a capital letter and use normal sentence capitalisation,",
              "rather than title case or ALL CAPITALS. Keep proper nouns, acronyms and code identifiers unchanged.",
              "Check these suggestions manually; automated checks cannot fully interpret comment prose."),
        comment$line1, comment$col1, parsed$lines[comment$line1])
    }
    previous <- list(line = comment$line1, standalone = standalone, content = content)
  }
  issues
}

code_runtime_limit <- function(line) {
  paste(if (is.null(line) || is.na(line)) "Execution stopped in this code block." else
    paste0("Execution stopped in the code block beginning on line ", line, "."),
    "None of the code below this block could be checked for runtime issues.",
    "Static checks may still identify style or formatting issues in that code.")
}
