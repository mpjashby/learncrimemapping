#' Write a portable HTML code-feedback report
#'
#' @param result Structured results returned by `check_code()` or the batch
#'   checker, including the shared `issues` list.
#' @param file Destination HTML file.
#' @param participant Optional Moodle participant identifier.
#' @return Invisibly, the absolute report path.
#' @keywords internal
write_code_feedback <- function(result, file, participant = NULL) {
  if (!is.list(result) || is.null(result$execution$status) ||
      is.null(result$issues)) stop("Supply structured code-checking results.")
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("Supply an HTML destination path.")
  }
  escape <- function(x) {
    # Do not expose the assessor's local workspace path in distributed logs.
    if (!is.null(result$execution_dir) && nzchar(result$execution_dir)) {
      x <- gsub(result$execution_dir, "<workspace>", x, fixed = TRUE)
    }
    feedback_html_escape(x)
  }
  identity <- if (is.null(participant)) basename(result$file) else participant
  identity <- sub("^Participant_", "Participant ", identity)
  title <- paste("Code feedback:", identity)
  categories <- c(requirements = "Submission requirements",
                  execution = "Potential problems",
                  incomplete = "Checks incomplete", environment = "Environment",
                  correctness = "Potential correctness problems",
                  output = "Output to review", style = "Potential style and formatting issues")
  blocks <- character()
  for (category in names(categories)) {
    selected <- code_order_issues(Filter(function(x) x$category == category, result$issues))
    if (!length(selected)) next
    blocks <- c(blocks, paste0("<section id='issues-", category, "'><h2>", categories[[category]], "</h2>"))
    # Group only identical rules/messages/actions; preserve every occurrence.
    keys <- vapply(selected, function(x) paste(x$id, x$message, x$action,
                                              sep = "\034"), character(1))
    for (key in unique(keys)) {
      group <- selected[keys == key]
      issue <- group[[1L]]
      locations <- unique(vapply(group, function(x) {
        if (x$id == "course.file_name") "Submitted file name" else
          if (is.na(x$line)) "Location unavailable" else {
          paste0("Line ", x$line, if (!is.na(x$column)) paste0(", column ", x$column))
        }
      }, character(1)))
      examples <- unique(vapply(group, function(x) x$evidence, character(1)))
      examples <- examples[nzchar(examples)]
      label <- if (issue$id %in% c("runtime.error", "runtime.environment", "syntax.error", "course.checker_in_script"))
        "<span class='condition-error'>ERROR: </span>" else
        if (issue$id == "runtime.warning") "<span class='condition-warning'>WARNING: </span>" else
          if (issue$id == "runtime.message") "Message: " else ""
      blocks <- c(blocks, paste0(
        "<article class='issue'><p><strong>", label, escape(issue$message), "</strong></p>",
        "<p class='locations'>", escape(paste(locations, collapse = "; ")), "</p>",
        "<p>", escape(issue$action), "</p>",
        if (length(examples)) paste0("<details><summary>Code or captured output</summary><pre>",
                                    escape(paste(examples, collapse = "\n\n")),
                                    "</pre></details>") else "",
        "<small>Rule: ", escape(issue$id), "</small></article>"))
    }
    blocks <- c(blocks, "</section>")
  }
  if (!length(result$issues)) {
    blocks <- "<p class='clean'>No issues were detected by the checks that ran. Manual review is still needed.</p>"
  }
  blocks <- c(blocks, feedback_submitted_code(result))
  # Make previews portable: no dependence on temporary plot files after writing.
  plots <- Filter(function(x) x$kind == "plot" && file.exists(x$plot),
                  result$execution$events)
  if (length(plots)) {
    blocks <- c(blocks, "<section><h2>Visualisations produced by your code</h2>")
    for (event in plots) {
      raw <- readBin(event$plot, "raw", n = file.info(event$plot)$size)
      blocks <- c(blocks, paste0("<figure><img alt='Captured plot near line ",
                                 event$line, "' src='data:image/png;base64,",
                                 jsonlite::base64_enc(raw), "'><figcaption>Expression near line ",
                                 event$line, "</figcaption></figure>"))
    }
    blocks <- c(blocks, "</section>")
  }
  events <- Filter(function(x) x$kind %in% c("message", "output", "warning", "error"),
                   result$execution$events)
  log <- vapply(events, function(x) paste0("[", x$kind, ", line ", x$line,
                                          "]\n", x$text), character(1))
  html <- c(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; img-src data:; style-src 'unsafe-inline'\">",
    paste0("<title>", escape(title), "</title>"),
    paste0("<style>", feedback_html_css(), "</style></head><body><main>"),
    paste0("<header><p class='eyebrow'>SECU0005 Crime Mapping</p><h1>", escape(title),
           "</h1><p>Submitted file name: <code>", escape(basename(result$file)),
           "</code></p><p>Submitted file type: ", escape(feedback_file_type(result$file)),
           "</p></header>"),
    "<p>This report shows the results of automated checks run on your code. The automated checks cannot check every skill required for crime mapping, so your assessment feedback will be based on a combination of automated checks like this and manual review of the code and the output it produces. Make sure you understand each of the points below so that you can use this feedback to help with future exercises.</p>",
    "<p>When this feedback mentions a line in your code file, if the issue is with a block of code that is spread across multiple lines the stated line number will be the first line of the code block.</p>",
    feedback_initial_checks(result),
    if (tolower(tools::file_ext(result$file)) %in% c("qmd", "rmd"))
      paste0("<p>", escape(result$scope), "</p>"),
    blocks,
    if (length(log)) paste0("<details><summary>Complete captured text log</summary><pre>",
                            escape(paste(log, collapse = "\n\n")), "</pre></details>"),
    paste0("<footer><p>Profile: ", escape(result$profile$name),
           "</p><details><summary>Checker versions</summary><pre>",
           escape(paste(names(result$versions), result$versions, collapse = "\n")),
           "</pre></details></footer></main></body></html>")
  )
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

feedback_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

feedback_file_type <- function(file) {
  switch(tolower(tools::file_ext(file)), r = "R script", qmd = "Quarto file",
         rmd = "R Markdown file", "Other file")
}

feedback_initial_checks <- function(result) {
  syntax <- result$syntax
  # Support results saved by earlier package versions without rerunning code.
  if (is.null(syntax)) {
    syntax <- list(status = "not_checked")
    if (file.exists(result$file) &&
        (result$execution$status != "skipped" || nrow(result$lints))) {
      extracted <- tryCatch(read_check_code(result$file,
        tolower(tools::file_ext(result$file))), error = function(cnd) NULL)
      if (!is.null(extracted)) syntax <- tryCatch({
        parse(text = extracted)
        list(status = "valid")
      }, error = function(cnd) list(status = "error"))
    }
  }
  categories <- vapply(result$issues, `[[`, character(1), "category")
  kinds <- vapply(code_feedback_events(result), `[[`, character(1), "kind")
  execution_complete <- result$execution$status %in% c("success", "error") &&
    syntax$status != "error"
  # Preflight prevents diagnostic calls from executing, but the embedded call
  # is still a runtime error for student-facing initial checks.
  embedded_checker <- any(vapply(result$issues, function(x)
    x$id == "course.checker_in_script", logical(1)))
  runtime <- if (syntax$status == "error") "not checked (syntax error)" else
    if ("error" %in% kinds || embedded_checker) "yes" else
      if (execution_complete) "no" else "full check not possible \u2013 check for errors below"
  checked_style <- !is.null(result$checks$style) && isTRUE(result$checks$style)
  if (is.null(result$checks)) checked_style <- result$execution$status != "skipped" ||
    nrow(result$lints) > 0L
  checks <- c(
    "Code free of syntax errors" = switch(syntax$status, valid = "no", error = "yes", "not checked"),
    "Code free of runtime errors" = runtime,
    "Code free of runtime warnings" = if ("warning" %in% kinds) "yes" else
      if (execution_complete) "no" else "full check not possible \u2013 check for errors below",
    "Execution environment checks passed" = if ("environment" %in% categories) "yes" else
      if (execution_complete || checked_style) "no" else "full check not possible \u2013 check for errors below",
    "Code execution stayed within the time limit" = if (result$execution$status == "timeout") "yes" else
      if (execution_complete) "no" else "not checked",
    "Automated checker free of failures" = if (result$execution$status == "failed" ||
      length(setdiff(names(result$failures), "preflight"))) "yes" else
        if (!is.null(result$checks) && !any(unlist(result$checks))) "not checked" else "no"
  )
  rows <- vapply(seq_along(checks), function(i) {
    status <- checks[[i]]
    class <- if (status == "no") "pass" else if (status == "yes") "fail" else "incomplete"
    marker <- switch(class,
      pass = "<span role='img' aria-label='Passed'>\u2705</span>",
      fail = "<span role='img' aria-label='Problem found'>\u274c</span>",
      incomplete = paste0("<span role='img' aria-label='Check incomplete'>\u26aa</span> ",
                          toupper(substr(status, 1L, 1L)), substring(status, 2L)))
    label <- names(checks)[i]
    if (i == 1L && syntax$status == "error") {
      label <- paste0("<a href='#issues-execution'>", label, "</a>")
    }
    paste0("<li class='initial-", class, "'>", label, ": ", marker, "</li>")
  }, character(1))
  paste0("<section><h2>Initial checks</h2><ul class='initial-checks'>",
         paste(rows, collapse = ""), "</ul></section>")
}

feedback_submitted_code <- function(result) {
  if (!tolower(tools::file_ext(result$file)) %in% c("r", "rmd", "qmd")) {
    return(paste0("<section><h2>Submitted code</h2><p>",
                  "The submitted file could not be checked.</p></section>"))
  }
  lines <- result$submitted_code
  if (is.null(lines) && file.exists(result$file) && !dir.exists(result$file)) {
    lines <- tryCatch(readLines(result$file, warn = FALSE, encoding = "UTF-8"),
                      error = function(cnd) NULL)
  }
  if (is.null(lines)) {
    return("<section><h2>Submitted code</h2><p>The submitted code is unavailable.</p></section>")
  }
  # Highlight only genuine R tokens. Everything, including Quarto prose and
  # invalid R, remains escaped text. No scripts or remote assets are required.
  extracted <- if (tolower(tools::file_ext(result$file)) == "r") lines else
    tryCatch(read_check_code(result$file, tolower(tools::file_ext(result$file))),
             error = function(cnd) character())
  pd <- tryCatch(utils::getParseData(parse(text = extracted, keep.source = TRUE)),
                 error = function(cnd) NULL)
  classes <- c(COMMENT = "comment", STR_CONST = "string", NUM_CONST = "number",
               SYMBOL_FUNCTION_CALL = "function", SYMBOL_PACKAGE = "function",
               FUNCTION = "keyword", IF = "keyword", ELSE = "keyword",
               FOR = "keyword", WHILE = "keyword", IN = "keyword",
               REPEAT = "keyword", NEXT = "keyword", BREAK = "keyword",
               NULL_CONST = "keyword")
  if (!is.null(pd)) pd <- pd[pd$terminal & pd$token %in% names(classes), , drop = FALSE]
  rendered <- vapply(seq_along(lines), function(i) {
    line <- lines[i]
    tokens <- if (is.null(pd)) NULL else pd[pd$line1 <= i & pd$line2 >= i, , drop = FALSE]
    content <- ""
    cursor <- 1L
    if (!is.null(tokens) && nrow(tokens) > 0L && !grepl("\t", line, fixed = TRUE)) {
      starts <- ifelse(tokens$line1 == i, tokens$col1, 1L)
      ends <- ifelse(tokens$line2 == i, tokens$col2, nchar(line))
      for (j in order(starts)) {
        start <- starts[j]
        end <- ends[j]
        if (start < cursor || end < start) next
        if (start > cursor) content <- paste0(content,
          feedback_html_escape(substr(line, cursor, start - 1L)))
        content <- paste0(content, "<span class='syntax-", classes[[tokens$token[j]]],
                          "'>", feedback_html_escape(substr(line, start, end)), "</span>")
        cursor <- end + 1L
      }
    }
    if (cursor <= nchar(line)) content <- paste0(content,
      feedback_html_escape(substr(line, cursor, nchar(line))))
    paste0("<span class='code-line' id='code-line-", i, "'>", content, "</span>")
  }, character(1))
  paste0("<section><h2>Submitted code</h2><pre class='submitted-code'><code>",
         paste(rendered, collapse = ""), "</code></pre></section>")
}

feedback_html_css <- function() {
  paste(
    "body{margin:0;background:#f3f5f7;color:#182b35;font:16px/1.6 system-ui,sans-serif}",
    "main{max-width:900px;margin:36px auto;padding:36px;background:white;border-radius:12px}",
    "header{border-bottom:3px solid #137b77;padding-bottom:18px}h1{font-size:30px;line-height:1.25}",
    "h2{font-size:21px;margin-top:32px}.eyebrow,small,.locations{color:#526770}",
    ".eyebrow{letter-spacing:.08em;font-size:12px}",
    "li{padding:12px 8px;border-bottom:1px solid #dde5e9}li p{margin:5px 0}",
    ".initial-checks{list-style:none;padding-left:0}.initial-checks li{padding:2px 0;border-bottom:0}",
    ".issue{margin:24px 0}.issue p{margin:5px 0}.condition-error,.status-error{color:#b42318}.status-error{font-weight:700}.condition-warning,.status-warning{color:#a95000}",
    "pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f3f5f7;padding:16px;font-size:13px}",
    ".submitted-code{padding:12px 0;counter-reset:code-line}.code-line{display:block;padding:0 16px 0 64px;position:relative;min-height:1.6em}",
    ".code-line:before{counter-increment:code-line;content:counter(code-line);position:absolute;left:0;width:48px;text-align:right;color:#61747c;user-select:none}",
    ".syntax-comment{color:#576c61}.syntax-string{color:#965000}.syntax-number{color:#9b3b73}.syntax-function{color:#185aa3}.syntax-keyword{color:#6842a0;font-weight:600}",
    "details{margin:12px 0}summary{cursor:pointer;color:#126d69}img{max-width:100%;height:auto}",
    "table{width:100%;border-collapse:collapse}td,th{text-align:left;padding:10px;border-bottom:1px solid #ddd}",
    "a{color:#126d69}footer{margin-top:36px;border-top:1px solid #ddd;font-size:13px}",
    "@media(max-width:650px){main{margin:0;padding:20px;border-radius:0}h1{font-size:25px}}"
  )
}
