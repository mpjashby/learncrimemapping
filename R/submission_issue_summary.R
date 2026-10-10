# Summarise exact rule/message pairs, counting each participant only once.
# Use only the current manifest, even if results.rds contains older submissions.
write_submission_issue_summary <- function(manifest, results, file, profile) {
  e <- feedback_html_escape
  groups <- list()
  missing <- 0L
  for (i in seq_len(nrow(manifest))) {
    participant <- manifest$participant[i]
    result <- results[[participant]]
    if (is.null(result)) {
      missing <- missing + 1L
      next
    }
    issues <- result$issues
    source <- ""
    for (event in result$execution$events) {
      if (event$kind == "source") source <- event$text
      # Include informational messages retained only in the individual log.
      if (event$kind == "message" && nzchar(trimws(event$text)) &&
          !any(vapply(issues, function(x) x$id == "runtime.message" &&
            identical(trimws(x$message), trimws(event$text)), logical(1)))) {
        issues[[length(issues) + 1L]] <- code_issue("runtime.message", "output",
          trimws(event$text), "Inspect this captured message.", event$line,
          evidence = source)
      }
    }
    for (issue in issues) {
      message <- issue$message
      if (!is.null(result$execution_dir) && nzchar(result$execution_dir)) {
        message <- gsub(result$execution_dir, "<workspace>", message, fixed = TRUE)
      }
      key <- paste(issue$category, issue$id, message, sep = "\034")
      group <- groups[[key]]
      if (is.null(group)) group <- list(issue = issue, message = message,
                                       participants = character(), example = NULL)
      group$participants <- unique(c(group$participants, participant))
      # Prefer evidence retained with the issue: runtime evidence contains the
      # complete expression, whereas output evidence can be captured text.
      code <- if (issue$id == "runtime.text_output") "" else issue$evidence
      if (!nzchar(code) && !is.na(issue$line) &&
          issue$line > 0L && issue$line <= length(result$submitted_code)) {
        code <- paste(result$submitted_code[seq.int(issue$line,
          min(length(result$submitted_code), issue$line + 2L))], collapse = "\n")
      }
      example <- list(code = code, participant = participant,
                      report = manifest$report[i], line = issue$line)
      if (is.null(group$example) ||
          (!nzchar(group$example$code) && nzchar(code))) group$example <- example
      groups[[key]] <- group
    }
  }
  counts <- vapply(groups, function(x) length(x$participants), integer(1))
  if (length(groups)) groups <- groups[order(-counts, names(groups))]
  blocks <- vapply(groups, function(group) {
    x <- group$example
    link <- paste0(x$report, if (!is.na(x$line) && x$line > 0L)
      paste0("#code-line-", x$line) else "")
    paste0("<article class='issue'><h2>", e(group$message), "</h2>",
      "<p><strong>", length(group$participants), " submissions</strong> \u00b7 ",
      e(group$issue$category), " \u00b7 Rule: <code>", e(group$issue$id), "</code></p>",
      if (nzchar(x$code)) paste0("<pre><code>", e(x$code), "</code></pre>") else
        "<p>No code example is available for this issue (for example, a submission requirement or an older saved result).</p>",
      "<p>Source: <a href='", e(link), "'>", e(x$participant),
      " \u2014 individual feedback report", if (!is.na(x$line)) paste0(", line ", x$line),
      "</a></p></article>")
  }, character(1))
  writeLines(c(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; style-src 'unsafe-inline'\">",
    "<title>Most common submission issues</title>",
    paste0("<style>", feedback_html_css(), "</style></head><body><main>"),
    "<h1>Most common submission issues</h1>",
    paste0("<p>", e(profile$name), " \u00b7 ", nrow(manifest), " submissions</p>"),
    "<p><a href='index.html'>Submission index</a></p>",
    "<p>Issues are ranked by the number of distinct submissions reporting the same rule and message. Repeated occurrences within a submission count once. Suggestions and review prompts are included; these counts are not marks. Only checks that ran can contribute findings.</p>",
    if (missing) paste0("<p>Structured results are unavailable for ", missing,
      " submissions; their existing reports are not included in these counts.</p>"),
    if (!length(groups)) "<p>No issues were recorded in the available results.</p>" else blocks,
    "</main></body></html>"), file, useBytes = TRUE)
  invisible(file)
}
