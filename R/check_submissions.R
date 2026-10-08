#' Check a Moodle ZIP of code submissions
#'
#' Uses the same backend and issue interpretation as [check_code()]. Each
#' Moodle participant receives a portable HTML report, including invalid
#' submissions. A batch index, CSV manifest and RDS results are also retained.
#'
#' @param zip Path to a Moodle submission ZIP.
#' @param output_dir A new or empty directory for persistent results.
#' @param profile Named list with `name`, optional `expected_extension`, optional
#'   literal `required_declaration` to find in an R comment, and `text_output`
#'   (`"review"` or `"allow"`). Share the same profile with students.
#' @param workspace_template Optional directory of exercise resources copied
#'   separately for each participant. Startup files must not be included.
#' @param reprex,style,timeout See [check_code()].
#' @param backend Execution backend: `"docker"` (default) restricts each student
#'   to a separate container; `"local"` retains unrestricted local checking.
#' @param container_image Prebuilt Linux image containing this package and the
#'   course dependencies. See `MARKING.md` for build instructions.
#' @param container_memory Docker memory limit for each student.
#' @param container_cpus Maximum CPUs available to each student.
#' @return Invisibly, a list with `manifest`, named `results`, and `index`.
#' @details Docker checking uses fresh, unprivileged containers with a read-only
#'   system filesystem. Only the current student's workspace is mounted from the
#'   host. Web access remains enabled. Packages may be installed into the
#'   student's private library within that workspace, never the host library.
#'   R and system libraries inside the image remain readable. No automatic
#'   fallback to local execution occurs. Local execution is not a sandbox.
#'   Quarto documents have enabled R chunks checked; full rendering is not done.
#'   ZIPs are limited to 5,000 entries and 100 MB of uncompressed content.
#'   Both named and anonymous Moodle folders are supported; report identities
#'   use numeric participant IDs, and original archive paths are retained.
#'   Existing results are never overwritten. Network failures and missing
#'   credentials may need assessor review; they are not automatic penalties.
#' @keywords internal
check_submissions <- function(zip, output_dir, profile = list(),
                              workspace_template = NULL, reprex = TRUE,
                              style = TRUE, timeout = 600,
                              backend = c("docker", "local"),
                              container_image = "learncrimemapping-checker:local",
                              container_memory = "4g", container_cpus = 2) {
  backend <- match.arg(backend)
  container <- NULL
  check_code_arguments(zip, reprex, style, workspace_template, timeout)
  profile <- code_feedback_profile(profile)
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) stop("Supply an output directory.")
  if (!file.exists(zip) || dir.exists(zip)) stop("Supply a readable ZIP file.")
  zip <- normalizePath(zip, winslash = "/", mustWork = TRUE)
  entries <- utils::unzip(zip, list = TRUE)
  if (!nrow(entries) || nrow(entries) > 5000L ||
      any(!is.finite(entries$Length) | entries$Length < 0) ||
      sum(entries$Length) > 100 * 1024^2) {
    stop("The archive is empty or exceeds the 5,000-entry / 100 MB limit.")
  }
  paths <- entries$Name
  unsafe <- grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.?(/|$))", paths) |
    grepl("[[:cntrl:]]", paths)
  if (any(unsafe) || anyDuplicated(tolower(paths))) {
    stop("Unsafe or duplicate archive paths (including case-only differences).")
  }
  # Ignore operating-system metadata, but do not silently ignore extra work.
  keep <- !grepl("(^|/)(__MACOSX|\\.DS_Store)(/|$)", paths)
  paths <- paths[keep]
  folder <- sub("/.*$", "", paths)
  if (any(!grepl("^.+_[0-9]+_assignsubmission_file$", folder))) {
    stop("Unsupported Moodle layout: expected Name_ID_assignsubmission_file or Participant_ID_assignsubmission_file folders.")
  }
  groups <- unique(folder)
  identities <- paste0("Participant_", sub("^.*_([0-9]+)_assignsubmission_file$", "\\1", groups))
  if (anyDuplicated(identities)) {
    stop("Ambiguous Moodle layout: multiple submission folders have the same participant ID.")
  }
  # Only flat submission files are extracted; nested files and links are not
  # allowed to create directory structures through which extraction can escape.
  is_file <- !grepl("/$", paths)
  if (any(is_file & !grepl("^[^/]+/[^/]+$", paths))) {
    stop("Nested submission paths are not supported.")
  }
  if (!is.null(workspace_template)) validate_submission_template(workspace_template)
  if (file.exists(output_dir) && !dir.exists(output_dir)) stop("output_dir is a file.")
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                 no.. = TRUE))) {
    stop("output_dir must be new or empty; previous results will not be overwritten.")
  }
  if (backend == "docker") {
    container <- submission_container_config(container_image, container_memory,
                                              container_cpus)
  } else if (reprex) {
    warning("Local checking is unrestricted: student code can change host files and packages.",
            call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  if (!is.null(workspace_template)) {
    workspace_template <- normalizePath(workspace_template, winslash = "/", mustWork = TRUE)
    if (output_dir == workspace_template ||
        startsWith(output_dir, paste0(workspace_template, "/"))) {
      stop("output_dir must not be inside workspace_template.")
    }
  }
  # Raw code and working files stay separate from the reports to distribute.
  dir.create(file.path(output_dir, "submissions"))
  dir.create(file.path(output_dir, "workspaces"))
  dir.create(file.path(output_dir, "feedback"))
  files <- paths[is_file]
  if (length(files)) {
    utils::unzip(zip, files = files, exdir = file.path(output_dir, "submissions"),
                 unzip = "internal", setTimes = FALSE)
    extracted <- file.path(output_dir, "submissions", files)
    links <- Sys.readlink(extracted)
    if (any(!file.exists(extracted)) || any(!is.na(links) & nzchar(links))) {
      stop("Extraction failed or produced symbolic links; no code was executed.")
    }
  }
  results <- list()
  rows <- list()
  for (group in groups) {
    participant <- identities[match(group, groups)]
    submitted <- paths[folder == group & is_file]
    cli::cli_text("Checking {participant}")
    file <- if (length(submitted) == 1L)
      file.path(output_dir, "submissions", submitted) else group
    report_dir <- file.path(output_dir, "workspaces", participant, "checker")
    workspace <- dirname(report_dir)
    dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
    valid <- length(submitted) == 1L &&
      tolower(tools::file_ext(file)) %in% c("r", "rmd", "qmd")
    if (valid) valid <- file.info(file)$size > 0
    if (!valid) {
      result <- invalid_submission_result(file, profile,
        if (length(submitted) != 1L) paste("Expected one code file; found", length(submitted)) else
          "The submitted file is empty or has an unsupported extension.")
    } else {
      result <- tryCatch({
        prepare_submission_workspace(workspace, workspace_template)
        if (backend == "docker") {
          check_submission_container(file, reprex, style, workspace, timeout,
                                     profile, report_dir, container)
        } else {
          check_code_backend(file, reprex, style, workspace, timeout, profile, report_dir)
        }
      }, error = function(cnd) {
        invalid_submission_result(file, profile, conditionMessage(cnd), checker = TRUE)
      })
    }
    target <- file.path(output_dir, "feedback", paste0(participant, ".html"))
    result$html_report <- write_code_feedback(result, target, participant)
    result$participant <- participant
    result$archive_path <- submitted
    results[[participant]] <- result
    rows[[length(rows) + 1L]] <- data.frame(
      participant = participant, archive_path = paste(submitted, collapse = "; "),
      checksum = if (valid) unname(tools::md5sum(file)) else NA_character_,
      execution = result$execution$status, status = code_feedback_status(result), issues = length(result$issues),
      incomplete = any(vapply(result$issues, function(x)
        x$category == "incomplete" || x$id == "runtime.environment", logical(1))),
      report = paste0("feedback/", participant, ".html"), stringsAsFactors = FALSE)
    # Save completed work after every participant, so interruptions retain it.
    manifest <- do.call(rbind, rows)
    utils::write.csv(manifest, file.path(output_dir, "manifest.csv"), row.names = FALSE)
    saveRDS(results, file.path(output_dir, "results.rds"))
    write_submission_index(manifest, file.path(output_dir, "index.html"), profile)
  }
  result <- list(manifest = manifest, results = results,
                 index = file.path(output_dir, "index.html"))
  cli::cli_text("Batch reports: {.file {result$index}}")
  invisible(result)
}

validate_submission_template <- function(template) {
  if (!dir.exists(template)) stop("workspace_template must be a directory.")
  files <- list.files(template, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  links <- Sys.readlink(files)
  if (any(basename(files) %in% c(".Rprofile", ".Renviron", ".RData", ".sandbox")) ||
      any(!is.na(links) & nzchar(links))) {
    stop("Workspace templates must not contain startup files, saved workspaces, .sandbox or symbolic links.")
  }
}

prepare_submission_workspace <- function(workspace, template) {
  if (!is.null(template)) {
    files <- list.files(template, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(files) && !all(file.copy(files, workspace, recursive = TRUE))) {
      stop("Could not copy exercise resources.")
    }
  }
  for (directory in c("data/raw", "data/processed", "outputs", "scripts")) {
    dir.create(file.path(workspace, directory), recursive = TRUE, showWarnings = FALSE)
  }
  # here detects this marker on first load in the fresh child session.
  file.create(file.path(workspace, ".here"))
  invisible(workspace)
}

invalid_submission_result <- function(file, profile, message, checker = FALSE) {
  if (!checker) {
    message <- paste("The submitted file could not be checked.", message)
    if (tolower(tools::file_ext(file)) == "zip") {
      message <- paste("The submitted file could not be checked because it is a ZIP archive.",
                       "Submit a single R script or Quarto file instead.")
    }
  }
  list(file = file, execution_dir = NULL,
       execution = list(status = if (checker) "failed" else "skipped", events = list()),
       checks = list(execution = FALSE, style = FALSE),
       syntax = list(status = "not_checked", message = NULL),
       lints = empty_check_lints(), failures = if (checker) list(batch = message) else list(),
       report = NULL, versions = c(check_dependency_versions(), policy = "6"),
       profile = profile, scope = "No code was executed.",
       issues = list(code_issue(if (checker) "checker.batch" else "submission.invalid",
                                if (checker) "incomplete" else "requirements", message,
                                if (checker) "Investigate the checker and rerun this submission." else
                                  "Submit one nonempty R or Quarto code file in the required format.")))
}

write_submission_index <- function(manifest, file, profile) {
  e <- feedback_html_escape
  rows <- vapply(seq_len(nrow(manifest)), function(i) {
    x <- manifest[i, ]
    paste0("<tr><td><a href='", e(x$report), "'>",
           e(sub("^Participant_", "Participant ", x$participant)),
           "</a></td><td>", paste0("<span class='status-", gsub(" ", "-", if ("status" %in% names(x)) x$status else x$execution), "'>",
                  e(if ("status" %in% names(x)) x$status else x$execution), "</span>"), "</td><td>", x$issues,
           "</td><td>", if (x$incomplete) "Review incomplete checks" else "", "</td></tr>")
  }, character(1))
  writeLines(c(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Submission feedback</title>",
    paste0("<style>", feedback_html_css(), "</style></head><body><main>"),
    "<header><p class='eyebrow'>SECU0005 Crime Mapping</p><h1>Submission feedback</h1></header>",
    paste0("<p>", e(profile$name), " · ", nrow(manifest), " submissions checked</p>"),
    "<p>Assessor index. Issue counts include suggestions and review prompts, and are not marks.</p>",
    "<table><thead><tr><th>Participant / report</th><th>Status</th><th>Issues</th><th>Review</th></tr></thead><tbody>",
    rows, "</tbody></table></main></body></html>"), file, useBytes = TRUE)
}
