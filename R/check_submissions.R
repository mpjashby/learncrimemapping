#' Check a Moodle ZIP of code submissions
#'
#' Uses the same backend and issue interpretation as [check_code()]. Each
#' Moodle participant receives a portable HTML report, including invalid
#' submissions. A batch index, common-issue HTML summary, CSV manifest and
#' RDS results are also retained.
#'
#' @param zip Path to a Moodle submission ZIP.
#' @param output_dir Directory for persistent results. Existing participant reports
#'   are skipped, and unrelated files are preserved.
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
#' @param tile_cache Directory shared by Docker submissions for the default
#'   `rosm.cache` tile cache. Defaults to `rosm.cache` inside `output_dir` and
#'   persists across resumed batches. Use a separate directory for each exercise.
#'   Set to `NULL` to disable sharing. Ignored by the local backend.
#' @param workspace_dirs Relative directories created in each student's workspace
#'   after copying exercise resources and before execution, for either backend.
#'   Defaults to `data/raw`, `data/processed` and `output`. Supply a character
#'   vector to replace these defaults, or `character()` to create none.
#' @param saved_files Relative glob patterns for saved outputs to embed. `NULL`
#'   collects PNG, JPEG and PDF files in `output/` and `outputs/`. Use `character()`
#'   to disable collection. Explicit patterns also report missing outputs.
#'   PDF previews require Poppler (`pdftoppm`) on the marking computer.
#' @return Invisibly, a list with `manifest`, named `results`, `index`, and
#'   `issue_summary` (the common-issue HTML report path).
#' @details Docker checking uses fresh, unprivileged containers with a read-only
#'   system filesystem. The current student's workspace and, when enabled, the
#'   shared tile cache are mounted from the host. Web access remains enabled.
#'   Packages may be installed into the
#'   student's private library within that workspace, never the host library.
#'   R and system libraries inside the image remain readable. No automatic
#'   fallback to local execution occurs. Local execution is not a sandbox.
#'   Docker forwards `CARTO_API_KEY` from the calling R session when set.
#'   Submitted code can access this key; host startup files are not mounted.
#'   Quarto documents have enabled R chunks checked; full rendering is not done.
#'   ZIPs are limited to 5,000 entries and 100 MB of uncompressed content.
#'   Both named and anonymous Moodle folders are supported; report identities
#'   use numeric participant IDs, and original archive paths are retained.
#'   With style checking enabled, staff batches also suggest reviewing chains of
#'   single-use intermediate objects and missing blank lines before section
#'   comments beside multiline statements. These advisory checks are not run
#'   by [check_code()] and require manual judgement.
#'   Existing participant reports are never overwritten. Batch metadata is updated
#'   after each new report. Extracted files and workspaces are temporary and
#'   removed after their self-contained HTML report has been saved.
#'   Network failures and missing
#'   credentials may need assessor review; they are not automatic penalties.
#' @keywords internal
check_submissions <- function(
  zip,
  output_dir,
  profile = list(),
  workspace_template = NULL,
  reprex = TRUE,
  style = TRUE,
  timeout = 600,
  backend = c("docker", "local"),
  container_image = "learncrimemapping-checker:local",
  container_memory = "4g",
  container_cpus = 2,
  tile_cache = file.path(output_dir, "rosm.cache"),
  saved_files = NULL,
  workspace_dirs = c("data/raw", "data/processed", "output")
) {
  workspace_dirs <- validate_submission_workspace_dirs(workspace_dirs)
  # Explicit output patterns also request missing-output feedback; defaults
  # only collect files that were actually created or changed.
  expected_saved_files <- !is.null(saved_files)
  if (is.null(saved_files)) {
    saved_files <- as.vector(outer(
      c("output", "outputs"),
      c("png", "jpg", "jpeg", "pdf"),
      function(directory, extension) paste0(directory, "/*.", extension)
    ))
  }
  saved_files <- validate_saved_files(saved_files)
  backend <- match.arg(backend)
  container <- NULL
  check_code_arguments(zip, reprex, style, workspace_template, timeout)
  profile <- code_feedback_profile(profile)
  if (
    !is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(output_dir)
  ) {
    stop("Supply an output directory.")
  }
  if (!file.exists(zip) || dir.exists(zip)) {
    stop("Supply a readable ZIP file.")
  }
  zip <- normalizePath(zip, winslash = "/", mustWork = TRUE)
  # Inspect archive sizes and paths before extracting or executing anything.
  entries <- utils::unzip(zip, list = TRUE)
  if (
    !nrow(entries) ||
      nrow(entries) > 5000L ||
      any(!is.finite(entries$Length) | entries$Length < 0) ||
      sum(entries$Length) > 100 * 1024^2
  ) {
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
    stop(
      "Unsupported Moodle layout: expected Name_ID_assignsubmission_file or Participant_ID_assignsubmission_file folders."
    )
  }
  groups <- unique(folder)
  identities <- paste0(
    "Participant_",
    sub("^.*_([0-9]+)_assignsubmission_file$", "\\1", groups)
  )
  if (anyDuplicated(identities)) {
    stop(
      "Ambiguous Moodle layout: multiple submission folders have the same participant ID."
    )
  }
  # Only flat submission files are extracted; nested files and links are not
  # allowed to create directory structures through which extraction can escape.
  is_file <- !grepl("/$", paths)
  if (any(is_file & !grepl("^[^/]+/[^/]+$", paths))) {
    stop("Nested submission paths are not supported.")
  }
  if (!is.null(workspace_template)) {
    validate_submission_template(workspace_template)
  }
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    stop("output_dir is a file.")
  }
  targets <- file.path(output_dir, "feedback", paste0(identities, ".html"))
  if (backend == "docker" && any(!file.exists(targets))) {
    container <- submission_container_config(
      container_image,
      container_memory,
      container_cpus
    )
    container$tile_cache <- prepare_submission_tile_cache(tile_cache)
  }
  if (backend == "local" && reprex) {
    warning(
      "Local checking is unrestricted: student code can change host files and packages.",
      call. = FALSE
    )
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  if (!is.null(workspace_template)) {
    workspace_template <- normalizePath(
      workspace_template,
      winslash = "/",
      mustWork = TRUE
    )
    if (
      output_dir == workspace_template ||
        startsWith(output_dir, paste0(workspace_template, "/"))
    ) {
      stop("output_dir must not be inside workspace_template.")
    }
  }
  feedback_dir <- file.path(output_dir, "feedback")
  if (file.exists(feedback_dir) && !dir.exists(feedback_dir)) {
    stop("feedback is a file; existing files will not be replaced.")
  }
  dir.create(feedback_dir, showWarnings = FALSE)
  results_file <- file.path(output_dir, "results.rds")
  manifest_file <- file.path(output_dir, "manifest.csv")
  # Load earlier checkpoints so interrupted batches can resume by participant.
  results <- if (file.exists(results_file)) readRDS(results_file) else list()
  previous <- if (file.exists(manifest_file)) {
    utils::read.csv(manifest_file, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  rows <- list()
  # Keep metadata for all completed reports in every checkpoint, including
  # participants later in the archive than the next unfinished submission.
  if (!is.null(previous)) {
    for (participant in identities) {
      row <- previous[previous$participant == participant, , drop = FALSE]
      if (
        nrow(row) &&
          file.exists(file.path(feedback_dir, paste0(participant, ".html")))
      ) {
        rows[[participant]] <- row[1L, , drop = FALSE]
      }
    }
  }
  temporary <- tempfile(".submission-batch-", tmpdir = output_dir)
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  for (group in groups) {
    participant <- identities[match(group, groups)]
    submitted <- paths[folder == group & is_file]
    target <- file.path(feedback_dir, paste0(participant, ".html"))
    if (dir.exists(target)) {
      stop("Report path is a directory: ", target)
    }
    if (file.exists(target)) {
      cli::cli_text("Skipping existing report for {participant}")
      row <- if (!is.null(previous)) {
        previous[previous$participant == participant, , drop = FALSE]
      } else {
        NULL
      }
      if (is.null(row) || !nrow(row)) {
        row <- data.frame(
          participant = participant,
          archive_path = paste(submitted, collapse = "; "),
          checksum = NA_character_,
          execution = "unknown",
          status = "unknown",
          issues = NA_integer_,
          incomplete = TRUE,
          report = paste0("feedback/", participant, ".html"),
          stringsAsFactors = FALSE
        )
      }
      rows[[participant]] <- row[1L, , drop = FALSE]
      next
    }
    cli::cli_text("Checking {participant}")
    participant_dir <- file.path(temporary, participant)
    extraction <- file.path(participant_dir, "submissions")
    workspace <- file.path(participant_dir, "workspace")
    dir.create(extraction, recursive = TRUE)
    dir.create(workspace)
    if (length(submitted)) {
      utils::unzip(
        zip,
        files = submitted,
        exdir = extraction,
        unzip = "internal",
        setTimes = FALSE
      )
      extracted <- file.path(extraction, submitted)
      links <- Sys.readlink(extracted)
      if (any(!file.exists(extracted)) || any(!is.na(links) & nzchar(links))) {
        stop(
          "Extraction failed or produced symbolic links; no code was executed."
        )
      }
    }
    file <- if (length(submitted) == 1L) {
      file.path(extraction, submitted)
    } else {
      group
    }
    report_dir <- file.path(workspace, "checker")
    valid <- length(submitted) == 1L &&
      tolower(tools::file_ext(file)) %in% c("r", "rmd", "qmd")
    if (valid) {
      valid <- file.info(file)$size > 0
    }
    if (!valid) {
      result <- invalid_submission_result(
        file,
        profile,
        if (length(submitted) != 1L) {
          paste("Expected one code file; found", length(submitted))
        } else {
          "The submitted file is empty or has an unsupported extension."
        }
      )
    } else {
      result <- tryCatch(
        {
          prepare_submission_workspace(
            workspace,
            workspace_template,
            workspace_dirs
          )
          # Snapshot after copying resources so unchanged template files do not
          # appear as student-generated outputs in the feedback.
          before <- submission_output_snapshot(workspace, saved_files)
          if (backend == "docker") {
            checked <- check_submission_container(
              file,
              reprex,
              style,
              workspace,
              timeout,
              profile,
              report_dir,
              container
            )
          } else {
            checked <- check_code_backend(
              file,
              reprex,
              style,
              workspace,
              timeout,
              profile,
              report_dir
            )
          }
          tryCatch(
            {
              assert_submission_workspace_files(workspace)
              collect_submission_outputs(
                checked,
                workspace,
                saved_files,
                before,
                report_missing = expected_saved_files
              )
            },
            error = function(cnd) {
              checked$saved_file_notes <- paste(
                "Saved output collection could not be completed:",
                conditionMessage(cnd)
              )
              checked
            }
          )
        },
        error = function(cnd) {
          invalid_submission_result(
            file,
            profile,
            conditionMessage(cnd),
            checker = TRUE
          )
        }
      )
    }
    if (valid && style) {
      result <- add_marking_style_feedback(result)
    }
    # Publish only a complete report, so an interrupted write can be retried.
    staged_report <- file.path(participant_dir, "feedback.html")
    write_code_feedback(result, staged_report, participant)
    if (file.exists(target) || !file.rename(staged_report, target)) {
      stop("Could not save report: ", target)
    }
    result$html_report <- normalizePath(target, winslash = "/", mustWork = TRUE)
    result$participant <- participant
    result$archive_path <- submitted
    results[[participant]] <- result
    rows[[participant]] <- data.frame(
      participant = participant,
      archive_path = paste(submitted, collapse = "; "),
      checksum = if (valid) unname(tools::md5sum(file)) else NA_character_,
      execution = result$execution$status,
      status = code_feedback_status(result),
      issues = length(unique(vapply(result$issues, `[[`, character(1), "id"))),
      incomplete = any(vapply(
        result$issues,
        function(x) {
          x$category == "incomplete" || x$id == "runtime.environment"
        },
        logical(1)
      )),
      report = paste0("feedback/", participant, ".html"),
      stringsAsFactors = FALSE
    )
    unlink(participant_dir, recursive = TRUE)
    # Save completed work after every participant, so interruptions retain it.
    manifest <- do.call(rbind, unname(rows))
    utils::write.csv(
      manifest,
      file.path(output_dir, "manifest.csv"),
      row.names = FALSE
    )
    saveRDS(results, file.path(output_dir, "results.rds"))
    write_submission_index(
      manifest,
      file.path(output_dir, "index.html"),
      profile
    )
    write_submission_issue_summary(
      manifest,
      results,
      file.path(output_dir, "issues.html"),
      profile
    )
  }
  # Restore archive participant order, including reports skipped during resume.
  manifest <- do.call(rbind, unname(rows[identities]))
  utils::write.csv(manifest, manifest_file, row.names = FALSE)
  saveRDS(results, results_file)
  write_submission_index(manifest, file.path(output_dir, "index.html"), profile)
  write_submission_issue_summary(
    manifest,
    results,
    file.path(output_dir, "issues.html"),
    profile
  )
  result <- list(
    manifest = manifest,
    results = results,
    index = file.path(output_dir, "index.html"),
    issue_summary = file.path(output_dir, "issues.html")
  )
  cli::cli_text("Batch reports: {.file {result$index}}")
  invisible(result)
}

# Reject startup files and links before copying resources into any workspace.
validate_submission_template <- function(template) {
  if (!dir.exists(template)) {
    stop("workspace_template must be a directory.")
  }
  files <- list.files(
    template,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  links <- Sys.readlink(files)
  if (
    any(
      basename(files) %in% c(".Rprofile", ".Renviron", ".RData", ".sandbox")
    ) ||
      any(!is.na(links) & nzchar(links))
  ) {
    stop(
      "Workspace templates must not contain startup files, saved workspaces, .sandbox or symbolic links."
    )
  }
}

# Accept only relative directory names that stay inside each workspace.
validate_submission_workspace_dirs <- function(directories) {
  if (
    !is.character(directories) ||
      anyNA(directories) ||
      any(
        !nzchar(directories) |
          grepl(
            "(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.?(/|$)|[[:cntrl:]])",
            directories
          )
      ) ||
      any(grepl("(^|/)(checker|rosm.cache|[.][^/]*)(/|$)", directories))
  ) {
    stop(
      "workspace_dirs must contain relative directory paths without parent, hidden or reserved checker paths."
    )
  }
  unique(directories)
}

prepare_submission_workspace <- function(
  workspace,
  template,
  directories = c("data/raw", "data/processed", "output")
) {
  directories <- validate_submission_workspace_dirs(directories)
  if (!is.null(template)) {
    files <- list.files(
      template,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    if (length(files) && !all(file.copy(files, workspace, recursive = TRUE))) {
      stop("Could not copy exercise resources.")
    }
  }
  for (directory in directories) {
    path <- file.path(workspace, directory)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop(
        "Could not create workspace directory: ",
        directory,
        ". Check for conflicting files or insufficient permissions."
      )
    }
  }
  # here detects this marker on first load in the fresh child session.
  file.create(file.path(workspace, ".here"))
  invisible(workspace)
}

# Keep invalid submissions in the same report pipeline as executable code.
invalid_submission_result <- function(file, profile, message, checker = FALSE) {
  if (!checker) {
    message <- paste("The submitted file could not be checked.", message)
    if (tolower(tools::file_ext(file)) == "zip") {
      message <- paste(
        "The submitted file could not be checked because it is a ZIP archive.",
        "Submit a single R script or Quarto file instead."
      )
    }
  }
  list(
    file = file,
    execution_dir = NULL,
    execution = list(
      status = if (checker) "failed" else "skipped",
      events = list()
    ),
    checks = list(execution = FALSE, style = FALSE),
    syntax = list(status = "not_checked", message = NULL),
    lints = empty_check_lints(),
    failures = if (checker) list(batch = message) else list(),
    report = NULL,
    versions = c(check_dependency_versions(), policy = "6"),
    profile = profile,
    scope = "No code was executed.",
    issues = list(code_issue(
      if (checker) "checker.batch" else "submission.invalid",
      if (checker) "incomplete" else "requirements",
      message,
      if (checker) {
        "Investigate the checker and rerun this submission."
      } else {
        "Submit one nonempty R or Quarto code file in the required format."
      }
    ))
  )
}

write_submission_index <- function(manifest, file, profile) {
  e <- feedback_html_escape
  rows <- vapply(
    seq_len(nrow(manifest)),
    function(i) {
      x <- manifest[i, ]
      paste0(
        "<tr><td><a href='",
        e(x$report),
        "'>",
        e(sub("^Participant_", "Participant ", x$participant)),
        "</a></td><td>",
        paste0(
          "<span class='status-",
          gsub(" ", "-", if ("status" %in% names(x)) x$status else x$execution),
          "'>",
          e(if ("status" %in% names(x)) x$status else x$execution),
          "</span>"
        ),
        "</td><td>",
        x$issues,
        "</td><td>",
        if (x$incomplete) "Review incomplete checks" else "",
        "</td></tr>"
      )
    },
    character(1)
  )
  writeLines(
    c(
      "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1'>",
      "<title>Submission feedback</title>",
      paste0("<style>", feedback_html_css(), "</style></head><body><main>"),
      "<header><p class='eyebrow'>SECU0005 Crime Mapping</p><h1>Submission feedback</h1></header>",
      paste0(
        "<p>",
        e(profile$name),
        " \u00b7 ",
        nrow(manifest),
        " submissions checked</p>"
      ),
      "<p>Assessor index. Issue counts count each type of issue once, include suggestions and review prompts, and are not marks.</p>",
      "<p><a href='issues.html'>Most common issues across submissions</a></p>",
      "<table><thead><tr><th>Participant / report</th><th>Status</th><th>Issues</th><th>Review</th></tr></thead><tbody>",
      rows,
      "</tbody></table></main></body></html>"
    ),
    file,
    useBytes = TRUE
  )
}

# Staff-only suggestions run on the host after either execution backend. This
# keeps student check_code() and the public function signatures unchanged.
add_marking_style_feedback <- function(result) {
  findings <- tryCatch(
    marking_style_issues(result$file),
    error = function(cnd) {
      list(code_issue(
        "checker.marking_style",
        "incomplete",
        "The additional marking style checks could not be completed.",
        "The assessor should investigate and rerun.",
        evidence = conditionMessage(cnd)
      ))
    }
  )
  result$issues <- code_order_issues(c(result$issues, findings))
  if (!is.null(result$report)) {
    write_check_report(result)
  }
  result
}

# Use source locations and parse relationships for staff-only style advice.
marking_style_issues <- function(file) {
  lines <- read_check_code(file, tolower(tools::file_ext(file)))
  expressions <- tryCatch(
    parse(text = lines, keep.source = TRUE),
    error = function(cnd) NULL
  )
  # Do not infer chains or section boundaries from incomplete syntax.
  if (is.null(expressions) || !length(expressions)) {
    return(list())
  }
  chain_issues <- marking_chain_blank_issues(
    lines,
    utils::getParseData(expressions)
  )
  # The older rules compare separate statements; a chain can stand alone.
  if (length(expressions) < 2L) {
    return(chain_issues)
  }
  refs <- attr(expressions, "srcref")
  pd <- utils::getParseData(expressions)
  starts <- vapply(refs, function(x) as.integer(x[1L]), integer(1))
  ends <- vapply(refs, function(x) as.integer(x[3L]), integer(1))
  issues <- chain_issues
  add <- function(x) issues[[length(issues) + 1L]] <<- x

  for (i in seq.int(2L, length(expressions))) {
    gap <- seq.int(ends[i - 1L] + 1L, starts[i] - 1L)
    if (starts[i] <= ends[i - 1L] + 1L) {
      next
    }
    # Only standalone prose comment blocks between complete statements.
    if (
      any(!nzchar(trimws(lines[gap]))) ||
        !all(grepl("^\\s*#", lines[gap], perl = TRUE)) ||
        any(grepl("^\\s*#([!'|]|\\s*nolint\\b)", lines[gap], perl = TRUE)) ||
        !any(grepl("[[:alpha:]]", lines[gap])) ||
        (starts[i] == ends[i] && starts[i - 1L] == ends[i - 1L])
    ) {
      next
    }
    add(code_issue(
      "course.section_spacing",
      "style",
      "This comment appears to introduce a new step without a blank line before it.",
      paste(
        "Consider adding a blank line before the comment to make the steps easier to distinguish.",
        "Keep the comment next to the code it describes; decide whether these statements belong together."
      ),
      gap[1L],
      1L,
      lines[gap[1L]]
    ))
  }

  assignment <- function(x) {
    is.call(x) &&
      identical(x[[1L]], as.name("<-")) &&
      length(x) == 3L &&
      is.symbol(x[[2L]])
  }
  assigned <- vapply(
    expressions,
    function(x) {
      if (assignment(x)) as.character(x[[2L]]) else ""
    },
    character(1)
  )
  # Restrict consumers to familiar transformations with the data as first input.
  transforms <- c(
    "select",
    "filter",
    "mutate",
    "transmute",
    "arrange",
    "rename",
    "relocate",
    "distinct",
    "group_by",
    "ungroup",
    "summarise",
    "summarize",
    "slice",
    "slice_head",
    "slice_tail"
  )
  consumes <- function(x, name) {
    if (!assignment(x)) {
      return(FALSE)
    }
    call <- x[[3L]]
    if (
      !is.call(call) ||
        length(call) < 2L ||
        !identical(call[[2L]], as.name(name))
    ) {
      return(FALSE)
    }
    head <- call[[1L]]
    if (
      is.call(head) &&
        length(head) == 3L &&
        identical(head[[1L]], as.name("::")) &&
        identical(head[[2L]], as.name("dplyr"))
    ) {
      head <- head[[3L]]
    }
    is.symbol(head) && as.character(head) %in% transforms
  }
  # Counting all parsed names is deliberately conservative, including nested
  # uses and assignments. Dynamic lookup makes single-use inference uncertain.
  names <- unlist(lapply(expressions, all.names), use.names = FALSE)
  if (
    any(
      names %in%
        c(
          "get",
          "mget",
          "eval",
          "evalq",
          "parse",
          "source",
          "sys.source",
          "assign",
          "substitute",
          "sym",
          "syms"
        )
    )
  ) {
    return(issues)
  }
  links <- vapply(
    seq_len(length(expressions) - 1L),
    function(i) {
      name <- assigned[i]
      nzchar(name) &&
        sum(names == name) == 2L &&
        consumes(expressions[[i + 1L]], name) &&
        assigned[i + 1L] != name
    },
    logical(1)
  )
  runs <- rle(links)
  finish <- cumsum(runs$lengths)
  for (j in which(runs$values & runs$lengths >= 2L)) {
    first <- finish[j] - runs$lengths[j] + 1L
    last <- finish[j] + 1L
    add(code_issue(
      "course.intermediate_objects",
      "style",
      "These steps create intermediate objects that appear to be used only by the following step.",
      paste(
        "Consider combining them into a pipeline to reduce the number of objects you need to track.",
        "Keep separate objects if their names help explain the steps or you need to inspect intermediate results.",
        "This suggestion is based on references in this submitted file only."
      ),
      starts[first],
      as.integer(refs[[first]][5L]),
      paste(lines[seq.int(starts[first], ends[last])], collapse = "\n")
    ))
  }
  issues
}

# Use parse nodes rather than matching source text: operators in strings and
# comments are not connections, and argument spacing is not chain spacing.
marking_chain_blank_issues <- function(lines, pd) {
  if (is.null(pd) || !nrow(pd)) {
    return(list())
  }
  # Index children once, retaining source order for left/operator/right lookup.
  pd <- pd[order(pd$line1, pd$col1, pd$id), , drop = FALSE]
  children <- split(pd, pd$parent)
  nodes <- list()
  plots <- character()

  visit <- function(node) {
    parts <- children[[as.character(node$id)]]
    if (is.null(parts)) {
      return(FALSE)
    }
    # Comments are source annotations, not operands or function-call syntax.
    parts <- parts[parts$token != "COMMENT", , drop = FALSE]
    expr <- which(parts$token == "expr")
    # Descend into all expressions, including pipelines inside call arguments.
    plot_parts <- vapply(expr, function(i) visit(parts[i, ]), logical(1))
    op <- which(parts$terminal & parts$text %in% c("|>", "%>%", "+"))
    binary <- nrow(parts) == 3L &&
      identical(expr, c(1L, 3L)) &&
      length(op) == 1L &&
      op == 2L
    if (binary) {
      pipe <- parts$text[2L] %in% c("|>", "%>%")
      # Addition is a plot stack only when its left operand is a known plot.
      if (pipe || plot_parts[1L]) {
        # Inspect only gaps at the connection, never inside either operand.
        gap <- unique(c(
          seq.int(parts$line2[1L], parts$line1[2L]),
          seq.int(parts$line2[2L], parts$line1[3L])
        ))
        blank <- gap[!nzchar(trimws(lines[gap]))]
        nodes[[as.character(node$id)]] <<- list(
          node = node,
          operands = parts$id[expr],
          blank = blank
        )
      }
      return(if (pipe) plot_parts[2L] else plot_parts[1L])
    }
    # Parentheses preserve plot identity without treating their gaps as steps.
    if (
      nrow(parts) == 3L &&
        identical(parts$text[c(1L, 3L)], c("(", ")")) &&
        length(plot_parts) == 1L
    ) {
      # Link a parenthesised chain to its surrounding chain to avoid duplicates.
      inner <- as.character(parts$id[expr])
      if (!is.null(nodes[[inner]])) {
        nodes[[as.character(node$id)]] <<- list(
          node = node,
          operands = parts$id[expr],
          blank = integer()
        )
      }
      return(plot_parts[1L])
    }
    # Recognise only actual ggplot constructors, including the qualified form.
    if (length(expr) && any(parts$text == "(")) {
      head <- parts[expr[1L], ]
      head_text <- paste(
        lines[seq.int(head$line1, head$line2)],
        collapse = "\n"
      )
      if (head$line1 == head$line2) {
        head_text <- substr(head_text, head$col1, head$col2)
        if (
          gsub("[[:space:]]", "", head_text) %in%
            c("ggplot", "ggplot2::ggplot")
        ) {
          return(TRUE)
        }
      }
    }
    # A bare symbol can extend a plot assigned earlier in this file.
    nrow(parts) == 1L && parts$token == "SYMBOL" && parts$text %in% plots
  }

  roots <- pd[pd$parent == 0L & pd$token == "expr", , drop = FALSE]
  for (i in seq_len(nrow(roots))) {
    root <- roots[i, ]
    parts <- children[[as.character(root$id)]]
    parts <- parts[parts$token != "COMMENT", , drop = FALSE]
    assignment <- nrow(parts) == 3L &&
      parts$token[2L] == "LEFT_ASSIGN" &&
      parts$text[2L] %in% c("<-", "=")
    # Track only straightforward top-level assignments; do not guess at calls
    # that might return plots or execute student code to determine their type.
    if (assignment) {
      lhs <- children[[as.character(parts$id[1L])]]
      is_plot <- visit(parts[3L, ])
      if (nrow(lhs) == 1L && lhs$token == "SYMBOL") {
        plots <- setdiff(plots, lhs$text)
        if (is_plot) plots <- union(plots, lhs$text)
      }
    } else {
      visit(root)
    }
  }

  # Connected binary nodes form one chain; report it once, at its first blank.
  nested <- unlist(lapply(nodes, function(x) x$operands), use.names = FALSE)
  outer <- setdiff(names(nodes), as.character(nested))
  collect <- function(id) {
    x <- nodes[[id]]
    if (is.null(x)) {
      return(integer())
    }
    c(
      x$blank,
      unlist(lapply(as.character(x$operands), collect), use.names = FALSE)
    )
  }
  issues <- list()
  for (id in outer) {
    blank <- sort(unique(collect(id)))
    if (!length(blank)) {
      next
    }
    node <- nodes[[id]]$node
    issues[[length(issues) + 1L]] <- code_issue(
      "course.chain_blank_lines",
      "style",
      "Remove blank lines between steps in this pipeline or ggplot2 stack.",
      paste(
        "Keep consecutive steps together so it is easier to see where the pipeline or stack starts and ends.",
        "Remove empty or whitespace-only lines between steps; use blank lines between separate statements."
      ),
      blank[1L],
      1L,
      paste(lines[seq.int(node$line1, node$line2)], collapse = "\n")
    )
  }
  issues
}
