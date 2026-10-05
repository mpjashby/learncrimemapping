#' Check your Learn Crime Mapping workspace
#'
#' Print a read-only setup report for students using Positron. Every check has
#' a labelled status and suggested next steps; checks continue after failures.
#'
#' @param workspace Optional path to the main `crime_mapping` folder. Checking
#'   a supplied folder does not establish that it is open in Positron. By
#'   default use the workspace reported by Positron, otherwise the R working
#'   directory (the folder R currently uses).
#' @param check_updates Whether to look up the current R release using the
#'   public R-hub R versions service, with a five-second timeout. Defaults to
#'   `TRUE`. Lookup failures produce a warning and a `not_checked` result,
#'   rather than a setup problem. Use `FALSE` to skip the online lookup.
#' @return Invisibly, a list with `workspace`, `workspace_source`,
#'   `workspace_confident`, `checks`, `problems` and `counts`. `checks` is a named
#'   list with
#'   stable identifiers (package identifiers are `package.<name>` and directory
#'   identifiers are `directory.<relative/path>`). Each entry contains `id`,
#'   `status`, `message`, `details` (non-secret diagnostic data), and `actions`
#'   (suggested instructions or R code). Statuses are `passed`, `problem`,
#'   `manual`, `not_checked`, or `not_applicable`. `counts` counts each status.
#'   `problems` repeats the entries from `checks` whose status is `problem`,
#'   without counting them twice. No API keys or settings file contents are
#'   returned.
#' @details
#' Required packages and version constraints come from this package's installed
#' DESCRIPTION, the same source used to install the course dependencies.
#' Packages are not attached or used to run analyses.
#'
#' Positron detection uses its documented `POSITRON` indicator and, when
#' supported, the rstudioapi compatibility API for the active workspace.
#' No editor folders are inferred from the working directory alone. A fallback
#' folder is considered confidently identified only when named `crime_mapping`
#' and containing `.here` or `.vscode/settings.json`, or when it is the
#' standard Posit Cloud folder `/cloud/project` (shown as Project in Explorer).
#' An explicitly supplied existing folder can be checked independently of
#' the editor.
#'
#' The current `here::here()` result is checked, including a previously cached
#' folder. The checker never resets it, changes the working directory, activates
#' a usethis project, or writes files. Permission checks use operating-system
#' access flags; they cannot guarantee future writes or cloud synchronisation.
#'
#' Air settings are read as JSON with comments and trailing commas. Only
#' explicit R language settings can be verified locally: user/profile/remote,
#' multi-folder workspace settings and policies may affect editor behaviour.
#' Absent local settings are not assumed to be incorrect. Actual formatting
#' and extension availability are not tested. Windows Rtools detection runs
#' pkgbuild in an isolated process so its detection does not change this
#' session's environment.
#'
#' The startup-file check follows `R_ENVIRON_USER`, then `.Renviron` in the
#' folder R would start in, then the user home folder (including R's
#' architecture-specific files). It recognises literal key assignments without
#' executing them. Variable expansion or unreadable files are marked
#' `not_checked`. Startup flags, custom startup code, or a changed starting
#' folder can affect which file was read previously. Presence is checked, not
#' whether CARTO accepts the key. No authenticated requests are made.
#' @examples
#' \dontrun{
#' learncrimemapping::check_workspace()
#' results <- learncrimemapping::check_workspace()
#' # Inspect a folder without claiming it is open in Positron:
#' learncrimemapping::check_workspace("~/Documents/crime_mapping")
#' }
#' @export
check_workspace <- function(workspace = NULL, check_updates = TRUE) {
  if (!is.null(workspace) && (!is.character(workspace) ||
      length(workspace) != 1L || is.na(workspace) || !nzchar(workspace))) {
    cli::cli_abort("{.arg workspace} must be NULL or one nonempty folder path.")
  }
  if (!is.logical(check_updates) || length(check_updates) != 1L ||
      is.na(check_updates)) {
    cli::cli_abort("{.arg check_updates} must be TRUE or FALSE.")
  }
  report_div <- cli::cli_div(
    theme = list(
      ".workspace-pass" = list(color = "green", "font-weight" = "normal"),
      ".workspace-problem" = list(color = "#8B0000", "font-weight" = "bold")
    ), .auto_close = FALSE
  )
  on.exit(cli::cli_end(report_div), add = TRUE)
  checks <- list()
  add <- function(id, status, message, actions = character(), details = list()) {
    checks[[id]] <<- list(id = id, status = status, message = message,
                         details = details, actions = actions)
    workspace_status_message(status, message)
    for (action in actions) cli::cli_verbatim(action)
    if (status %in% c("problem", "manual")) {
      cli::cli_text("Afterwards, run this in the R Console:")
      cli::cli_verbatim("learncrimemapping::check_workspace()")
    }
  }
  # Do not propagate arbitrary condition text: it could contain secrets read
  # from a file, or a package startup message containing personal settings.
  safe <- function(code) tryCatch(suppressWarnings(suppressMessages(code)),
                                  error = function(cnd) NULL)
  open_folder <- paste(
    "In Positron, choose File > Open Folder ... and select crime_mapping.",
    "Check that this folder is at the top of Explorer, then click Restart R",
    "in the Console panel. This makes the book's file paths use the right folder."
  )
  air_action <- paste(
    "In Positron, open the Command Palette with Ctrl+Shift+P (Windows/Linux)",
    "or Command+Shift+P (macOS). Type Air: Initialize Workspace Folder,",
    "select that command, and select crime_mapping if prompted."
  )
  cli::cli_h1("Checking your crime mapping setup")
  cli::cli_text("This report only reads your setup. Suggested code is for you to run.")
  cli::cli_h2("R and Positron")
  os <- workspace_os()
  r_version <- as.character(getRversion())
  add("r.session", "passed", paste("R", r_version, "is running on", os),
      details = list(version = r_version, os = os))
  desc <- safe(workspace_description())
  if (!is.null(desc)) {
    requirements <- workspace_requirements(desc)
    r_req <- requirements[requirements$name == "R", , drop = FALSE]
    ok <- !nrow(r_req) || workspace_version_ok(r_version, r_req$op, r_req$version)
    add("r.required_version", if (ok) "passed" else "problem",
        if (ok) "R meets this package's stated version requirement." else
          paste("R", r_version, "does not meet the package requirement.",
                "The course functions need", r_req$op, r_req$version),
        if (!ok) "Follow Step 1 at https://books.lesscrime.info/learncrimemapping/setup.html to update R.")
  } else {
    add("packages.requirements", "not_checked",
        "The installed course package's dependency information could not be read.",
        "Reinstall learncrimemapping using the book's setup code, then run this check again.")
    requirements <- NULL
  }
  if (check_updates) {
    latest <- safe(workspace_latest_r())
    if (is.null(latest)) {
      cli::cli_warn("The latest R version could not be checked because the online lookup failed or timed out. This is not a setup failure; the other checks will continue.",
                    class = "learncrimemapping_update_warning")
      add("r.current", "not_checked", "Unable to verify the latest R version because the online lookup failed or timed out. This is not a setup failure.",
          "Check your internet connection, then run learncrimemapping::check_workspace() again in the R Console.",
          list(reason = "lookup_failed"))
    } else {
      ok <- safe(utils::compareVersion(r_version, latest) >= 0L)
      if (is.null(ok)) cli::cli_warn("The online R-version response could not be compared. The other checks will continue.", class = "learncrimemapping_update_warning")
      add("r.current", if (is.null(ok)) "not_checked" else if (ok) "passed" else "problem",
          paste("Running R:", r_version, "Published R release:", latest),
          if (!isTRUE(ok)) "Follow Step 1 of the book's setup page to update R: https://books.lesscrime.info/learncrimemapping/setup.html",
          list(running = r_version, latest = latest))
    }
  } else {
    add("r.current", "not_checked", "The online R-version check was skipped because check_updates = FALSE.")
  }
  editor <- safe(workspace_editor())
  if (is.null(editor)) editor <- list(detected = FALSE, workspace = NULL, version = NULL)
  add("positron.session", if (isTRUE(editor$detected)) "passed" else "manual",
      if (isTRUE(editor$detected)) "This R session is running in Positron." else
        "Positron was not detected in this R session. This does not mean it is not installed.",
      if (!isTRUE(editor$detected)) c("Open Positron and use its R Console for the course. If it is not installed, follow Step 2: https://books.lesscrime.info/learncrimemapping/setup.html", open_folder))
  add("positron.version", if (is.null(editor$version)) "manual" else "passed",
      if (is.null(editor$version)) "The Positron version cannot be read reliably from this R session." else
        paste("Positron version:", editor$version),
      if (is.null(editor$version)) "In Positron, open Help > About (on macOS, Positron > About Positron) to see the version.")
  if (identical(os, "Windows")) {
    tools <- safe(workspace_rtools())
    add("rtools", if (is.null(tools)) "manual" else if (isTRUE(tools)) "passed" else "problem",
        if (is.null(tools)) "Compatible Windows Rtools could not be checked." else
          if (isTRUE(tools)) paste("pkgbuild detected Rtools compatible with R", r_version) else
            paste("Rtools compatible with R", r_version, "was not detected. R uses it when installing some course packages."),
        if (!isTRUE(tools)) "Follow Step 3 of the book's setup page: open https://cran.r-project.org/bin/windows/Rtools/, choose the version matching your running R version, and install it with the default options. Then restart R in Positron.")
    add("windows.redistributable", "manual", "The Microsoft Visual C++ Redistributable needed by Positron has not been verified from R.",
        "Follow the Windows instructions in Step 2: https://books.lesscrime.info/learncrimemapping/setup.html (including its Microsoft Visual C++ Redistributable link).")
  } else {
    add("rtools", "not_applicable", "Windows Rtools is not needed on this operating system. Compilation tools on this computer have not been tested.")
  }
  cli::cli_h2("Required R packages")
  if (!is.null(requirements)) {
    for (i in which(requirements$name != "R")) {
      req <- requirements[i, ]
      installed <- safe(workspace_package_version(req$name))
      ok <- !is.null(installed) && isTRUE(safe(workspace_version_ok(installed, req$op, req$version)))
      message <- if (ok) paste(req$name, "is installed (version", paste0(installed, ").")) else
        if (is.null(installed)) paste(req$name, "is missing or its installed version could not be read. The course setup requires it.") else
          paste(req$name, "has version", installed, "but the course package requires", req$op, req$version)
      add(paste0("package.", req$name), if (ok) "passed" else "problem", message,
          if (!ok) workspace_install_action(req$name, desc),
          list(package = req$name, installed = installed, operator = req$op,
               required = req$version))
    }
  }
  cli::cli_h2("Your main crime_mapping folder")
  wd <- workspace_path(getwd())
  active <- if (!is.null(editor$workspace)) workspace_path(editor$workspace) else NULL
  source <- if (!is.null(workspace)) "supplied" else if (!is.null(active)) "positron" else "working_directory"
  folder <- workspace_path(if (!is.null(workspace)) workspace else if (!is.null(active)) active else wd)
  cloud <- workspace_cloud_folder(folder)
  named <- identical(workspace_actual_name(folder), "crime_mapping")
  if (cloud) {
    open_folder <- paste(
      "In Posit Cloud, open your crime_mapping project and restart R using",
      "the Restart R button in Positron's Console. The main folder may be",
      "labelled Project in Explorer; do not rename the /cloud/project folder."
    )
    air_action <- paste(
      "In Positron, open the Command Palette with Ctrl+Shift+P (Windows/Linux)",
      "or Command+Shift+P (macOS). Type Air: Initialize Workspace Folder,",
      "select that command, and select Project if prompted."
    )
  }
  exists <- dir.exists(folder)
  marker <- file.exists(file.path(folder, ".here")) ||
    file.exists(file.path(folder, ".vscode", "settings.json"))
  confident <- exists && (source != "working_directory" || (named && marker) || cloud)
  cli::cli_text("Folder being checked ({source}): {.file {folder}}")
  add("workspace.folder", if (!exists) "problem" else if (confident) "passed" else "manual",
      if (!exists) paste("No directory exists at", folder) else if (confident)
        paste("The folder to inspect is established:", folder) else
          paste("R currently uses", wd, "but the main course folder cannot be established confidently."),
      if (!confident) open_folder, list(path = folder, source = source, confident = confident))
  add("workspace.name", if (named || cloud) "passed" else "problem",
      if (cloud) "Posit Cloud uses /cloud/project for the main folder and may display it as Project, even when your course project is called crime_mapping. This folder does not need to be renamed." else if (named) "The main folder is named crime_mapping, with the expected spelling." else
        paste("The folder is named", workspace_actual_name(folder), "rather than crime_mapping. The book uses that exact name."),
      if (!named && !cloud) c("If your course folder has another name, rename it to crime_mapping using your computer's file manager. If this is an unrelated folder, leave it alone and open your course folder instead.", open_folder))
  add("workspace.active", if (is.null(active)) "manual" else if (workspace_same_path(active, folder)) "passed" else "problem",
      if (is.null(active)) "The folder shown in Positron's Explorer could not be verified independently from R." else
        paste("Positron reports its active workspace as", active, "; the folder being checked is", folder),
      if (is.null(active) || !workspace_same_path(active, folder)) open_folder,
      list(active = active, expected = folder))
  add("workspace.working_directory", if (workspace_same_path(wd, folder)) "passed" else "problem",
      paste("R currently uses", wd, "; expected:", folder,
            ". This controls where ordinary file paths read and save your work."),
      if (!workspace_same_path(wd, folder)) open_folder, list(actual = wd, expected = folder))
  here_folder <- safe(workspace_here())
  if (!is.null(here_folder)) here_folder <- workspace_path(here_folder)
  add("workspace.here", if (is.null(here_folder)) "not_checked" else if (workspace_same_path(here_folder, folder)) "passed" else "problem",
      if (is.null(here_folder)) "here::here() could not be checked. Install the required here package first." else
        paste("here::here() uses", here_folder, "; expected:", folder,
              ". The book uses here() to find data and save maps."),
      if (is.null(here_folder)) 'R Console: install.packages("here")' else if (!workspace_same_path(here_folder, folder))
        c("here() remembers the folder it found earlier in this session. Restart R after opening the correct folder; this checker does not change the remembered folder.", open_folder),
      list(actual = here_folder, expected = folder))
  cli::cli_h2("Course directories")
  for (relative in c("data", "data/raw", "data/processed", "output", "R")) {
    directory <- safe(workspace_directory(folder, relative))
    if (is.null(directory)) directory <- list(kind = "unreadable", path = file.path(folder, relative))
    actions <- character()
    status <- switch(directory$kind, ok = "passed", blocked = "not_checked", unreadable = "manual", "problem")
    folder_label <- paste0("The folder called '", basename(relative), "'")
    parent <- if (dirname(relative) == ".") folder else file.path(folder, dirname(relative))
    message <- switch(directory$kind,
      ok = paste(folder_label, "is in the correct place, with read/write access flags."),
      missing = paste0(folder_label, " is missing from ", parent, ". The book needs this folder to organise data, scripts or saved maps."),
      file = paste0(directory$path, " is an ordinary file, but the book needs a folder called '", basename(relative), "' there."),
      case = paste0("Found ", directory$path, ", but the book expects a folder called '", basename(relative), "' with exactly that capitalisation."),
      blocked = paste(folder_label, "could not be checked because the folder it belongs inside is missing, is a file, or has the wrong capitalisation."),
      permission = paste(folder_label, "exists but read, write or folder-opening access is unavailable. R needs this access to use course files."),
      paste(folder_label, "could not be inspected reliably."))
    if (directory$kind == "missing") {
      actions <- if (confident && (named || cloud)) c("Run this line in the R Console to create the missing directory:",
        paste0("dir.create(", workspace_quote(file.path(folder, relative)), ", recursive = TRUE)")) else
          c("No folder-creation code is suggested until the main crime_mapping folder is confirmed.", open_folder)
    } else if (directory$kind == "file") {
      actions <- "In the file manager, inspect the blocking file and preserve it by moving it to a safe location or giving it another name. Then create the required directory in Explorer. Do not overwrite or delete the file."
    } else if (directory$kind == "case") {
      actions <- "In the file manager, rename this directory to the exact spelling shown above. If your computer ignores case-only changes, first use a temporary name, then the expected name. Preserve the contents."
    } else if (directory$kind == "permission") {
      actions <- "Use your computer's file manager to check this folder's permissions and allow your account to read and write it. Ask your lecturer or IT support if you cannot change them."
    } else if (directory$kind %in% c("blocked", "unreadable")) {
      actions <- "Fix the parent directory or access problem reported above, then run the checker again."
    }
    add(paste0("directory.", relative), status, message, actions, directory)
  }
  cli::cli_h2("Automatic code formatting with Air")
  if (!exists) {
    for (id in c("air.file", "air.settings", "air.format_on_save", "air.formatter")) {
      add(id, "not_checked", "Air files cannot be inspected because the folder being checked does not exist.", open_folder)
    }
  } else {
    air_file <- file.path(folder, "air.toml")
    ok <- file.exists(air_file) && !dir.exists(air_file) && file.access(air_file, 4) == 0
    add("air.file", if (ok) "passed" else "problem",
        if (ok) "air.toml is a readable file in the main folder. An empty file is valid." else
          "A readable air.toml file was not found in the main folder. Air uses this file for the course workspace.",
        if (!ok) air_action)
    settings_file <- file.path(folder, ".vscode", "settings.json")
    settings <- if (file.exists(settings_file)) safe(workspace_read_settings(settings_file)) else NULL
    add("air.settings", if (!is.null(settings)) "passed" else "manual",
        if (!is.null(settings)) "The workspace formatting settings file could be read (comments and trailing commas are allowed)." else
          "Workspace formatting settings are absent or could not be read. Settings might be inherited from Positron, so this alone does not prove formatting is broken.",
        if (is.null(settings)) air_action)
    for (setting in c("editor.formatOnSave", "editor.defaultFormatter")) {
      value <- workspace_r_setting(settings, setting)
      expected <- if (setting == "editor.formatOnSave") TRUE else "Posit.air-vscode"
      id <- if (setting == "editor.formatOnSave") "air.format_on_save" else "air.formatter"
      ok <- identical(value$value, expected)
      add(id, if (!value$verified) "manual" else if (ok) "passed" else "problem",
          if (!value$verified) paste(setting, "for R cannot be fully verified locally. User, profile or other workspace settings can supply it.") else
            if (ok) paste("The R-specific workspace setting", setting, "has the expected value.") else
              paste("The R-specific workspace setting", setting, "does not have the expected value. The course needs formatting on save and Air as the R formatter."),
          if (!ok || !value$verified) air_action,
          list(setting = setting, expected = expected, verified = value$verified))
    }
  }
  cli::cli_h2("CARTO base maps")
  saved <- safe(workspace_saved_key(if (!is.null(active)) active else wd))
  if (is.null(saved)) saved <- list(state = "unknown", path = NULL, source = "unknown")
  key <- workspace_key_present()
  add("carto.session", if (key) "passed" else "problem",
      if (key) "CARTO_API_KEY is available in this R session." else
        "CARTO_API_KEY is empty or missing in this R session. The course needs it to request CARTO base maps.",
      if (!key) {
        if (saved$state == "present") "The key is already saved. Click Restart R in Positron's Console panel, then run the checker again."
        else c(workspace_carto_actions(), "Save the file, then click Restart R in Positron's Console panel.")
      },
      list(present = key))
  status <- switch(saved$state, present = if (key) "passed" else "problem",
                   absent = "problem", "not_checked")
  message <- switch(saved$state,
    present = if (key) "CARTO_API_KEY is also saved in the selected R startup file." else
      "CARTO_API_KEY is saved in the selected R startup file but is unavailable in this session. An R restart may still be needed.",
    absent = if (key) "The key is available in this session but no nonempty assignment was found in the selected R startup file. It may not survive an R restart." else
      "No nonempty CARTO_API_KEY assignment was found in the selected R startup file.",
    "The saved CARTO key could not be verified safely (the file is unreadable or uses a value that cannot be interpreted without running it).")
  add("carto.saved", status, message,
      if (saved$state != "unknown") c(if (!is.null(saved$path)) paste("Startup file selected for the next R start:", saved$path),
        if (saved$source %in% c("workspace", "R_ENVIRON_USER") && !is.null(saved$path)) paste("R selects this file instead of the user .Renviron because of", saved$source,
          if (saved$state == "present") ". The selected file already contains an assignment. Preserve both files; a key stored only in the user file would not be read." else
            ". Preserve both files. A key added only to the user file may not be read. In Positron, choose File > Open File ... and open the selected startup file shown above. Add the assignment there as well, preserving its existing settings."),
        if (saved$source == "R_ENVIRON_USER" && is.null(saved$path)) "R_ENVIRON_USER is set to an empty value, so the normal user startup file cannot be assumed to be read. Ask your lecturer or IT support how this session is started; preserve existing settings.",
        if (saved$state != "present") workspace_carto_actions(),
        if (!key) "Save the file and click Restart R in Positron's Console panel.") else character(), saved)
  counts <- table(factor(vapply(checks, `[[`, character(1), "status"),
                         levels = c("passed", "problem", "manual", "not_checked", "not_applicable")))
  counts <- stats::setNames(as.integer(counts), names(counts))
  cli::cli_h2("Report totals")
  cli::cli_text("{counts[['passed']]} passed checks; {counts[['problem']]} problems; {counts[['manual']]} checks requiring manual verification; {counts[['not_checked']]} not checked; {counts[['not_applicable']]} not applicable.")
  problems <- Filter(function(check) check$status == "problem", checks)
  cli::cli_h2("Problems to fix")
  if (!length(problems)) {
    cli::cli_text("No problems were found by the checks that ran.")
  } else {
    for (problem in problems) {
      workspace_status_message("problem", problem$message)
      for (action in problem$actions) cli::cli_verbatim(action)
    }
  }
  if (counts[["manual"]] > 0L) {
    cli::cli_text("Complete any checks marked MANUAL CHECK as well as the repairs above.")
  }
  cli::cli_text("After changes, run this in the R Console:")
  cli::cli_verbatim("learncrimemapping::check_workspace()")
  invisible(list(workspace = folder, workspace_source = source,
                 workspace_confident = confident, checks = checks,
                 problems = problems, counts = counts))
}

# Styled labels retain their meaning in consoles without colour support.
workspace_status_message <- function(status, message) {
  if (status == "passed") {
    cli::cli_text("{.workspace-pass PASS}: {message}")
  } else if (status == "problem") {
    cli::cli_text("{.workspace-problem PROBLEM}: {message}")
  } else {
    labels <- c(manual = "MANUAL CHECK", not_checked = "NOT CHECKED",
                not_applicable = "NOT APPLICABLE")
    label <- labels[[status]]
    cli::cli_alert_info("{label}: {message}")
  }
}

# Posit Cloud's storage folder is independent of the displayed project title.
# See docs.posit.co/cloud/guide/articles/environment-variables.html.
# Do not accept unrelated desktop folders merely because they are named Project.
workspace_cloud_folder <- function(path) {
  identical(workspace_path(path), "/cloud/project")
}

workspace_os <- function() {
  if (.Platform$OS.type == "windows") return("Windows")
  system <- Sys.info()[["sysname"]]
  if (identical(system, "Darwin")) "macOS" else system
}
workspace_path <- function(path) normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
workspace_same_path <- function(a, b) identical(workspace_path(a), workspace_path(b))
workspace_quote <- function(x) encodeString(x, quote = '"')
workspace_actual_name <- function(path) {
  names <- list.files(dirname(path), all.files = TRUE, no.. = TRUE)
  exact <- names[names == basename(path)]
  if (length(exact)) return(exact[[1]])
  matching <- names[tolower(names) == tolower(basename(path))]
  if (length(matching) == 1L) matching else basename(path)
}
workspace_description <- function() utils::packageDescription("learncrimemapping")
workspace_requirements <- function(desc) {
  specs <- trimws(unlist(strsplit(paste(desc$Depends, desc$Imports, sep = ","), ",", fixed = TRUE)))
  specs <- specs[nzchar(specs)]
  data.frame(name = sub("\\s*\\(.*", "", specs),
    op = ifelse(grepl("\\(", specs), sub(".*\\(\\s*([<>=]+).*", "\\1", specs), ""),
    version = ifelse(grepl("\\(", specs), sub(".*[<>=]+\\s*([^ )]+)\\s*\\).*", "\\1", specs), ""),
    stringsAsFactors = FALSE)
}
workspace_version_ok <- function(installed, op, version) {
  if (!nzchar(op)) return(TRUE)
  comparison <- utils::compareVersion(as.character(installed), version)
  switch(op, ">=" = comparison >= 0, ">" = comparison > 0,
         "<=" = comparison <= 0, "<" = comparison < 0, "==" = comparison == 0,
         "=" = comparison == 0, FALSE)
}
workspace_package_version <- function(package) as.character(utils::packageVersion(package))
workspace_install_action <- function(package, desc) {
  remotes <- if (is.null(desc$Remotes)) character() else trimws(strsplit(desc$Remotes, ",")[[1]])
  remote <- remotes[sub(".*[/]", "", sub("@.*", "", remotes)) == package]
  if (length(remote)) {
    code <- c('if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")',
              paste0("remotes::install_github(", workspace_quote(sub("^github::", "", remote[[1]])), ")"))
  } else {
    code <- paste0("install.packages(", workspace_quote(package), ")")
  }
  c("Run the following in the R Console:", code,
    "To repeat the full book setup instead:",
    'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")',
    'remotes::install_github("mpjashby/learncrimemapping")')
}
workspace_latest_r <- function() {
  response <- httr2::request("https://api.r-hub.io/rversions/r-release") |>
    httr2::req_timeout(5) |> httr2::req_perform()
  version <- httr2::resp_body_json(response)$version
  if (!is.character(version) || length(version) != 1L ||
      !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version)) stop("Unrecognised release")
  version
}
workspace_editor <- function() {
  # Official documentation: migrate-rstudio-settings-and-extensions.html and
  # guide-r-session-hooks.html at https://positron.posit.co/.
  detected <- nzchar(Sys.getenv("POSITRON"))
  active <- NULL
  if (detected && requireNamespace("rstudioapi", quietly = TRUE)) {
    active <- tryCatch({
      if (rstudioapi::isAvailable() && rstudioapi::hasFun("getActiveProject")) {
        rstudioapi::getActiveProject()
      } else NULL
    }, error = function(cnd) NULL)
  }
  if (!is.character(active) || length(active) != 1L || is.na(active) || !nzchar(active)) active <- NULL
  # versionInfo() may report compatibility versions; do not call undocumented
  # Positron internals or infer a version from installation paths.
  list(detected = detected, workspace = active, version = NULL)
}
workspace_rtools <- function() {
  callr::r(function() suppressMessages(suppressWarnings(pkgbuild::has_rtools(debug = TRUE))),
           timeout = 15, user_profile = FALSE, system_profile = FALSE)
}
workspace_here <- function() here::here()
workspace_access <- function(path, mode) file.access(path, mode)
workspace_directory <- function(folder, relative) {
  parent <- folder
  parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
  for (i in seq_along(parts)) {
    path <- file.path(parent, parts[[i]])
    if (!dir.exists(parent)) return(list(kind = "blocked", path = path))
    if (workspace_access(parent, 4) != 0 ||
        (.Platform$OS.type != "windows" && workspace_access(parent, 1) != 0)) {
      return(list(kind = "unreadable", path = parent))
    }
    entries <- list.files(parent, all.files = TRUE, no.. = TRUE)
    if (!parts[[i]] %in% entries) {
      matching <- entries[tolower(entries) == tolower(parts[[i]])]
      kind <- if (length(matching)) "case" else "missing"
      if (i < length(parts)) kind <- "blocked"
      return(list(kind = kind, path = if (length(matching)) file.path(parent, matching[[1]]) else path))
    }
    if (!dir.exists(path)) return(list(kind = if (i == length(parts)) "file" else "blocked", path = path))
    parent <- path
  }
  accessible <- all(vapply(c(4, 2), function(mode) workspace_access(path, mode) == 0, logical(1)))
  if (.Platform$OS.type != "windows") accessible <- accessible && workspace_access(path, 1) == 0
  list(kind = if (accessible) "ok" else "permission", path = path)
}

# A small lexical JSONC adapter: remove comments and trailing commas only
# outside quoted strings, then let jsonlite validate and parse the document.
# Never evaluate settings as R or JavaScript; never include parser errors in
# the student report, since other settings can contain private values.
workspace_read_settings <- function(path) {
  chars <- strsplit(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "", fixed = TRUE)[[1]]
  n <- length(chars)
  quoted <- FALSE
  escaped <- FALSE
  i <- 1L
  while (i <= n) {
    ch <- chars[[i]]
    if (quoted) {
      if (escaped) escaped <- FALSE else if (ch == "\\") escaped <- TRUE else if (ch == '"') quoted <- FALSE
    } else if (ch == '"') {
      quoted <- TRUE
    } else if (ch == "/" && i < n && chars[[i + 1L]] %in% c("/", "*")) {
      block <- chars[[i + 1L]] == "*"
      start <- i
      i <- i + 2L
      if (block) {
        while (i < n && !(chars[[i]] == "*" && chars[[i + 1L]] == "/")) i <- i + 1L
        if (i >= n) stop("Unclosed comment")
        i <- i + 1L
      } else {
        while (i <= n && chars[[i]] != "\n") i <- i + 1L
        i <- i - 1L
      }
      chars[start:i] <- " "
    }
    i <- i + 1L
  }
  quoted <- FALSE
  escaped <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (quoted) {
      if (escaped) escaped <- FALSE else if (ch == "\\") escaped <- TRUE else if (ch == '"') quoted <- FALSE
    } else if (ch == '"') {
      quoted <- TRUE
    } else if (ch == "," && i < n) {
      j <- i + 1L
      while (j <= n && grepl("^\\s$", chars[[j]])) j <- j + 1L
      if (j <= n && chars[[j]] %in% c("}", "]")) chars[[i]] <- " "
    }
  }
  text <- paste(chars, collapse = "")
  if (!startsWith(trimws(text), "{")) stop("Settings must be an object")
  settings <- jsonlite::parse_json(text)
  if (!is.list(settings) || anyDuplicated(names(settings))) stop("Invalid settings object")
  settings
}
workspace_r_setting <- function(settings, setting) {
  # Language-specific user settings override generic workspace settings.
  # Verify only explicit [r] entries. Combined language blocks and policy
  # precedence require the manual editor check.
  r <- settings[["[r]"]]
  combined <- any(grepl("\\[r\\]", names(settings)) & names(settings) != "[r]")
  value <- if (is.list(r)) r[[setting]] else NULL
  list(value = value, verified = !combined && !anyDuplicated(names(r)) && !is.null(value))
}
workspace_key_present <- function() nzchar(trimws(Sys.getenv("CARTO_API_KEY")))
workspace_carto_actions <- function() c(
  "Register for a key at https://carto.com/basemaps/apikey/ (non-commercial; learning how to make crime maps; no restrictions).",
  "Run these lines in the R Console to open your user settings file:",
  'if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")',
  'usethis::edit_r_environ(scope = "user")',
  "In the text editor, preserve all existing settings and add this line, replacing your_key_here with the key from CARTO:",
  'CARTO_API_KEY="your_key_here"',
  "Save the file. Never share your key or paste it into a report."
)
workspace_home <- function() path.expand("~")
workspace_saved_key <- function(start_folder) {
  explicit <- Sys.getenv("R_ENVIRON_USER", unset = NA_character_)
  if (!is.na(explicit) && !nzchar(explicit)) {
    return(list(state = "unknown", path = NULL, source = "R_ENVIRON_USER"))
  }
  arch <- sub("^/", "", .Platform$r_arch)
  choose <- function(folder) {
    paths <- c(if (nzchar(arch)) file.path(folder, paste0(".Renviron.", arch)), file.path(folder, ".Renviron"))
    present <- paths[file.exists(paths)]
    if (length(present)) present[[1]] else NULL
  }
  local <- choose(start_folder)
  user <- choose(workspace_home())
  source <- if (!is.na(explicit)) "R_ENVIRON_USER" else if (!is.null(local)) "workspace" else "user"
  if (!is.na(explicit)) {
    expanded <- path.expand(explicit)
    absolute <- startsWith(expanded, "/") || startsWith(expanded, "\\") ||
      grepl("^[A-Za-z]:", expanded)
    explicit <- if (absolute) expanded else file.path(start_folder, expanded)
  }
  path <- if (!is.na(explicit)) workspace_path(explicit) else if (!is.null(local)) local else
    if (!is.null(user)) user else file.path(workspace_home(), ".Renviron")
  result <- list(state = "absent", path = path, source = source)
  if (!file.exists(path)) return(result)
  if (dir.exists(path) || file.access(path, 4) != 0) {
    result$state <- "unknown"
    return(result)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  assignments <- lines[grepl("^\\s*CARTO_API_KEY\\s*=", lines)]
  if (!length(assignments)) return(result)
  value <- trimws(sub("^\\s*CARTO_API_KEY\\s*=\\s*", "", utils::tail(assignments, 1)))
  # Recognise only simple literals. R's ${...} substitutions and unusual
  # quoting/escapes need manual verification; never execute a startup file.
  if (grepl("[$\\\\]", value)) {
    result$state <- "unknown"
  } else if (startsWith(value, '"') || startsWith(value, "'")) {
    quote <- substr(value, 1, 1)
    if (nchar(value) < 2L || substr(value, nchar(value), nchar(value)) != quote ||
        grepl(quote, substr(value, 2, nchar(value) - 1), fixed = TRUE)) {
      result$state <- "unknown"
    } else {
      result$state <- if (nzchar(trimws(substr(value, 2, nchar(value) - 1)))) "present" else "absent"
    }
  } else {
    result$state <- if (nzchar(value)) "present" else "absent"
  }
  result
}
