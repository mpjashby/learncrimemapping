#' Create your Learn Crime Mapping workspace directories
#'
#' Create `data/raw`, `data/processed`, `R` and `output` in the current
#' working directory. Run this once when setting up your workspace.
#'
#' @details The current working directory must be named `crime_mapping`.
#'   All paths are checked before any directories are created. Existing setup
#'   directories or files at those paths cause an error; nothing is overwritten.
#' @return Invisibly, the paths of the created directories.
#' @examples
#' \dontrun{
#' # With crime_mapping open as your working directory:
#' learncrimemapping::create_dirs()
#' }
#' @export
create_dirs <- function() {
  folder <- getwd()
  if (!identical(workspace_actual_name(folder), "crime_mapping")) {
    cli::cli_abort(c(
      "The working directory must be named {.file crime_mapping}.",
      "i" = "The current working directory is {.file {folder}}.",
      "i" = "Open crime_mapping in Positron and restart R before running create_dirs()."
    ))
  }
  directories <- c("data", "data/raw", "data/processed", "R", "output")
  paths <- file.path(folder, directories)
  existing <- directories[file.exists(paths) | dir.exists(paths)]
  if (length(existing)) {
    cli::cli_abort(c(
      "Cannot create the workspace directories: these setup paths already exist: {.file {existing}}.",
      "i" = "create_dirs() requires a workspace with no setup directories. Run check_workspace() to check an existing workspace."
    ))
  }
  for (path in paths) {
    if (!dir.create(path)) {
      cli::cli_abort("Could not create {.file {path}}. Check folder permissions before trying again.")
    }
  }
  cli::cli_alert_success("Created data/raw, data/processed, R and output in {.file {folder}}.")
  invisible(paths)
}
