#' Build the Docker environment used for marking
#'
#' Builds from resources bundled with the installed package; no separate source
#' folder is needed. Docker must be installed and running. The build downloads
#' packages and may take considerable time and disk space.
#' @param image Docker image name, matching `container_image` in check_submissions.
#' @param platform Linux platform, or NULL to use Docker's native platform.
#' @param packages Extra R packages to preinstall. NULL uses this package's Imports
#'   except gifski. character() builds only the core checker environment.
#' @param system_packages Additional Debian packages needed by the R packages.
#' @return Invisibly, the image name after a successful build.
#' @keywords internal
build_checker_image <- function(
    image = "learncrimemapping-checker:local", platform = NULL, packages = NULL,
    system_packages = c("git", "libgit2-dev", "libjpeg-dev", "libpng-dev",
                        "libtiff-dev", "libwebp-dev", "libicu-dev",
                        "libtbb-dev", "libnode-dev")) {
  if (!is.character(image) || length(image) != 1L || is.na(image) ||
      !nzchar(image) || startsWith(image, "-") || grepl("[[:space:]]", image)) {
    stop("image must be a single Docker image name.")
  }
  if (!is.null(platform) &&
      (length(platform) != 1L || is.na(platform) ||
       !platform %in% c("linux/arm64", "linux/amd64"))) {
    stop("platform must be NULL, 'linux/arm64' or 'linux/amd64'.")
  }
  if (is.null(packages)) {
    imports <- utils::packageDescription("learncrimemapping")$Imports
    packages <- trimws(sub("[(].*[)]", "",
                           strsplit(imports, ",", fixed = TRUE)[[1]]))
    packages <- setdiff(packages, c("gifski", "utils"))
  }
  if (!is.character(packages) || anyNA(packages) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", packages))) {
    stop("packages must contain R package names.")
  }
  if (!is.character(system_packages) || anyNA(system_packages) ||
      any(!grepl("^[a-z0-9][a-z0-9+.-]*$", system_packages))) {
    stop("system_packages must contain Debian package names.")
  }
  docker <- unname(Sys.which("docker"))
  if (!nzchar(docker) && identical(Sys.info()[["sysname"]], "Darwin")) {
    candidates <- c(path.expand("~/.docker/bin/docker"),
                    "/Applications/Docker.app/Contents/Resources/bin/docker")
    found <- candidates[file.exists(candidates) & file.access(candidates, 1) == 0]
    if (length(found)) docker <- found[1]
  }
  if (!nzchar(docker)) stop("Install Docker Desktop and start it before building the image.")
  resources <- system.file("container", package = "learncrimemapping")
  context <- tempfile("learncrimemapping-image-")
  dir.create(context)
  on.exit(unlink(context, recursive = TRUE), add = TRUE)
  prepare_checker_image_context(resources, context)
  args <- c("build", if (!is.null(platform)) c("--platform", platform),
            "--build-arg", paste0("EXERCISE_PACKAGES=", paste(unique(packages), collapse = ",")),
            "--build-arg", paste0("EXERCISE_SYSTEM_PACKAGES=", paste(unique(system_packages), collapse = " ")),
            "-f", file.path(context, "inst", "container", "Dockerfile"),
            "-t", image, context)
  # Docker Desktop's credential helper must be discoverable even when R started
  # before Docker's command-line tools were added to PATH.
  path <- paste(dirname(docker), Sys.getenv("PATH"), sep = .Platform$path.sep)
  message("Building ", image, ". The first build may take considerable time.")
  environment <- Sys.getenv()
  environment["PATH"] <- path
  processx::run(docker, args, env = environment, timeout = Inf,
                echo = TRUE, error_on_status = TRUE)
  message("Checker image ready: ", image)
  invisible(image)
}

# Keep the build context limited to trusted resources, never the marking folder.
prepare_checker_image_context <- function(resources, context) {
  files <- c("Dockerfile", "install-packages.R", "runner.R",
             "R/check_code.R", "R/code_feedback.R", "R/spatial_feedback.R")
  if (!nzchar(resources) || !all(file.exists(file.path(resources, files)))) {
    stop("Docker resources are missing. Reinstall an updated learncrimemapping package.")
  }
  dir.create(file.path(context, "inst", "container"), recursive = TRUE)
  dir.create(file.path(context, "R"))
  targets <- c(file.path(context, "inst", "container", files[1:3]),
               file.path(context, files[4:6]))
  if (!all(file.copy(file.path(resources, files), targets))) {
    stop("Could not prepare the Docker build files.")
  }
  invisible(context)
}
