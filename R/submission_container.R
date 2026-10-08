# Arguments are passed directly to processx, never interpreted by a shell.
submission_docker <- function(docker, args, timeout = 30000) {
  processx::run(docker, args, timeout = timeout, error_on_status = TRUE,
                echo = FALSE)
}

submission_container_config <- function(image, memory, cpus) {
  scalar <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(x)
  if (!scalar(image) || startsWith(image, "-") || grepl("[[:space:]]", image)) {
    stop("container_image must be a single Docker image name.")
  }
  if (!scalar(memory) || !grepl("^[1-9][0-9]*[kKmMgG]?$", memory)) {
    stop("container_memory must be a Docker size such as '4g'.")
  }
  if (!is.numeric(cpus) || length(cpus) != 1L || is.na(cpus) ||
      !is.finite(cpus) || cpus <= 0) stop("container_cpus must be positive.")
  docker <- unname(Sys.which("docker"))
  # R may have started before Docker Desktop added its CLI directory to PATH.
  if (!nzchar(docker) && identical(Sys.info()[["sysname"]], "Darwin")) {
    candidates <- c(path.expand("~/.docker/bin/docker"),
                    "/Applications/Docker.app/Contents/Resources/bin/docker")
    found <- candidates[file.exists(candidates) & file.access(candidates, 1) == 0]
    if (length(found)) docker <- found[1]
  }
  if (!nzchar(docker)) {
    stop("Docker is required for batch checking. Install/start Docker and build the checker image; see MARKING.md. No local fallback was attempted.")
  }
  info <- tryCatch(submission_docker(docker,
    c("image", "inspect", "--format", "{{.Id}} {{.Os}} {{.Architecture}}", image))$stdout,
    error = function(e) stop("Docker cannot inspect the checker image. Start Docker and build ",
                             image, "; see MARKING.md. No local fallback was attempted."))
  fields <- strsplit(trimws(info), "[[:space:]]+")[[1]]
  if (length(fields) != 3L || fields[2] != "linux" ||
      !fields[3] %in% c("amd64", "arm64")) {
    stop("The checker requires a Linux amd64 or arm64 container image.")
  }
  # Match ownership of the writable host workspace on Unix hosts, including
  # Docker Desktop mounts on macOS. Windows uses the image's non-root UID.
  user <- "1000:1000"
  if (Sys.info()[["sysname"]] %in% c("Linux", "Darwin")) {
    uid <- trimws(processx::run("id", "-u")$stdout)
    gid <- trimws(processx::run("id", "-g")$stdout)
    if (uid == "0") stop("Run batch checking as a non-root host user.")
    user <- paste(uid, gid, sep = ":")
  }
  list(docker = docker, image = fields[1], memory = memory,
       cpus = cpus, user = user, platform = paste0("linux/", fields[3]))
}

submission_container_args <- function(workspace, name, config) {
  # Docker's --mount syntax uses commas as separators.
  if (grepl("[,\r\n]", workspace)) {
    stop("Container workspace paths must not contain commas or newlines.")
  }
  c("run", "--detach", "--name", name, "--pull", "never",
    "--platform", config$platform,
    "--read-only", "--user", config$user, "--cap-drop", "ALL",
    "--security-opt", "no-new-privileges:true",
    "--memory", config$memory, "--memory-swap", config$memory,
    "--cpus", as.character(config$cpus), "--pids-limit", "256",
    "--log-driver", "local", "--log-opt", "max-size=5m",
    "--log-opt", "max-file=1", "--network", "bridge", "--ipc", "none",
    "--mount", paste0("type=bind,source=", workspace, ",target=/workspace"),
    "--workdir", "/workspace",
    "--env", "HOME=/workspace/.sandbox/home",
    "--env", "TMPDIR=/workspace/.sandbox/tmp",
    "--env", "R_LIBS_USER=/workspace/.sandbox/library",
    "--env", "XDG_CACHE_HOME=/workspace/.sandbox/cache",
    "--env", "XDG_CONFIG_HOME=/workspace/.sandbox/config",
    "--env", "XDG_DATA_HOME=/workspace/.sandbox/data",
    "--env", "R_ENVIRON_USER=/dev/null", "--env", "R_PROFILE_USER=/dev/null",
    "--entrypoint", "Rscript", config$image,
    "--vanilla", "/opt/learncrimemapping-runner.R")
}

check_submission_container <- function(file, reprex, style, workspace, timeout,
                                       profile, report_dir, config) {
  workspace <- normalizePath(workspace, winslash = "/", mustWork = TRUE)
  sandbox <- file.path(workspace, ".sandbox")
  if (file.exists(sandbox)) stop("The workspace template must not contain .sandbox.")
  for (directory in c("home", "tmp", "library", "cache", "config", "data")) {
    dir.create(file.path(sandbox, directory), recursive = TRUE, showWarnings = FALSE)
  }
  # Preserve the original filename for feedback rules and source positions.
  source <- file.path(sandbox, basename(file))
  if (!file.copy(file, source)) stop("Could not stage the submission.")
  saveRDS(list(file = paste0("/workspace/.sandbox/", basename(file)),
               reprex = reprex, style = style, timeout = timeout, profile = profile),
          file.path(sandbox, "request.rds"))
  name <- paste0("lcm-", Sys.getpid(), "-", basename(tempfile()))
  # Removing the container also terminates descendants/background processes.
  on.exit(try(submission_docker(config$docker, c("rm", "--force", name)),
              silent = TRUE), add = TRUE)
  submission_docker(config$docker, submission_container_args(workspace, name, config))
  completion <- tryCatch(submission_docker(config$docker, c("wait", name),
                                          timeout = (timeout + 30) * 1000),
                         error = function(e) e)
  # Stop everything before reading workspace files, even after normal R exit.
  submission_docker(config$docker, c("rm", "--force", name))
  assert_submission_workspace_files(workspace)
  result_file <- file.path(sandbox, "result.rds")
  if (inherits(completion, "error")) {
    result <- invalid_submission_result(file, profile,
      "The container exceeded its time limit or could not be monitored.", checker = TRUE)
    if (inherits(completion, "system_command_timeout_error")) {
      result$execution$status <- "timeout"
    }
  } else if (trimws(completion$stdout) != "0" || !file.exists(result_file)) {
    result <- invalid_submission_result(file, profile,
      "The checker container failed or exceeded its resource limits. Check the image and resource settings.",
      checker = TRUE)
  } else {
    if (file.info(result_file)$size > 20 * 1024^2) {
      stop("The container result exceeds the 20 MB report limit.")
    }
    result <- readRDS(result_file)
    if (!is.list(result) || !is.list(result$execution) || !is.list(result$issues) ||
        !is.data.frame(result$lints)) stop("Invalid container checking result.")
    result$execution$events <- lapply(result$execution$events, function(event) {
      if (identical(event$kind, "plot")) {
        if (!is.character(event$plot) || length(event$plot) != 1L ||
            !grepl("^/workspace/checker/plot-[0-9]+[.]png$", event$plot)) {
          stop("Invalid plot path returned by the container.")
        }
        event$plot <- file.path(report_dir, basename(event$plot))
      }
      event
    })
    result$report <- if (is.null(result$report)) NULL else file.path(report_dir, "report.txt")
  }
  result$file <- file
  result$execution_dir <- workspace
  result$versions <- c(result$versions, container_image = config$image)
  result
}

# A container-created link must never redirect host report reads outside its mount.
assert_submission_workspace_files <- function(workspace) {
  pending <- workspace
  while (length(pending)) {
    directory <- pending[1]
    pending <- pending[-1]
    files <- list.files(directory, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    links <- Sys.readlink(files)
    # Inspect links before descending: recursive list.files follows directory links.
    if (any(!is.na(links) & nzchar(links))) {
      stop("The student workspace contains symbolic links; host report processing was refused.")
    }
    roots <- normalizePath(files, winslash = "/", mustWork = FALSE)
    if (any(!startsWith(roots, paste0(workspace, "/")))) {
      stop("A student output path escaped its workspace.")
    }
    pending <- c(pending, files[dir.exists(files)])
  }
  invisible(TRUE)
}
