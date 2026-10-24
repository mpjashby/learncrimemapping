# Saved artefacts are collected only after execution has stopped. Patterns are
# relative to the exercise root; never inspect caches, libraries or checker plots.
validate_saved_files <- function(patterns) {
  if (!is.character(patterns) || anyNA(patterns) ||
      any(!nzchar(patterns) | grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.?(/|$)|[[:cntrl:]])", patterns)) ||
      any(grepl("(^|/)(checker|rosm.cache|[.][^/]*)(/|$)", patterns))) {
    stop("saved_files must contain relative output patterns without parent or hidden paths.")
  }
  patterns
}

submission_output_snapshot <- function(workspace, patterns) {
  paths <- unique(unlist(lapply(patterns, function(pattern)
    Sys.glob(file.path(workspace, pattern))), use.names = FALSE))
  paths <- paths[!dir.exists(paths)]
  if (any(!utils::file_test("-f", paths))) stop("Saved outputs must be regular files.")
  if (!length(paths)) return(stats::setNames(character(), character()))
  root <- paste0(normalizePath(workspace, winslash = "/"), "/")
  resolved <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  if (any(!startsWith(resolved, root)) ||
      any(nzchar(Sys.readlink(paths)), na.rm = TRUE)) stop("Unsafe saved output path.")
  info <- file.info(paths)
  # Do not hash unbounded student files; oversized files still get a diagnostic.
  hashes <- rep("oversized", length(paths))
  small <- !is.na(info$size) & info$size <= 10 * 1024^2
  hashes[small] <- unname(tools::md5sum(paths[small]))
  stats::setNames(paste(info$size, as.numeric(info$mtime), hashes), paths)
}

collect_submission_outputs <- function(result, workspace, patterns, before,
                                       report_missing = TRUE) {
  result$saved_files <- list()
  result$saved_file_notes <- character()
  if (!length(patterns)) return(result)
  after <- submission_output_snapshot(workspace, patterns)
  changed <- names(after)[is.na(before[names(after)]) | after != before[names(after)]]
  changed <- changed[!is.na(changed)]
  missing <- patterns[!vapply(patterns, function(pattern)
    any(changed %in% Sys.glob(file.path(workspace, pattern))), logical(1))]
  if (report_missing && length(missing)) result$saved_file_notes <- paste0(
    "No new or changed saved file matched: ", missing, ". Execution status: ",
    result$execution$status, ".")
  if (length(changed) > 10L) result$saved_file_notes <- c(result$saved_file_notes,
    "Only the first 10 saved files are included.")
  total <- 0
  preview_total <- 0
  for (path in utils::head(sort(changed), 10L)) {
    relative <- substring(path, nchar(workspace) + 2L)
    entry <- list(file = relative, previews = character(), status = "unavailable")
    entry <- tryCatch({
      size <- file.info(path)$size
      total <- total + size
      if (!is.finite(size) || size == 0 || size > 10 * 1024^2 || total > 30 * 1024^2)
        stop("Saved file exceeds the 10 MB per-file / 30 MB total limit, or is empty.")
      previews <- saved_file_previews(path)
      preview_total <- preview_total + sum(nchar(previews$images, type = "bytes"))
      if (preview_total > 40 * 1024^2) stop("Total embedded previews exceed the 40 MB limit.")
      entry$previews <- previews$images
      entry$note <- previews$note
      entry$status <- "included"
      entry
    }, error = function(cnd) {
      entry$note <- gsub(workspace, "<workspace>", conditionMessage(cnd), fixed = TRUE)
      entry
    })
    result$saved_files[[length(result$saved_files) + 1L]] <- entry
  }
  result
}

saved_file_previews <- function(path) {
  extension <- tolower(tools::file_ext(path))
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  signature <- as.integer(utils::head(bytes, 8L))
  if (extension %in% c("png", "jpg", "jpeg")) {
    png <- identical(signature, c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L))
    jpeg <- identical(utils::head(signature, 3L), c(255L, 216L, 255L))
    if ((extension == "png" && !png) || (extension != "png" && !jpeg))
      stop("The saved file does not have the expected image signature.")
    if ((png && (length(bytes) < 45L ||
        !identical(utils::tail(bytes, 12L)[5:8], charToRaw("IEND")))) ||
        (jpeg && !identical(as.integer(utils::tail(bytes, 2L)), c(255L, 217L))))
      stop("The saved image is incomplete or has an invalid trailer.")
    # Inspect headers without decoding potentially enormous pixel arrays.
    dimensions <- saved_image_dimensions(bytes, png)
    if (any(!is.finite(dimensions)) || any(dimensions <= 0) ||
        any(dimensions > 20000) || prod(dimensions) > 40e6)
      stop("Saved image exceeds the 20,000-pixel / 40-megapixel limit or has an invalid header.")
    mime <- if (png) "image/png" else "image/jpeg"
    return(list(images = paste0("data:", mime, ";base64,", jsonlite::base64_enc(bytes)), note = NULL))
  }
  if (extension != "pdf") stop("Preview format is unsupported; use PNG, JPEG or PDF.")
  if (!identical(rawToChar(utils::head(bytes, 5L)), "%PDF-")) stop("Invalid PDF signature.")
  converter <- Sys.which("pdftoppm")
  if (!nzchar(converter)) stop("PDF preview unavailable: install Poppler (pdftoppm) on the marking computer.")
  temporary <- tempfile("saved-pdf-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  # Rendering is bounded in time, dimensions and page count. One extra page
  # lets the report explicitly flag truncation without parsing PDF metadata.
  processx::run(converter, c("-f", "1", "-l", "6", "-scale-to", "1600",
    "-png", path, file.path(temporary, "page")), timeout = 30000,
    error_on_status = TRUE)
  pages <- sort(list.files(temporary, pattern = "[.]png$", full.names = TRUE))
  if (!length(pages)) stop("PDF conversion produced no preview.")
  images <- vapply(utils::head(pages, 5L), function(page) {
    if (file.info(page)$size > 10 * 1024^2) stop("PDF preview exceeds the size limit.")
    paste0("data:image/png;base64,", jsonlite::base64_enc(
      readBin(page, "raw", n = file.info(page)$size)))
  }, character(1))
  list(images = images, note = if (length(pages) > 5L) "Only the first five PDF pages are shown." else NULL)
}

saved_image_dimensions <- function(bytes, png) {
  b <- as.integer(bytes)
  if (png) {
    if (length(b) < 24L || rawToChar(bytes[13:16]) != "IHDR") return(c(NA, NA))
    return(c(sum(b[17:20] * 256^(3:0)), sum(b[21:24] * 256^(3:0))))
  }
  i <- 3L
  while (i + 8L <= length(b)) {
    if (b[i] != 255L) break
    while (i < length(b) && b[i] == 255L) i <- i + 1L
    marker <- b[i]
    if (marker %in% c(217L, 218L)) break
    if (marker %in% c(1L, 208:215)) { i <- i + 1L; next }
    size <- b[i + 1L] * 256 + b[i + 2L]
    if (size < 2L || i + size > length(b)) break
    if (marker %in% c(192:195, 197:199, 201:203, 205:207))
      return(c(b[i + 6L] * 256 + b[i + 7L], b[i + 4L] * 256 + b[i + 5L]))
    i <- i + size + 1L
  }
  c(NA, NA)
}

feedback_saved_files <- function(result) {
  if (!length(result$saved_files) && !length(result$saved_file_notes)) return(character())
  e <- feedback_html_escape
  blocks <- c("<section><h2>Saved files produced by your code</h2>",
    if (length(result$saved_file_notes))
      paste0("<p>", e(result$saved_file_notes), "</p>"))
  for (entry in result$saved_files) {
    blocks <- c(blocks, paste0("<h3><code>", e(entry$file), "</code></h3>"))
    for (i in seq_along(entry$previews)) {
      blocks <- c(blocks, paste0("<figure><img alt='Saved output: ", e(entry$file),
        ", page ", i, "' src='", e(entry$previews[i]), "'><figcaption>",
        e(entry$file), if (length(entry$previews) > 1L) paste0(" \u2014 page ", i),
        "</figcaption></figure>"))
    }
    if (length(entry$note)) blocks <- c(blocks, paste0("<p>", e(entry$note), "</p>"))
  }
  c(blocks, "</section>")
}
