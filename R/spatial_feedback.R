# This self-contained closure is passed to the isolated execution process.
# Observers inspect actual sf method arguments, including variables and pipes;
# no student expressions are rewritten or evaluated a second time.
code_spatial_observer <- function(record) {
  active <- FALSE
  point_stack <- logical()
  empty <- list(point_warning = function() FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(empty)

  inspect <- function(x, crs, pipeline) {
    if (active) return(invisible(NULL))
    active <<- TRUE
    on.exit(active <<- FALSE)
    result <- tryCatch(suppressWarnings(suppressMessages({
      target <- sf::st_crs(crs)
      answer <- list(status = "not_checked", epsg = target$epsg,
                     name = target$Name, reason = "")
      if (length(pipeline)) {
        answer$reason <- "A custom transformation pipeline was supplied."
      } else if (is.na(target) || is.null(target$epsg) || is.na(target$epsg)) {
        answer$reason <- "The target CRS has no identifiable EPSG code."
      } else if (is.na(sf::st_crs(x))) {
        answer$reason <- "The input geometry has no source CRS."
      } else if (!length(x) || all(sf::st_is_empty(x))) {
        answer$reason <- "There is no nonempty geometry to locate."
      } else {
        # Resolve the EPSG entry itself, rather than trusting a user-supplied
        # CRS name or attached area metadata. No online lookup is required.
        definition <- jsonlite::fromJSON(sf::st_crs(target$epsg)$ProjJson,
                                        simplifyVector = FALSE)
        usages <- if (!is.null(definition$usages)) definition$usages else list(definition)
        boxes <- lapply(usages, function(u) unlist(u$bbox[c(
          "west_longitude", "south_latitude", "east_longitude", "north_latitude")]))
        boxes <- Filter(function(b) length(b) == 4L && all(is.finite(b)), boxes)
        if (!length(boxes)) {
          answer$reason <- "The installed EPSG database has no area-of-use bounds for this CRS."
        } else {
          geographic <- sf::st_transform(x, "OGC:CRS84")
          # OGC:CRS84 explicitly uses longitude then latitude, independent of
          # the authority's EPSG:4326 axis order. All vertices are inspected.
          xy <- sf::st_coordinates(geographic)[, c("X", "Y"), drop = FALSE]
          if (!nrow(xy) || any(!is.finite(xy)) || any(abs(xy[, "Y"]) > 90)) {
            answer$reason <- "The input could not be located reliably in longitude/latitude."
          } else {
            longitude <- ((xy[, "X"] + 180) %% 360) - 180
            latitude <- xy[, "Y"]
            inside <- rep(FALSE, length(longitude))
            tolerance <- 1e-7
            for (b in boxes) {
              # Some EPSG extents cross the antimeridian (west > east).
              east_west <- if (b[1] <= b[3]) {
                longitude >= b[1] - tolerance & longitude <= b[3] + tolerance
              } else longitude >= b[1] - tolerance | longitude <= b[3] + tolerance
              # -180 and +180 are the same meridian.
              if (b[3] == 180) east_west <- east_west | abs(longitude + 180) < tolerance
              inside <- inside | (east_west & latitude >= b[2] - tolerance &
                                     latitude <= b[4] + tolerance)
            }
            answer$status <- if (all(inside)) "inside" else if (any(inside)) "partial" else "outside"
            answer$area <- paste(vapply(usages, function(u) {
              if (is.null(u$area)) "Area described by EPSG bounds" else u$area
            }, character(1)), collapse = "; ")
            answer$bounds <- boxes
            answer$data_bounds <- c(west = min(longitude), south = min(latitude),
                                    east = max(longitude), north = max(latitude))
            answer$vertices <- length(inside)
            answer$outside_vertices <- sum(!inside)
          }
        }
      }
      answer
    })), error = function(e) list(status = "not_checked",
      reason = paste("The CRS area check could not be completed:", conditionMessage(e))))
    text <- if (result$status == "not_checked") result$reason else
      paste0("EPSG:", result$epsg, " (", result$name, "): ", result$status,
             " the EPSG area-of-use bounds.")
    record("crs", text, details = result)
    invisible(NULL)
  }
  point_enter <- function(x) {
    is_point <- tryCatch(length(x) > 0L &&
      all(as.character(sf::st_geometry_type(x)) == "POINT"), error = function(e) FALSE)
    point_stack <<- c(point_stack, is_point)
    invisible(NULL)
  }
  point_exit <- function() {
    point_stack <<- utils::head(point_stack, -1L)
    invisible(NULL)
  }
  install <- function(...) {
    tryCatch({
      suppressMessages(invisible(trace("st_transform.sfc", where = asNamespace("sf"),
        tracer = bquote(.(inspect)(x, crs, pipeline)), print = FALSE)))
      suppressMessages(invisible(trace("st_point_on_surface.sfc", where = asNamespace("sf"),
        tracer = bquote(.(point_enter)(x)), exit = bquote(.(point_exit)()), print = FALSE)))
    }, error = function(e) record("spatial_checker", conditionMessage(e)))
  }
  # Do not preload sf or its imports before evaluate captures conditions.
  # Install after the student's own namespace load instead.
  if ("sf" %in% loadedNamespaces()) install() else {
    setHook(packageEvent("sf", "onLoad"), install, action = "append")
  }
  list(point_warning = function() length(point_stack) > 0L && utils::tail(point_stack, 1L))
}

code_crs_issue <- function(event, source) {
  x <- event$details
  if (is.null(x) || identical(x$status, "inside")) return(NULL)
  # ggplot routinely transforms empty geometry placeholders. They provide no
  # location evidence and are not a student problem; retain the raw observation.
  if (identical(x$reason, "There is no nonempty geometry to locate.")) return(NULL)
  if (identical(x$status, "not_checked")) {
    return(code_issue("spatial.crs_unchecked", "incomplete",
      paste("The CRS area-of-use check was not completed.", x$reason),
      "Review this transformation manually; no conclusion about CRS suitability was reached.",
      event$line, evidence = source))
  }
  label <- paste0("EPSG:", x$epsg, " (", x$name, ")")
  extent <- function(b) paste(sprintf("%.4f", b), collapse = ", ")
  evidence <- paste0(source, "\nData bounds (west, south, east, north): ",
    extent(x$data_bounds), "\nEPSG area bounds (west, south, east, north): ",
    paste(vapply(x$bounds, extent, character(1)), collapse = "; "),
    "\nEPSG area: ", x$area)
  code_issue("spatial.crs_area", "correctness",
    if (x$status == "outside")
      paste("The spatial data lie outside the EPSG area of use for", paste0(label, ".")) else
      paste("The spatial data extend beyond the EPSG area of use for", paste0(label, ".")),
    paste("Choose a CRS appropriate for the location of your data, for example using crsuggest::suggest_crs().",
          "This compares geometry vertices with EPSG's approximate area bounds; it does not prove that a projection is otherwise suitable.",
          "The check assumes that the source CRS was assigned correctly."),
    event$line, evidence = evidence)
}
