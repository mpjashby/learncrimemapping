# This self-contained closure is passed to the isolated execution process.
# Observers inspect actual sf method arguments, including variables and pipes;
# no student expressions are rewritten or evaluated a second time.
code_spatial_observer <- function(record) {
  # Prevent our own transformations from recursively triggering inspection.
  active <- FALSE

  # Track nested point-on-surface calls for the separate warning filter.
  point_stack <- logical()

  # Without JSON support we cannot read EPSG metadata, so install no observers.
  empty <- list(point_warning = function() FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(empty)
  }

  # Estimate one dataset location by minimising total spherical distance to
  # equally weighted feature centroids. This does not measure its full extent.
  locate <- function(x) {
    # CRS84 explicitly orders coordinates as longitude, latitude. Transform
    # from the source CRS independently of the target CRS being assessed.
    geographic <- sf::st_transform(x, "OGC:CRS84")

    # Empty features have no location and should not count as Null Island.
    geographic <- geographic[!sf::st_is_empty(geographic)]

    # Work on a temporary spherical copy, even if sf's global s2 setting is
    # disabled. Delay validation until crossing edges have been split below.
    geography <- sf::st_as_s2(geographic, check = FALSE)

    # Always rebuild, explicitly splitting crossings: rebuild = TRUE alone
    # leaves self-crossing rings intact with default s2 options. Repair keeps
    # one geometry per original feature, even when it creates several parts.
    geography <- s2::s2_rebuild(
      geography,
      options = s2::s2_options(split_crossing_edges = TRUE)
    )

    # Splitting can leave component rings with inconsistent orientations.
    # Reconvert with sf's default orientation handling and validation enabled
    # before using their centroids. No original student geometry is changed.
    geography <- sf::st_as_s2(sf::st_as_sfc(geography))

    # Count features whose entire geometry is at the origin. A polygon merely
    # touching or containing it does not count. Distances are metres; the tiny
    # tolerance of one micrometre accommodates floating-point round-off.
    null_count <- sum(
      s2::s2_max_distance(geography, s2::s2_lnglat(0, 0)) < 1e-6,
      na.rm = TRUE
    )

    # Record this before estimating the median so its warning survives an
    # inconclusive centre or missing target metadata. Do not remove these data.
    record(
      "null_island",
      paste(null_count, "features at Null Island"),
      details = list(count = null_count)
    )

    # Each feature contributes one spherical centroid, regardless of vertex
    # count. s2 accepts mixed geometry columns without st_coordinates().
    xy <- as.matrix(as.data.frame(
      s2::as_s2_lnglat(s2::s2_centroid(geography))
    ))

    # Undefined centroids must produce an incomplete check rather than a pass.
    if (!nrow(xy) || any(!is.finite(xy))) {
      stop("No reliable feature locations were available.")
    }

    # Convert degrees to radians for the trigonometric functions below.
    radians <- xy * pi / 180

    # Unit vectors avoid a discontinuity at longitudes -180 and +180 degrees.
    xyz <- cbind(
      cos(radians[, 2]) * cos(radians[, 1]),
      cos(radians[, 2]) * sin(radians[, 1]),
      sin(radians[, 2])
    )

    # Convert a candidate longitude/latitude pair in radians to a unit vector.
    vector <- function(a) {
      c(cos(a[2]) * cos(a[1]), cos(a[2]) * sin(a[1]), sin(a[2]))
    }

    # A geometric median minimises distances, rather than squared distances.
    objective <- function(a) {
      # Subtract the candidate from every feature vector. The length of each
      # difference is a straight chord through the unit sphere.
      cross <- sqrt(rowSums(sweep(xyz, 2, vector(a), "-")^2))

      # Convert chord lengths to great-circle angles. Clamp rounding errors
      # that could otherwise give asin() an argument slightly greater than 1.
      angle <- 2 * asin(pmin(1, cross / 2))

      # Multiplying by Earth's radius would not change the minimising point.
      sum(angle)
    }

    # Coordinate-wise medians give a robust initial direction, not the final
    # spherical median. Also start at the first and last feature locations.
    centre <- apply(xyz, 2, stats::median)
    starts <- rbind(
      c(
        atan2(centre[2], centre[1]),
        atan2(centre[3], sqrt(sum(centre[1:2]^2)))
      ),
      radians[1, ],
      radians[nrow(radians), ]
    )

    # Nelder-Mead needs no derivatives at the sharp minimum of distance to a
    # feature. Multiple starts reduce dependence on local minima, but cannot
    # guarantee a global minimum for arbitrary worldwide data.
    fits <- lapply(seq_len(nrow(starts)), function(i) {
      stats::optim(
        starts[i, ],
        objective,
        method = "Nelder-Mead",
        control = list(maxit = 2000, reltol = 1e-12)
      )
    })

    # Keep starting points too: one may already be an exact median that a
    # numerical optimiser cannot improve on. Score every candidate equally.
    candidates <- rbind(starts, do.call(rbind, lapply(fits, function(f) f$par)))
    values <- apply(candidates, 1, objective)

    # Require a converged fit unless a candidate already has effectively zero
    # total distance, as happens with a single feature or coincident features.
    if (
      min(values) > 1e-8 &&
        !any(vapply(fits, function(f) f$convergence == 0L, logical(1)))
    ) {
      stop("The spherical geometric median did not converge.")
    }

    # Choose the candidate with the smallest total great-circle distance.
    best <- candidates[which.min(values), ]

    # A near-zero mean direction flags balanced antipodal or symmetric data.
    # This conservative guard does not detect every possible nonunique median.
    direction <- colMeans(xyz)
    if (sqrt(sum(direction^2)) < 1e-8) {
      stop("The dataset has no unambiguous spherical centre.")
    }

    # Normalise the chosen angles through a unit vector, then return longitude
    # and latitude in degrees for comparison with the EPSG bounds.
    location <- unname(vector(best))
    c(
      X = atan2(location[2], location[1]) * 180 / pi,
      Y = atan2(location[3], sqrt(sum(location[1:2]^2))) * 180 / pi
    )
  }

  # Observe a transformation without replacing or modifying its student input.
  inspect <- function(x, crs, pipeline) {
    # Our inverse transformation also enters this traced method. Skip nested
    # inspection and always release the guard, including after an error.
    if (active) {
      return(invisible(NULL))
    }
    active <<- TRUE
    on.exit(active <<- FALSE)

    # Suppress conditions from our checks only; student conditions are captured
    # separately. Convert checker errors into incomplete-check observations.
    result <- tryCatch(
      suppressWarnings(suppressMessages({
        # Begin with an inconclusive result until all required checks succeed.
        target <- sf::st_crs(crs)
        answer <- list(
          status = "not_checked",
          epsg = target$epsg,
          name = target$Name,
          reason = ""
        )

        # Locate usable input before assessing the target so custom targets
        # can still receive the independent Null Island warning.
        location <- if (
          !is.na(sf::st_crs(x)) && length(x) && !all(sf::st_is_empty(x))
        ) {
          locate(x)
        } else {
          NULL
        }

        # Custom pipelines may override the usual CRS transformation, while
        # non-EPSG targets have no authoritative EPSG extent to compare.
        if (length(pipeline)) {
          answer$reason <- "A custom transformation pipeline was supplied."
        } else if (
          is.na(target) || is.null(target$epsg) || is.na(target$epsg)
        ) {
          answer$reason <- "The target CRS has no identifiable EPSG code."
        } else if (is.na(sf::st_crs(x))) {
          # Coordinates cannot locate a feature without a declared source CRS.
          answer$reason <- "The input geometry has no source CRS."
        } else if (!length(x) || all(sf::st_is_empty(x))) {
          # Plotting commonly transforms empty placeholders; retain this in
          # the log without treating it as a student problem.
          answer$reason <- "There is no nonempty geometry to locate."
        } else {
          # Read the EPSG entry from the local PROJ database, rather than
          # trusting user-supplied names or area metadata. No network is used.
          definition <- jsonlite::fromJSON(
            sf::st_crs(target$epsg)$ProjJson,
            simplifyVector = FALSE
          )

          # PROJ JSON contains either several usage records or one root record.
          usages <- if (!is.null(definition$usages)) {
            definition$usages
          } else {
            list(definition)
          }

          # Extract each extent in a consistent west/south/east/north order.
          boxes <- lapply(usages, function(u) {
            unlist(u$bbox[c(
              "west_longitude",
              "south_latitude",
              "east_longitude",
              "north_latitude"
            )])
          })

          # Missing or nonfinite bounds cannot establish a successful check.
          boxes <- Filter(
            function(b) {
              length(b) == 4L && all(is.finite(b))
            },
            boxes
          )
          if (!length(boxes)) {
            answer$reason <- paste(
              "The installed EPSG database has no area-of-use bounds",
              "for this CRS."
            )
          } else {
            # Use the existing coordinate matrix layout for this single point.
            xy <- matrix(
              location,
              nrow = 1,
              dimnames = list(NULL, c("X", "Y"))
            )

            # Reject undefined coordinates or impossible geographic latitudes.
            if (!nrow(xy) || any(!is.finite(xy)) || any(abs(xy[, "Y"]) > 90)) {
              answer$reason <- paste(
                "The input could not be located reliably",
                "in longitude/latitude."
              )
            } else {
              # Wrap longitude into [-180, 180), keeping latitude in degrees.
              longitude <- ((xy[, "X"] + 180) %% 360) - 180
              latitude <- xy[, "Y"]

              # Membership in any usage box is sufficient. The tolerance is
              # in degrees and only softens floating-point boundary effects.
              inside <- rep(FALSE, length(longitude))
              tolerance <- 1e-7
              for (b in boxes) {
                # Ordinary extents contain longitudes between west and east.
                # Antimeridian-crossing extents instead use either outer side.
                east_west <- if (b[1] <= b[3]) {
                  longitude >= b[1] - tolerance &
                    longitude <= b[3] + tolerance
                } else {
                  longitude >= b[1] - tolerance |
                    longitude <= b[3] + tolerance
                }

                # -180 and +180 identify the same meridian after wrapping.
                if (b[3] == 180) {
                  east_west <- east_west | abs(longitude + 180) < tolerance
                }

                # Combine longitude and latitude membership, accumulating
                # matches across all usage boxes with logical OR.
                inside <- inside |
                  (east_west &
                    latitude >= b[2] - tolerance &
                    latitude <= b[4] + tolerance)
              }

              # Preserve the existing result schema. A single representative
              # point gives inside/outside; partial supports earlier results.
              answer$status <- if (all(inside)) {
                "inside"
              } else if (any(inside)) {
                "partial"
              } else {
                "outside"
              }

              # Store readable area descriptions alongside the numeric bounds
              # so feedback provides evidence for the CRS finding.
              answer$area <- paste(
                vapply(
                  usages,
                  function(u) {
                    if (is.null(u$area)) {
                      "Area described by EPSG bounds"
                    } else {
                      u$area
                    }
                  },
                  character(1)
                ),
                collapse = "; "
              )
              answer$bounds <- boxes

              # Keep the existing bounds fields for report compatibility.
              # With one point, west equals east and south equals north.
              answer$data_bounds <- c(
                west = min(longitude),
                south = min(latitude),
                east = max(longitude),
                north = max(latitude)
              )
              answer$centre <- location
              answer$method <- "Equal-feature spherical geometric median"
            }
          }
        }

        # Return the completed observation to the error-handling wrapper.
        answer
      })),
      error = function(e) {
        list(
          status = "not_checked",
          reason = paste(
            "The CRS area check could not be completed:",
            conditionMessage(e)
          )
        )
      }
    )

    # Record both readable text and structured evidence for later reporting.
    text <- if (result$status == "not_checked") {
      result$reason
    } else {
      paste0(
        "EPSG:",
        result$epsg,
        " (",
        result$name,
        "): ",
        result$status,
        " the EPSG area-of-use bounds."
      )
    }
    record("crs", text, details = result)
    invisible(NULL)
  }

  # Remember whether a point-on-surface call received only point features.
  point_enter <- function(x) {
    # A failed geometry-type lookup should leave its warning unsuppressed.
    is_point <- tryCatch(
      length(x) > 0L &&
        all(as.character(sf::st_geometry_type(x)) == "POINT"),
      error = function(e) FALSE
    )

    # A stack handles nested calls without losing the outer call's context.
    point_stack <<- c(point_stack, is_point)
    invisible(NULL)
  }

  # Remove the context for the point-on-surface call that just returned.
  point_exit <- function() {
    point_stack <<- utils::head(point_stack, -1L)
    invisible(NULL)
  }

  # Install tracers in sf's namespace without rewriting student expressions.
  install <- function(...) {
    tryCatch(
      {
        # bquote() embeds our closure in the tracer, which reads the actual
        # method arguments, including values supplied through variables/pipes.
        suppressMessages(invisible(trace(
          "st_transform.sfc",
          where = asNamespace("sf"),
          tracer = bquote(.(inspect)(x, crs, pipeline)),
          print = FALSE
        )))

        # Entry/exit hooks keep the point-warning context balanced per call.
        suppressMessages(invisible(trace(
          "st_point_on_surface.sfc",
          where = asNamespace("sf"),
          tracer = bquote(.(point_enter)(x)),
          exit = bquote(.(point_exit)()),
          print = FALSE
        )))
      },
      # Tracing failures are checker observations, not student runtime errors.
      error = function(e) record("spatial_checker", conditionMessage(e))
    )
  }

  # Install immediately if sf is loaded; otherwise wait for the student's
  # namespace load so its startup messages remain visible to evaluate.
  if ("sf" %in% loadedNamespaces()) {
    install()
  } else {
    setHook(packageEvent("sf", "onLoad"), install, action = "append")
  }

  # Expose only the current call's point context to the warning filter.
  list(point_warning = function() {
    length(point_stack) > 0L && utils::tail(point_stack, 1L)
  })
}

# Convert a structured CRS observation into an actionable student finding.
code_crs_issue <- function(event, source) {
  # Successful checks and absent observations need no feedback.
  x <- event$details
  if (is.null(x) || identical(x$status, "inside")) {
    return(NULL)
  }

  # Empty plotting placeholders provide no location evidence. Retain their
  # observation in the log without presenting a student issue.
  if (identical(x$reason, "There is no nonempty geometry to locate.")) {
    return(NULL)
  }

  # Explain incomplete checks without claiming that the target CRS is wrong.
  if (identical(x$status, "not_checked")) {
    return(code_issue(
      "spatial.crs_unchecked",
      "incomplete",
      paste("The CRS area-of-use check was not completed.", x$reason),
      paste(
        "Review this transformation manually;",
        "no conclusion about CRS suitability was reached."
      ),
      event$line,
      evidence = source
    ))
  }

  # Format the CRS identifier and bounds consistently in the evidence panel.
  label <- paste0("EPSG:", x$epsg, " (", x$name, ")")
  extent <- function(b) paste(sprintf("%.4f", b), collapse = ", ")

  # Include the executing expression, representative location and EPSG extent
  # so staff can assess the finding without rerunning the submission.
  evidence <- paste0(
    source,
    "\nRepresentative location bounds (west, south, east, north): ",
    extent(x$data_bounds),
    "\nEPSG area bounds (west, south, east, north): ",
    paste(vapply(x$bounds, extent, character(1)), collapse = "; "),
    "\nEPSG area: ",
    x$area
  )

  # Current checks classify one representative point; retain the partial-case
  # wording for observations produced by earlier versions of the checker.
  code_issue(
    "spatial.crs_area",
    "correctness",
    if (x$status == "outside") {
      paste(
        "The representative dataset location lies outside the EPSG area",
        "of use for",
        paste0(label, ".")
      )
    } else {
      paste(
        "The spatial data extend beyond the EPSG area of use for",
        paste0(label, ".")
      )
    },
    # A median inside the area does not establish containment or suitability.
    paste(
      "Choose a CRS appropriate for the location of your data,",
      "for example using crsuggest::suggest_crs().",
      "This compares the equal-feature spherical geometric median with",
      "EPSG's approximate area bounds; it does not establish that the entire",
      "dataset is inside or that the projection is otherwise suitable.",
      "The check assumes that the source CRS was assigned correctly."
    ),
    event$line,
    evidence = evidence
  )
}
