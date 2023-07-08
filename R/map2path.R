#' Return the path from a likelihood map
#'
#' Find the position of the highest value in a map, typically most probable value in a likelihood
#' map.
#'
#' Note that this path is the most likely, considering only the observation model and ignoring the
#' movement model. Prefer to use `graph_most_likely()` for the most realistic path.
#'
#' `interp` can be used to interpolate unrealistic position from short stationary period based on
#' the position of the longer ones. The interpolation assumes that the first and last stationary
#' period can be safely estimated from the probability map.
#'
#' @inheritParams geopressure_map
#' @inheritParams graph_create
#' @param interp The position of the stationary period shorter than `interp` will be
#' replace by a linear average from other position (in days) .
#' @param .use_known If true, enforce the known position defined in `tag` in the path created. Known
#' position are not interpolated (even if shorter than `interp`) and used in the interpolation. In
#' most (all?) case, the likelihood map was computed using known, and therefore will result in the
#' same position (approx. to the map resolution).
#' @return A path data.frame
#' - `stap_id` stationary period
#' - `ind` indices of the coordinate in the 2D grid. Useful to retrieve map or graph information.
#' - `lat` Latitude,
#' - `lon` longitude
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |>
#'   tag_label()
#' tag <- tag_create(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 2,
#' ) |>
#'   geopressure_map(tag$pressure)
#'
#' # Compute the path
#' path <- map2path(tag)
#'
#' @seealso [`geopressure_map_likelihood()`], [`pressurepath_create()`], [GeoPressureManual |
#' Pressure Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-altitude)
#' @export
map2path <- function(tag,
                     likelihood = NA,
  assertthat::assert_that(is.list(tag))
                     interp = -1,
                     .use_known = TRUE) {

  # Construct the likelihood map
  # Same code used in `map2path`. Update simultaneously.
  if (all(is.na(likelihood))) {
    likelihood <- c("map_pressure", "map_light")
    tmp <- likelihood %in% names(tag)
    if (all(tmp)) {
      lk <- mapply(\(p, l) {
        if (is.null(p) | is.null(l)) {
          return(NULL)
        } else {
          return(p * l)
        }
      }, tag$map_pressure, tag$map_light, SIMPLIFY = FALSE)
    } else if (any(tmp)) {
      likelihood <- likelihood[tmp]
      lk <- tag[[likelihood]]
    } else {
      cli::cli_abort(c(
        x = "None of {.field {likelihood}} are present in {.var tag}",
        i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}"
      ))
    }
  } else {
    assertthat::assert_that(assertthat::has_name(tag, likelihood))
    lk <- tag[[likelihood]]
  }

  # find the index in the 2D grid
  ind <- rep(NA, length(lk))
  stap_id <- which(!sapply(lk, is.null))
  ind[stap_id] <- sapply(lk[stap_id], which.max)

  # Interpolation for short stationary period is only performed if interp>0
  if (interp > 0) {
    # Find the stap to be interpolated
    path_interp <- difftime(tag$stap$end,tag$stap$start, units = "days") <= interp

    # If known, the stap will not be interpolated
    path_interp[!is.na(tag$stap$known_lon) & .use_known] <- FALSE

    # Compute the grid information used for known or interp
    g <- geo_expand(tag$extent, tag$scale)

    # Compute the latitude and longitude ind
    lat_ind <- arrayInd(ind, g$dim)[, 1]
    lon_ind <- arrayInd(ind, g$dim)[, 2]

    # Enforce first and last stap constant
    fal <- c(1, length(path_interp))
    if (any(path_interp[fal])) {
      cli::cli_warn(c(
        "!" = "First and last modeled stationary periods ({.val {tag$stap$stap_id[fal]}}) are \\
         shorter than {interp} day{?s} but cannot be interpolated.",
        ">" = "They will not be interpolated."
      ))
      path_interp[fal] <- FALSE
      if (all(!is.na(lat_ind[fal]))) {
        cli::cli_abort(c(
          x = "First and last modeled stationary periods ({.val {tag$stap$stap_id[fal]}}) need \\
          to be have a map in {.var tag${field}} or be known."
        ))
      }
    }

    # Compute flight duration of the
    flight <- stap2flight(tag$stap)

    # Cummulate the flight duration to get a proxy of the over distance covered
    total_flight <- cumsum(as.numeric(c(0, flight$duration)))

    # Interpolate the lat and lon indices separately using `total_flight` as a spacing between
    # position
    lon_ind[path_interp] <- round(stats::approx(
      total_flight[!path_interp], lon_ind[!path_interp], total_flight[path_interp])$y)
    lat_ind[path_interp] <- round(stats::approx(
      total_flight[!path_interp], lat_ind[!path_interp], total_flight[path_interp])$y)

    # Account for water position
    # Not implemented as done by geopressure_timeseries

    # Update in
    ind[path_interp] <- (lon_ind[path_interp] - 1) * g$dim[1] + lat_ind[path_interp]
  } else {
    path_interp <- F
  }

  # Convert the index of the path in a path data.frame
  path <- ind2path(ind, tag, .use_known = .use_known)

  path$interp <- path_interp

  return(path)
}
