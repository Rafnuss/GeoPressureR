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
#' @return A path data.frame
#' - `stap_id` stationary period
#' - `ind` indices of the coordinate in the 2D grid. Useful to retrieve map or graph information.
#' - `lat` Latitude,
#' - `lon` longitude
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |>
#'   tag_label()
#' geostap <- geostap_create(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 2,
#' ) |>
#'   geopressure_map(tag$pressure)
#'
#' # Compute the path
#' path <- geostap2path(geostap)
#'
#' @seealso [`geopressure_map_likelihood()`], [`geopressure_timeseries()`], [GeoPressureManual |
#' Pressure Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-altitude)
#' @export
geostap2path <- function(geostap,
                         likelihood = NA,
                         interp = -1) {
  assertthat::assert_that(is.list(geostap))

  # Construct the likelihood map
  if (all(is.na(likelihood))) {
    likelihood <- c("map_pressure", "map_light")
    tmp <- likelihood %in% names(geostap)
    if (all(tmp)) {
      lk <- mapply(\(p, l) {
        if (is.null(p) | is.null(l)) {
          return(NULL)
        } else {
          return(p * l)
        }
      }, geostap$map_pressure, geostap$map_light, SIMPLIFY = FALSE)
    } else if (any(tmp)) {
      likelihood <- likelihood[tmp]
      lk <- geostap[[likelihood]]
    } else {
      cli::cli_abort(c(
        x = "None of {.field {likelihood}} are present in {.var geostap}",
        i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}"
      ))
    }
  } else {
    assertthat::assert_that(assertthat::has_name(geostap, likelihood))
    lk <- geostap[[likelihood]]
  }

  # find the index in the 2D grid
  ind <- rep(NA, length(lk))
  stap_id <- which(!sapply(lk, is.null))
  ind[stap_id] <- sapply(lk[stap_id], which.max)

  # Interpolation for short stationary period is only performed if interp>0
  if (interp > 0) {
    # Compute the grid information
    g <- geo_expand(geostap$extent, geostap$scale)

    # Compute the latitude and longitude for not known stap
    lat_ind <- arrayInd(ind, g$dim)[, 1]
    lon_ind <- arrayInd(ind, g$dim)[, 2]

    # Enforce first and last stap constant
    fal <- c(1, length(ind))
    if (any(path$interp[fal])) {
      cli::cli_warn(c(
        "!" = "First and last modeled stationary periods ({.val path$stap_id[fal]} are shorter \\
         than {interp} day{?s} but cannot be interpolated.",
        ">" = "They will be kept as constant."
      ))
      path$interp[fal] <- FALSE
      if (all(!is.na(lat_ind[fal]))) {
        cli::cli_abort(c(
          x = "First and last modeled stationary periods {.val {path$stap_id[fal]}} need to be \\
          have a map in {.var geostap${field}} or be known."
        ))
      }
    }

    # Compute flight duration of the
    flight <- stap2flight(geostap$stap)

    # Cumulate the flight duration to get a proxy of the over distance covered
    w <- numeric(c(0, flight$duration))

    # interpolate in between
    lon_ind[path$interp] <-
      round(stats::approx(w[!path$interp], lon_ind[!path$interp], w[path$interp])$y)
    lat_ind[path$interp] <-
      round(stats::approx(w[!path$interp], lat_ind[!path$interp], w[path$interp])$y)

    # Account for water position
    # Not implemented as done by geopressure_timeseries_latlon

    #
    ind <- (lon_ind - 1) * g$dim[1] + lat_ind
  }

  # Convert the index of the path in a path data.frame
  path <- ind2path(ind, geostap)

  return(path)
}
