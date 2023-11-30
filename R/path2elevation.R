#' Download ground elevation along a path
#'
#' @description
#' This function is a wrapper of the GeoPressureAPI elevationPath entry point. It queries the ground
#' elevation from SRTM ([SRTM90_V4](
#' https://developers.google.com/earth-engine/datasets/catalog/CGIAR_SRTM90_V4)) along a polyline
#' specify by `path`.
#'
#' Because the position are often defined on a relative coarse scale (e.g., 0.25° ~ 29km),
#' you can request the elevation at a specify resolution defined by `scale` and return pre-defined
#' `percentile` of the elevation at this resolution.
#'
#' The returned data.frame provide the ground elevation along the path with a resolution defined by
#' `sampling_scale`.
#'
#' @param path a GeoPressureR `path` data.frame
#' @param scale spatial resolution of the SRTM to use to query the elevation. `scale` is defined as
#' the number of pixels per 1° latitude-longitude (see `tag_set_map()` for details). Native
#' resolution of STRM is 30m.
#' @param sampling_scale spatial resolution of the point along the polyline on which the SRTME is
#' estimated. Same unit as `scale`.
#' @param percentile percentile of the ground elevation distribution found within each grid cell
#' of the SRTM at the resolution defined by `scale`. `50` corresponds to the median.
#' @param timeout maximum duration to make the httr request (see `httr2::req_timeout()`)
#' @param debug logical to display additional information to debug a request
#'
#' @return A data.frame containing
#' - `stap_id` numeric value corresponding to the ratio of distance between position of known stap
#' - `lon`
#' - `lat`
#' - `distance` distance in meter along the path starting at the first stap_id
#'
#' @examples
#' # Create a path
#' path <- data.frame(
#'   lon = c(8.47, 9.41, 9.01, -0.91, 14.24, 27.30, 34.39, 30.00),
#'   lat = c(48.89, 44.78, 40.07, 37.68, 17.33, 7.32, 8.09, -23.13),
#'   start = as.POSIXct(
#'     c(
#'       "2017-05-01 00:42", "2017-05-03 01:22", "2017-05-03 22:47", "2017-05-06 03:32",
#'       "2017-05-07 01:12", "2017-05-07 22:32", "2017-05-09 21:52", "2017-05-10 21:12"
#'     ),
#'     tz = "UTC"
#'   ),
#'   end = as.POSIXct(
#'     c(
#'       "2017-05-02 22:12", "2017-05-03 20:12", "2017-05-06 02:47", "2017-05-06 23:22",
#'       "2017-05-07 17:42", "2017-05-09 20:27", "2017-05-10 19:57", "2017-05-11 21:17"
#'     ),
#'     tz = "UTC"
#'   ),
#'   stap_id = seq(1, 8)
#' )
#'
#'
#' plot_path(path)
#'
#' elevation <- path2elevation(path)
#'
#' plot(elevation$distance / 1000, elevation$X50,
#'   type = "l",
#'   ylab = "Elevation (m)", xlab = "Distance from start (km)"
#' )
#' lines(elevation$distance / 1000, elevation$X10, lty = 5)
#' lines(elevation$distance / 1000, elevation$X90, lty = 5)
#' id <- elevation$stap_id %% 1 == 0
#' points(elevation$distance[id] / 1000, elevation$X90[id], col = "red")
#'
#' @family path
#' @export
path2elevation <- function(path,
                           scale = 4,
                           sampling_scale = scale,
                           percentile = c(10, 50, 90),
                           timeout = 60 * 5,
                           debug = FALSE) {
  # Check input
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lon", "lat", "stap_id")))
  assertthat::assert_that(is.numeric(scale))
  assertthat::assert_that(scale > 0)
  assertthat::assert_that(is.numeric(sampling_scale))
  assertthat::assert_that(sampling_scale > 0)
  assertthat::assert_that(all(is.numeric(percentile)))
  assertthat::assert_that(all(percentile > 0))
  assertthat::assert_that(all(percentile < 100))
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(timeout > 0)
  assertthat::assert_that(is.logical(debug))

  # 1. Interpolation of !include
  # In order to have the correct stap_id during the flight, we need to interpolate the position of
  # stap not included. As opposed to tag2path(), here we don't need to insure that the position
  # falls exactly on a grid cell (in order to be able to have an index).
  path_interp <- is.na(path$lat) | is.na(path$lon)

  # We only request elevation between the first and last defined position. Basically, if the path
  # is unknown at the end (or start), we just ignore this part
  stap_id_considered <- seq(min(which(!path_interp)), max(which(!path_interp)))
  stap_id_not_considered <- path$stap_id[!(path$stap_id %in% stap_id_considered)]
  if (length(stap_id_not_considered) > 0) {
    cli::cli_warn(c(
      "!" = "The position are not defined for stationary periods ({.val {stap_id_not_considered}})\\
      . Because they are at the begining or end of the path, we are not able to interpolate\\
      them and they will consequently not be considered in the computation of elevation."
    ))
  }

  # Retrieve only the stap_id to consider
  path_c <- path[stap_id_considered, ]
  path_interp_c <- path_interp[stap_id_considered]

  # Compute flight duration including all stap_id
  flight <- stap2flight(path_c, stap_include = path_c$stap_id)

  # Cummulate the flight duration to get a proxy of the over distance covered
  total_flight <- cumsum(as.numeric(c(0, flight$duration)))

  # Interpolate the lat and lon separately using `total_flight` as a spacing between position
  path_c$lon[path_interp_c] <- stats::approx(
    total_flight[!path_interp_c], path_c$lon[!path_interp_c], total_flight[path_interp_c]
  )$y
  path_c$lat[path_interp_c] <- stats::approx(
    total_flight[!path_interp_c], path_c$lat[!path_interp_c], total_flight[path_interp_c]
  )$y

  assertthat::assert_that(all(diff(path_c$stap_id) == 1))

  # Format query
  body <- list(
    lon = path_c$lon,
    lat = path_c$lat,
    scale = scale,
    samplingScale = sampling_scale,
    percentile = percentile
  )

  if (debug) {
    temp_file <- tempfile("log_elevationPath_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("glp.mgravey.com/GeoPressure/v2/elevationPath/") |>
    httr2::req_body_json(body, digit = 5) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(timeout)

  if (debug) {
    req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
  }

  # Perform the request and convert the response to json
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)$data

  out <- as.data.frame(resp_data$percentileData)

  # Rename stap_id
  names(out)[names(out) == "stapId"] <- "stap_id"

  # Adjust stap_id
  out$stap_id <- out$stap_id + path_c$stap_id[1]

  return(out)
}
