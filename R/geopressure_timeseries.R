#' Request and download pressure time series at a given location
#'
#' @description
#' This function returns the surface atmospheric pressure time series from ERA5 at any requested
#' location.
#'
#' If the location queried is over water, the location will be moved to the closest onshore
#' location.
#'
#' The ERA5 pressure time series of the response \eqn{P_{ERA}} will be provided on a hourly basis
#' between `start_time` and `end_time` or the same as `pressure$date` if `pressure` is supplied.
#'
#' If you supply the `pressure` of the geolocator \eqn{P_{gl}}, the function will
#' additionally return the altitude of the geolocator above sea level \eqn{z_{gl}} using the
#' barometric equation,
#' \deqn{ z_{{gl}}(x)=z_{ERA5}(x) + \frac{T_{ERA5}(x)}{L_b}  \left( \frac{P_{gl}}{P_{ERA5}(x)}
#' \right)^{\frac{RL_b}{g M}-1},}
#' where \eqn{z_{ERA}}, \eqn{T_{ERA}}, and \eqn{P_{ERA}} respectively correspond to the ground level
#' elevation, temperature at 2m, and ground level pressure of ERA5, \eqn{L_b}  is the standard
#' temperature lapse rate, \eqn{R} is the universal gas constant, \eqn{g} is the gravity constant
#' and  \eqn{M} is the molar mass of air. See more information at
#' [the GeoPressureAPI documentation](https://github.com/Rafnuss/GeoPressureAPI).
#'
#' To be able to compare the temporal variation of the retrieved pressure of ERA5 \eqn{P_{ERA}} to
#' the geolocator pressure \eqn{P_{gl}}, the function also returns the ERA pressure normalized with
#' the geolocator mean pressure measurement as `surface_pressure_norm`.
#' \deqn{ P_{ERA5,0}(\boldsymbol{x})[t] = \left( P_{ERA5}(\boldsymbol{x})[t]-P_{gl}[t]\right) -
#' \left( \frac{1}{n}\sum_{i=1}^{n} P_{ERA5}(\boldsymbol{x})[i]-P_{gl}[i] \right).}
#'
#' @param lon Longitude to query (-180° to 180°).
#' @param lat Latitude to query (0° to 90°).
#' @param pressure A data.frame of pressure time series, containing at least a `"date"` and
#' `"value"` column.
#' @param start_time If `pressure` is not provided, `start_time` defines the start time of
#' the time series as POSIXlt.
#' @param end_time If `pressure` is not provided, `end_time` defines the end time of
#' the time series as POSIXlt.
#' @param quiet logical to hide messages about the progress
#' @param debug logical to display additional information to debug a request
#'
#' @return A data.frame containing
#' - `date` POSIXct date time
#' - `surface_pressure` pressure (hPa)
#' - `lon` same as input `lon` except if over water
#' - `lat` same as input `lat` except if over water.
#' - `surface_pressure_norm` only if `pressure` is provided as input
#' - `altitude` only if `pressure` is provided as input
#'
#' @examples
#' # Request pressure at a given location
#' pressurepath <- geopressure_timeseries(
#'   lat = 46, lon = 6,
#'   start_time = "2017-01-01 00:00",
#'   end_time = "2017-01-02 00:00",
#'   quiet = TRUE
#' )
#'
#' str(pressurepath)
#'
#' plot(pressurepath$date, pressurepath$surface_pressure,
#'   type = "b", ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#'
#' # Retrieve the altitude of a bird being at this location adding random noise on the sensor.
#' pressurepath <- geopressure_timeseries(
#'   lat = 46, lon = 6,
#'   pressure = data.frame(
#'     data.frame(
#'       date = pressurepath$date,
#'       value = pressurepath$surface_pressure + rnorm(nrow(pressurepath))
#'     )
#'   ),
#'   quiet = TRUE
#' )
#'
#' str(pressurepath)
#'
#' plot(pressurepath$date, pressurepath$altitude,
#'   type = "b", ylab = "Altitude (m)", xlab = "Datetime"
#' )
#' @family pressurepath
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' Global Positioning with Animal‐borne Pressure Sensors. *Methods in Ecology and Evolution*, 14,
#' 1118–1129 \doi{10.1111/2041-210X.14043}.}
#' @export
geopressure_timeseries <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE
) {
  # Check input
  assertthat::assert_that(is.numeric(lon))
  assertthat::assert_that(is.numeric(lat))
  assertthat::assert_that(lon >= -180 & lon <= 180)
  assertthat::assert_that(lat >= -90 & lat <= 90)
  if (!is.null(pressure)) {
    assertthat::assert_that(is.data.frame(pressure))
    assertthat::assert_that("date" %in% names(pressure))
    assertthat::assert_that(assertthat::is.time(pressure$date))
    assertthat::assert_that("value" %in% names(pressure))
    assertthat::assert_that(is.numeric(pressure$value))
    end_time <- NULL
    start_time <- NULL
  } else {
    start_time <- as.POSIXct(start_time, tz = "UTC")
    end_time <- as.POSIXct(end_time, tz = "UTC")
    assertthat::assert_that(start_time <= end_time)
  }
  assertthat::assert_that(is.logical(quiet))

  # Format query
  body <- list(lon = lon, lat = lat)
  if (!is.null(pressure)) {
    assertthat::assert_that(nrow(pressure) > 0)
    body$time <- as.numeric(as.POSIXct(pressure$date))
    body$pressure <- pressure$value * 100
  } else {
    body$startTime <- as.numeric(as.POSIXct(start_time))
    body$endTime <- as.numeric(as.POSIXct(end_time))
  }

  if (!quiet) {
    cli::cli_progress_step(
      "Generate request on {.url glp.mgravey.com/GeoPressure/v2/timeseries}"
    )
  }

  if (debug) {
    temp_file <- tempfile("log_geopressure_timeseries_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/timeseries/") |>
    httr2::req_body_json(body) |>
    httr2::req_error(body = function(resp) {
      if (debug) {
        print(httr2::resp_body_json(resp))
      }
      c(
        "x" = "Error with your request on \
        {.url https://glp.mgravey.com/GeoPressure/v2/timeseries/}",
        ">" = httr2::resp_body_json(resp)$errorMessage,
        "i" = "Please try again with `debug=TRUE`"
      )
    })

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform the request and convert the response to json
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp)$data

  # Check for change in position
  if (resp_data$distInter > 0) {
    cli::cli_bullets(c(
      "!" = "Requested position is on water and will be move to the closet point \\
      on shore \\
      ({.url https://www.google.com/maps/dir/{lat},{lon}/{resp_data$lat},{resp_data$lon}}) \\
      located {round(resp_data$distInter / 1000)} km away."
    ))
  }

  if (!quiet) {
    cli::cli_progress_step("Sending request")
  }

  # Prepare request
  req <- httr2::request(resp_data$url)

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform request
  resp <- httr2::req_perform(req)

  # Convert the response to data.frame
  out <- utils::read.csv(text = httr2::resp_body_string(resp))

  # check for errors
  if (nrow(out) == 0) {
    temp_file <- tempfile("log_pressurepath_create", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_abort(c(
      x = "Returned csv file is empty.",
      i = "Check that the time range is none-empty. Log of your  JSON request: {.file {temp_file}}"
    ))
  }

  # convert Pa to hPa and rename
  out$pressure <- out$pressure / 100
  names(out)[names(out) == "pressure"] <- "surface_pressure"

  # convert time into date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Add exact location
  out$lat <- resp_data$lat
  out$lon <- resp_data$lon

  # Compute the ERA5 pressure normalized to the pressure level (i.e. altitude) of the bird
  if (!is.null(pressure)) {
    if (nrow(out) != nrow(pressure)) {
      cli::cli_warn(
        "The returned data.frame is had a different number of element than the requested\\
        pressure."
      )
    }

    if (!quiet) {
      cli::cli_progress_step("Compute normalized ERA5 pressure")
    }

    # Use a merge to combine all information possible from out into pressure.
    out <- merge(pressure, out, all.x = TRUE)
    names(out)[names(out) == "value"] <- "pressure_tag"

    # find when the bird was in flight or not to be considered
    if (!("stap_id" %in% names(pressure))) {
      pressure$stap_id <- 1
    }
    if (!("label" %in% names(pressure))) {
      pressure$label <- ""
    }
    # We compute the mean pressure of the geolocator only when the bird is on the ground
    # (id_q==0) and when not labelled as flight or discard
    id_norm <- pressure$stap_id != 0 & pressure$label != "discard"
    # If no ground (ie. only flight) is present, surface_pressure_norm has no meaning
    if (sum(id_norm) > 0) {
      pressure$elev <- ifelse(
        startsWith(pressure$label, "elev_"),
        gsub("^.*?elev_", "", pressure$label),
        "0"
      )
      elev <- unique(pressure$elev)
      for (elev_i in elev) {
        id_elev <- pressure$elev == elev_i
        pressure_tag_m <- mean(pressure$value[id_elev & id_norm])
        surface_pressure_m <- mean(out$surface_pressure[id_elev & id_norm])
        out$surface_pressure_norm[id_elev] <- out$surface_pressure[id_elev] -
          surface_pressure_m +
          pressure_tag_m
      }
    }
  }
  return(out)
}
