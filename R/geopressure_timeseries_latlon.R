#' Request and download pressure timeseries at a given location
#'
#' This function returns the surface atmospheric pressure timeseries from ERA5 at any requested
#' location.
#'
#' If the location queried is over water, the location will be moved to the closest onshore location.
#'
#' The ERA5 pressure timeseries of the response \eqn{P_{ERA}} will be provided on a hourly basis between
#' `start_time` and `end_time` or the same as `pressure$date` if `pressure` is supplied.
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
#' [the GeoPressureAPI documentation](https://raphaelnussbaumer.com/GeoPressureAPI/#description-1).
#'
#' To be able to compare the temporal variation of the retrieved pressure of ERA5 \eqn{P_{ERA}} to
#' the geolocator pressure \eqn{P_{gl}}, the function also returns the ERA pressure normalized with
#' the geolocator mean pressure measurement as `pressure_era5_norm`.
#' \deqn{ P_{ERA5,0}(\boldsymbol{x})[t] = \left( P_{ERA5}(\boldsymbol{x})[t]-P_{gl}[t]\right) -
#' \left( \frac{1}{n}\sum_{i=1}^{n} P_{ERA5}(\boldsymbol{x})[i]-P_{gl}[i] \right).}
#'
#' See [GeoPressureManual | Probability aggregation](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html)
#' for more information on the meaning of this value.
#' @inheritParams geopressure_map
#' @param lon Longitude to query (-180° to 180°).
#' @param lat Latitude to query (0° to 90°).
#' @param start_time If `pressure` is not provided, `start_time` defines the start time of
#' the timeseries as POSIXlt.
#' @param end_time If `pressure` is not provided, `end_time` defines the end time of
#' the timeseries as POSIXlt.
#' @param timeout Duration (sec) before the code is interrupted both for the request on
#' GeoPressureAPI and GEE. See [`httr::timeout()`].
#' @param quiet Hides the progress of the query (logical).
#' @return A data.frame containing
#' - `date` POSIXct date time
#' - `pressure_era5` pressure (hPa)
#' - `lon` same as input `lon` except if over water
#' - `lat` same as input `lat` except if over water.
#' - `pressure_era5_norm` only if `pressure` is provided as input
#' - `altitude` only if `pressure` is provided as input
#' @seealso [`geopressure_timeseries()`], [GeoPressureManual | Pressure Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' # Request pressure at a given location
#' path_pres <- geopressure_timeseries_latlon(
#'   lat = 46, lon = 6,
#'   start_time = "2017-01-01 00:00",
#'   end_time = "2017-01-02 00:00"
#' )
#'
#' str(path_pres)
#'
#' plot(path_pres$date, path_pres$pressure,
#'   type = "b", ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#'
#' # Retrieve the altitude of a bird being at this location adding random noise on the sensor.
#' path_pres <- geopressure_timeseries_latlon(
#'   lat = 46, lon = 6,
#'   pressure = data.frame(
#'     data.frame(
#'       date = path_pres$date,
#'       value = path_pres$pressure_era5 + rnorm(nrow(path_pres))
#'     )
#'   )
#' )
#'
#' str(path_pres)
#'
#' plot(path_pres$date, path_pres$altitude,
#'   type = "b", ylab = "Altitude (m)", xlab = "Datetime"
#' )
#'
#' @export
geopressure_timeseries_latlon <- function(lat,
                                          lon,
                                          pressure = NULL,
                                          start_time = NULL,
                                          end_time = NULL,
                                          timeout = 60 * 5,
                                          quiet = FALSE) {
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
  body_df <- list(lon = lon, lat = lat)
  if (!is.null(pressure)) {
    body_df$time <- jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date)))
    body_df$pressure <- jsonlite::toJSON(pressure$value * 100)
  } else {
    body_df$startTime <- as.numeric(as.POSIXct(start_time))
    body_df$endTime <- as.numeric(as.POSIXct(end_time))
  }

  if (!quiet) cli::cli_progress_step("Generate request (on GeoPressureAPI)", spinner = TRUE)
  res <- httr::POST("https:///glp.mgravey.com/GeoPressure/v1/timeseries/",
    body = body_df,
    encode = "form",
    httr::timeout(timeout)
  )

  if (httr::http_error(res)) {
    message(httr::http_status(res)$message)
    message(httr::content(res))
    temp_file <- tempfile("log_geopressure_timeseries", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    cli::cli_abort(c(
      x = "Error with youre request on {.url https://glp.mgravey.com/GeoPressure/v1/timeseries/}.",
      i = "Please try again, and if the problem persists, file an issue on Github \\
      {.url https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_timeseries&labels=crash}
      with this log file located on your computer: {.file {temp_file}}."
    ))
  }

  # Retrieve response data
  res_data <- httr::content(res)$data

  # Check for change in position
  if (res_data$distInter > 0) {
    cli::cli_warn(c(
      "!" = "Requested position is on water.",
      i = "We will proceeed the request with the closet point to the shore ({.url
      https://www.google.com/maps/dir/{lat},{lon}/{res_data$lat},{res_data$lon}}) located \\
      {round(res_data$distInter / 1000)} km away."
    ))
  }

  # Download the csv file
  if (!quiet) cli::cli_progress_step("Sending request", spinner = TRUE)
  res2 <- httr::GET(res_data$url, httr::timeout(timeout))

  # read csv
  if (!quiet) {
    cli::cli_progress_step("Compute timeseries (on GEE server) and download csv",
      spinner = TRUE
    )
  }
  out <- as.data.frame(httr::content(res2,
    type = "text/csv",
    encoding = "UTF-8",
    show_col_types = FALSE
  ))

  # check for errors
  if (nrow(out) == 0) {
    temp_file <- tempfile("log_geopressure_timeseries", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    cli::cli_abort(c(
      x = "Returned csv file is empty.",
      i = "Check that the time range is none-empty. Log of your  JSON request: {.file {temp_file}}"
    ))
  }

  # convert Pa to hPa
  out$pressure <- out$pressure / 100

  # convert time into date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"
  names(out)[names(out) == "pressure"] <- "pressure_era5"

  # Add exact location
  out$lat <- res_data$lat
  out$lon <- res_data$lon

  # Compute the ERA5 pressure normalized to the pressure level (i.e. altitude) of the bird
  if (!is.null(pressure)) {
    if (!quiet) cli::cli_progress_step("Compute normalized ERA5 pressure")
    if (nrow(out) != nrow(pressure)) {
      cli::cli_warn(
        "The returned data.frame is had a different number of element than the requested pressure."
      )
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
    # (id_q==0) and when not labeled as flight or discard
    id_norm <- pressure$stap_id != 0 & pressure$label != "discard"
    # If no ground (ie. only flight) is present, pressure_era5_norm has no meaning
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
        pressure_era5_m <- mean(out$pressure_era5[id_elev & id_norm])
        out$pressure_era5_norm[id_elev] <- out$pressure_era5[id_elev] - pressure_era5_m + pressure_tag_m
      }
    }
  }
  return(out)
}
