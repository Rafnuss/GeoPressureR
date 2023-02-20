#' Request and download pressure timeseries at location
#'
#' This function return the surface atmospheric pressure timeseries from ERA5 at a queried location.
#'
#' If you supply the pressure (and time) of the geolocator \eqn{P_{gl}}, the function will
#' additionally return the altitude of the geolocator above sea level \eqn{z_{gl}} using the
#' barometric equation,
#' \deqn{ z_{{gl}}(x)=z_{ERA5}(x) + \frac{T_{ERA5}(x)}{L_b}  \left( \frac{P_{gl}}{P_{ERA5}(x)}
#' \right)^{\frac{RL_b}{g M}-1},}
#' where \eqn{z_{ERA}}, \eqn{T_{ERA}} and \eqn{P_{ERA}} respectively correspond to the ground level
#' elevation, temperature at 2m and ground level pressure of ERA5, \eqn{L_b}  is the standard
#' temperature lapse rate, \eqn{R} is the universal gas constant, \eqn{g} is the gravity constant
#' and  \eqn{M} is the molar mass of air. See more information on
#' [the GeoPressureAPI documentation](https://raphaelnussbaumer.com/GeoPressureAPI/#description-1).
#'
#' The timeseries of the response will be on the same as time if supply, otherwise, it will return
#' on a hourly basis between `start_time` and `end_time`.
#'
#' If the location query is over water, the location will be moved to the closest onshore location.
#'
#' To be able to compare the temporal variation of the retrieved pressure of ERA5 \eqn{P_{ERA}} to
#' the geolocator pressure \eqn{P_{gl}}, the function also return the ERA pressure normalized with
#' the geolocator mean pressure measurement as `pressure0`.
#' \deqn{ P_{0}(\boldsymbol{x})[t] = \left( P_{ERA5}(\boldsymbol{x})[t]-P_{gl}[t]\right) -
#' \left( \frac{1}{n}\sum_{i=1}^{n} P_{ERA5}(\boldsymbol{x})[i]-P_{gl}[i] \right).}
#'
#' See [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html)
#' for more information on the meaning of this value.
#' @param lon Longitude to query (-180° to 180°).
#' @param lat Latitude to query (0° to 90°).
#' @param pressure Pressure list from data logger dataset list (optional). Needs to contains at
#' least `date` and `value`.
#' @param start_time If `pressure` is not provided, then `start_time` define the starting time of
#' the timeseries as POSIXlt.
#' @param end_time If `pressure` is not provided, then `end_time` define the ending time of
#' the timeseries as POSIXlt.
#' @param timeout duration (sec) before the code is interrupted both for the request on
#' GeoPressureAPI and GEE. See [`httr::timeout()`].
#' @param verbose Display (or not) the progress of the query (logical).
#' @return A data.frame containing
#' - `date` POSIXct date time
#' - `pressure_era5` pressure (hPa)
#' - `longitude`(different if over water)
#' - `latitude`
#' - `pressure_era5_norm` only if `pressure` is provided as input
#' - `altitude` only if `pressure` is provided as input
#' @seealso [`geopressure_timeseries_path()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html),
#' @examples
#' pressure_timeserie <- geopressure_timeseries(
#'   lon = 6, lat = 46,
#'   start_time = as.POSIXct("2017-01-01 00:00:00", tz = "UTC"),
#'   end_time = as.POSIXct("2017-01-02 00:00:00", tz = "UTC")
#' )
#'
#' str(pressure_timeserie)
#'
#' plot(pressure_timeserie$date, pressure_timeserie$pressure,
#'   type = "b", ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#'
#' pressure_timeserie <- geopressure_timeseries(
#'   lon = 6, lat = 46,
#'   pressure = data.frame(
#'     data.frame(
#'       date = as.POSIXct(c(
#'         "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
#'         "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
#'       ), tz = "UTC"),
#'       value = c(1000, 1000, 1000, 1000)
#'     )
#'   )
#' )
#'
#' str(pressure_timeserie)
#'
#' plot(pressure_timeserie$date, pressure_timeserie$altitude,
#'   type = "b", ylab = "Altitude (m)", xlab = "Datetime"
#' )
#'
#' @export
geopressure_timeseries <- function(lon,
                                   lat,
                                   pressure = NULL,
                                   start_time = NULL,
                                   end_time = NULL,
                                   timeout = 60 * 5,
                                   verbose = TRUE) {
  # Check input
  assertthat::assert_that(is.numeric(lon))
  assertthat::assert_that(is.numeric(lat))
  assertthat::assert_that(lon >= -180 & lon <= 180)
  assertthat::assert_that(lat >= -90 & lat <= 90)
  if (!is.null(pressure)) {
    assertthat::assert_that(is.data.frame(pressure))
    assertthat::assert_that("date" %in% names(pressure))
    assertthat::assert_that(inherits(pressure$date, "POSIXt"))
    assertthat::assert_that("value" %in% names(pressure))
    assertthat::assert_that(is.numeric(pressure$value))
    end_time <- NULL
    start_time <- NULL
  } else {
    assertthat::assert_that(!is.na(end_time))
    assertthat::assert_that(!is.na(start_time))
    assertthat::assert_that(inherits(end_time, "POSIXt"))
    assertthat::assert_that(inherits(start_time, "POSIXt"))
    assertthat::assert_that(start_time <= end_time)
  }
  assertthat::assert_that(is.logical(verbose))

  # Format query
  body_df <- list(lon = lon, lat = lat)
  if (!is.null(pressure)) {
    body_df$time <- jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date)))
    body_df$pressure <- jsonlite::toJSON(pressure$value * 100)
  } else {
    body_df$startTime <- as.numeric(as.POSIXct(start_time))
    body_df$endTime <- as.numeric(as.POSIXct(end_time))
  }

  if (verbose) message("Generate request (on GeoPressureAPI):")
  res <- httr::POST("https:///glp.mgravey.com/GeoPressure/v1/timeseries/",
                    body = body_df,
                    encode = "form",
                    httr::timeout(timeout)
  )

  if (httr::http_error(res)) {
    message(httr::http_status(res)$message)
    message(httr::content(res))
    temp_file <- tempfile("log_geopressure_timeseries_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    stop(paste0(
      "Error with youre request on https://glp.mgravey.com/GeoPressure/v1/timeseries/.",
      "Please try again, and if the problem persists, file an issue on Github:
      https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_timeseries&labels=crash
      with this log file located on your computer: ", temp_file
    ))
  }

  # Retrieve response data
  res_data <- httr::content(res)$data

  # Check for change in position
  if (res_data$distInter > 0) {
    warning(
      "Requested position is on water. We will proceeed the request with the closet point to the ",
      "shore (https://www.google.com/maps/dir/", lat, ",", lon, "/", res_data$lat, ",",
      res_data$lon, ") located ", round(res_data$distInter / 1000), " km away). Sending request."
    )
  } else {
    if (verbose) message("Request generated successfully. Sending request.")
  }

  # Download the csv file
  res2 <- httr::GET(res_data$url, httr::timeout(timeout))

  # read csv
  out <- as.data.frame(httr::content(res2,
                                     type = "text/csv",
                                     encoding = "UTF-8",
                                     show_col_types = FALSE
  ))

  # check for errors
  if (nrow(out) == 0) {
    temp_file <- tempfile("log_geopressure_timeseries_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    stop(paste0(
      "Returned csv file is empty. Check that the time range is none-empty. Log of your ",
      "JSON request: ", temp_file
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
    if (nrow(out) != nrow(pressure)) {
      warning(
        "The returned data.frame is had a different number of element than the requested ",
        "pressure."
      )
    }

    # Use a merge to combine all information possible from out into pressure.
    out <- merge(pressure, out, all.x = TRUE)
    names(out)[names(out) == "value"] <- "pressure_tag"

    # find when the bird was in flight or not to be considered
    id_0 <- pressure$stap == 0 | is.na(pressure$stap)
    # If no ground (ie. only flight) is present, pressure0 has no meaning
    if (!all(id_0)) {
      # We compute the mean pressure of the geolocator only when the bird is on the ground
      # (id_q==0) and when not labeled as flight or discard
      id_norm <- !id_0 & pressure$label == ""

      pressure_tag_m <- mean(pressure$value[id_norm])
      pressure_era5_m <- mean(out$pressure_era5[id_norm])

      out$pressure_era5_norm <- out$pressure_era5 - pressure_era5_m + pressure_tag_m
    }
  }
  return(out)
}



#' Wrapper of `geopressure_timeseries()` for a path
#'
#' This function request and download multiple pressure timeseries from a path and data logger
#' pressure measurement by calling `geopressure_timeseries()` in parallel.
#'
#'
#' It uses the `stap` to match the pressure timeseries to request for each position of the path.
#'
#' You can include previous and/or next flight period in each query. This is typically useful to
#' estimate flight altitude with greater precision.
#'
#' If a position of the path is over water, it will be moved to the closest point onshore as
#' explained in `geopressure_timeseries()`.
#'
#' @param path A data.frame of the position containing latitude (`lat`), longitude  (`lon`) and the
#' stationary period id (`stap`) as column.
#' @param pressure Pressure data.frame from data logger.
#' @param include_flight Extend request to also query the pressure and altitude during the previous
#' and/or next flight. Flights are defined by a `stap=0`. Accept Logical or vector of -1 (previous
#' flight), 0 (stationary) and/or 1 (next flight). (e.g. `include_flight=c(-1, 1)` will only search
#' for the flight before and after but not the stationary period). Note that next and previous
#' flights are defined by the +/1 of the `stap` value (and not the previous/next `stap` value).
#' @param workers number of parrellel request on GEE. Between 1 and 99.
#' @param verbose Display (or not) the progress of the queries (logical).
#' @return A data.frame containing:
#' - `date` equivalent to `tag$date`
#' - `pressure_tag`: equivalent to `tag$value`
#' - `label` equivalent to `tag$label`
#' - `stap` equivalent to `tag$stap`
#' - `pressure_era5` from `geopressure_timeseries()`
#' - `altitude` from `geopressure_timeseries()`
#' - `lat` from `geopressure_timeseries()`
#' - `lon` from `geopressure_timeseries()`
#' - `pressure_era5_norm` from `geopressure_timeseries()`
#' - `stap_ref` stap used as reference (same as stap except for flight where `stap=0`)
#' (same as [`geopressure_timeseries()`]).
#' @seealso [`geopressure_timeseries()`], [`map2path()`], [GeoPressureManual | Pressure
#' Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' \dontrun{
#' path <- map2path(pressure_likelihood, interp = 1)
#' pressure_timeseries <- geopressure_timeseries_path(
#'   path = path,
#'   pressure = tag$pressure,
#'   include_flight = TRUE
#' )
#' }
#' # Load pre-computed pressure mismatch
#' pressure_timeseries <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_timeseries.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#'
#' str(pressure_timeseries)
#'
#' plot(pressure_timeseries$date, pressure_timeseries$value,
#'   col = pressure_timeseries$stap,
#'   ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#'
#' lines(pressure_timeseries_df$date, pressure_timeseries_df$pressure0, col = "red")
#' @export
geopressure_timeseries_path <- function(path,
                                        pressure,
                                        include_flight = FALSE,
                                        workers = 90,
                                        verbose = TRUE) {
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "value", "stap")))
  assertthat::assert_that(inherits(pressure$date, "POSIXt"))
  assertthat::assert_that(is.numeric(pressure$value))
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap")))
  if (nrow(path) == 0) warning("path is empty")
  if (!all(path$stap %in% pressure$stap)) {
    warning("Some path stap are not present in pressure")
  }
  if (is.logical(include_flight)) {
    include_flight <- (if (include_flight) c(-1, 0, 1) else 0)
  }
  assertthat::assert_that(is.numeric(include_flight))
  assertthat::assert_that(all(include_flight %in% c(-1, 0, 1)))
  assertthat::assert_that(is.logical(verbose))
  assertthat::assert_that(is.numeric(workers))
  assertthat::assert_that(workers > 0 & workers < 100)

  # Interpolate stap for flight period so that, a flight between stap 2 and 3 will have a
  # `stap_interp` between 2 and 3.
  id_0 <- pressure$stap == 0 | is.na(pressure$stap)
  stap_interp <- pressure$stap
  stap_interp[id_0] <- stats::approx(which(!id_0),
                                     pressure$stap[!id_0], which(id_0),
                                     rule = 2
  )$y

  # Define the number of parallel workers (Google Earth Engine allowance is currently 100)
  future::plan(future::multisession, workers = workers)
  f <- c()

  if (verbose) {
    message("Sending requests for ", nrow(path), " stationary periods:")
    progress_bar(0, max = nrow(path))
  }

  for (i_s in seq_len(nrow(path))) {
    i_stap <- path$stap[i_s]
    if (verbose) progress_bar(i_s, max = nrow(path), text = paste0("| stap = ", i_stap))
    # Subset the pressure of the stationary period
    id_q <- rep(NA, length(stap_interp))
    if (any(0 == include_flight)) {
      id_q[path$stap[i_s] == stap_interp] <- 0
    }
    if (any(-1 == include_flight)) {
      id_q[i_stap - 1 < stap_interp & stap_interp < i_stap] <- -1
    }
    if (any(1 == include_flight)) {
      id_q[i_stap < stap_interp & stap_interp < i_stap + 1] <- 1
    }
    # Send the query
    f[[i_s]] <- future::future({
      geopressure_timeseries(path$lon[i_s], path$lat[i_s],
                             pressure = subset(pressure, !is.na(id_q)),
                             verbose = FALSE
      )
    })
  }

  pressure_timeseries <- list()
  message("Compute and download the data (on GEE):")
  progress_bar(0, max = nrow(path))
  for (i_s in seq_len(length(f))) {
    i_stap <- path$stap[i_s]
    progress_bar(i_s, max = nrow(path), text = paste0("| stap = ", i_stap))
    tryCatch(
      expr = {
        pressure_timeseries[[i_s]] <- future::value(f[[i_s]])
        pressure_timeseries[[i_s]]$stap_ref <- i_stap
      },
      error = function(cond) {
        warning(paste0("Error for stap = ", path$stap[i_s], ".\n", cond))
      }
    )
  }

  pressure_timeseries_df <- do.call("rbind", pressure_timeseries)

  return(pressure_timeseries_df)
}
