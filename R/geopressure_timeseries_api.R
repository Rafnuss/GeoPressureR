#' Retrieve an ERA5 pressure time series from GeoPressureAPI
#'
#' This is the explicit hosted API backend for [geopressure_timeseries()]. See the parent function
#' for the shared workflow, backend comparison, ECMWF key setup, and output details.
#'
#' @inheritParams geopressure_timeseries
#' @return See [geopressure_timeseries()].
#' @family pressurepath
#' @export
geopressure_timeseries_api <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE
) {
  geopressure_timeseries(
    lat = lat,
    lon = lon,
    pressure = pressure,
    start_time = start_time,
    end_time = end_time,
    source = "api",
    quiet = quiet,
    debug = debug
  )
}

geopressure_timeseries_api_impl <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE
) {
  # Format query
  body <- list(lon = lon, lat = lat)
  if (!is.null(pressure)) {
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
      "!" = "Requested position is on water and will be move to the closet point on shore ({.url https://www.google.com/maps/dir/{lat},{lon}/{resp_data$lat},{resp_data$lon}}) located {round(resp_data$distInter / 1000)} km away."
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
      i = "Check that the time range is none-empty. Log of your JSON request: {.file {temp_file}}"
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
        "The returned data.frame is had a different number of element than the requested pressure."
      )
    }

    if (!quiet) {
      cli::cli_progress_step("Compute normalized ERA5 pressure")
    }

    # Add default metadata before merging to preserve these columns in the output.
    pressure_merge <- pressure
    if (!("stap_id" %in% names(pressure_merge))) {
      pressure_merge$stap_id <- 1
    }
    if (!("label" %in% names(pressure_merge))) {
      pressure_merge$label <- ""
    }
    out <- merge(pressure_merge, out, all.x = TRUE)
    names(out)[names(out) == "value"] <- "pressure_tag"

    # Use merged metadata so normalization masks stay aligned with `out` rows.
    stap_id <- if ("stap_id" %in% names(out)) out$stap_id else rep(1, nrow(out))
    label <- if ("label" %in% names(out)) out$label else rep("", nrow(out))
    # Normalize only non-flight observations that are not marked as discarded.
    id_norm <- stap_id != 0 & label != "discard"
    # If no ground (ie. only flight) is present, surface_pressure_norm has no meaning
    if (sum(id_norm) > 0) {
      elev <- ifelse(
        startsWith(label, "elev_"),
        gsub("^.*?elev_", "", label),
        "0"
      )
      for (elev_i in unique(elev)) {
        id_elev <- elev == elev_i
        pressure_tag_m <- mean(out$pressure_tag[id_elev & id_norm])
        surface_pressure_m <- mean(out$surface_pressure[id_elev & id_norm])
        out$surface_pressure_norm[id_elev] <- out$surface_pressure[id_elev] -
          surface_pressure_m +
          pressure_tag_m
      }
    }
  }
  return(out)
}
