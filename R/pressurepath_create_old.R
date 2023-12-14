#' Create a `pressurepath`
#'
#' @description
#' A `pressurepath` is a data.frame representing a timeseries of ERA5 reanalysis pressure along a
#' `path` (position over time). It can be thought as the equivalent of the hypothetical measurement
#' of a pressure sensor equiped on a bird moving along a specific `path`.
#'
#' `pressurepath` are useful for labeling tag data by allowing a direct comparison of the actual
#' pressure measured by the sensor to the one equivalent to the best estimate of the path.
#'
#' You can include previous and/or next flight period in each query, for instance to estimate
#' flight altitude.
#'
#' The function essentially splits the pressure of the geolocator into stationary periods and
#' downloads the ERA5 timeseries of pressure for the corresponding location of the path by calling
#' `geopressure_timeseries()` at each stationary period. The computation is optimized by
#' paralleling the call of each stationary period.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param path a GeoPressureR `path` data.frame.
#' @param variable ERA5 variable/parameters available to download. See [Parameter listings in ERA5 doc](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#heading-Parameterlistings)
#' @param include_flight logical
#' @param preprocess logical to use `geopressure_map_preprocess`.
#' @param quiet logical to hide messages about the progress
#'
#' @return A GeoPressureR `pressurepath` data.frame with columns:
#' - `date` same as `pressure$date`
#' - `pressure_tag`: same as `pressure$value`
#' - `label` same as `pressure$label`
#' - `stap_id` same as `pressure$stap_id` (i.e., including 0)
#' - `pressure_era5` pressure retrieved from ERA5.
#' - `altitude` Altitude computed
#' - `lat` same as `path$lat`
#' - `lon` same as `path$lon`
#' - `pressure_era5_norm` pressure retrieved from ERA5 normalized to the average of `pressure_tag`
#' over the stationary period.
#' - `stap_ref` stap_id grouping of the requests (same as `stap_id` except for flights where
#' `stap_id = 0` and `stap_ref` is to the stationary period of the position used from the path).
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#'
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#'
#' pressurepath <- pressurepath_create(tag, path = path, quiet = TRUE)
#'
#' str(pressurepath)
#'
#' plot_pressurepath(pressurepath)
#'
#' pressurepath <- pressurepath_create(
#'   tag,
#'   path[c(2, 3, 4), ],
#'   include_flight = TRUE,
#'   quiet = TRUE
#' )
#'
#' plot_pressurepath(pressurepath)
#' @family pressurepath
#' @seealso [GeoPressureManual | Pressure
#' Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @export
pressurepath_create_old <- function(tag,
                                path = tag2path(tag),
                                variable = c("altitude", "pressure_era5", "skin_temperature"),
                                include_flight = TRUE,
                                preprocess = FALSE,
                                quiet = FALSE) {
  if (!quiet) {
    cli::cli_progress_step("Prepare pressure")
  }
  # Assert tag
  tag_assert(tag, "stap")

  # Assert preprocess
  assertthat::assert_that(is.logical(preprocess))
  if (preprocess) {
    pressure <- geopressure_map_preprocess(tag, compute_known = TRUE)
  } else {
    pressure <- tag$pressure
  }
  assertthat::assert_that(nrow(pressure) > 0)

  # Assert path
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("{.var path} is empty.")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some {.field stap_id} of {.var path} are not present in {.var tag$pressure}.\f")
  }

  # Assert include_flight
  assertthat::assert_that(is.logical(include_flight))

  # Create pressurepath by combining pressure and path
  pressurepath <- merge(
    pressure,
    path[!(names(path) %in%  c("start", "end") )],
    by = "stap_id",
    all.x = TRUE
  )

  # Because merge change the order of pressure, we sort by date to keep the same original order
  pressurepath <- pressurepath[order(pressurepath$date), ]

  # Interpolate lat lon during flight
  id <- pressurepath$stap_id == 0
  sequence <- seq_len(nrow(pressurepath))
  pressurepath$lat[id] <- stats::approx(sequence[!id],
                                        pressurepath$lat[!id], sequence[id],
                                        rule = 2
  )$y

  pressurepath$lon[id] <- stats::approx(sequence[!id],
                                        pressurepath$lon[!id], sequence[id],
                                        rule = 2
  )$y

  # Format query
  body <- list(
    lon = pressurepath$lon,
    lat = pressurepath$lat,
    time = as.numeric(as.POSIXct(pressurepath$date)),
    variable = variable,
    pressure = pressurepath$value * 100
  )

  if (!quiet) cli::cli_progress_step("Generate request (on GeoPressureAPI)")

  if (debug) {
    temp_file <- tempfile("log_pressurepath_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/pressurePath/") |>
    httr2::req_body_json(body, digit = 5, auto_unbox = FALSE)

  if (debug) {
    req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
  }

  # Perform the request and convert the response to data.frame
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)$data

  out <- as.data.frame(resp_data)

  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"
  out$surface_pressure <- out$surface_pressure / 100

  pressurepath <- merge(
    pressurepath,
    out,
    all.x = TRUE
  )

  names(pressurepath)[names(pressurepath) == 'value'] <- 'pressure_tag'

  df = pressurepath
  df$stap_id[df$label == "discard"] = 0

  agg = merge(
    aggregate(surface_pressure ~ stap_id, data = df, FUN=mean),
    aggregate(pressure_tag ~ stap_id, data = df, FUN=mean)
  )

  id = match(pressurepath$stap_id, agg$stap_id)

  pressurepath$surface_pressure_norm <- pressurepath$surface_pressure - agg$surface_pressure[id] + agg$pressure_tag[id]

  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "include_flight") <- include_flight
  attr(pressurepath, "sd") <- tag$param$sd
  return(pressurepath)
}
