#' Create a `pressurepath`
#'
#' @description
#'
#' A `pressurepath` is a data.frame merging `path` and `tag$pressure`. It can be thought as the
#' equivalent of the measurement of variables by sensors equipped on the bird moving along a
#' specific `path`.
#'
#' Any ERA variables can be retrieve but the functions is most notably used to retrieve ERA5
#' surface pressure to then be able to compare it to the pressure measured by the tag, and
#' potentially adjust labeling tag data.
#'
#' We use the [ERA5 LAND ](https://doi.org/10.24381/cds.e2161bac) or [ERA5 surface level
#' ](https://doi.org/10.24381/cds.adbb2d47) if position over water. Available variable can be found
#' on the [Parameter listings in ERA5 doc](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#heading-Parameterlistings)
#'
#' Positions during the flight are estimated by linear interpolation of the position of the stap
#' before and after. `pressurepath_create()` does not return measurement for stationary periods
#' which are not provided in `path` as well as the flight before and after.
#'
#' `pressurepath_create()` also return the altitude of the bird along its trajectory. The altitude
#' \eqn{z_{tag}} (a.s.l.) is computed from the tag pressure \eqn{P_{tag}}, using the barometric
#' equation \deqn{ z_{{tag}}(x)=z_{ERA5}(x) + \frac{T_{ERA5}(x)}{L_b}  \left(
#' \frac{P_{tag}}{P_{ERA5}(x)} \right)^{\frac{RL_b}{g M}-1},}
#' where \eqn{z_{ERA}}, \eqn{T_{ERA}}, and \eqn{P_{ERA}} respectively correspond to the ground level
#' elevation, temperature at 2m, and ground level pressure of ERA5, \eqn{L_b}  is the standard
#' temperature lapse rate, \eqn{R} is the universal gas constant, \eqn{g} is the gravity constant
#' and  \eqn{M} is the molar mass of air. See more information at
#' [the GeoPressureAPI documentation](https://raphaelnussbaumer.com/GeoPressureAPI/#description-1).
#'
#' To be able to compare the temporal variation of the retrieved pressure of ERA5 \eqn{P_{ERA}} to
#' the tag pressure \eqn{P_{tag}}, the function also returns the ERA pressure normalized with
#' the tag mean pressure measurement as `surface_pressure_norm`.
#' \deqn{ P_{ERA5,0}(\boldsymbol{x})[t] = \left( P_{ERA5}(\boldsymbol{x})[t]-P_{tag}[t]\right) -
#' \left( \frac{1}{n}\sum_{i=1}^{n} P_{ERA5}(\boldsymbol{x})[i]-P_{tag}[i] \right).}
#'
#' @param tag a GeoPressureR `tag` object.
#' @param path a GeoPressureR `path` data.frame.
#' @param variable ERA5 variable/parameters available to download. See
#' [Parameter listings in ERA5 doc](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#heading-Parameterlistings)
#' @param preprocess logical to use `geopressure_map_preprocess`.
#' @param quiet logical to hide messages about the progress
#' @param workers number of parallel requests on GEE. Integer between 1 and 99.
#' @param debug logical to display additional information to debug a request
#' @param timeout duration before the code is interrupted both for the request on
#' GeoPressureAPI and on GEE (in seconds, see `httr2::req_timeout()`).
#'
#' @return A GeoPressureR `pressurepath` data.frame with columns:
#' - `date` same as `pressure$date`
#' - `stap_id` same as `pressure$stap_id`
#' - `pressure_tag` same as `pressure$value`
#' - `label` same as `pressure$label`
#' - `j` same as `path$j`
#' - `ind` same as `path$ind`
#' - `lat` same as `path$lat`
#' - `lon` same as `path$lon`
#' - `include` same as `path$include`
#' - `known` same as `path$known`
#' - `altitude` altitude of the bird along the path (see detail)
#' - `surface_pressure` pressure retrieved from ERA5.
#' - `surface_pressure_norm` pressure retrieved from ERA5 normalized to the average of
#' `pressure_tag` over the stationary period.
#' - `...` any other ERA5 variable requested by `variable`
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
#'   quiet = TRUE
#' )
#'
#' plot_pressurepath(pressurepath)
#' @family pressurepath
#' @seealso [GeoPressureManual | Pressure
#' Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @export
pressurepath_create <- function(tag,
                                path = tag2path(tag),
                                variable = c("altitude", "surface_pressure"),
                                preprocess = FALSE,
                                timeout = 60 * 5,
                                workers = "auto",
                                quiet = FALSE,
                                debug = FALSE) {
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

  # Remove pressure for stap not provided in path as well as flight
  # Idenfify flight which have no provided position in path for the stap before and after
  stap_id_interp <- pressure$stap_id
  id <- stap_id_interp == 0
  sequence <- seq_len(nrow(pressure))
  stap_id_interp[id] <- stats::approx(sequence[!id],
    stap_id_interp[!id], sequence[id],
    rule = 2
  )$y
  id <- ceiling(stap_id_interp) %in% path$stap_id[!is.na(path$lon)] &
    floor(stap_id_interp) %in% path$stap_id[!is.na(path$lon)]
  pressure_w_path <- pressure[id, ]

  # Create pressurepath by combining pressure and path
  pressurepath <- merge(
    pressure_w_path,
    path[!(names(path) %in% c("start", "end"))],
    by = "stap_id",
    all.x = TRUE
  )

  # Because merge change the order of pressure, we sort by date to keep the same original order
  pressurepath <- pressurepath[order(pressurepath$date), ]

  names(pressurepath)[names(pressurepath) == "value"] <- "pressure_tag"

  # Interpolate lat lon during flight
  id <- pressurepath$stap_id == 0
  sequence <- seq_len(nrow(pressurepath))
  pressurepath$lat[id] <- stats::approx(sequence[!id],
    pressurepath$lat[!id], sequence[id],
    rule = 1
  )$y

  pressurepath$lon[id] <- stats::approx(sequence[!id],
    pressurepath$lon[!id], sequence[id],
    rule = 1
  )$y

  # Check workers
  assertthat::assert_that(is.numeric(workers) | workers == "auto")
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  if (workers == "auto") {
    workers <- 2 # min(90, round(nrow(pressurepath)/100))
  }
  assertthat::assert_that(workers > 0 & workers < 100)


  # Format query
  body <- list(
    lon = pressurepath$lon,
    lat = pressurepath$lat,
    time = as.numeric(as.POSIXct(pressurepath$date)),
    variable = variable,
    pressure = pressurepath$pressure_tag * 100,
    workers = workers
  )

  if (!quiet) cli::cli_progress_step("Generate request on GeoPressureAPI")

  if (debug) {
    temp_file <- tempfile("log_pressurepath_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/pressurePath/") |>
    httr2::req_body_json(body, digit = 5, auto_unbox = FALSE) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3)

  if (debug) {
    req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
  }

  # Perform the request and convert the response to data.frame
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)$data

  # If variable requested does not exist, the API return an empty list, which we
  # convert here as a NA
  out <- as.data.frame(lapply(resp_data, function(col) {
    if (length(col) == 0) {
      return(NA)
    } else {
      return(col)
    }
  }))

  # Convert time to date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Convert pressure Pa in hPa
  out$surface_pressure <- out$surface_pressure / 100

  # Add out to pressurepath
  pressurepath <- merge(
    pressurepath,
    out,
    all.x = TRUE
  )

  # Compute surface_pressure_norm
  # compute average pressure per stap without including discard
  df <- pressurepath
  df$stap_id[df$label == "discard"] <- 0
  agg <- merge(
    stats::aggregate(surface_pressure ~ stap_id, data = df, FUN = mean),
    stats::aggregate(pressure_tag ~ stap_id, data = df, FUN = mean)
  )
  id <- match(pressurepath$stap_id, agg$stap_id)
  pressurepath$surface_pressure_norm <- pressurepath$surface_pressure -
    agg$surface_pressure[id] + agg$pressure_tag[id]

  # Add additional parameter to pressurepath
  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "sd") <- tag$param$sd
  return(pressurepath)
}

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
#' @param variable ERA5 variable/parameters available to download. See
#' [Parameter listings in ERA5 doc](
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
#'
#' @noRd
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
    path[!(names(path) %in% c("start", "end"))],
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

  names(pressurepath)[names(pressurepath) == "value"] <- "pressure_tag"

  df <- pressurepath
  df$stap_id[df$label == "discard"] <- 0

  agg <- merge(
    stats::aggregate(surface_pressure ~ stap_id, data = df, FUN = mean),
    stats::aggregate(pressure_tag ~ stap_id, data = df, FUN = mean)
  )

  id <- match(pressurepath$stap_id, agg$stap_id)

  pressurepath$surface_pressure_norm <- pressurepath$surface_pressure -
    agg$surface_pressure[id] + agg$pressure_tag[id]

  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "include_flight") <- include_flight
  attr(pressurepath, "sd") <- tag$param$sd
  return(pressurepath)
}
