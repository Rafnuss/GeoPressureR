#' Download flight data
#'
#' @description
#' This function downloads data associated to each flight from the [ERA5 hourly pressure levels](https://doi.org/10.24381/cds.bd0915c6)
#' with the [Climate Data Store (CDS)](https://cds.climate.copernicus.eu/) and through the [`ecmwfr`
#' R package](https://bluegreen-labs.github.io/ecmwfr/index.html).
#'
#' [Any variable available from the ERA5 pressure level](https://confluence.ecmwf.int/display/CKB/ERA5:+data+documentation#ERA5:datadocumentation-Table9)
#' can be downloaded.
#'
#' The flights are determined from the stationary periods classified `tag$stap`. It requests a
#' single file for each flight using the exact time (hourly basis) and pressure (altitude). To make
#' the download more efficient, [`wf_request_batch()`](
#' https://bluegreen-labs.github.io/ecmwfr/articles/advanced_vignette.html#batch-parallel-requests)
#' is used to download files in parallel. CDS queue limits vary with the current system workload.
#' If CDS reports that queued requests are temporarily limited, reduce `workers` and try again.
#'
#' More information [in the GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/geopressuretemplate-wind.html).
#'
#' @template ecmwf-key
#' @param tag a GeoPressureR `tag` object.
#' @param extent geographical extent of the map on which the likelihood will be computed.
#' Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param include_stap_id stationary period identifiers of the start of the flight to download.
#' Default is to download all flights.
#' @param variable list of variables to download from [the ERA5 pressure level
#' ](https://confluence.ecmwf.int/display/CKB/ERA5:+data+documentation#ERA5:datadocumentation-Table9):
#' `"u_component_of_wind"`, `"v_component_of_wind"`,  `"temperature"`,
#' `"fraction_of_cloud_cover"`, `"relative_humidity"`, `"vertical_velocity"`,
#' `"specific_cloud_ice_water_content"`, `"specific_cloud_liquid_water_content"`,
#' `"specific_humidity"`, `"specific_rain_water_content"`, `"specific_snow_water_content"`,
#' `"divergence"`, `"geopotential"`, `"ozone_mass_mixing_ratio"`, `"potential_vorticity"`,
#' `'vorticity"`.
#' @param file absolute or relative path of the ERA5 wind data file to be downloaded. Function
#' taking as arguments (1) the stationary period identifier and (2) the tag_id.
#' @param overwrite logical. If `TRUE`, file is overwritten.
#' @param workers maximum number of simultaneous requests submitted to CDS. Defaults to `19`.
#' CDS queue limits are dynamic; use a lower value if queued requests are temporarily limited.
#' @param cds_token `r lifecycle::badge("deprecated")` Enter the API token with
#' [`ecmwfr::wf_set_key()`]
#' @inheritParams ecmwfr::wf_request_batch
#' @inheritDotParams ecmwfr::wf_request_batch
#'
#' @return The path of the downloaded (requested file) or the an R6 object with download/transfer
#' information
#' @examplesIf FALSE
#'   tag_download_wind(tag)
#' @family movement
#' @seealso [`wf_request_batch()`
#' ](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html),
#' [GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/trajectory-with-wind.html)
#' @export
tag_download_wind <- function(
  tag,
  extent = tag$param$tag_set_map$extent,
  include_stap_id = NULL,
  variable = c("u_component_of_wind", "v_component_of_wind"),
  file = \(stap_id, tag_id) {
    glue::glue("./data/wind/{tag_id}/{tag_id}_{stap_id}.nc")
  },
  overwrite = FALSE,
  workers = 19,
  cds_token = lifecycle::deprecated(),
  ...
) {
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ecmwfr} is required for {.fun tag_download_wind}.",
      "i" = "Install it with {.run install.packages('ecmwfr')}."
    ))
  }

  if (lifecycle::is_present(cds_token)) {
    lifecycle::deprecate_warn(
      "3.3.4",
      "tag_download_wind(cds_token)",
      "ecmwfr::wf_set_key(key)"
    )
    ecmwfr::wf_set_key(key = cds_token)
  }

  tag_assert(tag, "setmap")

  tag_id <- tag$param$id

  stap <- tag$stap

  assertthat::assert_that(length(extent) == 4)

  assertthat::assert_that(is.function(file))
  directory <- dirname(file(1, tag_id))
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    cli::cli_warn(c(
      "!" = "The directory {.file {directory}} did not exist.",
      ">" = "We created the directory."
    ))
  }

  if (is.null(include_stap_id)) {
    include_stap_id <- utils::head(tag$stap$stap_id, -1)

    # Take all stap_id without an existing wind file
    if (!overwrite) {
      include_stap_id <- include_stap_id[
        !file.exists(file(include_stap_id, tag_id))
      ]
    }
  }
  assertthat::assert_that(is.numeric(include_stap_id))
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))

  assertthat::assert_that(is.character(variable))

  # remove the last include_stap_id if it was added by mistake
  if (utils::tail(tag$stap$stap_id, 1) %in% include_stap_id) {
    include_stap_id <- utils::head(sort(include_stap_id), -1)
    cli::cli_warn(c(
      "!" = "{.var include_stap_id} included the last stationary period for which no wind can be computed.",
      ">" = "We removed this stationary period."
    ))
  }

  if (any(file.exists(file(include_stap_id, tag_id))) && !overwrite) {
    tmp <- file.exists(file(include_stap_id, tag_id))
    cli::cli_abort(c(
      "x" = "There are already ERA5 data file for stationary periods {.var {include_stap_id[tmp]}}",
      ">" = "Delete the corresponding file or use the argument {.code overwrite = TRUE}."
    ))
  }

  # see https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Levellistings
  possible_pressure <- c(
    1,
    2,
    3,
    5,
    7,
    10,
    20,
    30,
    50,
    70,
    seq(100, 250, 25),
    seq(300, 750, 50),
    seq(775, 1000, 25)
  )

  # create list of request
  request_list <- list()

  for (i_s in include_stap_id) {
    # Get the time series of the flight on a 1 hour resolution
    flight_time <- seq(
      round.POSIXt(stap$end[i_s] - 30 * 60, units = "hours"),
      round.POSIXt(stap$start[i_s + 1] + 30 * 60, units = "hours"),
      by = 60 * 60
    )

    # Find the pressure level needed during this flight
    flight_id <- flight_time[1] <= tag$pressure$date &
      tag$pressure$date <= utils::tail(flight_time, 1)
    flight_pressure <- tag$pressure$value[flight_id]
    flight_pressure <- flight_pressure[is.finite(flight_pressure)]
    if (length(flight_pressure) == 0) {
      cli::cli_abort(c(
        "x" = "No finite pressure observations are available for the flight between stationary periods {.val {stap$stap_id[i_s]}} and {.val {stap$stap_id[i_s + 1]}}.",
        "i" = "Flight period: {.val {format(flight_time[1], '%Y-%m-%d %H:%M UTC')}} to {.val {format(utils::tail(flight_time, 1), '%Y-%m-%d %H:%M UTC')}}.",
        "i" = "Pressure observations are required to select the ERA5 pressure levels for the wind download.",
        ">" = "Review the pressure data or exclude this flight with {.arg include_stap_id}."
      ))
    }
    pres_id_min <- min(
      sum(min(flight_pressure) >= possible_pressure),
      length(possible_pressure) - 1
    )
    pres_id_max <- min(
      sum(max(flight_pressure) > possible_pressure) + 1,
      length(possible_pressure)
    )
    flight_pres_id <- seq(pres_id_min, pres_id_max)

    # Make some check
    assertthat::assert_that(length(possible_pressure[flight_pres_id]) > 1)
    assertthat::assert_that(length(flight_time) > 1)

    # Prepare the query
    request_list[[i_s]] <- list(
      dataset_short_name = "reanalysis-era5-pressure-levels",
      product_type = "reanalysis",
      data_format = "netcdf",
      variable = variable,
      pressure_level = possible_pressure[flight_pres_id],
      year = sort(unique(format(flight_time, "%Y"))),
      month = sort(unique(format(flight_time, "%m"))),
      day = sort(unique(format(flight_time, "%d"))),
      time = sort(unique(format(flight_time, "%H:%M"))),
      area = c(extent[4], extent[1], extent[3], extent[2]), # N, W, S, E
      target = basename(file(i_s, tag_id))
    )
  }

  ecmwfr::wf_request_batch(
    request_list[include_stap_id],
    workers = workers,
    path = directory,
    time_out = 3 * 3600,
    ...
  )
}
