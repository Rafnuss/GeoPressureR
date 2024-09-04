#' Download flight data
#'
#' @description
#' This function download data associated to each flight from the [ERA5 hourly pressure levels](
#' https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview)
#' with the [Climate Data Store (CDS)](https://cds.climate.copernicus.eu/) and through the [`ecmwfr`
#' R package](https://bluegreen-labs.github.io/ecmwfr/index.html).
#'
#' [Any variable available from the ERA5 pressure level](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table9)
#' can be downloaded.
#'
#' The flights are determined from the stationary periods classified `tag$stap`. It request a
#' single file for each flight using the exact time (hourly basis) and pressure (altitude). To make
#'  the download more efficient, [`wf_request_batch()`](
#' https://bluegreen-labs.github.io/ecmwfr/articles/advanced_vignette.html#batch-parallel-requests)
#' is used to download all files at the same time (up to 20 requests in parallel).
#'
#' To be able to download data from the Climate Data Store (CDS), you will need to create an account
#' on [https://cds.climate.copernicus.eu](https://cds.climate.copernicus.eu). Once created, you can
#' retrieve your API key on [https://cds.climate.copernicus.eu/user/
#' ](https://cds.climate.copernicus.eu/user/) and save them in your environment file
#' (i.e., `.Renviron`). You can open this file with `usethis::edit_r_environ()` and add:
#' \code{
#'   cds_token = "{Personal Access Token}"
#' }
#'
#' @param tag a GeoPressureR `tag` object.
#' @param extent geographical extent of the map on which the likelihood will be computed.
#' Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param include_stap_id stationary period identifiers of the start of the flight to download.
#' Default is to download all flights.
#' @param variable list of variables to download from [the ERA5 pressure level](
#' https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Table9):
#' `"u_component_of_wind"`, `"v_component_of_wind"`,  `"temperature"`, `"fraction_of_cloud_cover"`,
#' `"relative_humidity"`, , `"vertical_velocity"`, `"specific_cloud_ice_water_content"`,
#' `"specific_cloud_liquid_water_content"`, `"specific_humidity"`, `"specific_rain_water_content"`,
#' `"specific_snow_water_content"`, `"divergence"`, `"geopotential"`, `"ozone_mass_mixing_ratio"`,
#' `"potential_vorticity"`, `'vorticity"`.
#' @param cds_token CDS Personal Access Token available from [your user page
#' ](https://cds.climate.copernicus.eu/user/). See `wf_set_key()`.
#' @param file absolute or relative path of the ERA5 wind data file to be downloaded. Function
#' taking as single argument the stationary period identifier.
#' @param overwrite logical. If `TRUE`, file is overwritten.
#' @return the path of the downloaded (requested file) or the an R6 object with download/transfer
#' information
#'
#' @family movement
#' @seealso [`wf_request()`](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html),
#' [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
#' @export
tag_download_wind <- function(
    tag,
    extent = tag$param$extent,
    include_stap_id = NULL,
    variable = c("u_component_of_wind", "v_component_of_wind"),
    cds_token = Sys.getenv("cds_token"),
    file = \(stap_id) glue::glue("./data/wind/{tag$param$id}/{tag$param$id}_{stap_id}.nc"),
    overwrite = FALSE) {
  tag_assert(tag, "setmap")

  stap <- tag$stap

  assertthat::assert_that(length(extent) == 4)

  assertthat::assert_that(is.function(file))
  directory <- dirname(file(1))
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    cli::cli_warn(c(
      "!" = "The directory {.file {directory}} did not exist.",
      ">" = "We created the directory.\f"
    ))
  }

  if (is.null(include_stap_id)) {
    include_stap_id <- utils::head(tag$stap$stap_id, -1)

    # Take all stap_id without an existing wind file
    if (!overwrite) {
      include_stap_id <- include_stap_id[!file.exists(file(include_stap_id))]
    }
  }
  assertthat::assert_that(is.numeric(include_stap_id))
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))

  assertthat::assert_that(is.character(variable))

  # remove the last include_stap_id if it was added by mistake
  if (utils::tail(tag$stap$stap_id, 1) %in% include_stap_id) {
    include_stap_id <- utils::head(sort(include_stap_id), -1)
    cli::cli_warn(c(
      "!" = "{.var include_stap_id} included the last stationary period for which no wind can be \\
      computed.",
      ">" = "We removed this stationary period.\f"
    ))
  }

  ecmwfr::wf_set_key(key = cds_token)

  if (any(file.exists(file(include_stap_id))) && !overwrite) {
    # nolint start
    tmp <- file.exists(file(include_stap_id))
    cli::cli_abort(c(
      "x" = "There are already ERA5 data file for stationary periods {.var {include_stap_id[tmp]}}",
      ">" = "Delete the corresponding file or use the argument {.code overwrite = TRUE}."
    ))
    # nolint end
  }

  # nolint start
  # see https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Levellistings
  # nolint end
  possible_pressure <- c(
    1, 2, 3, 5, 7, 10, 20, 30, 50, 70, seq(100, 250, 25), seq(300, 750, 50), seq(775, 1000, 25)
  )

  # create list of request
  request_list <- list()

  for (i_s in include_stap_id) {
    # Get the time series of the flight on a 1 hour resolution
    flight_time <- seq(round.POSIXt(stap$end[i_s] - 30 * 60, units = "hours"),
      round.POSIXt(stap$start[i_s + 1] + 30 * 60, units = "hours"),
      by = 60 * 60
    )

    # Find the pressure level needed during this flight
    flight_id <- flight_time[1] <= tag$pressure$date &
      tag$pressure$date <= utils::tail(flight_time, 1)
    pres_id_min <- min(
      sum(!(min(tag$pressure$value[flight_id]) < possible_pressure)),
      length(possible_pressure) - 1
    )
    pres_id_max <- min(
      sum(max(tag$pressure$value[flight_id]) > possible_pressure) + 1,
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
      format = "netcdf",
      variable = variable,
      pressure_level = possible_pressure[flight_pres_id],
      year = sort(unique(format(flight_time, "%Y"))),
      month = sort(unique(format(flight_time, "%m"))),
      day = sort(unique(format(flight_time, "%d"))),
      time = sort(unique(format(flight_time, "%H:%M"))),
      area = c(extent[4], extent[1], extent[3], extent[2]), # N, W, S, E
      target = basename(file(i_s))
    )
  }

  ecmwfr::wf_request_batch(
    request_list[include_stap_id],
    workers = 20,
    # user = ,
    path = directory,
    # time_out = 3600,
    # total_timeout = length(request_list) * time_out/workers
  )
}
