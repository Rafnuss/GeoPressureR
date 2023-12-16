#' Download wind data
#'
#' @description
#' This function downloads the wind data from [ERA5 hourly pressure levels](
#' https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview)
#' with the [Climate Data Store (CDS)](https://cds.climate.copernicus.eu) and through the [`ecmwfr`
#' R package](https://bluegreen-labs.github.io/ecmwfr/index.html).
#'
#' The flights are determined from the stationary periods classified `tag$sta`
#' (see `tag_label_auto()`). It request a single file for each flight using the exact time
#' (hourly basis) and pressure (altitude). To make the download more efficient,
#' [`wf_request_batch()`](
#' https://bluegreen-labs.github.io/ecmwfr/articles/advanced_vignette.html#batch-parallel-requests)
#' is used to download all wind files at the same time (up to 20 requests in parallel).
#'
#' To be able to download data from the Climate Data Store (CDS), you will need to create an account
#' on [https://cds.climate.copernicus.eu](https://cds.climate.copernicus.eu). You can then save
#' your credentials (`cds_key` and `cds_user`) in your `.Rprofile` (see
#' [GeoPressureManual](
#' https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)).
#'
#' @param tag a GeoPressureR `tag` object.
#' @param extent Geographical extent of the map on which the likelihood will be computed.
#' Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param stap_id Stationary period identifier of the start of the flight to download. Be default,
#' download for all flights.
#' @param cds_key User (email address) used to sign up for the ECMWF data service. See
#' [`wf_set_key()`].
#' @param cds_user Token provided by ECMWF. See [`wf_set_key()`].
#' @param file Absolute or relative path of the ERA5 wind data file to be downloaded. Function
#' taking as single argument the stationary period identifier.
#' @param overwrite logical. If `TRUE`, file is overwritten.
#' @return The folder with the downloaded file (same as the directory).
#'
#' @family movement
#' @seealso [`wf_request()`](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html),
#' [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
#' @export
tag_download_wind <- function(
    tag,
    extent = tag$param$extent,
    stap_id = utils::head(tag$stap$stap_id, -1),
    cds_key = Sys.getenv("cds_key"),
    cds_user = Sys.getenv("cds_user"),
    file = \(stap_id) glue::glue("./data/wind/{tag$param$id}/{tag$param$id}_{stap_id}.nc"),
    overwrite = FALSE) {

  tag_assert(tag, "setmap")

  stap <- tag$stap

  assertthat::assert_that(length(extent) == 4)
  assertthat::assert_that(is.numeric(stap_id))
  assertthat::assert_that(all(stap_id %in% stap$stap_id))

  # remove the last stap_id if it was added by mistake
  if (utils::tail(tag$stap$stap_id, 1) %in% stap_id) {
    stap_id <- utils::head(sort(stap_id), -1)
    cli::cli_warn(c(
      "!" = "{.var stap_id} included the last stationarp period for which no wind can be \\
      computed.",
      ">" = "We removed this stationary period.\f"
    ))
  }

  ecmwfr::wf_set_key(user = cds_user, key = cds_key, service = "cds")

  directory <- dirname(file(1))
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    cli::cli_warn(c(
      "!" = "The directory {.file {directory}} did not exist.",
      ">" = "We created the directory.\f"
    ))
  }
  if (any(file.exists(file(stap_id))) && !overwrite) {
    # nolint start
    tmp <- file.exists(file(stap_id))
    cli::cli_abort(c(
      "x" = "There are already wind data file for stationary periods {.var {stap_id[tmp]}}",
      ">" = "Delete the corresponding file or use the arguement {.code overwrite = TRUE}."
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

  for (i_s in stap_id) {
    # Get the timeserie of the flight on a 1 hour resolution
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
      variable = c("u_component_of_wind", "v_component_of_wind"),
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
    request_list[stap_id],
    workers = 20,
    # user = ,
    path = directory,
    # time_out = 3600,
    # total_timeout = length(request_list) * time_out/workers
  )
}
