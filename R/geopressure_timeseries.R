#' Retrieve an ERA5 pressure time series
#'
#' `geopressure_timeseries()` retrieves an hourly ERA5 surface-pressure time series at one
#' location. Supply a tag pressure series to also normalise ERA5 pressure and estimate altitude.
#'
#' @section Data sources:
#' `source = "arco"` reads ECMWF's Analysis-Ready Cloud-Optimised (ARCO) archive directly. It is
#' generally faster for a long time series at one location, supports ERA5-Land and global ERA5,
#' and requires an ECMWF API key plus the optional `Rarr` and `ecmwfr` packages.
#'
#' `source = "api"` asks the hosted GeoPressureAPI to prepare the data. It needs no ECMWF key or
#' `Rarr` installation, but depends on that service and supports its fixed ERA5 configuration.
#'
#' The default, `source = "auto"`, uses ARCO when a key stored by [ecmwfr::wf_set_key()] is
#' available and GeoPressureAPI otherwise. Use [geopressure_timeseries_arco()] or
#' [geopressure_timeseries_api()] to select a backend explicitly.
#'
#' @section ERA5 datasets and matching:
#' With ARCO, `era5_dataset = "land"` uses ERA5-Land on a 0.1 degree grid. It has finer spatial
#' resolution but is masked over oceans; ocean locations are moved to the closest land cell.
#' `era5_dataset = "single-levels"` uses global ERA5 on a 0.25 degree grid and retains locations
#' over water. GeoPressureAPI chooses its ERA5 data internally and moves ocean locations onshore.
#'
#' Without `pressure`, the requested interval is returned hourly. With ARCO and `pressure`, each tag
#' time is matched to its closest ERA5 hour and restored after matching; no temporal interpolation
#' is used. GeoPressureAPI receives the original tag timestamps and performs the matching remotely.
#'
#' @section Altitude and pressure normalisation:
#' When tag pressure is supplied, altitude above mean sea level is
#' computed with the barometric equation from tag pressure, ERA5 surface pressure, ERA5 2 m
#' temperature, and surface geopotential. Tag pressure is expected in hPa and altitude is returned
#' in metres.
#'
#' ERA5 surface pressure is also shifted to the mean tag-pressure level within each elevation-label
#' group. Flight observations (`stap_id == 0`) and observations labelled `"discard"` are excluded
#' from the group means. The adjusted series is returned as `surface_pressure_norm`.
#'
#' @template ecmwf-key
#' @param lat Numeric scalar latitude, between -90 and 90 degrees.
#' @param lon Numeric scalar longitude, between -180 and 180 degrees.
#' @param pressure Optional data.frame with a date-time `date` column and numeric pressure `value`
#'   column in hPa. Additional columns are retained.
#' @param start_time,end_time Start and end of the requested interval when `pressure` is `NULL`.
#' @param source Data source: `"auto"`, `"arco"`, or `"api"`.
#' @param era5_dataset ERA5 product used by ARCO: `"land"` at 0.1 degree resolution or
#'   `"single-levels"` at 0.25 degree resolution. GeoPressureAPI uses its own configuration.
#' @param quiet Logical to suppress progress messages.
#' @param debug Logical to display request details.
#'
#' @return A data.frame containing `date`, `surface_pressure`, `lat`, and `lon`. With `pressure`,
#'   it also retains the input columns, renames `value` to `pressure_tag`, and adds
#'   `surface_pressure_norm` and `altitude`.
#'
#' @examplesIf FALSE
#' geopressure_timeseries(
#'   lat = 46,
#'   lon = 6,
#'   start_time = "2020-01-01",
#'   end_time = "2020-01-02"
#' )
#'
#' @family pressurepath
#' @export
geopressure_timeseries <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  source = c("auto", "arco", "api"),
  era5_dataset = c("land", "single-levels")
) {
  input <- geopressure_timeseries_prepare(
    lat,
    lon,
    pressure,
    start_time,
    end_time,
    quiet
  )
  pressure <- input$pressure
  start_time <- input$start_time
  end_time <- input$end_time
  source <- ecmwf_select_source(source, quiet)

  if (source == "arco") {
    arco_require_dependencies()
    return(geopressure_timeseries_arco_impl(
      lat = lat,
      lon = lon,
      pressure = pressure,
      start_time = start_time,
      end_time = end_time,
      quiet = quiet,
      debug = debug,
      era5_dataset = era5_dataset
    ))
  }

  geopressure_timeseries_api_impl(
    lat = lat,
    lon = lon,
    pressure = pressure,
    start_time = start_time,
    end_time = end_time,
    quiet = quiet,
    debug = debug
  )
}

geopressure_timeseries_prepare <- function(
  lat,
  lon,
  pressure,
  start_time,
  end_time,
  quiet
) {
  assertthat::assert_that(is.numeric(lon))
  assertthat::assert_that(is.numeric(lat))
  assertthat::assert_that(lon >= -180 & lon <= 180)
  assertthat::assert_that(lat >= -90 & lat <= 90)
  assertthat::assert_that(is.logical(quiet))
  if (!is.null(pressure)) {
    assertthat::assert_that(is.data.frame(pressure))
    assertthat::assert_that("date" %in% names(pressure))
    assertthat::assert_that(assertthat::is.time(pressure$date))
    assertthat::assert_that("value" %in% names(pressure))
    assertthat::assert_that(is.numeric(pressure$value))
    assertthat::assert_that(nrow(pressure) > 0)
    start_time <- end_time <- NULL
  } else {
    start_time <- as.POSIXct(start_time, tz = "UTC")
    end_time <- as.POSIXct(end_time, tz = "UTC")
    assertthat::assert_that(start_time <= end_time)
  }
  list(pressure = pressure, start_time = start_time, end_time = end_time)
}
