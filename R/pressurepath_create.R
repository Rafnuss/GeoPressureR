#' Download ERA5 pressure timeseries along a path
#'
#' @description
#' This function downloads the ERA5 pressure timeseries matching geolocator pressure data along a
#' path. The function essentially splits the pressure of the geolocator into stationary periods and
#' downloads the ERA5 timeseries of pressure for the corresponding location of the path by calling
#' `geopressure_timeseries()`. The computation is optimized by paralleling the call of
#' each stationary period.
#'
#' The location of the path is matched with the geolocator pressure using `stap_id`.
#'
#' You can include previous and/or next flight period in each query, for instance to estimate
#' flight altitude.
#'
#' Read more about how this is computed with [`geopressure_timeseries()`]
#'
#' @param path A data.frame of the position containing latitude (`lat`), longitude  (`lon`), and the
#' stationary period id (`stap`) as columns.
#' @param pressure Pressure data.frame from data logger.
#' @param include_flight Extend request to also query the pressure and altitude during the previous
#' and/or next flight. Flights are defined by a `stap_id = 0`. Accept Logical or vector of -1 (
#' previous flight), 0 (stationary) and/or 1 (next flight). (e.g. `include_flight = c(-1, 1)` will
#' only search for the flight before and after but not the stationary period). Note that next and
#' previous flights are defined by the +/1 of the `stap_id` value (and not the previous/next
#' `stap_id` value).
#' @param workers Number of parallel requests on GEE. Between 1 and 99.
#' @return A data.frame containing the row bind of the data.frames of each stationary period
#' computed with `geopressure_timeseries()`:
#' - `date` same as `pressure$date`
#' - `pressure_tag`: same as `pressure$value`
#' - `label` same as `pressure$label`
#' - `stap_id` same as `pressure$stap_id` (i.e., including 0)
#' - `pressure_era5`
#' - `altitude`
#' - `lat`
#' - `lon`
#' - `pressure_era5_norm`
#' - `stap_ref` stap_id grouping of the requests (same as `stap_id` except for flights where
#' `stap_id = 0` and `stap_id>0` is to the stationary period of the position used from the path).
#'
#' See [`geopressure_timeseries()`] for more details
#' @seealso [`geopressure_timeseries()`], [`tag2path()`], [GeoPressureManual | Pressure
#' Map](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |>
#'   tag_label()
#'
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#'
#' pressurepath <- pressurepath_create(path, tag$pressure)
#'
#' str(pressurepath)
#'
#' plot(pressurepath$date, pressurepath$pressure_tag,
#'   col = pressurepath$stap_id,
#'   ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#' lines(pressurepath$date, pressurepath$pressure_era5_norm, col = "red")
#'
#'
#' pressurepath <- pressurepath_create(
#'   path[c(2, 3, 4), ], tag$pressure,
#'   include_flight = TRUE
#' )
#'
#' plot(pressurepath$date, pressurepath$altitude,
#'   type = "l", ylab = "Pressure (hPa)", xlab = "Datetime"
#' )
#' @family pressurepath
#' @export
pressurepath_create <- function(tag,
                                path = tag2path(tag),
                                include_flight = FALSE,
                                workers = "auto",
                                .preprocess = FALSE) {
  # Assert tag
  tag_assert(tag, cond = "stap")

  # Assert preprocess
  assertthat::assert_that(is.logical(.preprocess))
  if (.preprocess) {
    pressure <- geopressure_map_preprocess(tag)
  } else {
    pressure <- tag$pressure
  }

  # Assert path
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("Path is empty")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some path stap_id are not present in pressure")
  }

  # Assert include_flight
  if (is.logical(include_flight)) {
    if (include_flight) {
      include_flight <- c(-1, 0, 1)
    } else {
      include_flight <- 0
    }
  }
  assertthat::assert_that(is.numeric(include_flight))
  assertthat::assert_that(all(include_flight %in% c(-1, 0, 1)))

  # Assert worker
  assertthat::assert_that(is.numeric(workers) | workers == "auto")

  # Interpolate stap_id for flight period so that, a flight between stap_id 2 and 3 will have a
  # `stap_interp` between 2 and 3.
  id_0 <- pressure$stap_id == 0 | is.na(pressure$stap)
  stap_interp <- pressure$stap
  stap_interp[id_0] <- stats::approx(which(!id_0),
    pressure$stap_id[!id_0], which(id_0),
    rule = 2
  )$y

  # Define the number of parallel workers (Google Earth Engine allowance is currently 100)
  if (workers == "auto") {
    workers <- min(90, nrow(path))
  } else {
    assertthat::assert_that(workers > 0 & workers < 100)
  }
  future::plan(future::multisession, workers = workers)
  f <- c()

  cli::cli_progress_step(
    "Generate requests (on GeoPressureAPI) for {nrow(path)} stationary periods",
    spinner = TRUE
  )
  cli::cli_progress_bar(total = nrow(path), type = "task")
  for (i_s in seq_len(nrow(path))) {
    i_stap <- path$stap_id[i_s]
    # Subset the pressure of the stationary period
    id_q <- rep(NA, length(stap_interp))
    if (any(0 == include_flight)) {
      id_q[path$stap_id[i_s] == stap_interp] <- 0
    }
    if (any(-1 == include_flight)) {
      id_q[i_stap - 1 < stap_interp & stap_interp < i_stap] <- -1
    }
    if (any(1 == include_flight)) {
      id_q[i_stap < stap_interp & stap_interp < i_stap + 1] <- 1
    }
    # Send the query
    f[[i_s]] <- future::future({
      geopressure_timeseries(path$lat[i_s], path$lon[i_s],
        pressure = subset(pressure, !is.na(id_q)),
        quiet = TRUE
      )
    })
    cli::cli_progress_update(force = TRUE)
  }

  pressure_timeseries <- list()
  cli::cli_progress_step("Compute and download timeseries (on GEE server)",
    spinner = TRUE
  )
  cli::cli_progress_bar(total = length(f), type = "task")
  for (i_s in seq_len(length(f))) {
    i_stap <- path$stap_id[i_s]
    tryCatch(
      expr = {
        pressure_timeseries[[i_s]] <- future::value(f[[i_s]])
        pressure_timeseries[[i_s]]$stap_ref <- i_stap
        cli::cli_progress_update(force = TRUE)
      },
      error = function(cond) {
        cli::cli_warn("Error for stap {path$stap_id[i_s]}")
        message(cond)
      }
    )
  }

  pressurepath <- do.call("rbind", pressure_timeseries)

  return(pressurepath)
}
