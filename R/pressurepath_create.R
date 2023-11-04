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
#' @param include_flight extend request to also query the pressure and altitude during the previous
#' and/or next flight. Flights are defined by a `stap_id = 0`. Accept logical or vector of -1 (
#' previous flight), 0 (stationary) and/or 1 (next flight). (e.g. `include_flight = c(-1, 1)` will
#' only search for the flight before and after but not the stationary period). Note that next and
#' previous flights are defined by the +/1 of the `stap_id` value (and not the previous/next
#' `stap_id` value).
#' @param workers number of parallel requests on GEE. Between 1 and 99. `"auto"` adjust the number
#' of workers to the number of `stap_elev` to query.
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
pressurepath_create <- function(tag,
                                path = tag2path(tag),
                                include_flight = FALSE,
                                workers = "auto",
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
  id_0 <- pressure$stap_id == 0 | is.na(pressure$stap_id)
  stap_interp <- pressure$stap_id
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

  if (!quiet) {
    # nolint start
    msg <- glue::glue("0/{nrow(path)}")
    cli::cli_progress_step(
      "Generate requests (on GeoPressureAPI) for stap: {msg}"
    )
    # nolint end
  }
  for (i_s in seq_len(nrow(path))) {
    i_stap <- path$stap_id[i_s]

    if (!quiet) {
      msg <- glue::glue("{i_s}/{nrow(path)}")
      cli::cli_progress_update(force = TRUE)
    }

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

    # extract pressure for the stap
    pressure_q <- subset(pressure, !is.na(id_q))

    if (nrow(pressure_q) == 0) {
      cli::cli_warn("No pressure to query for stap_id {.val {i_stap}}.\f")
    }

    # Send the query
    if (!is.na(path$lat[i_s]) && !is.na(path$lon[i_s]) && nrow(pressure_q) > 0) {
      f[[i_s]] <- future::future({
        geopressure_timeseries(path$lat[i_s], path$lon[i_s],
          pressure = pressure_q,
          quiet = TRUE
        )
      }, seed = TRUE)
    }
  }

  pressure_timeseries <- list()
  if (!quiet) {
    # nolint start
    msg2 <- glue::glue("0/{length(f)}")
    cli::cli_progress_step(
      "Compute and download timeseries (on GEE server): {msg2}"
    )
    # nolint end
  }
  for (i_s in seq_len(length(f))) {
    i_stap <- path$stap_id[i_s]
    if (!quiet) {
      msg2 <- glue::glue("{i_s}/{length(f)}")
      cli::cli_progress_update(force = TRUE)
    }
    if (inherits(f[[i_s]], "Future")) {
      tryCatch(
        expr = {
          pressure_timeseries[[i_s]] <- future::value(f[[i_s]])
          pressure_timeseries[[i_s]]$stap_ref <- i_stap
        },
        error = function(cond) {
          cli::cli_warn("Error for stap {path$stap_id[i_s]}\f")
          message(cond)
          pressure_timeseries[[i_s]] <- data.frame()
        }
      )
    } else {
      pressure_timeseries[[i_s]] <- data.frame()
    }
  }

  # Explicitly close multisession workers by switching plan
  future::plan(future::sequential)

  if (!quiet) {
    cli::cli_progress_step("Build pressurepath")
  }
  pressurepath <- do.call("rbind", pressure_timeseries)

  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "include_flight") <- include_flight
  attr(pressurepath, "sd") <- tag$param$sd
  return(pressurepath)
}
