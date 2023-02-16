#' Request and download mismatch maps from pressure
#'
#' This function returns for each stationary period two map of mismatch between the pressure
#' measured by a geolocator and the ERA5 pressure database.
#'
#' These maps are generated on Google Earth Engine via the map entry point of the
#' [GeoPressure API](https://raphaelnussbaumer.com/GeoPressureAPI/#description). The computation
#' performed by this function consists of the following:
#' 1. Send a request to generate the Google Earth Engine (GEE) url of the code producing the maps
#' for each stationary periods separately.
#' 2. Download and read these geotiff maps as matrix. The computation on GEE only happen in this
#' second step when fetching the url.
#'
#' The maps of each stationary period are returned in two layers:
#' 1. The Mean Square Error (MSE) between the data logger pressure timeseries and the reanalysis.
#' The mean error is removed because we assume no specific altitude of the geolocator, thus
#' allowing an altitudinal shift of the pressure timeserie.
#' 2. The mask of the proportion of the pressure measurement corresponding to altitude value
#' which fall within the min and max ground elevation found at each location. The altitude value
#' of the geolocator pressure timeseries is computed with the barometric formula accounting for the
#' temporal variation of pressure (surface-pressure) and temperature (2m-temperature) based on
#' ERA5 data. The min and max ground elevation of each pixel is computed from SRTM-90.
#'
#' For each stationary period, the pressure measurements are smoothed and downscale to 1 hour
#' resolution in order to match ERA-5 resolution.
#'
#' It is possible to indicates different elevation level when the bird was spending time at
#' places with different elevation within general area (~10km), and thus within the same stationary
#' period. This can be done by using `tag$label="elev_*n*"`for all measurements of the same
#' elevation level *n*. See example in [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html).
#'
#' For more background and detail on the algorithm, read the [associated scientific publication
#' ]( https://doi.org/10.1111/2041-210X.14043). For more information on the exact computation, read
#' the [GeoPressure API documentation](https://raphaelnussbaumer.com/GeoPressureAPI/).
#'
#' @param tag Data logger list (see [`tag_read()`]). This list needs to contains a `pressure`
#' data.frame with variable :
#' - `date` datetime of measurement as POSIXt,
#' - `value` pressure measurement in hPa,
#' - `stap` grouping observation measured during the same stationary period and
#' - `label` indicates the observation to be discarded (`"discard"` and `"flight"`) as well as
#' grouped into sub
#' In addition, `tag` also need to contains `stap` with exact time of the stationary period and in
#' the between flights.
#' @param extent Geographical extent of the map on which the likelihood will be computed. Vector of
#' length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param scale Number of pixel per 1° latitude-longitude. For instance, `scale = 10` for a
#' resolution of 0.1° (~10km) and `scale=4` for a resolution of 0.25° (~30km). To avoid
#' interpolating the ERA5 data, scale should be smaller than 10. Read more about [scale on Google
#' earth Engine documentation](https://developers.google.com/earth-engine/guides/scale).
#' @param max_sample The computation of the maps is only performed on `max_sample` datapoints of
#' pressure to reduce computational time. The samples are randomly (uniformly) selected on the
#' timeserie.
#' @param margin The margin is used in the mask map to accept measurement error, small scale
#' topography and vertical movement of the bird (unit in meter, 1hPa~10m).
#' @param timeout Duration before the code is interrupted both for the request on
#' GeoPressureAPI and GEE (in seconds, see [`httr::timeout()`]).
#' @param workers number of parallel requests on GEE. Integer between 1 and 99.
#' @return List of the misfit map for each stationary period, containing:
#' - `stap` index of stationary period
#' - `start` POSIXct date time of the start of the stationary period
#' - `end` POSIXct date time of the end of the stationary period
#' - `nb_sample` number of pressure datapoint used.
#' - `mse` matrix of the map of the mean square error
#' - `mask` matrix of the map of the mask
#' - `extent` datetime of the start and end of the stationary period
#' @seealso [`geopressure_likelihood()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' \dontrun{
#' # See `tag_stap()` for generating tag
#' pressure_mismatch <- geopressure_mismatch(
#'   tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 4
#' )
#' }
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#'
#' str(pressure_mismatch)
#'
#' # Plot the matrix as a terra Rast
#' terra::plot(
#'   c(
#'     terra::rast(pressure_mismatch[[1]]$mse, extent = pressure_mismatch[[1]]$extent),
#'     terra::rast(pressure_mismatch[[1]]$mask, extent = pressure_mismatch[[1]]$extent)
#'   ),
#'   main = c("Mean Square Error", "Mask")
#' )
#' @export
geopressure_mismatch <- function(tag,
                                 extent,
                                 scale = 10,
                                 max_sample = 250,
                                 margin = 30,
                                 timeout = 60 * 5,
                                 workers = 90) {
  # Check input
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value", "label", "stap")))
  assertthat::assert_that(inherits(tag$pressure$date, "POSIXt"))
  assertthat::assert_that(is.numeric(tag$pressure$value))
  assertthat::assert_that(nrow(tag$pressure) >= 3)
  assertthat::assert_that(min(diff(as.numeric(tag$pressure$date))) / 60 / 60 <= 1)
  assertthat::assert_that(assertthat::has_name(tag, "stap"))
  assertthat::assert_that(is.data.frame(tag$stap))
  assertthat::assert_that(all(unique(tag$pressure$stap) %in% c(0, unique(tag$stap$stap))))
  assertthat::assert_that(is.numeric(extent))
  assertthat::assert_that(length(extent) == 4)
  assertthat::assert_that(extent[1] >= -180 & extent[1] <= 180)
  assertthat::assert_that(extent[2] >= -180 & extent[2] <= 180)
  assertthat::assert_that(extent[3] >= -90 & extent[3] <= 90)
  assertthat::assert_that(extent[4] >= -90 & extent[4] <= 90)
  assertthat::assert_that(extent[1] < extent[2])
  assertthat::assert_that(extent[3] < extent[4])
  assertthat::assert_that(is.numeric(scale))
  assertthat::assert_that(0 < scale)
  assertthat::assert_that(scale <= 10)
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(is.numeric(workers))
  assertthat::assert_that(workers > 0 & workers < 100)

  pres <- tag$pressure

  if (length(unique(diff(pres$date))) > 1) {
    warning("The pressure data is not on a regular interval. This might caused issue later.")
  }

  # remove flight and discard label
  pres <- pres[pres$label != "flight" & pres$label != "discard", ]

  # check values
  if (min(pres$value, na.rm = TRUE) < 250 || 1100 < max(pres$value, na.rm = TRUE)) {
    stop(paste0(
      "Pressure observation should be between 250 hPa (~10000m) and 1100 hPa (sea level at 1013",
      "hPa). Check unit returned by `tag_read()`"
    ))
  }

  # Create the stapelev of pressure to query: stationary period and elevation
  pres$stapelev <- paste0(
    pres$stap, "|",
    ifelse(startsWith(pres$label, "elev_"),
      gsub("^.*?elev_", "", pres$label),
      "0"
    )
  )

  # Split the data.frame per stapelev
  pres_stapelev <- split(pres, pres$stapelev)

  # Smooth and downscale each stapelev
  pres_stapelev_clean <- lapply(pres_stapelev, function(pgi) {
    # Define a regular temporal grid for smoothing and down scaling, rounded to the hours
    date_reg <- seq(
      round.POSIXt(min(pgi$date), units = "hours"),
      round.POSIXt(max(pgi$date), units = "hours"),
      by = min(diff(pgi$date))
    )

    # Remove observation be
    pgi <- pgi[pgi$date >= date_reg[1] & pgi$date <= date_reg[length(date_reg)], ]

    # Re-sample to the new temporal grid
    id <- sapply(pgi$date, function(d) {
      which.min(abs(d - date_reg))
    })
    assertthat::assert_that(length(id) == length(unique(id)))
    assertthat::assert_that(all(difftime(pgi$date, date_reg[id], units = "hours") <= 0.5))
    pgi$date <- date_reg[id]

    # Create the dataset on the new grid, allowing for NA if no data available
    pgi_reg <- merge(
      data.frame(date = date_reg),
      pgi,
      by = "date",
      all.x = TRUE
    )

    # smooth the data with a moving average of 1hr
    # find the size of the windows for 1 hour
    dtall <- diff(pgi_reg$date)
    units(dtall) <- "hours"
    dt <- as.numeric(stats::median(dtall))
    n <- round(1 / dt + 1)

    # check that there are enough datapoint for the smoothing
    if (nrow(pgi_reg) > n) {
      smoothna <- stats::filter(
        c(FALSE, !is.na(pgi_reg$value), FALSE),
        rep(1 / n, n)
      )
      pgi_reg$value[is.na(pgi_reg$value)] <- 0
      smooth <- stats::filter(c(0, pgi_reg$value, 0), rep(1 / n, n))

      tmp <- smooth / smoothna
      tmp <- tmp[seq(2, length(tmp) - 1)]

      pgi_reg$value <- tmp
    }

    # downscale to 1 hour
    # Pressure is an instantaneous parameters
    # (https://confluence.ecmwf.int/display/CKB/Parameters+valid+at+the+specified+time), so we take
    # the value at the exact hour
    pgi_reg <- pgi_reg[seq(1, nrow(pgi_reg), by = 1 / dt), ]

    # Remove time without measure
    pgi_reg <- pgi_reg[!is.na(pgi_reg$stap), ]

    return(pgi_reg)
  })

  nb_pres_stapelev_clean <- sapply(pres_stapelev_clean, function(x) {
    nrow(x)
  })
  if (any(nb_pres_stapelev_clean < 3)) {
    warning(
      "There is less than 3 datapoints used for the following stationary periods: ",
      paste0(names(nb_pres_stapelev_clean)[nb_pres_stapelev_clean < 3], collapse = ", "), "."
    )
  }

  pres_query <- do.call("rbind", pres_stapelev_clean)

  if (nrow(pres_query) == 0) {
    stop("No pressure to query. Check outlier and staID==0 (for flight).")
  }

  assertthat::assert_that(all(!is.na(pres_query$date)))
  assertthat::assert_that(all(!is.na(pres_query$value)))
  assertthat::assert_that(all(pres_query$stapelev != ""))

  # Format query
  body_df <- list(
    time = jsonlite::toJSON(as.numeric(as.POSIXct(pres_query$date))),
    label = jsonlite::toJSON(pres_query$stapelev),
    pressure = jsonlite::toJSON(round(pres_query$value * 100)), # convert from hPa to Pa
    W = extent[1],
    E = extent[2],
    S = extent[3],
    N = extent[4],
    scale = scale,
    max_sample = max_sample,
    margin = margin
  )

  # Request URLS
  message("Generate requests (on GeoPressureAPI):")
  res <- httr::POST("https://glp.mgravey.com/GeoPressure/v1/map/",
    body = body_df,
    encode = "form",
    httr::timeout(timeout)
  )

  if (httr::http_error(res)) {
    message(httr::content(res))
    temp_file <- tempfile("log_geopressure_mismatch_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    stop(paste0(
      "Error with your request on https://glp.mgravey.com/GeoPressure/v1/map/. ",
      "Please try again, and if the problem persists, file an issue on Github:",
      "https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_map&labels=crash
        with this log file located on your computer: ", temp_file
    ))
  }

  # Get urls
  urls <- httr::content(res)$data$urls
  urls[sapply(urls, is.null)] <- NA
  urls <- unlist(urls)
  labels <- unlist(httr::content(res)$data$labels)

  # Check that the urls exist
  if (all(is.na(urls))) {
    stop(
      "There was no urls returned for all stationary periods. It is probably due to request(s) ",
      "made for periods where no data are available. Note that ERA5 data is usually only ",
      "available on GEE ~3-5 months after."
    )
  } else if (any(is.na(urls))) {
    warning(
      "There was no urls returned for stationary periods: ",
      paste(labels[is.na(urls)], collapse = ", "), ". It is probably due to request(s) made ",
      "for periods where no data are available. Note that ERA5 data is usually only ",
      "available on GEE ~3-5 months after."
    )
    labels <- labels[!is.na(urls)]
    urls <- urls[!is.na(urls)]
  }
  message(
    "Requests generated successfully for ", length(urls), " stationary periods (",
    paste(labels, collapse = ", "), ")"
  )

  # Perform the call in parallel
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  future::plan(future::multisession, workers = workers)

  f <- c()
  message("Send requests:")
  progress_bar(0, max = length(urls))
  for (i_u in seq_len(length(urls))) {
    f[[i_u]] <- future::future(expr = {
      filename <- tempfile()
      res <- httr::GET(
        urls[i_u],
        httr::write_disk(filename),
        httr::timeout(timeout)
      )
      if (httr::http_error(res)) {
        httr::warn_for_status(res, task = "download GEE data")
        cat(readChar(filename, 1e5))
      }
      return(filename)
    }, seed = TRUE)
    progress_bar(i_u, max = length(urls))
  }

  # Get maps
  filename <- c()
  map <- c()
  message("Compute and download geotiff (GEE server):")
  progress_bar(0, max = length(urls))
  tryCatch(
    expr = {
      for (i_u in seq_len(length(urls))) {
        filename[i_u] <- future::value(f[[i_u]])
        map[[i_u]] <- terra::rast(filename[i_u])
        progress_bar(i_u, max = length(urls))
      }
    },
    error = function(cond) {
      message(paste0(
        "\nThere was an error during the downloading and reading of the file. ",
        "Here is the original error: "
      ))
      message(cond)
      return(list(
        urls = urls,
        filename = filename,
        map = map,
        future = f
      ))
    }
  )

  # Find the stap of each urls from labels (same order)
  labels_stap <- sub("\\|.*", "", labels)

  # Find the number of sample (datapoint) for each map
  nb_sample <- pmin(max_sample, unlist(lapply(labels, function(l) {
    sum(pres_query$stapelev == l)
  })))

  # Initialize the return list from tag$stap to make sure all stap are present
  pressure_mismatch <- lapply(split(tag$stap, tag$stap$stap), function(l) {
    # convert tag from df into a list
    l <- as.list(l)

    # Find all stapelevs that belong to this stap
    i_s <- which(labels_stap == l$stap)

    # compute the total number of sample for that stap.
    l$nb_sample <- sum(nb_sample[i_s])

    if (length(i_s) > 0) {
      # Compute the average of the mse and mask map weighted by the number of sample
      tmp <- Reduce(`+`, mapply(function(m, w) {
        w * m
      }, map[i_s], nb_sample[i_s])) / l$nb_sample

      # Extract the two map
      l$mse <- terra::as.matrix(tmp[[1]], wide = TRUE) / 100 / 100 # convert MSE from Pa to hPa
      l$mask <- terra::as.matrix(tmp[[2]], wide = TRUE)
      l$extent <- as.vector(terra::ext(tmp[[1]]))
    }
    return(l)
  })

  # return the pressure_mismatch in the same order than requested
  return(pressure_mismatch)
}








#' Compute likelihood map of pressure
#'
#' This function convert the mismatch maps (MSE and mask) into a likelihood map.
#'
#' Convert the map of the mean square error \eqn{MSE} and altitude mask \eqn{z_{mask}} computed
#' by [`geopressure_mismatch()`] into a likelihood map with,
#'
#' \eqn{L = \exp \left(-w \frac{MSE}{\sigma} \right) \left[z_{mask}>T \right],}
#'
#' where \eqn{\sigma} is the standard deviation of pressure and \eqn{T} is the mask threshold.
#'
#' Because the auto-correlation of the timeseries is not accounted for in this equation, we use a
#' log-linear pooling weight \eqn{w=\log(n)/n}, with \eqn{n} is the number of data point in the
#' timeserie.
#'
#' For more background and detail on the algorithm, read the [associated scientific publication
#' ]( https://doi.org/10.1111/2041-210X.14043) and [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html).
#'
#' @param pressure_mismatch List of mismatch built with [`geopressure_mismatch()`].
#' @param sd Standard deviation of the pressure error.
#' @param thr_mask Threshold of the percentage of data point outside the elevation range to be considered
#' not possible.
#' @param fun_w Weighting function of the log-linear pooling,taking the number of sample of the
#' stationary period used and return the weight of the aggregation. See the
#' [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html) for more details.
#' @return A list for each stationary period in order 1,2,...,n containing:
#' - `stap` stationary period. Needs to be in continuous
#' - `start` POSIXct date time of the start of the stationary period
#' - `end` POSIXct date time of the end of the stationary period and start of the flight
#' - `likelihood` matrix of the likelihood map
#' - `extent` vector length 4 of the extent of the map `c(xmin, xmax, ymin, ymax)`
#' @seealso [`geopressure_mismatch()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' # See `geopressure_mismatch()` for generating pressure_mismatch
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#'
#' pressure_likelihood <- geopressure_likelihood(
#'   pressure_mismatch,
#'   sd = 0.4,
#'   thr_mask = 0.9
#' )
#'
#' str(pressure_likelihood)
#'
#' terra::plot(
#'   terra::rast(pressure_likelihood[[1]]$likelihood, extent = pressure_likelihood[[1]]$extent),
#'   main = "Pressure likelihood",
#'   xlim = c(5, 20), ylim = c(42, 50)
#' )
#' @export
geopressure_likelihood <- function(pressure_mismatch,
                                   sd = 1,
                                   thr_mask = 0.9,
                                   fun_w = function(n) {
                                     log(n) / n
                                   }) {
  assertthat::assert_that(is.list(pressure_mismatch))
  assertthat::assert_that(is.list(pressure_mismatch[[1]]))
  assertthat::assert_that(assertthat::has_name(pressure_mismatch[[1]], c("stap", "start", "end")))
  assertthat::assert_that(is.numeric(sd))
  assertthat::assert_that(sd >= 0)
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.function(fun_w))

  pressure_likelihood <- lapply(pressure_mismatch, function(x) {
    l <- list(
      stap = x$stap,
      start = x$start,
      end = x$end
    )

    if ("mse" %in% names(x)) {
      # Check that all variables needed are presents
      assertthat::assert_that(assertthat::has_name(x, c("nb_sample", "mse", "mask")))

      # Log-linear pooling weight
      w <- fun_w(x$nb_sample)

      # get MSE layer
      mse <- x$mse
      # change 0 (water) in NA
      mse[mse == 0] <- NA

      # compute probability with equation
      likelihood <- (1 / (2 * pi * sd^2))^(x$nb_sample * w / 2) *
        exp(-w * x$nb_sample / 2 / (sd^2) * mse)

      # mask value of threshold
      likelihood <- likelihood * (x$mask >= thr_mask)

      l$likelihood <- likelihood
      l$extent <- x$extent
    }
    return(l)
  })

  return(pressure_likelihood)
}






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
