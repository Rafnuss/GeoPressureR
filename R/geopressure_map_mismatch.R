#' Request and download mismatch maps from pressure
#'
#' This function returns, for each stationary period, two maps of mismatch between the pressure
#' measured by the geolocator and the ERA5 pressure database.
#'
#' These maps are generated on Google Earth Engine via the map entry point of the
#' [GeoPressure API](https://raphaelnussbaumer.com/GeoPressureAPI/#description). The computation
#' performed by this function consists of the following:
#' 1. Send a request to generate the Google Earth Engine (GEE) URL of the code producing the maps
#' for each stationary period separately.
#' 2. Download and read these geotiff maps as matrix. The computation on GEE only happens in this
#' second step when fetching the URL.
#'
#' The maps of each stationary period are returned in two layers:
#' 1. The Mean Square Error (MSE) between the data logger pressure timeseries and the reanalysis.
#' The mean error is removed because we assume no specific altitude of the geolocator, thus
#' allowing an altitudinal shift of the pressure timeseries.
#' 2. The mask of the proportion of pressure measurements corresponding to altitude values
#' found within the min and max ground elevation at each location. The altitude value
#' of the geolocator pressure timeseries is computed with the barometric formula accounting for the
#' temporal variation of pressure (surface-pressure) and temperature (2m-temperature) based on
#' ERA5 data. The min and max ground elevation of each pixel is computed from SRTM-90.
#'
#' For each stationary period, the pressure measurements are smoothed and downscaled to a 1-hour
#' resolution in order to match ERA-5 resolution.
#'
#' It is possible to indicate different elevation levels when the bird was spending time at
#' locations with different elevations within a general area (~10km), and thus within the same stationary
#' period. This can be done by using `tag$label="elev_*n*"`for all measurements of the same
#' elevation level *n*. See example in [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html).
#'
#' For more background and details on this algorithm, please refer to the [associated scientific publication
#' ]( https://doi.org/10.1111/2041-210X.14043). For more information on the exact computation, read
#' the [GeoPressure API documentation](https://raphaelnussbaumer.com/GeoPressureAPI/).
#'
#' @inheritParams geopressure_map
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' “Global Positioning with Animal‐borne Pressure Sensors.” *Methods in Ecology and Evolution*.
#'  <https://doi.org/10.1111/2041-210X.14043>.}
#' @family geopressure_map
#' @export
geopressure_map_mismatch <- function(tag,
                                     pressure,
                                     max_sample = 250,
                                     margin = 30,
                                     timeout = 60 * 5,
                                     workers = 90) {
  # Check tag
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "stap"))
  assertthat::assert_that(assertthat::has_name(tag, "scale"))
  assertthat::assert_that(assertthat::has_name(tag, "extent"))

  # Format query
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(is.numeric(workers))
  assertthat::assert_that(workers > 0 & workers < 100)

  cli::cli_progress_step("Prepare pressure data")
  # Prepare data
  pres <- geopressure_map_preprocess(pressure, tag$stap)

  body_df <- list(
    time = jsonlite::toJSON(as.numeric(as.POSIXct(pres$date))),
    label = jsonlite::toJSON(pres$stapelev),
    pressure = jsonlite::toJSON(round(pres$value * 100)), # convert from hPa to Pa
    W = tag$extent[1],
    E = tag$extent[2],
    S = tag$extent[3],
    N = tag$extent[4],
    scale = tag$scale,
    max_sample = max_sample,
    margin = margin
  )

  # Request URLS
  cli::cli_progress_step("Generate requests (on GeoPressureAPI)", spinner = TRUE)
  res <- httr::POST("https://glp.mgravey.com/GeoPressure/v1/map/",
    body = body_df,
    encode = "form",
    httr::timeout(timeout)
  )

  if (httr::http_error(res)) {
    message(httr::content(res))
    temp_file <- tempfile("log_geopressure_map_mismatch_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    cli::cli_abort(c(
      x = "Error with your request on {.url https://glp.mgravey.com/GeoPressure/v1/map/}.",
      i = "Please try again, and if the problem persists, file an issue on Github: \\
    {.url https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_map&labels=crash} \\
     with this log file located on your computer: {.file {temp_file}}"
    ))
  }

  # Get urls
  urls <- httr::content(res)$data$urls
  urls[sapply(urls, is.null)] <- NA
  urls <- unlist(urls)
  labels <- unlist(httr::content(res)$data$labels)

  # Check that the urls exist
  if (all(is.na(urls))) {
    cli::cli_abort(c(
      x = "There was no urls returned for all stationary periods.",
      i = "It is probably due to request(s)  made for periods where no data are available. \\
            Note that ERA5 data is usually only available on GEE ~3-5 months after."
    ))
  } else if (any(is.na(urls))) {
    cli::cli_warn(c(
      "!" = "There was no urls returned for stationary periods {.val {labels[is.na(urls)]}}.",
      i = "It is probably due to request(s) made for periods where no data are available. Note \\
      that ERA5 data is usually only available on GEE ~3-5 months after."
    ))
    labels <- labels[!is.na(urls)]
    urls <- urls[!is.na(urls)]
  }

  # Perform the call in parallel
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  future::plan(future::multisession, workers = workers)

  f <- c()
  cli::cli_progress_step(
    "Sending requests for {length(urls)} stationary periods: {labels}",
    spinner = TRUE
  )
  cli::cli_progress_bar(total = length(urls), type = "task")
  for (i_u in seq_len(length(urls))) {
    cli::cli_progress_update(force = TRUE)
    f[[i_u]] <- future::future(expr = {
      file <- tempfile()
      res <- httr::GET(
        urls[i_u],
        httr::write_disk(file),
        httr::timeout(timeout)
      )
      if (httr::http_error(res)) {
        httr::warn_for_status(res, task = "download GEE data")
        cat(readChar(file, 1e5))
      }
      return(file)
    }, seed = TRUE)
  }

  # Get maps
  file <- c()
  map <- c()
  tryCatch(
    expr = {
      cli::cli_progress_step("Compute maps (on GEE server) and download geotiff")
      cli::cli_progress_bar(total = length(urls), type = "tasks")
      for (i_u in seq_len(length(urls))) {
        cli::cli_progress_update(force = TRUE)
        file[i_u] <- future::value(f[[i_u]])
        map[[i_u]] <- terra::rast(file[i_u])
        names(map[[i_u]]) <- c("mse", "mask")
      }
    },
    error = function(cond) {
      cli::cli_alert_danger("There was an error during the downloading and reading of the file. \\
      The original error is displayed below.")
      message(cond)
      return(list(
        urls = urls,
        file = file,
        map = map,
        future = f
      ))
    }
  )

  cli::cli_progress_step("Process maps")

  # Find the stap of each urls from labels (same order)
  labels_stap <- as.numeric(sub("\\|.*", "", labels))

  # Find the number of sample (datapoint) for each map
  nb_sample <- pmin(max_sample, unlist(lapply(labels, function(l) {
    sum(pres$stapelev == l)
  })))

  # Initialize the return list from tag$stap to make sure all stap are present
  if (!("mse" %in% names(tag)) | length(tag$mse) == nrow(tag$stap)) {
    tag$mse <- vector("list", nrow(tag$stap))
  }
  if (!("mask" %in% names(tag)) | length(tag$mask) == nrow(tag$stap)) {
    tag$mask <- vector("list", nrow(tag$stap))
  }
  if (!("nb_sample" %in% tag & length(tag$mask) == nrow(tag$stap))) {
    tag$stap$nb_sample <- 0
  }

  for (i_stap in unique(labels_stap)) {
    # Find all stapelevs that belong to this stap
    i_label <- which(labels_stap == i_stap)

    # compute the total number of sample for that stap.
    tag$stap$nb_sample[i_stap] <- sum(nb_sample[i_label])

    # Compute the average of the mse and mask map weighted by the number of sample
    tmp <- Reduce(`+`, mapply(function(m, w) {
      w * m
    }, map[i_label], nb_sample[i_label])) / sum(nb_sample[i_label])

    # Extract the two map
    tag$mse[[i_stap]] <- terra::as.matrix(tmp[[1]], wide = TRUE) / 100 / 100 # convert MSE from Pa to hPa
    tag$mask[[i_stap]] <- terra::as.matrix(tmp[[2]], wide = TRUE)
  }

  # keep parameters used
  tag$param$max_sample <- max_sample
  tag$param$margin <- margin

  # return the pressure_mismatch in the same order than requested
  return(tag)
}