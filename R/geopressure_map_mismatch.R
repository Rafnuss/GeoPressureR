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
#' @param debug Logical to display additional information to debug a request
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' “Global Positioning with Animal‐borne Pressure Sensors.” *Methods in Ecology and Evolution*.
#'  <https://doi.org/10.1111/2041-210X.14043>.}
#' @family geopressure_map
#' @export
geopressure_map_mismatch <- function(tag,
                                     max_sample = 250,
                                     margin = 30,
                                     timeout = 60 * 5,
                                     workers = "auto",
                                     compute_known = FALSE,
                                     debug = FALSE) {
  # Check tag
  tag_assert(tag, "setmap")

  # Format query
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(is.numeric(workers) | workers == "auto")

  cli::cli_progress_step("Prepare pressure data")
  # Prepare data
  pres <- geopressure_map_preprocess(tag, compute_known = compute_known)

  body_df <- list(
    time = jsonlite::toJSON(as.numeric(as.POSIXct(pres$date))),
    label = jsonlite::toJSON(pres$stapelev),
    pressure = jsonlite::toJSON(round(pres$value * 100)), # convert from hPa to Pa
    W = tag$param$extent[1],
    E = tag$param$extent[2],
    S = tag$param$extent[3],
    N = tag$param$extent[4],
    scale = tag$param$scale,
    max_sample = max_sample,
    margin = margin
  )

  if (debug) {
    temp_file <- tempfile("log_geopressure_map_mismatch_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  # Request URLS
  cli::cli_progress_step("Generate requests for {.val {length(unique(pres$stapelev))}} stapelev (on GeoPressureAPI)", spinner = TRUE)
  res <- httr::POST("https://glp.mgravey.com/GeoPressure/v1/map/",
    body = body_df,
    encode = "form",
    httr::config(
      timeout = timeout,
      verbose = ifelse(debug, httr::verbose(data_out = TRUE, data_in = FALSE, info = TRUE, ssl = FALSE), FALSE)
    )
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

  if (debug) {
    cli::cli_text("urls: ")
    print(urls)
    cli::cli_text("Labels: ")
    print(labels)
  }

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
  if (workers == "auto") {
    workers <- min(90, length(urls))
  } else {
    assertthat::assert_that(workers > 0 & workers < 100)
  }
  future::plan(future::multisession, workers = workers)

  f <- c()
  cli::cli_progress_step(
    "Sending requests for {.val {length(urls)}} stationary periods: {.field {labels}}",
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
        # httr::verbose(data_out = TRUE, data_in = FALSE, info = TRUE, ssl = FALSE)
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
      cli::cli_progress_step("Compute maps (on GEE server) and download .geotiff")
      cli::cli_progress_bar(total = length(urls), type = "tasks")
      for (i_u in seq_len(length(urls))) {
        cli::cli_progress_update(force = TRUE)
        file[i_u] <- future::value(f[[i_u]])
        map[[i_u]] <- terra::rast(file[i_u])
        names(map[[i_u]]) <- c("map_pressure_mse", "map_pressure_mask")
      }
    },
    error = function(cond) {
      cli::cli_inform(c("x" = "There was an error during the downloading and reading of the file. \\
      The original error is displayed below.\f"))
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
  mse <- vector("list", nrow(tag$stap))
  mask <- vector("list", nrow(tag$stap))
  tag$stap$nb_sample <- 0

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
    mse[[i_stap]] <- terra::as.matrix(tmp[[1]], wide = TRUE) / 100 / 100 # convert MSE from Pa to hPa
    mask[[i_stap]] <- terra::as.matrix(tmp[[2]], wide = TRUE)
  }

  # Add attribute
  tag$map_pressure_mask <- map_create(
    data = mask,
    extent = tag$param$extent,
    scale = tag$param$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "pressure_mask"
  )
  tag$map_pressure_mse <- map_create(
    data = mse,
    extent = tag$param$extent,
    scale = tag$param$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "pressure_mse"
  )

  # keep parameters used
  tag$param$max_sample <- max_sample
  tag$param$margin <- margin

  # return the pressure_mismatch in the same order than requested
  return(tag)
}
