#' @family geopressure_map
#' @rdname geopressure_map
#' @export
geopressure_map_mismatch <- function(tag,
                                     max_sample = 250,
                                     margin = 30,
                                     keep_mask = TRUE,
                                     thr_mask = 0.9,
                                     timeout = 60 * 5,
                                     workers = "auto",
                                     compute_known = FALSE,
                                     debug = FALSE,
                                     quiet = FALSE) {
  # Check tag
  tag_assert(tag, "setmap")

  # Assert input
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.logical(keep_mask))
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(is.numeric(workers) | workers == "auto")
  assertthat::assert_that(is.logical(debug))
  assertthat::assert_that(is.logical(quiet))

  if (!quiet) {
    cli::cli_progress_step("Pre-process pressure data")
  }
  # Prepare data
  pres <- geopressure_map_preprocess(tag, compute_known = compute_known)

  body <- list(
    time = as.numeric(as.POSIXct(pres$date)),
    label = pres$stapelev,
    pressure = round(pres$value * 100), # convert from hPa to Pa
    W = tag$param$extent[1],
    E = tag$param$extent[2],
    S = tag$param$extent[3],
    N = tag$param$extent[4],
    scale = tag$param$scale,
    max_sample = max_sample,
    margin = margin,
    includeMask = keep_mask,
    maskThreshold = thr_mask
  )

  if (debug) {
    temp_file <- tempfile("log_geopressure_map_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  if (!quiet) {
    cli::cli_progress_step(
      msg = "Generate requests for {.val {length(unique(pres$stapelev))}} stapelev \\
    on {.url glp.mgravey.com/GeoPressure/v2/map/}: {.field {unique(pres$stapelev)}}",
      msg_done = "Generate requests for {.val {length(unique(pres$stapelev))}} stapelev \\
    on {.url glp.mgravey.com/GeoPressure/v2/map/}"
    )
  }

  # Request URLS
  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/map/") |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = function(resp) {
      if (debug) {
        print(httr2::resp_body_json(resp))
      }
      c(
        "x" = "Error with your request on https://glp.mgravey.com/GeoPressure/v2/map/.",
        ">" = httr2::resp_body_json(resp)$errorMessage,
        "i" = "Please try again with `debug=TRUE`"
      )
    })

  if (debug) {
    httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
  }

  # Perform the request and convert the response to json
  resp <- httr2::req_perform(req)
  resp_json <- httr2::resp_body_json(resp)

  if (debug) {
    print(resp_json)
  }

  # Get urls
  urls <- resp_json$data$urls
  urls[sapply(urls, is.null)] <- NA
  urls <- unlist(urls)
  labels <- unlist(resp_json$data$labels)

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
      that ERA5 data is usually only available on GEE ~3-5 months after.\f"
    ))
    labels <- labels[!is.na(urls)]
    urls <- urls[!is.na(urls)]
  }

  # Perform the call in parallel
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  if (workers == "auto") {
    workers <- min(parallel::detectCores(), length(urls))
  } else {
    assertthat::assert_that(workers > 0 & workers < 100)
  }
  future::plan(future::multisession, workers = workers)

  f <- vector("list", length(urls))

  if (!quiet) {
    # nolint start
    i_u <- 1
    cli::cli_progress_step(
      msg = "Compute (on GEE server) and download .geotiff for {.val {length(urls)}} stapelev \\
      (on {.val {workers}} workers): {.val {labels[i_u]}} | {i_u}/{length(urls)}",
      msg_done = "Compute (on GEE server) and download .geotiff for {.val {length(urls)}} stapelev"
    )
    # nolint end
  }
  for (i_u in seq_len(length(urls))) {
    if (!quiet) {
      cli::cli_progress_update(force = TRUE)
    }
    f[[i_u]] <- future::future(expr = {
      # Request URLS
      req <- httr2::request(urls[i_u]) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = function(resp) FALSE)

      if (debug) {
        req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE, info = TRUE)
      }

      # Perform the request and write the response to file
      file <- tempfile(fileext = ".geotiff")
      httr2::req_perform(req, path = file)

      # return the path to the file
      return(file)
    }, seed = TRUE)
  }

  # Get maps
  map <- vector("list", length(urls))
  if (!quiet) {
    # nolint start
    i_u <- 1
    cli::cli_progress_step(
      msg = "Read .geotiff: {.val {labels[i_u]}} | {i_u}/{length(urls)}",
      msg_done = "Read .geotiff"
    )
    # nolint end
  }
  for (i_u in seq_len(length(urls))) {
    if (!quiet) {
      cli::cli_progress_update(force = TRUE)
    }
    file <- future::value(f[[i_u]])
    map[[i_u]] <- terra::rast(file)
    names(map[[i_u]][[1]]) <- "map_pressure_mse"
    if (keep_mask) {
      names(map[[i_u]][[2]]) <- "map_pressure_mask"
    }
  }

  if (!quiet) {
    cli::cli_progress_step("Post-process maps")
  }

  # Find the stap of each urls from labels (same order)
  labels_stap <- as.numeric(sub("\\|.*", "", labels))

  # Find the number of sample (datapoint) for each map
  nb_sample <- pmin(max_sample, unlist(lapply(labels, function(l) {
    sum(pres$stapelev == l)
  })))

  # Initialize the return list from tag$stap to make sure all stap are present
  mse <- vector("list", nrow(tag$stap))
  if (keep_mask) {
    mask <- vector("list", nrow(tag$stap))
  }
  tag$stap$nb_sample <- 0

  for (i_stap in unique(labels_stap)) {
    # Find all stapelevs that belong to this stap
    i_label <- which(labels_stap == i_stap)

    # compute the total number of sample for that stap.
    tag$stap$nb_sample[i_stap] <- sum(nb_sample[i_label])

    # Only if the map was correctly computed and returned
    if (!is.null(map[i_label])) {
      # When elev is present, use an average of the mse and mask maps weighted by the number of
      # sample
      tmp <- Reduce(`+`, mapply(function(m, w) {
        w * m
      }, map[i_label], nb_sample[i_label])) / sum(nb_sample[i_label])

      # Find pixel below threshold (i.e., -1) in any of the elev and apply to the combined map of
      # mse
      tmp2 <- Reduce(`|`, lapply(map[i_label], function(x) x[[1]] == -1))
      tmp[[1]][tmp2] <- -1

      # Convert the maps to matrix
      mse[[i_stap]] <- terra::as.matrix(tmp[[1]], wide = TRUE)
      # convert MSE from Pa to hPa
      mse[[i_stap]][mse[[i_stap]] > 0] <- mse[[i_stap]][mse[[i_stap]] > 0] / 100 / 100
      if (keep_mask) {
        mask[[i_stap]] <- terra::as.matrix(tmp[[2]], wide = TRUE)
      }
    }
  }

  # Explicitly close multisession workers by switching plan
  future::plan(future::sequential)

  # Add attribute
  tag$map_pressure_mse <- map_create(
    data = mse,
    extent = tag$param$extent,
    scale = tag$param$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "pressure_mse"
  )

  if (keep_mask) {
    tag$map_pressure_mask <- map_create(
      data = mask,
      extent = tag$param$extent,
      scale = tag$param$scale,
      stap = tag$stap,
      id = tag$param$id,
      type = "pressure_mask"
    )
  }

  # keep parameters used
  tag$param$max_sample <- max_sample
  tag$param$margin <- margin
  tag$param$thr_mask <- thr_mask

  # return the pressure_mismatch in the same order than requested
  return(tag)
}
