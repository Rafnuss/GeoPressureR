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

  # Format query
  assertthat::assert_that(is.numeric(max_sample))
  assertthat::assert_that(0 < max_sample)
  assertthat::assert_that(is.numeric(margin))
  assertthat::assert_that(0 < margin)
  assertthat::assert_that(is.logical(keep_mask))
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.numeric(timeout))
  assertthat::assert_that(is.numeric(workers) | workers == "auto")

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
    temp_file <- tempfile("log_geopressure_map_mismatch_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  # Request URLS
  if (!quiet) {
    cli::cli_progress_step("Generate requests for {.val {length(unique(pres$stapelev))}} stapelev \\
                           (on GeoPressureAPI): {.field {unique(pres$stapelev)}}")
  }
  res <- httr::POST("https://glp.mgravey.com/GeoPressure/v2/map/",
    body = body,
    encode = "json",
    httr::timeout(timeout),
    httr::config(
      verbose = debug # httr::verbose(data_out = TRUE, data_in = FALSE, info = TRUE, ssl = FALSE)
    )
  )

  if (httr::http_error(res)) {
    temp_file <- tempfile("log_geopressure_map_mismatch_", fileext = ".json")
    write(jsonlite::toJSON(body), temp_file)
    if (httr::status_code(res) == 400 || httr::status_code(res) == 400) {
      # message(httr::content(res))
      github_link <- glue::glue(
        "https://github.com/Rafnuss/GeoPressureAPI/issues/new?title=crash\\%20geopressure_map%20\\
      task_id:{httr::content(res)$taskID}&labels=crash"
      )
      cli::cli_abort(c(
        "x" = "Error (Status code {.val {httr::status_code(res)}}) with your request on \\
        {.url https://glp.mgravey.com/GeoPressure/v2/map/}.",
        ">" = httr::content(res)$errorMessage,
        "i" = "Please try again, and if the problem persists, file an issue on Github: \\
        {.url {github_link}} with the request body file located on your computer: \\
        {.file {temp_file}}"
      ))
    } else {
      github_link <- glue::glue(
        "https://github.com/Rafnuss/GeoPressureAPI/issues/new?title=crash\\%20geopressure_map%20\\
      &labels=crash"
      )
      print(res)
      cli::cli_abort(c(
        "x" = "Error (Status code {.val {httr::status_code(res)}}) with your request on \\
        {.url https://glp.mgravey.com/GeoPressure/v2/map/}.",
        "i" = "Please try again, and if the problem persists, file an issue on Github: \\
        {.url {github_link}} with the request body file located on your computer: \\
        {.file {temp_file}}"
      ))
    }
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
      that ERA5 data is usually only available on GEE ~3-5 months after.\f"
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

  f <- vector("list", length(urls))

  if (!quiet) {
    # nolint start
    i_u <- 1
    cli::cli_progress_step(
      msg = "Send requests for {.val {length(urls)}} stapelev (in parallel): {.val {labels[i_u]}} | {i_u}/{length(urls)}",
      msg_done = "Send requests for {.val {length(urls)}} stapelev (in parallel)",
      spinner = TRUE
    )
    # nolint end
  }
  for (i_u in seq_len(length(urls))) {
    if (!quiet) {
      cli::cli_progress_update(force = TRUE)
    }
    f[[i_u]] <- future::future(expr = {
      file <- tempfile(fileext = ".geotiff")
      res <- httr::GET(
        urls[i_u],
        httr::write_disk(file),
        httr::timeout(timeout),
        httr::config(
          verbose = debug
          # httr::verbose(data_out = TRUE, data_in = FALSE, info = TRUE, ssl = FALSE)
        )
        # httr::verbose(data_out = TRUE, data_in = FALSE, info = TRUE, ssl = FALSE)
      )
      if (httr::http_error(res)) {
        return(res)
      } else {
        return(file)
      }
    }, seed = TRUE)
  }

  # Get maps
  map <- vector("list", length(urls))
  if (!quiet) {
    # nolint start
    i_u <- 1
    cli::cli_progress_step(
      msg = "Compute the map (on GEE server) and download .geotiff: {.val {labels[i_u]}} | {i_u}/{length(urls)}",
      msg_done = "Compute the map (on GEE server) and download .geotiff",
      spinner = TRUE
    )
    # nolint end
  }
  for (i_u in seq_len(length(urls))) {
    if (!quiet) {
      cli::cli_progress_update(force = TRUE)
    }
    file <- future::value(f[[i_u]])
    if (inherits(file, "response")) {
      cli::cli_warn(c(
        "x" = "There was an error for stap {.val {labels[i_u]}} during the downloading and \\
        reading of the response url {.url {urls[i_u]}}. It returned a status code \\
        {.val {httr::status_code(res)}}. The original error is displayed below."
      ))
      cat(httr::content(res))
    } else {
      if (debug) {
        print(file)
      }
      map[[i_u]] <- terra::rast(file)
      names(map[[i_u]][[1]]) <- "map_pressure_mse"
      if (keep_mask) {
        names(map[[i_u]][[2]]) <- "map_pressure_mask"
      }
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

      # Convert the maps to matrix
      mse[[i_stap]] <- terra::as.matrix(tmp[[1]], wide = TRUE)
      # convert MSE from Pa to hPa
      mse[[i_stap]][mse[[i_stap]] > 0] <- mse[[i_stap]][mse[[i_stap]] > 0] / 100 / 100
      if (keep_mask) {
        mask[[i_stap]] <- terra::as.matrix(tmp[[2]], wide = TRUE)
      }
    }
  }

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
