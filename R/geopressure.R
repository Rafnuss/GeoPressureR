#' Request and download mismatch maps of pressure
#'
#' This function return the mismatch map of atmospheric pressure measured by a
#' geolocator (`PAM_data`). It performs the following actions: (1) Send a query
#' to produce the Google Earth Engine (GEE) url of the code producing the maps
#' for each stationary periods separately, (2) then read these map (geotiff) in
#' a raster and (3) compute the likelihood map from the mismatch. See [the
#' GeoPressure API documentation
#' ](https://raphaelnussbaumer.com/GeoPressureServer/#description).
#'
#' @param pressure pressure data from a PAM logger. This data.frame needs to
#' contains `date` as POSIXt, `obs` in hPa, `sta_id` grouping observation
#' measured during the same stationary period and `isoutliar` as logical to
#' label observation which need to be ignorede. It is best practice to use
#' `pam_read()` and `pam_sta()` to build this data.frame.
#' @param extent Geographical extent of the map to query as a list ordered by
#' North, West, South, East  (e.g. `c(50,-16,0,20)`).
#' @param scale Number of pixel per latitude, longitude. 10 for a resoltion of
#' 0.1° (~10) and 4 for a resolution of 0.25° (~30km). To avoid interpolating
#' the ERA5 data, scale should be smaller than 10. Read more about scale on
#' Google earth Engine documentation.
#' @param max_sample The computation of the mismatch is only performed on
#' `max_sample` datapoints of pressure to reduce computational time. The samples
#' are randomly (uniformly) selected on the timeserie.
#' @param margin The margin is used in the threshold map to accept some
#' measurement error. unit in meter. (1hPa~10m)
#' @return List of raster map
#' @examples
#' \dontrun{
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' pam_data <- pam_sta(pam_data)
#' pressure_maps <- geopressure_map(
#'   pam_data$pressure,
#'   extent = c(-16, 20, 0, 50),
#'   scale = 10,
#'   max_sample = 250,
#'   margin = 30
#' )
#' }
#' data("pressure_maps", package = "GeoPressureR")
#' raster::metadata(pressure_maps[[1]])
#' raster::plot(pressure_maps[[1]],
#'   main = c("Mean Square Error", "Mask of pressure")
#' )
#' @export
geopressure_map <-
  function(pressure,
           extent,
           scale = 10,
           max_sample = 250,
           margin = 30) {
    # Check input
    stopifnot(is.data.frame(pressure))
    stopifnot("date" %in% names(pressure))
    stopifnot(inherits(pressure$date, "POSIXt"))
    stopifnot("obs" %in% names(pressure))
    stopifnot(is.numeric(pressure$obs))
    stopifnot("sta_id" %in% names(pressure))
    if (!("isoutliar" %in% names(pressure))) {
      pressure$isoutliar <- FALSE
    }
    if (min(pressure$obs[!pressure$isoutliar]) < 250 | 1100 <
      max(pressure$obs[!pressure$isoutliar])) {
      stop(paste0(
        "Pressure observation should be between 250 hPa (~10000m) ",
        "and 1100 hPa (sea level at 1013 hPa)"
      ))
    }
    stopifnot(is.logical(pressure$isoutliar))
    stopifnot(is.numeric(extent))
    stopifnot(length(extent) == 4)
    stopifnot(extent[1] >= -90 & extent[1] <= 90)
    stopifnot(extent[2] >= -180 & extent[2] <= 180)
    stopifnot(extent[3] >= -90 & extent[3] <= 90)
    stopifnot(extent[4] >= -180 & extent[4] <= 180)
    stopifnot(extent[3] < extent[1])
    stopifnot(extent[2] < extent[4])
    stopifnot(is.numeric(scale))
    stopifnot(0 < scale)
    stopifnot(scale <= 10)
    stopifnot(is.numeric(max_sample))
    stopifnot(0 < max_sample)
    stopifnot(is.numeric(margin))
    stopifnot(0 < margin)

    # convert from hPa to Pa
    pres <- pressure$obs * 100

    # remove outliar as labeled in TRAINSET
    pres[pressure$isoutliar] <- NA

    # remove flight period
    pres[pressure$sta_id == 0] <- NA

    # remove stationary period with NA
    pres[is.na(pressure$sta_id)] <- NA

    # smooth the data with a moving average of 1hr
    # find the size of the windows for 1 hour
    dt <- as.numeric(difftime(pressure$date[2], pressure$date[1],
      units = "hours"
    ))
    n <- 1 / dt + 1
    # make the convolution for each stationary period separately
    for (i_s in seq(1, max(pressure$sta_id, na.rm = T))) {
      pres_i_s <- pres
      pres_i_s[pressure$sta_id != i_s] <- NA
      pres_i_s_smoothna <- stats::filter(
        c(F, !is.na(pres_i_s), F),
        rep(1 / n, n)
      )
      pres_i_s[is.na(pres_i_s)] <- 0
      pres_i_s_smooth <- stats::filter(c(0, pres_i_s, 0), rep(1 / n, n))

      tmp <- pres_i_s_smooth / pres_i_s_smoothna
      tmp <- tmp[seq(2, length(tmp) - 1)]

      pres[!is.na(pressure$sta_id) & pressure$sta_id == i_s] <-
        tmp[!is.na(pressure$sta_id) & pressure$sta_id == i_s]
    }

    # downscale to 1hour
    pres[format(pressure$date, "%M") != "00"] <- NA

    if (sum(!is.na(pres)) == 0) {
      stop("No pressure to query. Check outliar and staID==0 (for flight).")
    }

    # Format query
    body_df <- list(
      time = jsonlite::toJSON(
        as.numeric(as.POSIXct(pressure$date[!is.na(pres)]))
      ),
      label = jsonlite::toJSON(pressure$sta_id[!is.na(pres)]),
      pressure = jsonlite::toJSON(pres[!is.na(pres)]),
      N = extent[1],
      W = extent[2],
      S = extent[3],
      E = extent[4],
      scale = scale,
      max_sample = max_sample,
      margin = margin
    )

    # Request URLS
    message("Generate requests:")
    res <-
      httr::POST("http://glp.mgravey.com:24853/GeoPressure/v1/map/",
        body = body_df,
        encode = "form"
      )
    if (httr::http_error(res)){
      print(httr::content(res))
      stop("Error with request son http://glp.mgravey.com:24853/GeoPressure/v1/map/. Please contact us with the error message if the error persists")
    }

    # Get URIS
    uris <- unlist(httr::content(res)$data$urls)
    labels <- unlist(httr::content(res)$data$labels)
    message(
      "Requests generated successfully for ",
      length(labels),
      " stationary periods (",
      sprintf("%d, ", sort(labels)),
      ")"
    )

    # Perform the call in parallel
    # GEE allows up to 12 requests at the same time, so we set the worker to 10
    future::plan(future::multisession, workers = 10)
    f <- c()
    message("Send requests:")
    progress_bar(0, max = length(uris))
    for (i_u in seq_len(length(uris))) {
      f[[i_u]] <- future::future({
          filename <- tempfile()
          utils::download.file(uris[i_u], filename)
          return(raster::brick(filename))
        },
        seed = TRUE
      )
      progress_bar(i_u, max = length(uris))
    }

    # Get the raster
    pressure_maps <- c()
    message("Download geotiff:")
    progress_bar(0, max = length(uris))
    for (i_u in seq_len(length(uris))) {
      pressure_maps[[i_u]] <- future::value(f[[i_u]])
      progress_bar(i_u, max = length(uris))

      # Add datum
      raster::crs(pressure_maps[[i_u]]) <-
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

      # convert MSE from Pa to hPa
      pressure_maps[[i_u]][[1]] <- pressure_maps[[i_u]][[1]] / 100 / 100

      # Writing some metadata
      raster::metadata(pressure_maps[[i_u]]) <- list(
        sta_id = labels[i_u],
        nb_sample = sum(pressure$sta_id[!is.na(pres)] == labels[i_u]),
        max_sample = max_sample,
        temporal_extent = c(
          min(pressure$date[!is.na(pres) & pressure$sta_id == labels[i_u]]),
          max(pressure$date[!is.na(pres) & pressure$sta_id == labels[i_u]])
        ),
        margin = margin
      )
    }

    return(pressure_maps)
  }








#' Compute probability raster
#'
#' This function convert the raster of noramlized MSE and altitude threshold
#' \eqn{z_{thr}} computed by `geopressure_map()` into a probability map with,
#' \eqn{p = \exp \left(-w \frac{MSE}{s} \right) \left[z_{thr}>thr \right],}
#' where \eqn{s} is the standard deviation of pressure and \eqn{thr} is the
#' threashold. Because the auto-correlation of the timeseries is not accounted
#' for in this equation, we use a log-linear pooling weight \eqn{w=\log(n) - 1},
#' with \eqn{n} is the number of data point in the timeserie. This operation is
#' describe in
#'
#' @param pressure_maps list of raster built with `geopressure_map()`
#' @param s standard deviation of the pressure error
#' @param thr threshold of the percentage of data point outside the elevation
#' range to be considered not possible
#' @return List of the probability raster map
#' @examples
#' \dontrun{
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' pam_data <- pam_sta(pam_data)
#' pressure_maps <- geopressure_map(
#'   pam_data$pressure,
#'   extent = c(50, -16, 0, 20),
#'   scale = 10
#' )
#' pressure_prob <- geopressure_prob_map(
#'   pressure_maps,
#'   s = 0.4,
#'   thr = 0.9
#' )
#' }
#' data("pressure_prob", package = "GeoPressureR")
#' raster::metadata(pressure_prob[[1]])
#' raster::plot(pressure_prob[[1]],
#'   main = "Probability",
#'   xlim = c(5, 20), ylim = c(42, 50)
#' )
#' @export
geopressure_prob_map <- function(pressure_maps, s = 1, thr = 0.9) {
  raster_prob_list <- c()
  for (i_s in seq_len(length(pressure_maps))) {
    # get metadata
    mt <- raster::metadata(pressure_maps[[i_s]])

    # get MSE layer
    raster_prob_list[[i_s]] <- pressure_maps[[i_s]][[1]]
    # change 0 (water) in NA
    raster_prob_list[[i_s]][raster_prob_list[[i_s]] == 0] <- NA

    # compute Log-linear pooling weight
    # Number of datapoint could also be measured with
    # pres_n <- as.numeric(difftime(mt$temporal_extent[2],
    # mt$temporal_extent[1], units = "hours"))
    pres_n <- mt$nb_sample

    # Weight
    w <- log(pres_n) / pres_n

    # compute probability with equation
    raster_prob_list[[i_s]] <-
      (1 / (2 * pi * s^2))^(pres_n * w / 2) * exp(-w * pres_n / 2 / (s^2)
        * raster_prob_list[[i_s]])
    # mask value of threshold
    raster_prob_list[[i_s]] <-
      raster_prob_list[[i_s]] * (pressure_maps[[i_s]][[2]] > thr)

    raster::metadata(raster_prob_list[[i_s]]) <-
      raster::metadata(pressure_maps[[i_s]])
  }
  return(raster_prob_list)
}






#' Request and download surface pressure timeseries at location
#'
#' This function return the surfrace atmospheric pressure timeseries from ERA5
#' at a particualy location specify by lat and lon. I uses SRTM-30 to translate
#' the pressure for the exact elevation of the ground level, accounting for
#' both temporal varation of pressure and temperature.
#'
#' If you supply the pressure (and time) of the geolocator, it will additionally
#' return the elevation of the geolocator above sea level considering that the
#' bird was located at the location specify
#'
#'  The timeserie of the response will be on the same as time if supply,
#'  otherwise, it will return on a hourly basis between `start_time` and
#' `end_time`.
#'
#' @param lon longitude to query (-180° to 180°).
#' @param lat latitude to query (0° to 90°).
#' @param pressure pressure list from PAM logger dataset list
#' @param start_time if pressure not provided, then the start_time of the
#' timeserie return is needed
#' @param end_time same as start_time
#' @return Timeserie of date, pressure and optionally altitude
#' @examples
#' \dontrun{
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' pam_data <- pam_sta(pam_data)
#' pressure_timeserie[[1]] <- geopressure_ts(
#'   lon = 16.85,
#'   lat = 48.75,
#'   pressure = subset(pam_data$pressure, sta_id == 1)
#' )
#' }
#' data("pressure_timeserie", package = "GeoPressureR")
#' par(mfrow = c(2, 1), mar = c(2, 5, 1, 1))
#' plot(pressure_timeserie[[1]]$date,
#'   pressure_timeserie[[1]]$pressure,
#'   ylab = "Pressure [hPa]", xlab = ""
#' )
#' plot(pressure_timeserie[[1]]$date,
#'   pressure_timeserie[[1]]$altitude,
#'   ylab = "Altitude [m asl]", xlab = ""
#' )
#' @export
geopressure_ts <-
  function(lon,
           lat,
           pressure = NULL,
           end_time = NULL,
           start_time = NULL) {
    # Check input
    stopifnot(is.numeric(lon))
    stopifnot(is.numeric(lat))
    stopifnot(lon >= -180 & lon <= 180)
    stopifnot(lat >= -90 & lat <= 90)
    if (!is.null(pressure)) {
      stopifnot(is.data.frame(pressure))
      stopifnot("date" %in% names(pressure))
      stopifnot(inherits(pressure$date, "POSIXt"))
      stopifnot("obs" %in% names(pressure))
      stopifnot(is.numeric(pressure$obs))
      end_time <- NULL
      start_time <- NULL
    } else {
      stopifnot(!is.na(end_time))
      stopifnot(!is.na(start_time))
      stopifnot(inherits(end_time, "POSIXt"))
      stopifnot(inherits(start_time, "POSIXt"))
      stopifnot(start_time <= end_time)
    }

    # Format query
    body_df <- list(
      lon = lon,
      lat = lat
    )
    if (!is.null(pressure)) {
      body_df$time <-
        jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date)))
      body_df$pressure <- jsonlite::toJSON(pressure$obs * 100)
    } else {
      body_df$startTime <- as.numeric(as.POSIXct(start_time))
      body_df$endTime <- as.numeric(as.POSIXct(end_time))
    }

    # Request URLS
    message("Sending request...")
    res <-
      httr::POST("http://glp.mgravey.com:24853/GeoPressure/v1/timeseries/",
        body = body_df,
        encode = "form"
      )

    if (httr::http_error(res)){
      print(httr::content(res))
      stop("Error with request son http://glp.mgravey.com:24853/GeoPressure/v1/timeseries/. Please contact us with the error message if the error persists")
    } else {
      message("Request generated successfully.")
    }

    # Download the csv file
    message("Downloading csv data...")
    res2 <- httr::GET(httr::content(res)$data$url)

    # read csv
    out <-
      as.data.frame(httr::content(
        res2,
        type = "text/csv",
        encoding = "UTF-8",
        show_col_types = F
      ))

    # check for errors
    if (nrow(out) == 0) {
      stop(paste0(
        "Returned csv file is empty. Check that the time range is ",
        "none-empty and that the location is not on water: ",
        "maps.google.com/maps?q=", lat, ",", lon
      ))
    }

    # convert Pa to hPa
    out$pressure <- out$pressure / 100

    # convert time into date
    out$time <- as.POSIXct(out$time, origin = "1970-01-01")
    names(out)[names(out) == "time"] <- "date"

    return(out)
  }






# Progress bar function
progress_bar <- function(x, max = 100) {
  percent <- x / max * 100
  cat(sprintf(
    "\r[%-50s] %d / %d",
    paste(rep("=", percent / 2), collapse = ""),
    x, max
  ))
  if (x == max) {
    cat("\n")
  }
}
