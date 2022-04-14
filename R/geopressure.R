#' Request and download mismatch maps of pressure
#'
#' This function return the mismatch map of atmospheric pressure measured by a geolocator
#' (`PAM_data`). It performs the following actions: (1) Send a query to produce the Google Earth
#' Engine (GEE) url of the code producing the maps for each stationary periods separately, (2) then
#' read these map (geotiff) in a raster and (3) compute the likelihood map from the mismatch. See
#' [the GeoPressure API documentation](https://raphaelnussbaumer.com/GeoPressureServer/#description)
#'
#' @param pressure pressure data from a PAM logger. This data.frame needs to contains `date` as
#' POSIXt, `obs` in hPa, `sta_id` grouping observation measured during the same stationary period
#' and `isoutliar` as logical to label observation which need to be ignorede. It is best practice to
#'  use `pam_read()` and `pam_sta()` to build this data.frame.
#' @param extent Geographical extent of the map to query as a list ordered by North, West, South,
#' East  (e.g. `c(50,-16,0,20)`).
#' @param scale Number of pixel per latitude, longitude. 10 for a resoltion of 0.1° (~10) and 4 for
#' a resolution of 0.25° (~30km). To avoid interpolating the ERA5 data, scale should be smaller than
#' 10. Read more about scale on Google earth Engine documentation.
#' @param max_sample The computation of the mismatch is only performed on `max_sample` datapoints of
#'  pressure to reduce computational time. The samples are randomly (uniformly) selected on the
#'  timeserie.
#' @param margin The margin is used in the threshold map to accept some measurement error. unit in
#' meter. (1hPa~10m)
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
#' pressure_maps <- readRDS(system.file("extdata", "18LX_pressure_maps.rda",
#'   package = "GeoPressureR"
#' ))
#' raster::metadata(pressure_maps[[1]])
#' raster::plot(pressure_maps[[1]],
#'   main = c("Mean Square Error", "Mask of pressure")
#' )
#' @export
geopressure_map <-
  function(pressure, extent, scale = 10, max_sample = 250, margin = 30) {
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
        "Pressure observation should be between 250 hPa (~10000m)  and 1100 hPa (sea ",
        "level at 1013 hPa)"
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
    for (i_s in seq(1, max(pressure$sta_id, na.rm = TRUE))) {
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
      time = jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date[!is.na(pres)]))),
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
    res <- httr::POST("http://glp.mgravey.com:24853/GeoPressure/v1/map/",
      body = body_df,
      encode = "form"
    )
    if (httr::http_error(res)) {
      print(httr::content(res))
      stop(paste0(
        "Error with request on http://glp.mgravey.com:24853/GeoPressure/v1/map/. ",
        "Please contact us with the error message if the error persists"
      ))
    }

    # Get URIS
    uris <- unlist(httr::content(res)$data$urls)
    # Note that the order of the uris will be different than requested to optimized the
    # parralelization
    labels <- unlist(httr::content(res)$data$labels)
    labels_order <- order(labels)
    message(
      "Requests generated successfully for ", length(labels), " stationary periods (",
      paste(labels, collapse = ", "), ")"
    )

    # Perform the call in parallel
    # GEE allows up to 12 requests at the same time, so we set the worker to 10
    future::plan(future::multisession, workers = 10)

    f <- c()
    message("Send requests:")
    progress_bar(0, max = length(uris))
    for (i_u in seq_len(length(uris))) {
      f[[i_u]] <- future::future(expr = {
        filename <- tempfile()
        options(timeout = 60 * 5)
        utils::download.file(uris[i_u], filename)
        return(filename)
      }, seed = TRUE)
      progress_bar(i_u, max = length(uris))
    }

    # Get the raster
    pressure_maps <- c()
    filename <- c()
    message("Download geotiff:")
    progress_bar(0, max = length(uris))
    tryCatch(
      expr = {
        for (i_u in seq_len(length(uris))) {
          filename[i_u] <- future::value(f[[i_u]])
          pressure_maps[[i_u]] <- raster::brick(filename[i_u])
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
        # return the pressure_maps in the same order than requested
        return(pressure_maps[labels_order])
      },
      error = function(cond) {
        message(paste0(
          "Error during the reading of the file. We return the uris of the gee request, ",
          "the filename to the file already downloaded and the pressure_maps already computed. ",
          "Here is the original error: "
        ))
        message(cond)
        return(list(
          uris = uris,
          filename = filename,
          pressure_maps = pressure_maps,
          future = f
        ))
      }
    )
  }








#' Compute probability raster
#'
#' This function convert the raster of noramlized MSE and altitude threshold \eqn{z_{thr}} computed
#' by `geopressure_map()` into a probability map with,
#' \eqn{p = \exp \left(-w \frac{MSE}{s} \right) \left[z_{thr}>thr \right],}
#' where \eqn{s} is the standard deviation of pressure and \eqn{thr} is the threashold. Because the
#' auto-correlation of the timeseries is not accounted for in this equation, we use a log-linear
#' pooling weight \eqn{w=\log(n) - 1}, with \eqn{n} is the number of data point in the timeserie.
#' This operation is describe in ...
#'
#' @param pressure_maps list of raster built with `geopressure_map()`
#' @param s standard deviation of the pressure error
#' @param thr threshold of the percentage of data point outside the elevation range to be considered
#' not possible
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
#' pressure_prob <- readRDS(system.file("extdata", "18LX_pressure_prob.rda",
#'   package = "GeoPressureR"
#' ))
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
    # pres_n <- as.numeric(difftime(mt$temporal_extent[2], mt$temporal_extent[1], units = "hours"))
    pres_n <- mt$nb_sample

    # Weight
    w <- log(pres_n) / pres_n

    # compute probability with equation
    raster_prob_list[[i_s]] <- (1 / (2 * pi * s^2))^(pres_n * w / 2) * exp(-w * pres_n / 2 / (s^2)
      * raster_prob_list[[i_s]])
    # mask value of threshold
    raster_prob_list[[i_s]] <- raster_prob_list[[i_s]] * (pressure_maps[[i_s]][[2]] > thr)

    raster::metadata(raster_prob_list[[i_s]]) <- raster::metadata(pressure_maps[[i_s]])
  }
  return(raster_prob_list)
}






#' Request and download surface pressure timeseries at location
#'
#' This function return the surface atmospheric pressure timeseries from ERA5 at a queried location.
#'
#' If you supply the pressure (and time) of the geolocator, it will additionally return the
#' elevation of the geolocator above sea level.
#'
#'  The timeserie of the response will be on the same as time if supply, otherwise, it will return
#'  on a hourly basis between `start_time` and `end_time`.
#'
#'  If the location query is over water, the location will be moved to the closest onshore location.
#'
#' @param lon longitude to query (-180° to 180°).
#' @param lat latitude to query (0° to 90°).
#' @param pressure pressure list from PAM logger dataset list
#' @param start_time if pressure not provided, then the start_time of the timeserie return is needed
#' @param end_time same as start_time
#' @param verbose logical display progress of the query
#' @return Timeserie of date, pressure, latitude, longitude and optionally altitude. Latitude and
#' longitude differs from the requested coordinates if over water.
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
#' pressure_timeserie <- readRDS(system.file("extdata", "18LX_pressure_timeserie.rda",
#'   package = "GeoPressureR"
#' ))
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
  function(lon, lat, pressure = NULL, end_time = NULL, start_time = NULL, verbose = T) {
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
      if (!("isoutliar" %in% names(pressure))) {
        pressure$isoutliar <- FALSE
      }
    } else {
      stopifnot(!is.na(end_time))
      stopifnot(!is.na(start_time))
      stopifnot(inherits(end_time, "POSIXt"))
      stopifnot(inherits(start_time, "POSIXt"))
      stopifnot(start_time <= end_time)
    }
    stopifnot(is.logical(verbose))

    # Format query
    body_df <- list(lon = lon, lat = lat)
    if (!is.null(pressure)) {
      body_df$time <- jsonlite::toJSON(as.numeric(as.POSIXct(pressure$date)))
      body_df$pressure <- jsonlite::toJSON(pressure$obs * 100)
    } else {
      body_df$startTime <- as.numeric(as.POSIXct(start_time))
      body_df$endTime <- as.numeric(as.POSIXct(end_time))
    }

    if (verbose) message("Generate request.")
    res <- httr::POST("http:///glp.mgravey.com:24853/GeoPressure/v1/timeseries/",
      body = body_df,
      encode = "form"
    )

    if (httr::http_error(res)) {
      message(httr::http_status(res)$message)
      message(httr::content(res))
      stop(paste0(
        "Error with request son http://glp.mgravey.com:24853/GeoPressure/v1/timeseries/. ",
        "Please contact us with the error message if the error persists"
      ))
    }

    # Retrieve response data
    res_data <- httr::content(res)$data

    # Check for change in position
    if (res_data$distInter > 0) {
      warning(
        "Requested position is on water We will proceeed the request with the closet ",
        "point to the shore (https://www.google.com/maps/dir/", lat, ",", lon, "/",
        res_data$lat, ",", res_data$lon, ") located ", round(res_data$distInter / 1000),
        " km away). Sending request.", immediate.=T
      )
    } else {
      if (verbose) message("Request generated successfully. Sending request.")
    }

    # Download the csv file
    message("")
    res2 <- httr::GET(res_data$url)

    # read csv
    out <- as.data.frame(httr::content(res2,
      type = "text/csv",
      encoding = "UTF-8",
      show_col_types = FALSE
    ))

    # check for errors
    if (nrow(out) == 0) {
      stop(paste0("Returned csv file is empty. Check that the time range is none-empty"))
    }

    # convert Pa to hPa
    out$pressure <- out$pressure / 100

    # convert time into date
    out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
    names(out)[names(out) == "time"] <- "date"

    # Add exact location
    out$lat <- res_data$lat
    out$lon <- res_data$lon

    # Compute the ERA5 pressure normalized to the pressure level (i.e. altitude) of the bird
    if (!is.null(pressure)) {
      # find when the bird was in flight or not to be considered
      id_0 <- pressure$sta_id == 0 | is.na(pressure$sta_id)
      # If no ground (ie. no flight) is present, pressure0 has no meaning
      if (!all(id_0)) {
        # We compute the mean pressure of the geolocator only when the bird is on the ground
        # (id_q==0) and when not marked as outliar
        id_norm <- !id_0 & !pressure$isoutliar

        pressure_obs_m <- mean(pressure$obs[id_norm])
        pressure_out_m <- mean(out$pressure[id_norm])

        out$pressure0 <- out$pressure - pressure_out_m + pressure_obs_m
      }

      # Add sta_id, lat and lon
      out$sta_id <- pressure$sta_id
    }
    return(out)
  }



#' Query the timeserie of pressure from a path and geolocator pressure
#'
#' This function runs in parallel `geopressure_ts()` based on a path and pressure timeserie. It
#' uses the `sta_id` to match the pressure timeserie to request for each position of the path.
#'
#' You can include previous and/or next flight period in each query. This is typically useful to
#' estimate flight altitude with greater precision.
#'
#' If a position of the path is over water, it will be moved to the closest point onshore as
#' explained in `geopressure_ts()`.
#'
#' @param path a data.frame of the position containing latitude (`lat`), longitude  (`lon`) and the
#' stationay period id (`sta_id`) as column.
#' @param pressure pressure list from PAM logger dataset list
#' @param include_flight extend request to also query the pressure and altitude during the previous
#' and/or next flight. Flights are defined by a `sta_id=0`. Accept Logical or vector of -1 (previous
#' flight), 0 (stationary) and/or 1 (next flight). (e.g. `include_flight=c(-1, 1)` will only search
#' for the flight before and after but not the stationary period). Note that next and previous
#' flights are defined by the +/1 of the `sta_id` value (and not the previous/next `sta_id` value).
#' @param verbose logical display progress of the queries
#' @return list of data.frame containing for each stationary period, the date, pressure, altitude
#' (same as `geopressure_ts()`) but also sta_id, lat, lon and pressure0, pressure normalized to
#' match geolocator pressure measurement.
#' @examples
#' # Create pam_data
#' pam_data <- pam_read(
#'   pathname = system.file("extdata", package = "GeoPressureR"),
#'   crop_start = "2017-06-20", crop_end = "2018-05-02"
#' )
#' pam_data <- trainset_read(pam_data,
#'   pathname = system.file("extdata", package = "GeoPressureR")
#' )
#' pam_data <- pam_sta(pam_data)
#' \dontrun{
#' # load probability map of pressure
#' pressure_prob <- readRDS(system.file("extdata", "18LX_pressure_prob.rda",
#'   package = "GeoPressureR"
#' ))
#' # Find the most likely position
#' path <- geopressure_map2path(pressure_prob)
#' # compute the pressure at those location for the period in question
#' pressure_timeserie <- geopressure_ts_path(path, pam_data$pressure)
#' }
#' pressure_timeserie <- readRDS(system.file("extdata", "18LX_pressure_timeserie.rda",
#'   package = "GeoPressureR"
#' ))
#' p <- ggplot2::ggplot() +
#'   ggplot2::geom_line(
#'     data = pam_data$pressure,
#'     ggplot2::aes(x = date, y = obs), colour = "grey"
#'   ) +
#'   ggplot2::geom_point(
#'     data = subset(pam_data$pressure, isoutliar),
#'     ggplot2::aes(x = date, y = obs), colour = "black"
#'   ) +
#'   ggplot2::geom_line(
#'     data = do.call("rbind", pressure_timeserie),
#'     ggplot2::aes(x = date, y = pressure0, col = as.factor(sta_id))
#'   ) +
#'   ggplot2::theme_bw() +
#'   ggplot2::scale_colour_manual(values = rep(RColorBrewer::brewer.pal(9, "Set1"), times = 4))
#'
#' py <- plotly::ggplotly(p, dynamicTicks = TRUE)
#' py <- plotly::layout(py,
#'   showlegend = FALSE,
#'   legend = list(orientation = "h", x = -0.5),
#'   yaxis = list(title = "Pressure [hPa]")
#' )
#' py
#' @export
geopressure_ts_path <- function(path, pressure, include_flight = F, verbose = T) {
  stopifnot(is.data.frame(pressure))
  stopifnot("date" %in% names(pressure))
  stopifnot(inherits(pressure$date, "POSIXt"))
  stopifnot("obs" %in% names(pressure))
  stopifnot(is.numeric(pressure$obs))
  stopifnot("sta_id" %in% names(pressure))
  if (!("isoutliar" %in% names(pressure))) {
    pressure$isoutliar <- FALSE
  }
  stopifnot(is.data.frame(path))
  stopifnot(c("lat", "lon", "sta_id") %in% names(path))
  if (nrow(path) == 0) warning("path is empty")
  if (!all(path$sta_id %in% pressure$sta_id)) {
    warning("Some path sta_id are not present in pressure")
  }
  if (is.logical(include_flight)) {
    include_flight <- (if (include_flight) c(-1, 0, 1) else 0)
  }
  stopifnot(is.numeric(include_flight))
  stopifnot(all(include_flight %in% c(-1, 0, 1)))
  stopifnot(is.logical(verbose))

  # Interpolate sta_id for flight period so that, a flight between sta_id 2 and 3 will have a
  # `sta_id_interp` between 2 and 3.
  id_0 <- pressure$sta_id == 0 | is.na(pressure$sta_id)
  sta_id_interp <- pressure$sta_id
  sta_id_interp[id_0] <- stats::approx(which(!id_0),
    pressure$sta_id[!id_0], which(id_0),
    rule = 2
  )$y

  # Define the parallel with 10 workers (ideal for Google Earth Engine allowance)
  future::plan(future::multisession, workers = 10)
  f <- c()

  if (verbose) {
    message("Sending requests for ", nrow(path), " stationary periods:")
    progress_bar(0, max = nrow(path))
  }

  for (i_s in seq_len(nrow(path))) {
    i_sta <- path$sta_id[i_s]
    if (verbose) progress_bar(i_s, max = nrow(path), text = paste0("| sta = ", i_sta))
    # Subset the pressure of the stationary period
    id_q <- rep(NA, length(sta_id_interp))
    if (any(0 == include_flight)) {
      id_q[path$sta_id[i_s] == sta_id_interp] <- 0
    }
    if (any(-1 == include_flight)) {
      id_q[i_sta - 1 < sta_id_interp & sta_id_interp < i_sta] <- -1
    }
    if (any(1 == include_flight)) {
      id_q[i_sta < sta_id_interp & sta_id_interp < i_sta + 1] <- 1
    }
    # Send the query
    f[[i_s]] <- future::future({
      geopressure_ts(path$lon[i_s], path$lat[i_s],
        pressure = subset(pressure, !is.na(id_q)),
        verbose = F
      )
    })
  }

  pressure_timeserie <- list()
  message("Downloading the data:")
  progress_bar(0, max = nrow(path))
  for (i_s in seq_len(length(f))) {
    progress_bar(i_s, max = nrow(path), text = paste0("| sta = ", i_sta))
    tryCatch(
      expr = {
        pressure_timeserie[[i_s]] <- future::value(f[[i_s]])
      },
      error = function(cond) {
        warning(paste0("Error for sta_id = ", path$sta_id[i_s], ".\n", cond))
      }
    )
  }
  return(pressure_timeserie)
}



#' Return the most likely path from a probability map
#'
#' Find the location of the highest value in the map and return a path data.frame containing the
#' latitude and longitude. `interp` can be used to interpolate unrealistic position from short
#' stationary period based on the position of the longer ones. The interpolation assumes that the
#' first and last stationary period can be safely estimated from the probability map.
#'
#' @param map list of raster containing probability map of each stationary period. The metadata of
#' `map` needs to include the start and end time of the stationary period .
#' @param interp (in days) The position of the stationary period shorter than `interp` will be
#' replace by a linear average from other position.
#' @param format one of `"lonlat"`, `"ind"`, `"arr.ind"`). return the path in lon-lat or indices
#' @return a data.frame of the position containing latitude (`lat`), longitude (`lon`) and the
#' stationary period id (`sta_id`) as column. Optionally, if indexes were requested, it will be
#' return. You will need to use `which.max(as.matrix(raster))` and not `which.max(raster)` to get
#' the correct location
#' @examples
#' pressure_prob <- readRDS(system.file("extdata", "18LX_pressure_prob.rda",
#'   package = "GeoPressureR"
#' ))
#' path_all <- geopressure_map2path(pressure_prob)
#' path_interp <- geopressure_map2path(pressure_prob, interp = 2)
#' sta_duration <- unlist(lapply(pressure_prob, function(x) {
#'   as.numeric(difftime(raster::metadata(x)$temporal_extent[2],
#'     raster::metadata(x)$temporal_extent[1],
#'     units = "days"
#'   ))
#' }))
#' m <- leaflet::leaflet(width = "100%")
#' m <- leaflet::addProviderTiles(m, leaflet::providers$Stamen.TerrainBackground)
#' m <- leaflet.extras::addFullscreenControl(m)
#' m <- leaflet::addPolylines(m,
#'   lng = path_all$lon, lat = path_all$lat, opacity = 1,
#'   color = "#a6cee3", weight = 3
#' )
#' m <- leaflet::addCircles(m,
#'   lng = path_all$lon, lat = path_all$lat, opacity = 1,
#'   color = "#1f78b4", weight = sta_duration^(0.3) * 10
#' )
#' m <- leaflet::addPolylines(m,
#'   lng = path_interp$lon, lat = path_interp$lat, opacity = 1,
#'   color = "#b2df8a", weight = 3
#' )
#' m <- leaflet::addCircles(m,
#'   lng = path_interp$lon, lat = path_interp$lat, opacity = 1,
#'   color = "#33a02c", weight = sta_duration^(0.3) * 10
#' )
#' m
#' @export
geopressure_map2path <- function(map, interp = 0, format = "lonlat") {
  stopifnot(is.list(map))
  stopifnot(inherits(map[[1]], "RasterLayer"))
  stopifnot(is.numeric(interp))
  stopifnot(interp >= 0)
  stopifnot(format %in% c("lonlat", "ind", "arr.ind"))

  # Set the initial path to the most likely from static prob
  # There is a difference between which.max(r) andwhich.max(as.matrix(r)) which appeared to be
  # necessary to get correctly the position. Not really practicle, maybe the way lat lon are
  # index in a raster.
  path <- do.call("rbind", lapply(map, function(r) {
    if (format == "lonlat") {
      pos <- raster::xyFromCell(r, raster::which.max(r))
      p <- data.frame(
        lon = pos[1],
        lat = pos[2]
      )
    } else {
      pos <- arrayInd(which.max(raster::as.matrix(r)), dim(r))
      p <- data.frame(
        lon = pos[2],
        lat = pos[1]
      )
    }
    p$sta_id <- raster::metadata(r)$sta_id
    return(p)
  }))

  # Interpolation for short stationary period is only performed if interp>0
  if (interp > 0) {
    if (!("temporal_extent" %in%
      names(raster::metadata(map[[1]])))) {
      stop("`temporal_extent` is required as metadata in map to perform an interpolation")
    }

    # remove short stationary period
    duration <- unlist(lapply(map, function(r) {
      mt <- raster::metadata(r)
      as.numeric(difftime(mt$temporal_extent[2],
        mt$temporal_extent[1],
        units = "days"
      ))
    }))
    id_interp <- duration < interp
    id_interp[1] <- F
    id_interp[length(id_interp)] <- F

    # Find the spacing between the position
    if (is.null(raster::metadata(map[[1]])$flight)) {
      # Or if flight duration are not available (e.g. `prob_pressure`), assumes homogenous spacing
      # between consecutive stationary period
      x <- path$sta_id
    } else {
      # If flight are avialabe, sum of the all flights between stationay period
      flight_duration <- unlist(lapply(map, function(r) {
        fl <- raster::metadata(r)$flight
        sum(as.numeric(difftime(fl$end,
          fl$start,
          units = "hours"
        )))
      }))
      # Cumultate the flight duration to get a proxy of the over distance covered
      x <- c(0, cumsum(utils::head(flight_duration, -1)))
    }
    # interpolate in between
    path$lon[id_interp] <- stats::approx(x[!id_interp], path$lon[!id_interp], x[id_interp])$y
    path$lat[id_interp] <- stats::approx(x[!id_interp], path$lat[!id_interp], x[id_interp])$y

    if (format != "lonlat") {
      path <- round(path)
    }

    # Account for water position
    #
    # sf::sf_use_s2(FALSE)
    # pts <- st_as_sf(path, coords = c("lon","lat"), crs = st_crs(4326))
    # # poly <- ne_countries(returnclass="sf")
    # poly <- ne_download(category = "physical", type="land", returnclass="sf")
    # a <- st_join(pts, poly, join = st_intersects)
  }

  if (format == "ind") {
    path$ind <- (path$lon - 1) * dim(map[[1]])[1] + path$lat
  }
  return(path)
}


# Progress bar function
progress_bar <- function(x, max = 100, text = "") {
  percent <- x / max * 100
  cat(sprintf(
    "\r[%-50s] %d / %d %s",
    paste(rep("=", percent / 2), collapse = ""),
    x, max, text
  ))
  if (x == max) {
    cat("\n")
  }
}
