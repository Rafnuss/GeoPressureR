#' Request and download mismatch maps of pressure
#'
#' This function return a maps of mismatch of atmospheric pressure measured by a geolocator. It
#' performs the following actions:
#' 1. Send a query to produce the Google Earth Engine (GEE) url of the code producing the maps for
#' each stationary periods separately
#' 2. Read these map (geotiff) in a raster
#' 3 compute the likelihood map from the mismatch. See
#' [the GeoPressure API documentation](https://raphaelnussbaumer.com/GeoPressureAPI/#description)
#'
#' @param pressure Pressure data.frame from a PAM logger. This data.frame needs to contains `date`
#' as POSIXt, `obs` in hPa, `sta_id` grouping observation measured during the same stationary period
#' and `isoutliar` as logical to label observation which need to be ignored. It is best practice to
#' use `pam_read()` and `pam_sta()` to build this data.frame.
#' @param extent Geographical extent of the map to query as a list ordered by North, West, South,
#'   East  (e.g. `c(50,-16,0,20)`).
#' @param scale Number of pixel per 1° latitude-longitude. For instance, `scale = 10` for a
#'   resolution of 0.1° (~10km) and 4 for a resolution of 0.25° (~30km). To avoid interpolating the
#'   ERA5 data, scale should be smaller than 10. Read more about [scale on Google earth Engine
#'   documentation](https://developers.google.com/earth-engine/guides/scale).
#' @param max_sample The computation of the mismatch is only performed on `max_sample` datapoints of
#'   pressure to reduce computational time. The samples are randomly (uniformly) selected on the
#'   timeserie.
#' @param margin The margin is used in the threshold map to accept some measurement error. unit in
#'   meter. (1hPa~10m)
#' @return List of raster map
#' @seealso [`geopressure_prob_map()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' # See `pam_sta()` for generating pam
#' \dontrun{
#' pressure_maps <- geopressure_map(
#'   pam$pressure,
#'   extent = c(50, -16, 0, 23),
#'   scale = 4,
#'   max_sample = 250,
#'   margin = 30
#' )
#' pressure_maps_1 <- pressure_maps[[1]]
#' }
#' pressure_maps_1 <- readRDS(system.file("extdata/1_pressure/", "18LX_pressure_maps_1.rda",
#'   package = "GeoPressureR"
#' ))
#' raster::metadata(pressure_maps_1)
#' raster::plot(pressure_maps_1,
#'   main = c("Mean Square Error", "Mask of pressure")
#' )
#' @export
geopressure_map <- function(pressure,
                            extent,
                            scale = 10,
                            max_sample = 250,
                            margin = 30) {
  # Check input
  stopifnot(is.data.frame(pressure))
  stopifnot(nrow(pressure)>1)
  stopifnot("date" %in% names(pressure))
  stopifnot(inherits(pressure$date, "POSIXt"))
  stopifnot("obs" %in% names(pressure))
  stopifnot(is.numeric(pressure$obs))
  stopifnot("sta_id" %in% names(pressure))
  if (!("isoutliar" %in% names(pressure))) {
    pressure$isoutliar <- FALSE
  }
  if (min(pressure$obs[!pressure$isoutliar]) < 250 || 1100 <
    max(pressure$obs[!pressure$isoutliar])) {
    stop(paste0(
      "Pressure observation should be between 250 hPa (~10000m)  and 1100 hPa (sea level at 1013",
      "hPa). Check unit return by `pam_read()`"
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
  dtall <- diff(pressure$date)
  units(dtall) <- "hours"
  if (length(unique(dtall)) > 1) {
    warning("Date of pressure are not on a regular interval. This might cause issue later.")
  }
  dt <- as.numeric(stats::median(dtall))
  n <- round(1 / dt + 1)
  # make the convolution for each stationary period separately
  for (i_s in seq(1, max(pressure$sta_id, na.rm = TRUE))) {
    if (length(pres) > n){
      pres_i_s <- pres
      pres_i_s[pressure$sta_id != i_s] <- NA
      pres_i_s_smoothna <- stats::filter(
        c(FALSE, !is.na(pres_i_s), FALSE),
        rep(1 / n, n)
      )
      pres_i_s[is.na(pres_i_s)] <- 0
      pres_i_s_smooth <- stats::filter(c(0, pres_i_s, 0), rep(1 / n, n))

      tmp <- pres_i_s_smooth / pres_i_s_smoothna
      tmp <- tmp[seq(2, length(tmp) - 1)]

      pres[!is.na(pressure$sta_id) & pressure$sta_id == i_s] <-
        tmp[!is.na(pressure$sta_id) & pressure$sta_id == i_s]
    }
  }

  # downscale to 1 hour
  # Find the start time closer to the hour
  idt_s <- which.min(abs(round.POSIXt(pressure$date[seq_len(1 / dt)], units = "hours") -
    pressure$date[seq_len(1 / dt)]))
  # Define the index of time to keep
  idt <- seq(idt_s, length(pressure$date), by = 1 / dt)
  # Remove the other ones
  pres[!(seq(1, length(pres)) %in% idt)] <- NA

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
    message(httr::content(res))
    temp_file <- tempfile("log_geopressure_map_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    stop(paste0(
      "Error with your request on http://glp.mgravey.com:24853/GeoPressure/v1/map/. ",
      "Please try again, and if the problem persists, file an issue on Github:",
      "https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_ts&labels=crash
        with this log file located on your computer: ", temp_file
    ))
  }

  # Get urls
  urls <- httr::content(res)$data$urls
  urls[sapply(urls, is.null)] <- NA
  urls <- unlist(urls)
  # Note that the order of the urls will be different than requested to optimized the
  # parralelization
  labels <- unlist(httr::content(res)$data$labels)
  labels_order <- order(labels)
  # Check that the uri exist
  if (any(is.na(urls))){
    warning("Not urls returned for stationary periods: ",
            paste(labels[is.na(urls)], collapse = ", "), ". It is probably due to request(s) made ",
            "for periods where no data are available. Note that ERA5 data is usually only ",
            "available on GEE ~3-5 months after.")
    labels <- labels[is.na(urls)]
    labels_order <- labels_order[is.na(urls)]
    urls <- urls[is.na(urls)]
  }
  message(
    "Requests generated successfully for ", sum(!is.na(urls)), " stationary periods (",
    paste(labels[!is.na(urls)], collapse = ", "), ")"
  )

  # Perform the call in parallel
  # GEE allows up to 12 requests at the same time, so we set the worker to 10
  future::plan(future::multisession, workers = 10)

  f <- c()
  message("Send requests:")
  progress_bar(0, max = length(urls))
  for (i_u in seq_len(length(urls))) {
    f[[i_u]] <- future::future(expr = {
      filename <- tempfile()
      options(timeout = 60 * 5)
      httr::GET(urls[i_u], httr::write_disk(filename))
      return(filename)
    }, seed = TRUE)
    progress_bar(i_u, max = length(urls))
  }

  # Get the raster
  pressure_maps <- c()
  filename <- c()
  message("Download geotiff:")
  progress_bar(0, max = length(urls))
  tryCatch(
    expr = {
      for (i_u in seq_len(length(urls))) {
        filename[i_u] <- future::value(f[[i_u]])
        pressure_maps[[i_u]] <- raster::brick(filename[i_u])
        progress_bar(i_u, max = length(urls))

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
        "Error during the reading of the file. We return the urls of the gee request, ",
        "the filename to the file already downloaded and the pressure_maps already computed. ",
        "Here is the original error: "
      ))
      message(cond)
      return(list(
        urls = urls,
        filename = filename,
        pressure_maps = pressure_maps,
        future = f
      ))
    }
  )
}








#' Compute probability raster
#'
#' This function convert the raster of normalized MSE and altitude threshold \eqn{z_{thr}} computed
#' by [`geopressure_map()`] into a probability map with,
#' \eqn{p = \exp \left(-w \frac{MSE}{s} \right) \left[z_{thr}>thr \right],}
#' where \eqn{s} is the standard deviation of pressure and \eqn{thr} is the threshold. Because the
#' auto-correlation of the timeseries is not accounted for in this equation, we use a log-linear
#' pooling weight \eqn{w=\log(n) - 1}, with \eqn{n} is the number of data point in the timeserie.
#' This operation is describe in \doi{10.21203/rs.3.rs-1381915/v1}.
#'
#' @param pressure_maps List of raster built with [`geopressure_map()`].
#' @param s Standard deviation of the pressure error.
#' @param thr Threshold of the percentage of data point outside the elevation range to be considered
#' not possible.
#' @param fun_w function taking the number of sample of the timeseries used to compute the
#' probability map and return the log-linear pooling weight (see the
#' [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html))
#' @return List of the probability raster map
#' @seealso [`geopressure_map()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @examples
#' # See `geopressure_map()` for generating pressure_maps
#' \dontrun{
#' pressure_prob <- geopressure_prob_map(
#'   pressure_maps,
#'   s = 0.4,
#'   thr = 0.9
#' )
#' pressure_prob_1 <- pressure_prob[[1]]
#' }
#' pressure_prob_1 <- readRDS(system.file("extdata/1_pressure/", "18LX_pressure_prob_1.rda",
#'   package = "GeoPressureR"
#' ))
#' raster::metadata(pressure_prob_1)
#' raster::plot(pressure_prob_1,
#'   main = "Probability",
#'   xlim = c(5, 20), ylim = c(42, 50)
#' )
#' @export
geopressure_prob_map <- function(pressure_maps,
                                 s = 1,
                                 thr = 0.9,
                                 fun_w = function(n) {
                                   log(n) / n
                                 }) {
  stopifnot(is.numeric(s))
  stopifnot(s >= 0)
  stopifnot(is.numeric(thr))
  stopifnot(thr >= 0 & thr <= 1)
  stopifnot(is.function(fun_w))

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
    w <- fun_w(pres_n)

    # compute probability with equation
    raster_prob_list[[i_s]] <- (1 / (2 * pi * s^2))^(pres_n * w / 2) * exp(-w * pres_n / 2 / (s^2)
      * raster_prob_list[[i_s]])
    # mask value of threshold
    raster_prob_list[[i_s]] <- raster_prob_list[[i_s]] * (pressure_maps[[i_s]][[2]] >= thr)

    raster::metadata(raster_prob_list[[i_s]]) <- raster::metadata(pressure_maps[[i_s]])
  }
  return(raster_prob_list)
}






#' Request and download surface pressure timeseries at location
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
#' @param pressure Pressure list from PAM logger dataset list (optional).
#' @param start_time If `pressure` is not provided, then `start_time` define the starting time of
#' the timeserie as POSIXlt.
#' @param end_time If `pressure` is not provided, then `end_time` define the ending time of
#' the timeserie as POSIXlt.
#' @param verbose Display (or not) the progress of the query (logical).
#' @return A data.frame containing the timeserie of ERA5 pressure (date, pressure) as well as
#' longitude  and latitude (different if over water). If `pressure` is provided, the return
#' data.frame is the same as `pressure` with altitude and pressure0.
#' @seealso [`geopressure_ts_path()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html),
#' @export
geopressure_ts <- function(lon,
                           lat,
                           pressure = NULL,
                           end_time = NULL,
                           start_time = NULL,
                           verbose = TRUE) {
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
    temp_file <- tempfile("log_geopressure_ts_", fileext = ".json")
    write(jsonlite::toJSON(body_df), temp_file)
    stop(paste0(
      "Error with youre request on http://glp.mgravey.com:24853/GeoPressure/v1/timeseries/.",
      "Please try again, and if the problem persists, file an issue on Github:
        https://github.com/Rafnuss/GeoPressureAPI/issues/new?body=geopressure_ts&labels=crash
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
  res2 <- httr::GET(res_data$url)

  # read csv
  out <- as.data.frame(httr::content(res2,
    type = "text/csv",
    encoding = "UTF-8",
    show_col_types = FALSE
  ))

  # check for errors
  if (nrow(out) == 0) {
    temp_file <- tempfile("log_geopressure_ts_", fileext = ".json")
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

    # find when the bird was in flight or not to be considered
    id_0 <- pressure$sta_id == 0 | is.na(pressure$sta_id)
    # If no ground (ie. only flight) is present, pressure0 has no meaning
    if (!all(id_0)) {
      # We compute the mean pressure of the geolocator only when the bird is on the ground
      # (id_q==0) and when not marked as outliar
      id_norm <- !id_0 & !pressure$isoutliar

      pressure_obs_m <- mean(pressure$obs[id_norm])
      pressure_out_m <- mean(out$pressure[id_norm])

      out$pressure0 <- out$pressure - pressure_out_m + pressure_obs_m
    }
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
#' @param path A data.frame of the position containing latitude (`lat`), longitude  (`lon`) and the
#' stationary period id (`sta_id`) as column.
#' @param pressure Pressure list from PAM logger dataset list.
#' @param include_flight Extend request to also query the pressure and altitude during the previous
#' and/or next flight. Flights are defined by a `sta_id=0`. Accept Logical or vector of -1 (previous
#' flight), 0 (stationary) and/or 1 (next flight). (e.g. `include_flight=c(-1, 1)` will only search
#' for the flight before and after but not the stationary period). Note that next and previous
#' flights are defined by the +/1 of the `sta_id` value (and not the previous/next `sta_id` value).
#' @param verbose Display (or not) the progress of the queries (logical).
#' @return List of data.frame containing for each stationary period, the date, pressure, altitude
#' (same as [`geopressure_ts()`]).
#' @seealso [`geopressure_ts()`], [`geopressure_map2path()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html)
#' @export
geopressure_ts_path <- function(path,
                                pressure,
                                include_flight = FALSE,
                                verbose = TRUE) {
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
        verbose = FALSE
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
#' @param map List of raster containing probability map of each stationary period. The metadata of
#' `map` needs to include the start and end time of the stationary period .
#' @param interp The position of the stationary period shorter than `interp` will be
#' replace by a linear average from other position (in days) .
#' @param format One of `"lonlat"`, `"ind"`, `"arr.ind"`. return the path in lon-lat or indices
#' @return a data.frame of the position containing latitude (`lat`), longitude (`lon`) and the
#' stationary period id (`sta_id`) as column. Optionally, if indexes were requested, it will be
#' return. You will need to use `which.max(as.matrix(raster))` and not `which.max(raster)` to get
#' the correct location.
#' @seealso [`geopressure_prob_map()`], [`geopressure_ts_path()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#compute-altitude)
#' @export
geopressure_map2path <- function(map,
                                 interp = 0,
                                 format = "lonlat") {
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
    id_interp[1] <- FALSE
    id_interp[length(id_interp)] <- FALSE

    # Find the spacing between the position
    if (is.null(raster::metadata(map[[1]])$flight)) {
      # Or if flight duration are not available (e.g. `prob_pressure`), assumes homogenous spacing
      # between consecutive stationary period
      x <- path$sta_id
    } else {
      # If flight are avialabe, sum of the all flights between stationary period
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
