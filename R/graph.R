#' Create graph
#'
#' This function return a graph representing the trajectory of a bird based on filtering and triming
#' the probability maps provided.
#'
#' In the final graph, we only keep the most likely node (position in time) defined as: 1. those
#' which cumulative probability reach up to `thr_prob_percentile` for each stationary period. 2.
#' those which average ground speed is lower than `thr_gs` km/h.
#'
#' The graph returned is a list of the edges of the graph containing:
#' * `s`: source node (index in the 3d grid lat-lon-sta),
#' * `t`: target node (index in the 3d grid lat-lon-sta),
#' * `gs`: average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary.
#' * `ps`: static probability of each target node,
#' * `sz`: size of the 3d grid lat-lon-sta,
#' * `equipment`: node(s) of the first sta (index in the 3d grid lat-lon-sta),
#' * `retrieval`: node(s) of the last sta (index in the 3d grid lat-lon-sta),
#' * `flight_duration`: list of flight duration to next sta in hours,
#' * `lat`: list of the `static_prob` latitude in cell center,
#' * `lon`: list of the `static_prob` longitude in cell center,
#' * `extent`: raster geographical extent of the `static_prob`,
#' * `resolution`: raster res of the `static_prob`,
#' * `temporal_extent`: start and end date time retrieved from the metadata of `static_prob`.
#'
#' The [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph) provided an
#' example how to prepare the
#' data for the function and the output of this function.
#'
#' @param static_prob List of raster containing probability map of each stationary period. The
#'   metadata of `static_prob` needs to include the flight information to the next stationary period
#'   in the metadata `flight`.
#' @param thr_prob_percentile Threshold of percentile (see details).
#' @param thr_gs Threshold of groundspeed (km/h)  (see details).
#' @return Graph as a list (see details).
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#' @export
graph_create <- function(static_prob,
                         thr_prob_percentile = .99,
                         thr_gs = 150) {

  # Check input
  assertthat::assert_that(is.list(static_prob))
  assertthat::assert_that(inherits(static_prob[[1]], "RasterLayer"))
  assertthat::assert_that(assertthat::has_name(raster::metadata(static_prob[[1]]), c("flight", "sta_id")))
  assertthat::assert_that(is.numeric(thr_prob_percentile))
  assertthat::assert_that(length(thr_prob_percentile) == 1)
  assertthat::assert_that(thr_prob_percentile >= 0 & thr_prob_percentile <= 1)
  assertthat::assert_that(is.numeric(thr_gs))
  assertthat::assert_that(length(thr_gs) == 1)
  assertthat::assert_that(thr_gs >= 0)

  # compute size
  sz <- c(nrow(static_prob[[1]]), ncol(static_prob[[1]]), length(static_prob))
  nll <- sz[1] * sz[2]

  # convert raster into normalized matrix
  static_prob_n <- lapply(static_prob, function(x) {
    probt <- raster::as.matrix(x)
    if (sum(probt, na.rm = TRUE) == 0) {
      probt[probt == 0] <- 1
    }
    probt[is.na(probt)] <- 0
    probt / sum(probt, na.rm = TRUE)
  })

  sta_id_0 <- unlist(lapply(static_prob_n, sum)) == 0
  if (any(is.na(sta_id_0))) {
    stop(paste0(
      "static_prob is invalid for index ",
      paste(which(is.na(sta_id_0)), collapse = ", "),
      " (check that the probability map is not null/na)."
    ))
  }
  if (any(sta_id_0)) {
    stop(paste0(
      "The `static_prob` provided has an invalid probability map for the stationary period: ",
      which(sta_id_0)
    ))
  }

  # find the pixels above to the percentile
  nds <- lapply(static_prob_n, function(probi) {
    # First, compute the threshold of prob corresponding to percentile
    probis <- sort(probi)
    id_prob_percentile <- sum(cumsum(probis) <= (1 - thr_prob_percentile))
    thr_prob <- probis[id_prob_percentile + 1]

    # filter the pixels above the threshold
    nds <- probi >= thr_prob
    # return
    nds
  })

  nds_0 <- unlist(lapply(nds, sum)) == 0
  if (any(nds_0)) {
    stop(paste0(
      "Using the `thr_prob_percentile` of ", thr_prob_percentile, " provided, there are not any ",
      "nodes left for the stationary period: ", which(nds_0)
    ))
  }

  # Get latitude and longitude of the center of the pixel
  lat <- seq(raster::ymax(static_prob[[1]]), raster::ymin(static_prob[[1]]),
    length.out = nrow(static_prob[[1]]) + 1
  )
  lat <- utils::head(lat, -1) + diff(lat[1:2]) / 2
  lon <- seq(raster::xmin(static_prob[[1]]), raster::xmax(static_prob[[1]]),
    length.out = ncol(static_prob[[1]]) + 1
  )
  lon <- utils::head(lon, -1) + diff(lon[1:2]) / 2

  # Approximate resolution of the grid in km assuming 111km/lat-lon
  resolution <- mean(diff(lon)) * 111

  # extract the flight duration
  flight_duration <- unlist(lapply(static_prob, function(x) {
    mtf <- raster::metadata(x)
    as.numeric(sum(difftime(mtf$flight$end, mtf$flight$start, units = "hours")))
  }))

  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  cond <- TRUE
  while (cond) {
    n_old <- sum(unlist(lapply(nds, sum)))
    for (i_s in seq_len(sz[3] - 1)) {
      nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
      if (sum(nds[[i_s + 1]]) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary distance, ",
          "there are not any nodes left at stationary period ",
          raster::metadata(static_prob[[i_s + 1]])$sta_id, " from stationary period ",
          raster::metadata(static_prob[[i_s]])$sta_id
        ))
      }
    }
    for (i_sr in seq_len(sz[3] - 1)) {
      i_s <- sz[3] - i_sr + 1
      nds[[i_s - 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s - 1] * thr_gs & nds[[i_s - 1]]
      if (sum(nds[[i_s - 1]]) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary distance, ",
          "there are not any nodes left at stationary period ",
          raster::metadata(static_prob[[i_s - 1]])$sta_id, " from stationary period ",
          raster::metadata(static_prob[[i_s]])$sta_id
        ))
      }
    }
    n_new <- sum(unlist(lapply(nds, sum)))
    if (n_new == n_old) {
      cond <- FALSE
    }
  }

  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary distance, there are not ",
      "any nodes left"
    ))
  }

  # Identify equipment and retrieval
  equipment <- which(nds[[1]] == TRUE)
  retrieval <- which(nds[[sz[3]]] == TRUE) + (sz[3] - 1) * nll


  # Create the graph from nds with the exact groundspeed

  # Run each transition in parallel with decreasing order of edges
  nds_sum <- unlist(lapply(nds, sum))
  nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
  nds_sorted_idx <- order(nds_expend_sum, decreasing = TRUE)
  nds_expend_sum <- sort(nds_expend_sum, decreasing = TRUE)
  future::plan(future::multisession, workers = future::availableCores() / 2)
  f <- list()

  message(
    "Computing the groundspeed for ", sum(nds_expend_sum), " edges for ",
    length(nds_expend_sum), " stationary periods in parallel."
  )
  progress_bar(0, max = sum(nds_expend_sum))
  for (i in seq_len(length(nds_sorted_idx))) {
    i_s <- nds_sorted_idx[i]
    nds_i_s <- nds[[i_s]]
    nds_i_s_1 <- nds[[i_s + 1]]
    static_prob_n_i_s_1 <- static_prob_n[[i_s + 1]]
    f[[i_s]] <- future::future(expr = {
      # find all the possible equipment and target based on nds and expand to all possible
      # combination
      grt <- expand.grid(
        s = as.integer(which(nds_i_s) + (i_s - 1) * nll),
        t = as.integer(which(nds_i_s_1) + i_s * nll)
      )

      # Find the index in lat, lon, sta of those equipment and target
      s_id <- arrayInd(grt$s, sz)
      t_id <- arrayInd(grt$t, sz)

      # compute the groundspeed for all transition
      gs_abs <- geosphere::distGeo(
        cbind(lon[s_id[, 2]], lat[s_id[, 1]]),
        cbind(lon[t_id[, 2]], lat[t_id[, 1]])
      ) / 1000 / flight_duration[i_s]

      # filter the transition based on the groundspeed
      id <- gs_abs < thr_gs
      if (sum(id) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the exact distance of ",
          "edges, there are not any nodes left for the stationary period: ", i_s
        ))
      }
      grt <- grt[id, ]

      # Compute the bearing of the trajectory
      gs_bearing <- geosphere::bearingRhumb(
        cbind(lon[s_id[id, 2]], lat[s_id[id, 1]]),
        cbind(lon[t_id[id, 2]], lat[t_id[id, 1]])
      )
      # bearing is NA if gs==0, fix for computing the complex representation
      gs_bearing[is.na(gs_bearing)] <- 0

      # save groundspeed in complex notation
      gs_arg <- (450 - gs_bearing) %% 360
      grt$gs <- gs_abs[id] * cos(gs_arg * pi / 180) +
        1i * gs_abs[id] * sin(gs_arg * pi / 180)

      # assign the static probability of the target node (pressure * light)
      # We use here the normalized probability assuming that the bird needs to be somewhere at each
      # stationary period. The log-linear pooling (`geopressure_prob_map`) is supposed to account
      # for the variation in staionary period duration.
      # For un-normalized use raster::as.matrix(static_prob[[i_s + 1]]))
      grt$ps <- static_prob_n_i_s_1[grt$t - i_s * nll]

      if (sum(id) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the exact distance of ",
          "edges, there are not any nodes left for the stationary period: ", i_s
        ))
      }
      return(grt)
    }, seed = TRUE)
    progress_bar(sum(nds_expend_sum[seq(1, i)]),
      max = sum(nds_expend_sum),
      text = paste0("| sta = ", i, "/", sz[3] - 1)
    )
  }

  # Retrieve the graph
  gr <- future::value(f)

  # Trim
  gr <- graph_trim(gr)

  # Convert gr to a graph list
  grl <- as.list(do.call("rbind", gr))
  grl$sz <- sz
  grl$equipment <- equipment
  grl$retrieval <- retrieval

  # Add metadata information
  grl$flight_duration <- flight_duration
  grl$lat <- lat
  grl$lon <- lon
  grl$extent <- raster::extent(static_prob[[1]])
  grl$resolution <- raster::res(static_prob[[1]])
  grl$temporal_extent <- lapply(static_prob, function(x) {
    raster::metadata(x)$temporal_extent
  })
  grl$flight <- lapply(static_prob, function(x) {
    raster::metadata(x)$flight
  })
  grl$sta_id <- unlist(lapply(static_prob, function(x) {
    raster::metadata(x)$sta_id
  }))
  grl$mask_water <- is.na(raster::as.matrix(static_prob[[1]]))
  return(grl)
}



#' Trim a graph
#'
#' Trimming consists in removing "dead branch" of a graph, that is removing the edges which are not
#' connected to both the source (i.e, equipment) or sink (i.e. retrieval site).
#'
#' @param gr Graph constructed with [`graph_create()`].
#' @return Graph trimmed
#' @seealso [`graph_create()`]
graph_trim <- function(gr) {
  message("Trimming the graph:")

  progress_bar(1, max = (length(gr) - 1) * 2)

  # First, trim the graph from equipment to retrieval
  for (i_s in seq(2, length(gr))) {

    # Select the source id which exist in the target of the previous stationary period.
    s <- unique(gr[[i_s]]$s)
    t_b <- unique(gr[[i_s - 1]]$t)
    unique_s_new <- s[s %in% t_b]

    # Keep the edge from which the source id was found in the previous step
    id <- gr[[i_s]]$s %in% unique_s_new
    gr[[i_s]] <- gr[[i_s]][id, ]

    if (nrow(gr[[i_s]]) == 0) {
      stop(paste0(
        "Triming the graph killed it at stationary period ", i_s, " moving forward."
      ))
    }
    progress_bar(i_s - 1, max = (length(gr) - 1) * 2)
  }
  # Then, trim the graph from retrieval to equipment
  for (i_s in seq(length(gr) - 1, 1)) {
    t <- unique(gr[[i_s]]$t)
    s_a <- unique(gr[[i_s + 1]]$s)
    unique_t_new <- t[t %in% s_a]

    id <- gr[[i_s]]$t %in% unique_t_new
    gr[[i_s]] <- gr[[i_s]][id, ]

    if (nrow(gr[[i_s]]) == 0) {
      stop(paste0(
        "Triming the graph killed it at stationary period ", i_s, " moving backward."
      ))
    }
    progress_bar(length(gr) * 2 - 1 - i_s, max = (length(gr) - 1) * 2)
  }
  return(gr)
}


#' Download wind data
#'
#' This function download the wind data from ERA5 for all flights. The flight are determined from
#' the stationary periods classified `pam$sta` (see `pam_classify()`). It request a single file for
#' each flight using the exact time (hourly basis) and pressure (altitude). We use
#' `wf_request_batch()` from `ecmwfr` to query multiple wind data at the same time.
#'
#' To be able to download data from the Climate Data Store (CDS), you will need to create an account
#'  on  [https://cds.climate.copernicus.eu](https://cds.climate.copernicus.eu). You can then save
#'  your credential (`cds_key` and `cds_user`) in your `.Rprofile` (see
#'  [GeoPressureManual | Wind graph](
#'  https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data)).
#'
#' @param pam PAM logger dataset list with `pam$sta` computed. See [`pam_read()`] and [`pam_sta()`].
#' @param area Geographical extent of the map to query. Either a raster (e.g. `static_prob`) or a
#' list ordered by North, West, South, East  (e.g. `c(50,-16,0,20)`).
#' @param sta_id Stationary period identifier of the start of the flight to query as defined in
#' `pam$sta`. Be default, download for all the flight.
#' @param cds_key User (email address) used to sign up for the ECMWF data service. See
#' [`wf_set_key()`].
#' @param cds_user Token provided by ECMWF. See [`wf_set_key()`].
#' @param path Path were to store the downloaded data.
#' @return The path of the downloaded (requested file).
#' @seealso [`wf_request()`], [GeoPressureManual | Wind graph
#' ](https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data)
#' @export
graph_download_wind <- function(pam,
                                area, # area is specified as N, W, S, E
                                sta_id = seq_len(nrow(pam$sta) - 1),
                                cds_key = Sys.getenv("cds_key"),
                                cds_user = Sys.getenv("cds_user"),
                                path = paste0("data/5_wind_graph/", pam$id, "/")) {
  assertthat::assert_that(is.list(pam))
  assertthat::assert_that(assertthat::has_name(pam, "pressure"))
  assertthat::assert_that(is.data.frame(pam$pressure))
  assertthat::assert_that(assertthat::has_name(pam$pressure, c("date", "obs")))
  assertthat::assert_that(assertthat::has_name(pam, "sta"))
  assertthat::assert_that(is.data.frame(pam$sta))
  assertthat::assert_that(assertthat::has_name(pam$sta, c("end", "start")))

  if (is.list(area)) {
    area <- area[[1]]
  }
  area <- raster::extent(area)
  area <- c(area@ymax, area@xmin, area@ymin, area@xmax)

  assertthat::assert_that(is.numeric(sta_id))
  assertthat::assert_that(all(sta_id %in% pam$sta$sta_id))

  ecmwfr::wf_set_key(user = cds_user, key = cds_key, service = "cds")

  if (!file.exists(path)) {
    stop(paste0("The path ", path, " does not exist. You can create it with: `dir.create(path)`."))
  }

  # see https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Levellistings
  possible_pressure <- c(
    1, 2, 3, 5, 7, 10, 20, 30, 50, 70,
    seq(100, 250, 25), seq(300, 750, 50), seq(775, 1000, 25)
  )

  request_list <- list()
  for (i_id in sta_id) {
    # Find the index of sta_id
    i_s <- which(pam$sta$sta_id == i_id)

    # Get the timeserie of the flight on a 1 hour resolution
    flight_time <- seq(round.POSIXt(pam$sta$end[i_s] - 30 * 60, units = "hours"),
      round.POSIXt(pam$sta$start[i_s + 1] + 30 * 60, units = "hours"),
      by = 60 * 60
    )

    # Find the pressure level needed during this flight
    flight_id <- flight_time[1] <= pam$pressure$date &
      pam$pressure$date <= utils::tail(flight_time, 1)
    pres_id_min <- sum(!(min(pam$pressure$obs[flight_id]) < possible_pressure))
    pres_id_max <- sum(max(pam$pressure$obs[flight_id]) > possible_pressure) + 1
    flight_pres_id <- seq(pres_id_min, min(pres_id_max, length(possible_pressure)))

    # Prepare the query
    request_list[[i_s]] <- list(
      dataset_short_name = "reanalysis-era5-pressure-levels",
      product_type = "reanalysis",
      format = "netcdf",
      variable = c("u_component_of_wind", "v_component_of_wind"),
      pressure_level = possible_pressure[flight_pres_id],
      year = sort(unique(format(flight_time, "%Y"))),
      month = sort(unique(format(flight_time, "%m"))),
      day = sort(unique(format(flight_time, "%d"))),
      time = sort(unique(format(flight_time, "%H:%M"))),
      area = area,
      target = paste0(pam$id, "_", i_s, ".nc")
    )
  }

  ecmwfr::wf_request_batch(
    request_list,
    workers = 20,
    # user = ,
    path = path,
    # time_out = 3600,
    # total_timeout = length(request_list) * time_out/workers
  )
}


#' Add windspeed and airspeed
#'
#' Read NetCDF file downloaded on your computer and add the average windspeed experienced by the
#' bird and the corresponding airspeed for each edge of the graph.
#'
#' See the [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data) for
#' explanations and example on how to download the `NetCDF` files from ERA-5.
#'
#' @param grl graph constructed with [`graph_create()`]
#' @param pressure pressure data from a PAM logger. This data.frame needs to contains `date` as
#' POSIXt and `obs` in hPa. It is best practice to use [`pam_read()`] and [`pam_sta()`] to build
#' this data.frame.
#' @param filename Character of the path where to find the netCDF file.
#' @param thr_as Threshold of airspeed (km/h).
#' @return Graph as a list with windspeed and airspeed as `ws` and `as` respectively (see
#' [`graph_create()`] for more detail on the graph returned).
#' @seealso [`graph_create()`], [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#add-wind-to-graph)
#' @export
graph_add_wind <- function(grl,
                           pressure,
                           filename,
                           thr_as = Inf) {
  assertthat::assert_that(is.list(grl))
  assertthat::assert_that(assertthat::has_name(grl, c("s", "t", "gs", "sz", "lat", "lon", "flight")))
  assertthat::assert_that(length(grl$s) > 0)
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "obs")))
  assertthat::assert_that(inherits(pressure$date, "POSIXt"))
  assertthat::assert_that(is.numeric(pressure$obs))
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(file.exists(paste0(filename, "1.nc")))
  assertthat::assert_that(is.numeric(thr_as))
  assertthat::assert_that(length(thr_as) == 1)
  assertthat::assert_that(thr_as >= 0)

  # Extract the index in lat, lon, sta from the source and target of all edges
  s <- arrayInd(grl$s, grl$sz)
  t <- arrayInd(grl$t, grl$sz)

  # Prepare the matrix of speed to return
  uv <- matrix(NA, nrow = length(grl$s), ncol = 2)

  # Check that all the files of wind_speed exist and match the data request
  for (i1 in seq_len(grl$sz[3] - 1)) {
    fl_s <- grl$flight[[i1]]
    for (i2 in seq_len(length(fl_s$sta_id))) {
      i_s <- fl_s$sta_id[i2]

      if (!file.exists(paste0(filename, i_s, ".nc"))) {
        stop(paste0("No file for sta=", i_s))
      }
      nc <- ncdf4::nc_open(paste0(filename, i_s, ".nc"))

      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60, origin = "1900-01-01", tz = "UTC")
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"), tz = "UTC")
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"), tz = "UTC")
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        stop(paste0("Time not matching for for sta=", i_s))
      }

      pres <- ncdf4::ncvar_get(nc, "level")
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)
      pres_obs <- pressure$obs[pressure$date > t_s & pressure$date < t_e]
      if (length(pres_obs) == 0 ||
        !(min(pres) <= min(pres_obs) &&
          max(pres) >= min(1000, max(pres_obs)))) {
        stop(paste0("Pressure not matching for sta=", i_s))
      }

      # Check if flight duration is
      if (fl_s$start[i2] >= fl_s$end[i2]) {
        stop(paste0(
          "Flight starting on sta_id=", fl_s$sta_id[i2], " has a start time equal or greater than ",
          "the end time. Please review your labeling file."
        ))
      }
    }
  }

  # Start progress bar
  nds_expend_sum <- table(s[, 3])
  progress_bar(0,
    max = sum(nds_expend_sum),
    text = paste0("| sta = ", 0, "/", grl$sz[3] - 1)
  )

  # Loop through the stationary period kept in the graph
  for (i1 in seq_len(grl$sz[3] - 1)) {

    # Extract the flight information from the current sta to the next one considered in the graph.
    # It can be the next, or if some sta are skipped at construction, it can contains multiples
    # flights
    fl_s <- grl$flight[[i1]]

    # Extract the duration of each flights.
    fl_s_dur <- as.numeric(difftime(fl_s$end, fl_s$start, units = "hours"))

    # Determine the id of edges of the graph corresponding to this/these flight(s).
    st_id <- which(s[, 3] == i1)

    # We are assuming that the bird flight as a straight line between the source and the target node
    # of each edge. If multiple flights happen during this transition, we assume that the bird flew
    # with a cosz[3]nt groundspeed during each flight, thus considering its stop-over position to be
    # spread according to the flight duration. This does not account for habitat, so that it would
    # assume a bird can stop over water. While we could improve this part of the code to assume
    # cosz[3]nt airspeed rather than groundspeed, we suggest to create the graph considering all
    # stopovers.
    ratio_sta <- as.matrix(c(0, cumsum(fl_s_dur) / sum(fl_s_dur)))

    # Prepare the u- and v- windspeed for each flight (row) and edge (col)
    u_sta <- matrix(NA, nrow = length(fl_s$sta_id), ncol = length(st_id))
    v_sta <- matrix(NA, nrow = length(fl_s$sta_id), ncol = length(st_id))

    # Loop through each flight of the transition
    for (i2 in seq_len(length(fl_s$sta_id))) {

      # Find the stationary period ID from this specific flight (source)
      i_s <- fl_s$sta_id[i2]

      # Read the netCDF file
      nc <- ncdf4::nc_open(paste0(filename, i_s, ".nc"))

      # Read data from netCDF file and convert the time of data to posixt
      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60,
        origin = "1900-01-01", tz = "UTC"
      )
      pres <- ncdf4::ncvar_get(nc, "level")
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")

      # Find the start and end latitude and longitude of each edge
      lat_s <- grl$lat[s[st_id, 1]] +
        ratio_sta[i2] * (grl$lat[t[st_id, 1]] - grl$lat[s[st_id, 1]])
      lon_s <- grl$lon[s[st_id, 2]] +
        ratio_sta[i2] * (grl$lon[t[st_id, 2]] - grl$lon[s[st_id, 2]])
      lat_e <- grl$lat[s[st_id, 1]] +
        ratio_sta[i2 + 1] * (grl$lat[t[st_id, 1]] - grl$lat[s[st_id, 1]])
      lon_e <- grl$lon[s[st_id, 2]] +
        ratio_sta[i2 + 1] * (grl$lon[t[st_id, 2]] - grl$lon[s[st_id, 2]])

      # As ERA5 data is available every hour, we build a one hour resolution timeserie including the
      # start and end time of the flight. Thus, we first round the start end end time.
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)

      # We assume that the bird is moving with a constant groundspeed between `flight$start` and
      # `flight$end`. Using a linear interpolation, we extract the position (lat, lon) at every hour
      # on `t_q`. Extrapolation outside (before the bird departure or after he arrived) is with a
      # nearest neighbor.

      dt <- as.numeric(difftime(fl_s$end[i2], fl_s$start[i2], units = "hours"))
      dlat <- (lat_e - lat_s) / dt
      dlon <- (lon_e - lon_s) / dt
      w <- pmax(pmin(as.numeric(
        difftime(t_q, fl_s$start[i2], units = "hours")
      ), dt), 0)
      w2 <- matrix(w, nrow = length(dlat), ncol = length(w), byrow = TRUE)
      lat_int <- lat_s + w2 * replicate(length(w), dlat)
      lon_int <- lon_s + w2 * replicate(length(w), dlon)

      # As we are interesting in the average windspeed experienced during the entire flight, we need
      # to find the weights of each 1hr interval extracted from ERA5. We can estimate these weight
      # assuming a linear integration of the time (trapezoidal rule) or a step integration (Riemann
      # sum)

      # Linear integration
      w <- numeric(length(t_q))
      assertthat::assert_that(length(w) > 1)
      alpha <- 1 - as.numeric(difftime(fl_s$start[i2], t_q[1],
        units = "hours"
      ))
      assertthat::assert_that(alpha >= 0 & alpha <= 1)
      w[c(1, 2)] <- w[c(1, 2)] + c(alpha, 1 - alpha) * alpha
      alpha <- 1 - as.numeric(difftime(utils::tail(t_q, 1), fl_s$end[i2],
        units = "hours"
      ))
      assertthat::assert_that(alpha >= 0 & alpha <= 1)
      w[length(w) - c(1, 0)] <- w[length(w) - c(1, 0)] +
        c(1 - alpha, alpha) * alpha

      if (length(w) >= 4) {
        w[c(2, length(w) - 1)] <- w[c(2, length(w) - 1)] + 0.5
      }
      if (length(w) >= 5) {
        w[seq(3, length(w) - 2)] <- w[seq(3, length(w) - 2)] + 1
      }
      # normalize the weight
      w <- w / sum(w)

      # step integration
      # w <- difftime(pmin(pmax(t_q+60*60/2,fl_s$start[i2]),fl_s$end[i2]),
      #               pmin(pmax(t_q-60*60/2,fl_s$start[i2]),fl_s$end[i2]),
      #               units = "hours")

      # Prepare the interpolated u- v- vector for each flight
      u_int <- matrix(NA, nrow = length(t_q), ncol = length(st_id))
      v_int <- matrix(NA, nrow = length(t_q), ncol = length(st_id))

      # Loop through the 1hr interval
      for (i3 in seq_len(length(t_q))) {

        # find the time step to query in ERA5
        id_time <- which(time == t_q[i3])
        # find the two pressure level to query (one above, one under) based on the geolocator
        # pressure at this timestep
        pres_obs <- stats::approx(pressure$date, pressure$obs, t_q[i3])$y
        df <- pres_obs - pres
        df[df < 0] <- NA
        id_pres <- which.min(df)
        # if the pressure is higher than the highest level (i.e. bird below the ground level
        # pressure), we extract only the last layer
        n_pres <- ifelse(id_pres == length(df), 1, 2)

        dlon <- lon[2] - lon[1]
        id_lon <- which(lon >= (min(lon_int[, i3]) - dlon) &
          (max(lon_int[, i3]) + dlon) >= lon)

        dlat <- abs(lat[2] - lat[1])
        id_lat <- which(lat >= (min(lat_int[, i3]) - dlat) &
          (max(lat_int[, i3]) + dlat) >= lat)


        # get the two maps of u- and v-
        u <- ncdf4::ncvar_get(nc, "u",
          start = c(id_lon[1], id_lat[1], id_pres, id_time),
          count = c(length(id_lon), length(id_lat), n_pres, 1)
        )
        v <- ncdf4::ncvar_get(nc, "v",
          start = c(id_lon[1], id_lat[1], id_pres, id_time),
          count = c(length(id_lon), length(id_lat), n_pres, 1)
        )

        # Interpolate linearly the map of wind based on pressure.
        if (n_pres == 2) {
          w2 <- abs(pres[id_pres + c(0, 1)] - pres_obs)
          w2 <- w2 / sum(w2)
          u <- w2[1] * u[, , 1] + w2[2] * u[, , 2]
          v <- w2[1] * v[, , 1] + w2[2] * v[, , 2]
        }

        # Interpolation the u- and v- component at the interpolated position at the current time
        # step.
        ll_int <- round(cbind(lat_int[, i3], lon_int[, i3]), 1)
        ll_int_uniq <- unique(ll_int)
        id_uniq <- match(
          ll_int[, 1] * 1000 + ll_int[, 2],
          ll_int_uniq[, 1] * 1000 + ll_int_uniq[, 2]
        )

        tmp <- pracma::interp2(rev(lat[id_lat]), lon[id_lon],
          u[, rev(seq_len(ncol(u)))],
          ll_int_uniq[, 1], ll_int_uniq[, 2],
          method = "linear"
        )
        u_int[i3, ] <- tmp[id_uniq]
        tmp <- pracma::interp2(rev(lat[id_lat]), lon[id_lon],
          v[, rev(seq_len(ncol(u)))],
          ll_int_uniq[, 1], ll_int_uniq[, 2],
          method = "linear"
        )
        v_int[i3, ] <- tmp[id_uniq]
      }
      # Compute the average wind component of the flight accounting for the weighting scheme
      u_sta[i2, ] <- colSums(u_int * w)
      v_sta[i2, ] <- colSums(v_int * w)

      progress_bar(sum(nds_expend_sum[seq(1, i1)]),
        max = sum(nds_expend_sum),
        text = paste0("| sta = ", i1, "/", grl$sz[3] - 1)
      )
    }
    # Compute the average  over all the flight of the transition accounting for the duration of the
    # flight.
    uv[st_id, 1] <- colSums(u_sta * fl_s_dur / sum(fl_s_dur))
    uv[st_id, 2] <- colSums(v_sta * fl_s_dur / sum(fl_s_dur))
  }

  # save windspeed in complex notation and convert from m/s to km/h
  grl$ws <- (uv[, 1] + 1i * uv[, 2]) / 1000 * 60 * 60

  # compute airspeed
  grl$as <- grl$gs - grl$ws

  # filter edges based on airspeed
  id <- abs(grl$as) <= thr_as
  sta_pass <- which(!(seq_len(grl$sz[3] - 1) %in% unique(s[id, 3])))
  if (length(sta_pass) > 0) {
    stop(paste0(
      "Using the `thr_as` of ", thr_as,
      " km/h provided with the exact distance of edges, there are ",
      "not any nodes left for the stationary period: ", paste(sta_pass, collapse = ", "),
      " with a minimum airspeed of ", min(abs(grl$as[s[, 3] == sta_pass])), " km/h"
    ))
  }

  grl$s <- grl$s[id]
  grl$t <- grl$t[id]
  grl$gs <- grl$gs[id]
  grl$as <- grl$as[id]
  grl$ws <- grl$ws[id]
  grl$ps <- grl$ps[id]

  return(grl)
}



#' Marginal Probability Map
#'
#' This function return the marginal probability map as raster from a graph.
#'
#' @param grl graph constructed with [`graph_create()`]
#' @return list of raster of the marginal probability at each stationary period
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @export
graph_marginal <- function(grl) {
  assertthat::assert_that(is.list(grl))
  assertthat::assert_that(assertthat::has_name(grl, c("s", "t", "p", "sz", "lat", "lon", "mask_water", "equipment", "retrieval", "resolution", "extent", "temporal_extent", "flight", "sta_id")))
  assertthat::assert_that(length(grl$s) > 0)

  # number of nodes in the 3d grid
  n <- prod(grl$sz)

  # matrix of forward transition
  trans_f <- Matrix::sparseMatrix(grl$s, grl$t, x = grl$p, dims = c(n, n))

  # matrix of backward transition
  trans_b <- Matrix::sparseMatrix(grl$t, grl$s, x = grl$p, dims = c(n, n))

  # forward mapping of marginal probability
  map_f <- Matrix::sparseMatrix(rep(1, length(grl$equipment)),
    grl$equipment,
    x = 1, dims = c(1, n)
  )

  # backward mapping of marginal probability
  map_b <- Matrix::sparseMatrix(rep(1, length(grl$retrieval)),
    grl$retrieval,
    x = 1, dims = c(1, n)
  )

  # build iteratively the marginal probability backward and forward by re-using the mapping
  # computed for previous stationary period. Set the equipment and retrieval site in each loop
  for (i_s in seq_len(grl$sz[3] - 1)) {
    map_f[1, grl$equipment] <- 1
    map_f <- map_f %*% trans_f

    map_b[1, grl$retrieval] <- 1
    map_b <- map_b %*% trans_b
  }
  # add the retrieval and equipment at the end to finish it
  map_f[1, grl$equipment] <- 1
  map_b[1, grl$retrieval] <- 1

  # combine the forward and backward
  map <- map_f * map_b

  # reshape mapping as a full (non-sparce matrix of correct size)
  map <- as.matrix(map)
  dim(map) <- grl$sz

  # convert to raster
  static_prob_marginal <- list()
  for (i_s in seq_len(dim(map)[3])) {
    tmp <- map[, , i_s]
    tmp[grl$mask_water] <- NA
    if (sum(tmp, na.rm = TRUE) == 0) {
      stop(
        "The probability of some transition are too small to find numerical solution. ",
        "Please check the data used to create the graph."
      )
    }
    static_prob_marginal[[i_s]] <- raster::raster(grl$extent,
      resolution = grl$resolution,
      vals = tmp
    )
    raster::crs(static_prob_marginal[[i_s]]) <-
      "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    raster::metadata(static_prob_marginal[[i_s]]) <- list(
      sta_id = grl$sta_id[i_s],
      temporal_extent = grl$temporal_extent[[i_s]],
      flight = grl$flight[[i_s]]
    )
  }

  return(static_prob_marginal)
}




#' Simulation of trajectory
#'
#' This function simulates multiple trajectory from a graph. The trajectories consist of the
#' positions at each stationary periods.
#'
#' @param grl Graph constructed with [`graph_create()`].
#' @param nj Number of simulation.
#' @return List of simulated paths.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @export
graph_simulation <- function(grl,
                             nj = 10) {
  assertthat::assert_that(is.list(grl))
  assertthat::assert_that(assertthat::has_name(grl, c("s", "t", "p", "sz", "lat", "lon", "equipment", "retrieval", "resolution", "extent", "temporal_extent", "sta_id")))
  assertthat::assert_that(length(grl$s) > 0)
  assertthat::assert_that(is.numeric(nj))
  assertthat::assert_that(nj > 0)

  # number of nodes in the 3d grid
  n <- prod(grl$sz)
  nll <- grl$sz[1] * grl$sz[2]

  # Initialize path. As we will simulate the path chronological order, only the first equipment
  # site needs to be set.
  path <- matrix(ncol = grl$sz[3], nrow = nj)
  path[, 1] <- grl$equipment

  # Find the stationary index of all the source so that only the edges from a specific stationary
  # period can be easily query
  s_id <- arrayInd(grl$s, grl$sz)

  # As we will simulate in forward chronological order, we will be able to create map_f inside the
  # simulation. However, map_b needs to be computed for all stationary period in advance, starting
  # by the last stationary period and moving backward in time as follow
  map_b <- list()
  map_b[[grl$sz[3]]] <- Matrix::sparseMatrix(rep(1, length(grl$retrieval)),
    grl$retrieval,
    x = 1, dims = c(1, n)
  )

  for (i_sta in (grl$sz[3] - 1):1) {
    id <- s_id[, 3] == i_sta
    map_b[[i_sta]] <- map_b[[i_sta + 1]] %*%
      Matrix::sparseMatrix(grl$t[id], grl$s[id], x = grl$p[id], dims = c(n, n))
  }

  # Loop through the simulation along chronological order
  progress_bar(1, max = grl$sz[3])
  for (i_sta in seq(2, grl$sz[3])) {
    # find edges arriving to this stationary period
    id <- s_id[, 3] == (i_sta - 1)

    # create the local trans_f (only edges from previous sta to this sta
    trans_f <- Matrix::sparseMatrix(grl$s[id], grl$t[id],
      x = grl$p[id],
      dims = c(n, n)
    )

    # build the forward mapping from the simulated nodes of the previous stationary period to the
    # current one using trans_f
    map_f <- Matrix::sparseMatrix(seq_len(nj), path[, i_sta - 1],
      x = 1,
      dims = c(nj, n)
    ) %*% trans_f

    # Combine forward and backward and samples
    if (nj > 1) {
      ids <- apply(map_f[, nll * (i_sta - 1) + (1:nll)], 1, function(x) {
        map <- x * map_b[[i_sta]][nll * (i_sta - 1) + (1:nll)]
        sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
      })
    } else {
      map <- map_f[, nll * (i_sta - 1) + (1:nll)] * map_b[[i_sta]][nll * (i_sta - 1) + (1:nll)]
      ids <- sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
    }

    #
    path[, i_sta] <- ids + nll * (i_sta - 1)

    # Update progress bar
    progress_bar(i_sta, max = grl$sz[3])
  }

  return(graph_path2lonlat(path, grl))
}


#' Find the latitude and longitude from a path index
#'
#' @param path_id List or matrix of node index.
#' @param grl Graph constructed with [`graph_create()`].
#' @return List of the path with latitude and longitude and index of the the path provided.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-1-shortest-path)
#' @export
graph_path2lonlat <- function(path_id,
                              grl) {
  assertthat::assert_that(is.list(grl))
  assertthat::assert_that(assertthat::has_name(grl, c("s", "t", "sz", "lat", "lon")))
  assertthat::assert_that(length(grl$s) > 0)
  assertthat::assert_that(is.numeric(path_id))
  assertthat::assert_that(all(path_id > 0 & path_id <= prod(grl$sz)))

  ind <- arrayInd(path_id, grl$sz)
  p <- list()
  p$id <- path_id
  p$lat <- grl$lat[ind[, 1]]
  dim(p$lat) <- dim(p$id)
  p$lon <- grl$lon[ind[, 2]]
  dim(p$lon) <- dim(p$id)
  p$sta_id <- grl$sta_id
  return(p)
}

#' Find the edge index from a path index
#'
#' Very inefficient way to find the edges...
#'
#' @param path_id List or matrix of node index (`nj x nsta`).
#' @param grl Graph constructed with [`graph_create()`].
#' @return List or matrix of the edge `nj x (nsta-1)`.
#' @seealso [`graph_create()`], [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#energy)
#' @export
graph_path2edge <- function(path_id,
                            grl) {
  assertthat::assert_that(is.list(grl))
  assertthat::assert_that(assertthat::has_name(grl, c("s", "t")))
  assertthat::assert_that(length(grl$s) > 0)
  assertthat::assert_that(is.numeric(path_id))
  assertthat::assert_that(all(path_id > 0 & path_id <= prod(grl$sz)))

  if (is.matrix(path_id)) {
    # Number of paths
    nj <- dim(path_id)[1]
    # number of stationary period
    nsta <- dim(path_id)[2]
    assertthat::assert_that(nsta == grl$sz[3])

    # Get the source and target
    path_s <- path_id[, 1:(nsta - 1)]
    path_t <- path_id[, 2:nsta]

    # put as vector
    dim(path_s) <- (nsta - 1) * nj
    dim(path_t) <- (nsta - 1) * nj
  } else {
    nsta <- length(path_id)
    nj <- 1
    assertthat::assert_that(nsta == grl$sz[3])

    path_s <- path_id[1:(nsta - 1)]
    path_t <- path_id[2:nsta]
  }


  # Use mapply to loop through each edge. THE WORST!
  edge <- mapply(function(s, t) {
    which(grl$s == s & grl$t == t)
  }, path_s, path_t)

  # reshape in original shapre
  dim(edge) <- c(nj, (nsta - 1))

  return(edge)
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
