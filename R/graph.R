#' Create graph
#'
#' This function return a graph representing the trajectory of a bird based on
#' filtering and triming the probability maps provided.
#'
#' In the final graph, we only keep the most likely node (position in time)
#' defined as:
#' 1. those which cumulative probability reach up to `thr_prob_percentile` for
#' each stationary period.
#' 2. those which average ground speed is lower than `thr_gs` km/h.
#'
#' The graph returned is a list of the edges of the graph containing:
#' - `s`: source node (index in the 3d grid lat-lon-sta),
#' - `t`: target node (index in the 3d grid lat-lon-sta),
#' - `gs`: average ground speed required to make that transition (km/h)
#' as complex number representing the E-W as real and S-N as imaginary.
#' - `ps`: static probability of each target node
#' - `sz`: size of the 3d grid lat-lon-sta
#' - `equipement`: node(s) of the first sta (index in the 3d grid lat-lon-sta)
#' - `retrival`: node(s) of the last sta (index in the 3d grid lat-lon-sta)
#' - `flight_duration`: list of flight duration to next sta in hours
#' - `lat`: list of the `static_prob` latitude in cell center
#' - `lon`: list of the `static_prob` longitude in cell center
#' - `extent`: raster geographical extent of the `static_prob``
#' - `resolution`: raster res of the `static_prob`
#' - `temporal_extent`: start and end date time retrieved from the metadata of
#' `static_prob`
#'
#'
#' The vignette [Basic graph](./articles/basic-graph.html) provided an example
#' how to prepare the data for the function and the output of this function.
#'
#' @param static_prob list of raster containing probability map of each
#' stationary period. The metadata of `static_prob` needs to include the flight
#' information to the next stationary period in the metadata `flight`.
#' @param thr_prob_percentile threshold of percentile (see explanation above)
#' @param thr_gs threshold of groundspeed (km/h)  (see explanation above)
#' @return graph as a list (see description above)
#' @export
graph_create <- function(static_prob,
                         thr_prob_percentile = .99,
                         thr_gs = 150) {

  # Check input
  stopifnot(is.list(static_prob))
  stopifnot(inherits(static_prob[[1]], "RasterLayer"))
  stopifnot("flight" %in%
    names(raster::metadata(static_prob[[1]])))
  stopifnot(is.numeric(thr_prob_percentile))
  stopifnot(length(thr_prob_percentile) == 1)
  stopifnot(thr_prob_percentile >= 0 & thr_prob_percentile <= 1)
  stopifnot(is.numeric(thr_gs))
  stopifnot(length(thr_gs) == 1)
  stopifnot(thr_gs >= 0)

  # compute size
  sz <- c(nrow(static_prob[[1]]), ncol(static_prob[[1]]), length(static_prob))
  nll <- sz[1] * sz[2]

  # convert raster into normalized matrix
  static_prob_n <- lapply(static_prob, function(x) {
    probt <- raster::as.matrix(x)
    probt[is.na(probt)] <- 0
    probt / sum(probt, na.rm = T)
  })

  tmp <- unlist(lapply(static_prob_n, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "The `static_prob` provided has a probability map equal to zero",
      "for the stationay period: ", which(tmp)
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

  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "Using the `thr_prob_percentile` of ", thr_prob_percentile,
      " provided, there are not any nodes left for the stationay period: ",
      which(tmp)
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

  tmp <- (thr_gs * utils::head(flight_duration, -1) / resolution) < 1
  if (any(tmp)) {
    stop(paste0("The flight duration provided is too small for the stationay
                period: ", which(tmp)))
  }

  # filter the pixels which are not in reach of any location of the previous
  # and next stationary period
  cond <- T
  while (cond) {
    n_old <- sum(unlist(lapply(nds, sum)))
    for (i_s in seq_len(sz[3] - 1)) {
      nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
      if (sum(nds[[i_s + 1]]) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary",
          "distance, there are not any nodes left at stationay period ",
          i_s + 1, " from stationay period ", i_s
        ))
      }
    }
    for (i_sr in seq_len(sz[3] - 1)) {
      i_s <- sz[3] - i_sr + 1
      nds[[i_s - 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s - 1] * thr_gs & nds[[i_s - 1]]
      if (sum(nds[[i_s - 1]]) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary",
          "distance, there are not any nodes left at stationay period ",
          i_s - 1, " from stationay period ", i_s
        ))
      }
    }
    n_new <- sum(unlist(lapply(nds, sum)))
    if (n_new == n_old) {
      cond <- F
    }
  }

  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary",
      "distance, there are not any nodes left"
    ))
  }

  # Identify equipement and retrival
  equipement <- which(nds[[1]] == T)
  retrival <- which(nds[[sz[3]]] == T) + (sz[3] - 1) * nll


  # Create the graph list from nds together with the exact groundspeed
  gr <- list()
  nds_sum <- unlist(lapply(nds, sum))
  nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
  progress_bar(0, max = sum(nds_expend_sum))
  for (i_s in seq_len(sz[3] - 1)) {
    # find all the possible equipment and target based on nds and expand to
    # all possible combination
    grt <- expand.grid(
      s = as.integer(which(nds[[i_s]]) + (i_s - 1) * nll),
      t = as.integer(which(nds[[i_s + 1]]) + i_s * nll)
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
      stop(paste0("Using the `thr_gs` of ", thr_gs, " km/h provided with the
                  exact distance of edges, there are not any nodes left for
                  the stationay period: ", i_s))
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
    gs_arg <- (450-gs_bearing) %% 360
    grt$gs <- gs_abs[id] * cos(gs_arg * pi / 180) +
      1i * gs_abs[id] * sin(gs_arg * pi / 180)

    # assign the static probability of the target node (pressure * light)
    grt$ps <- static_prob_n[[i_s + 1]][grt$t - i_s * nll]

    # add the edges from this stationary period to all others
    gr[[i_s]] <- grt

    if (sum(id) == 0) {
      stop(paste0("Using the `thr_gs` of ", thr_gs, " km/h provided with the
                  exact distance of edges, there are not any nodes left for
                  the stationay period: ", i_s))
    }
    progress_bar(sum(nds_expend_sum[seq(1, i_s)]),
      max = sum(nds_expend_sum),
      text = paste0("| sta = ", i_s, "/", sz[3] - 1)
    )
  }

  # convert gr to a list to a graph list with basic information
  grl <- as.list(do.call("rbind", gr))
  grl$sz <- sz
  grl$equipement <- equipement
  grl$retrival <- retrival

  # Trim
  grl <- graph_trim(grl)

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

  return(grl)
}



#' Trim a graph
#'
#'
#'
#' @param grl graph constructed with `graph_create()`
#' @return graph trimmed
graph_trim <- function(grl) {
  for (i in seq_len(grl$sz[3])) {
    unique_s <- c(grl$retrival, unique(grl$s))
    unique_t <- c(grl$equipement, unique(grl$t))

    unique_s_new <- unique_s[unique_t %in% unique_s]
    unique_t_new <- unique_t[unique_s %in% unique_t]

    id <- grl$s %in% unique_s_new & grl$t %in% unique_t_new
    if (all(id)) {
      break
    }
    grl$s <- grl$s[id]
    grl$t <- grl$t[id]
    grl$gs <- grl$gs[id]
    grl$ps <- grl$ps[id]
  }

  if (length(grl$s) == 0) {
    stop(paste0("Triming in the graph resulted in an empty graph"))
  }

  return(grl)
}

#' Add windspeed and airspeed
#'
#' Read NetCDF file downloaded on your computer and add the average
#' windspeed experienced by the bird and the corresponding airspeed for each
#' edge of the graph.
#'
#' See the vignette [Graph with wind](./articles/wind-graph.html) for
#' explanations and example on how to download the NetCDF file.
#'
#' @param grl graph constructed with `graph_create()`
#' @param pressure pressure data from a PAM logger. This data.frame needs to
#' contains `date` as POSIXt and `obs` in hPa. It is best practice to use
#' `pam_read()` and `pam_sta()` to build this data.frame.
#' @param filename character of the path where to find the netCDF file.
#' @param thr_as threshold of airspeed (km/h)
#' @return graph as a list with windspeed and airspeed as `ws` and `as`
#' respectively (see `graph_create()` for more detail on the graph returned)
#' @export
graph_add_wind <- function(grl, pressure, filename, thr_as = Inf) {
  stopifnot(is.list(grl))
  stopifnot(c("s", "t", "gs", "sz", "lat", "lon", "flight") %in% names(grl))
  stopifnot(length(grl$s) > 0)
  stopifnot(is.data.frame(pressure))
  stopifnot("date" %in% names(pressure))
  stopifnot(inherits(pressure$date, "POSIXt"))
  stopifnot("obs" %in% names(pressure))
  stopifnot(is.numeric(pressure$obs))
  stopifnot(is.character(filename))
  stopifnot(file.exists(paste0(filename, "1.nc")))
  stopifnot(is.numeric(thr_as))
  stopifnot(length(thr_as) == 1)
  stopifnot(thr_as >= 0)

  # Extract the index in lat, lon, sta from the source and target of all edges
  s <- arrayInd(grl$s, grl$sz)
  t <- arrayInd(grl$t, grl$sz)

  # Prepare the matrix of speed to return
  uv <- matrix(NA, nrow = length(grl$s), ncol = 2)

  # Start progress bar
  nds_expend_sum <- table(s[, 3])
  progress_bar(0,
    max = sum(nds_expend_sum),
    text = paste0("| sta = ", 0, "/", grl$sz[3] - 1)
  )

  # Loop through the stationary period kept in the graph
  for (i1 in seq_len(grl$sz[3] - 1)) {

    # Extract the flight information from the current sta to the next one
    # considered in the graph. It can be the next, or if some sta are skipped
    # at construction, it can contains multiples flights
    fl_s <- grl$flight[[i1]]

    # Extract the duration of each flights.
    fl_s_dur <- as.numeric(difftime(fl_s$end, fl_s$start, units = "hours"))

    # Determine the id of edges of the graph corresponding to this/these
    # flight(s).
    st_id <- which(s[, 3] == i1)

    # We are assuming that the bird flight as a straight line between the source
    # and the target node of each edge.
    # If multiple flights happen during this transition, we assume that the bird
    # flew with a cosz[3]nt groundspeed during each flight, thus considering its
    # stop-over position to be spread according to the flight duration. This
    # does not account for habitat, so that it would assume a bird can stop over
    # water.
    # While we could improve this part of the code to assume cosz[3]nt airspeed
    # rather than groundspeed, we suggest to create the graph considering all
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

      # As ERA5 data is available every hour, we build a one hour resolution
      # timeserie including the start and end time of the flight. Thus, we first
      # round the start end end time.
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)

      # We assume that the bird is moving with a constant groundspeed between
      # `flight$start` and `flight$end`. Using a linear interpolation, we
      # extract the position (lat, lon) at every hour on `t_q`. Extrapolation
      # outside (before the bird departure or after he arrived) is with a
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

      # As we are interesting in the average windspeed experienced during the
      # entire flight, we need to find the weights of each 1hr interval
      # extracted from ERA5. We can estimate these weight assuming a linear
      # integration of the time (trapezoidal rule) or a step integration
      # (Riemann sum)

      # Linear integration
      w <- numeric(length(t_q))
      stopifnot(length(w) > 2)
      alpha <- 1 - as.numeric(difftime(fl_s$start[i2], t_q[1],
        units = "hours"
      ))
      stopifnot(alpha >= 0 & alpha <= 1)
      w[c(1, 2)] <- w[c(1, 2)] + c(alpha, 1 - alpha) * alpha
      alpha <- 1 - as.numeric(difftime(utils::tail(t_q, 1), fl_s$end[i2],
        units = "hours"
      ))
      stopifnot(alpha >= 0 & alpha <= 1)
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
        # find the two pressure level to query (one above, one under) based on
        # the geolocator pressure at this timestep
        pres_obs <- pressure$obs[pressure$date == t_q[i3]]
        df <- pres_obs - pres
        df[df < 0] <- NA
        id_pres <- which.min(df)
        # if the pressure is higher than the highest level
        # (i.e. bird below the ground level pressure), we extract only the last
        # layer
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

        # Interpolation the u- and v- component at the interpolated position at
        # the current time step.
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
      # Compute the average wind component of the flight accounting for the
      # weighting scheme
      u_sta[i2, ] <- colSums(u_int * w)
      v_sta[i2, ] <- colSums(v_int * w)

      progress_bar(sum(nds_expend_sum[seq(1, i_s)]),
        max = sum(nds_expend_sum),
        text = paste0("| sta = ", i_s, "/", grl$sz[3] - 1)
      )
    }
    # Compute the average  over all the flight of the transition accounting for
    # the duration of the flight.
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
      "Using the `thr_as` of ", thr_as, " km/h provided with the
                  exact distance of edges, there are not any nodes left for
                  the stationay period: ", paste(sta_pass, collapse = ", "),
      " with a minimum airspeed of ",
      min(abs(grl$as[s[, 3] == sta_pass])), " km/h"
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
#' @param grl graph constructed with `geopressure_graph_create()`
#' @return list of raster of the marginal probability at each stationary period
#' @export
graph_marginal <- function(grl) {

  # number of nodes in the 3d grid
  n <- prod(grl$sz)

  # matrix of forward transition
  trans_f <- Matrix::sparseMatrix(grl$s, grl$t, x = grl$p, dims = c(n, n))

  # matrix of backward transition
  trans_b <- Matrix::sparseMatrix(grl$t, grl$s, x = grl$p, dims = c(n, n))

  # forward mapping of marginal probability
  map_f <- Matrix::sparseMatrix(1, grl$equipement, x = 1, dims = c(1, n))

  # backward mapping of marginal probability
  map_b <- Matrix::sparseMatrix(1, grl$retrival, x = 1, dims = c(1, n))

  # build iterativelly the marginal probability backward and forward by re-using
  # the mapping computed for previous stationary period. Set the equipement and
  # retrival site in each loop
  for (i_s in seq_len(grl$sz[3] - 1)) {
    map_f[1, grl$equipement] <- 1
    map_f <- map_f %*% trans_f

    map_b[1, grl$retrival] <- 1
    map_b <- map_b %*% trans_b
  }
  # add the retrival and equipement at the end to finish it
  map_f[1, grl$equipement] <- 1
  map_b[1, grl$retrival] <- 1

  # combine the forward and backward
  map <- map_f * map_b

  # reshape mapping as a full (non-sparce matrix of correct size)
  map <- as.matrix(map)
  dim(map) <- grl$sz

  # convert to raster
  static_prob_marginal <- list()
  for (i_s in seq_len(dim(map)[3])) {
    static_prob_marginal[[i_s]] <- raster::raster(grl$extent,
      resolution = grl$resolution,
      vals = map[, , i_s]
    )
    raster::crs(static_prob_marginal[[i_s]]) <-
      "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }

  return(static_prob_marginal)
}




#' Simulation of trajectory
#'
#' This function generate simulated path from a graph
#'
#' @param grl graph constructed with `geopressure_graph_create()`
#' @param nj number of simulation
#' @return list of the simulated path
#' @export
graph_simulation <- function(grl, nj = 100) {

  # number of nodes in the 3d grid
  n <- prod(grl$sz)
  nll <- grl$sz[1] * grl$sz[2]

  # Initialize path. As we will simulate the path chronological order, only the
  # first equipement site needs to be set.
  path <- matrix(ncol = grl$sz[3], nrow = nj)
  path[, 1] <- grl$equipement

  # Find the stationary index of all the source so that only the edges from a
  # specific stationay period can be easily query
  s_id <- arrayInd(grl$s, grl$sz)

  # As we will simulate in forward chronolofical order, we will be able to
  # create map_f inside the simulation. However, map_b needs to be computed for
  # all stationary period in advence, starting by the last stationary period
  # and moving backward in time as follow
  map_b <- list()
  map_b[[grl$sz[3]]] <- Matrix::sparseMatrix(1, grl$retrival,
    x = 1,
    dims = c(1, n)
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

    # build the forward mapping from the simulated nodes of the previous
    # stationary period to the current one using trans_f
    map_f <- Matrix::sparseMatrix(seq_len(nj), path[, i_sta - 1],
      x = 1,
      dims = c(nj, n)
    ) %*% trans_f

    # Combine forward and backward and samples
    ids <- apply(map_f[, nll * (i_sta - 1) + (1:nll)], 1, function(x) {
      map <- x * map_b[[i_sta]][nll * (i_sta - 1) + (1:nll)]
      sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
    })

    #
    path[, i_sta] <- ids + nll * (i_sta - 1)

    # Update progress bar
    progress_bar(i_sta, max = grl$sz[3])
  }

  return(graph_path2lonlat(path, grl))
}


#' Find the lattitude and longitude from a path index
#'
#' @param path_id list or matrix of node index
#' @param grl graph constructed with `geopressure_graph_create()`
#' @return list of the path with latitude and longitude and index fo the the path provided
#' @export
graph_path2lonlat <- function(path_id, grl) {
  ind <- arrayInd(path_id, grl$sz)
  p <- list()
  p$id <- path_id
  p$lat <- grl$lat[ind[, 1]]
  dim(p$lat) <- dim(p$id)
  p$lon <- grl$lon[ind[, 2]]
  dim(p$lon) <- dim(p$id)
  return(p)
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
