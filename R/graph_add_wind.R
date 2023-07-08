#' Add windspeed and airspeed
#'
#' @description
#' Reads the NetCDF files downloaded in `directory` and adds the average windspeed experienced by the
#' bird, and the corresponding airspeed for each edge of the graph.
#'
#' See the [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data) for
#' explanations and an example on how to download the `NetCDF` files from ERA-5.
#'
#' @param graph graph constructed with [`graph_create()`]
#' @param pressure pressure data from a data logger. This data.frame needs to contain `date` as
#' POSIXt and `value` in hPa. It is good practice to use [`tag_create()`] and [`tag_label_stap()`] to build
#' this data.frame.
#' @param directory Character of the path where the netCDF files can be found.
#' @param thr_as Threshold of airspeed (km/h).
#' @return Graph as a list with windspeed and airspeed as `ws` and `as` respectively (see
#' [`graph_create()`] for more details on the graph returned).
#' @seealso [`graph_create()`], [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#add-wind-to-graph)
#' @export
graph_add_wind <- function(graph,
                           pressure,
                           directory = glue::glue("data/5_wind_graph/{graph$id}/"),
                           thr_as = Inf) {
  assertthat::assert_that(is.graph(graph))
  assertthat::assert_that(assertthat::has_name(
    graph, c("s", "t", "gs", "sz", "lat", "lon", "flight")
  ))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "value")))
  assertthat::assert_that(assertthat::is.time(pressure$date))
  assertthat::assert_that(is.numeric(pressure$value))
  assertthat::assert_that(is.character(directory))
  file_prefix <- glue::glue("{graph$id}_")
  assertthat::assert_that(file.exists(file.path(directory, glue::glue("{file_prefix}1.nc"))))
  assertthat::assert_that(is.numeric(thr_as))
  assertthat::assert_that(length(thr_as) == 1)
  assertthat::assert_that(thr_as >= 0)

  # Extract the index in lat, lon, stap from the source and target of all edges
  s <- arrayInd(graph$s, graph$sz)
  t <- arrayInd(graph$t, graph$sz)

  # Prepare the matrix of speed to return
  uv <- matrix(NA, nrow = length(graph$s), ncol = 2)

  # Check that all the files of wind_speed exist and match the data request
  for (i1 in seq_len(graph$sz[3] - 1)) {
    fl_s <- graph$flight[[i1]]
    for (i2 in seq_len(length(fl_s$stap_s))) {
      i_s <- fl_s$stap_s[i2]

      full_path <- file.path(directory, glue::glue("{file_prefix}{i_s}.nc"))
      if (!file.exists(full_path)) {
        cli::cli_abort(c(x = "No wind file {.file {full_path}}"))
      }
      nc <- ncdf4::nc_open(full_path)

      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60, origin = "1900-01-01", tz = "UTC")
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"), tz = "UTC")
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"), tz = "UTC")
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        cli::cli_abort(c(x = "Time not matching for {.file {directory}{i_s}.nc}"))
      }

      pres <- ncdf4::ncvar_get(nc, "level")
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)
      pres_value <- pressure$value[pressure$date > t_s & pressure$date < t_e]
      if (length(pres_value) == 0 ||
        !(min(pres) <= min(pres_value) &&
          max(pres) >= min(1000, max(pres_value)))) {
        cli::cli_abort(c(x = "Pressure not matching for {.file {directory}{i_s}.nc}"))
      }

      # Check if spatial extend match
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      if (min(graph$lat) < min(lat) || max(graph$lat) > max(lat) ||
        min(graph$lon) < min(lon) || max(graph$lon) > max(lon)) {
        cli::cli_abort(c(x = "Spatial extend not matching for {.file {directory}{i_s}.nc}"))
      }

      # Check if flight duration is
      if (fl_s$start[i2] >= fl_s$end[i2]) {
        cli::cli_abort(c(
          x = "Flight starting on stap {fl_s$stap_s[i2]} has a start time equal or greater than \\
                         the end time. Please review your labelling file."
        ))
      }
    }
  }

  # Start progress bar
  nds_expend_sum <- table(s[, 3])
  cli::cli_progress_bar(0, total = sum(nds_expend_sum))

  # Loop through the stationary period kept in the graph
  for (i1 in seq_len(graph$sz[3] - 1)) {
    # Extract the flight information from the current stap to the next one considered in the graph.
    # It can be the next, or if some stap are skipped at construction, it can contains multiples
    # flights
    fl_s <- graph$flight[[i1]]

    # Extract the duration of each flights.
    fl_s_dur <- as.numeric(difftime(fl_s$end, fl_s$start, units = "hours"))

    # Determine the id of edges of the graph corresponding to this/these flight(s).
    st_id <- which(s[, 3] == i1)

    # We are assuming that the bird flight as a straight line between the source and the target node
    # of each edge. If multiple flights happen during this transition, we assume that the bird flew
    # with a constant groundspeed during each flight, thus considering its cli::cli_abort-over position to be
    # spread according to the flight duration. This does not account for habitat, so that it would
    # assume a bird can cli::cli_abort over water. While we could improve this part of the code to assume
    # constant airspeed rather than groundspeed, we suggest to create the graph considering all
    # cli::cli_abortovers.
    ratio_stap <- as.matrix(c(0, cumsum(fl_s_dur) / sum(fl_s_dur)))

    # Prepare the u- and v- windspeed for each flight (row) and edge (col)
    u_stap <- matrix(NA, nrow = length(fl_s$stap_s), ncol = length(st_id))
    v_stap <- matrix(NA, nrow = length(fl_s$stap_s), ncol = length(st_id))

    # Loop through each flight of the transition
    for (i2 in seq_len(length(fl_s$stap_s))) {
      # Find the stationary period ID from this specific flight (source)
      i_s <- fl_s$stap_s[i2]

      # Read the netCDF file
      nc <- ncdf4::nc_open(file.path(directory, i_s, ".nc"))

      # Read data from netCDF file and convert the time of data to posixt
      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60,
        origin = "1900-01-01", tz = "UTC"
      )
      pres <- ncdf4::ncvar_get(nc, "level")
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")

      # Find the start and end latitude and longitude of each edge
      lat_s <- graph$lat[s[st_id, 1]] +
        ratio_stap[i2] * (graph$lat[t[st_id, 1]] - graph$lat[s[st_id, 1]])
      lon_s <- graph$lon[s[st_id, 2]] +
        ratio_stap[i2] * (graph$lon[t[st_id, 2]] - graph$lon[s[st_id, 2]])
      lat_e <- graph$lat[s[st_id, 1]] +
        ratio_stap[i2 + 1] * (graph$lat[t[st_id, 1]] - graph$lat[s[st_id, 1]])
      lon_e <- graph$lon[s[st_id, 2]] +
        ratio_stap[i2 + 1] * (graph$lon[t[st_id, 2]] - graph$lon[s[st_id, 2]])

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

      assertthat::assert_that(all(!is.na(w)))

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
        pres_value <- stats::approx(pressure$date, pressure$value, t_q[i3])$y
        df <- pres_value - pres
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
          w2 <- abs(pres[id_pres + c(0, 1)] - pres_value)
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
        assertthat::assert_that(all(!is.na(tmp)))
        u_int[i3, ] <- tmp[id_uniq]
        tmp <- pracma::interp2(rev(lat[id_lat]), lon[id_lon],
          v[, rev(seq_len(ncol(u)))],
          ll_int_uniq[, 1], ll_int_uniq[, 2],
          method = "linear"
        )
        assertthat::assert_that(all(!is.na(tmp)))
        v_int[i3, ] <- tmp[id_uniq]
      }
      # Compute the average wind component of the flight accounting for the weighting scheme
      u_sta[i2, ] <- colSums(u_int * w)
      v_sta[i2, ] <- colSums(v_int * w)

      cli::cli_progress_update(set = sum(nds_expend_sum[seq(1, i1)]))
    }
    # Compute the average  over all the flight of the transition accounting for the duration of the
    # flight.
    uv[st_id, 1] <- colSums(u_stap * fl_s_dur / sum(fl_s_dur))
    uv[st_id, 2] <- colSums(v_stap * fl_s_dur / sum(fl_s_dur))
  }

  # save windspeed in complex notation and convert from m/s to km/h
  graph$ws <- (uv[, 1] + 1i * uv[, 2]) / 1000 * 60 * 60

  # compute airspeed
  as <- graph$gs - graph$ws

  # filter edges based on airspeed
  id <- abs(as) <= thr_as
  sta_pass <- which(!(seq_len(graph$sz[3] - 1) %in% unique(s[id, 3])))
  if (length(sta_pass) > 0) {
    cli::cli_abort(c(
      x =
        "Using the {.val thr_as} of {thr_as} km/h provided with the exact distance of edges, there \\
      are not any nodes left for the stationary period: {sta_pass} with a minimum airspeed of \\
      {min(abs(as[s[, 3] == sta_pass]))} km/h."
    ))
  }

  graph$s <- graph$s[id]
  graph$t <- graph$t[id]
  graph$gs <- graph$gs[id]
  graph$ws <- graph$ws[id]
  graph$obs <- graph$obs[id]
  graph$param$thr_as <- thr_as

  return(graph)
}
