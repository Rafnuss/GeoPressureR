#' Retrieve ERA5 variable along edge
#'
#' @description
#' Reads the NetCDF files and extracts the variable requested along each flight defined by the
#' edges.
#'
#' - Time: linear interpolation using the resolution requested with `rounding_interval`
#' - Space: nearest neighbour interpolation by default or bi-linear with `pracma::interp2` if
#' `interp_spatial_linear=TRUE`
#' - Pressure/altitude: linear interpolation using the exact `pressure` values
#'
#' @param graph either a `tag` or a `graph` GeoPressureR object.
#' @param edge_s a index of the source node of the edge. Either a vector with 3D index or a matrix
#' of 3 columns, one for each dimension.
#' @param edge_t a index of the target node of the edge. Either a vector with 3D index or a matrix
#' of 3 columns, one for each dimension.
#' @param pressure pressure measurement of the associated `tag` data used to estimate the pressure
#' level (i.e., altitude) of the bird during the flights. This data.frame needs to contain `date` as
#' POSIXt and `value` in hPa.
#' @param variable list of the variables to extract from [the ERA5 pressure level
#' ](https://bit.ly/3BrwLBM) using the `shortName` notation: `"u"`, `"v"`,  `"t"`, `"cc"`, `"r"`,
#' `"w"`, `"ciwc"`, `"clwc"`, `"q"`, `"cswc"`, `"d"`, `"z"`, `"o3"`, `"pv"`, `"vo"`.
#' @param rounding_interval temporal resolution on which to query the variable (min). Default is to
#' match ERA5 native resolution (1hr).
#' @param interp_spatial_linear logical to interpolate the variable linearly over space, if `FALSE`
#' takes the nearest neighbour. ERA5 native resolution is 0.25°
#' @param return_averaged_variable logical to return the variable for each timestep or average for
#' the entire flight.
#' @param quiet logical to hide messages about the progress
#' @inheritParams tag_download_wind
#'
#' @return A data.frame with columns:
#' - `stap_s` id of the source/origin stationary period
#' - `stap_t` id of the target/destination stationary period
#' - `s` node id of the source (same as/similar to `edge_s`)
#' - `t` node id of the target (same as/similar to `edge_t`)
#' - `lat_s` latitude of the source
#' - `lat_t` latitude of the target
#' - `lon_s` longitude of the source
#' - `lon_t` longitude of the target
#' - `start` start datetime of the flight
#' - `end` end datetime of the flight
#' - `duration` flight duration
#' - `n` number of flight
#' - `distance` distance of the flight
#' - `bearing` bearing of the flight
#' - `gs` groundspeed
#' - `ws` windspeed (if `graph` provided)
#'
#' @seealso [GeoPressureManual](
#' https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
#' @export
edge_add_wind <- function(
    graph,
    edge_s,
    edge_t,
    pressure = NULL,
    variable = c("u", "v"),
    rounding_interval = 60,
    interp_spatial_linear = FALSE,
    return_averaged_variable = FALSE,
    file = \(stap_id, tag_id) glue::glue( # nolint
      "./data/wind/{tag_id}/{tag_id}_{stap_id}.nc"
    ),
    quiet = FALSE) {
  if (is.null(pressure) && inherits(graph, "tag")) {
    pressure <- graph$pressure
  }

  tag_id <- graph$param$id

  edge_add_wind_check(graph,
    pressure = pressure,
    variable = variable,
    file = file
  )

  # Compute lat-lon coordinate of the grid
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)

  # Compute flight from stap
  flight <- stap2flight(graph$stap, format = "list")

  # Check edges
  if (!is.matrix(edge_s)) {
    edge_s <- arrayInd(edge_s, c(g$dim, nrow(graph$stap)))
  }
  if (!is.matrix(edge_t)) {
    edge_t <- arrayInd(edge_t, c(g$dim, nrow(graph$stap)))
  }

  assertthat::assert_that(assertthat::are_equal(dim(edge_s), dim(edge_t)))
  assertthat::assert_that(assertthat::are_equal(dim(edge_s)[2], 3))
  assertthat::assert_that(assertthat::are_equal(dim(edge_t)[2], 3))
  assertthat::assert_that(assertthat::are_equal(edge_t[, 1], as.integer(edge_t[, 1])))
  assertthat::assert_that(assertthat::are_equal(edge_t[, 2], as.integer(edge_t[, 2])))
  assertthat::assert_that(assertthat::are_equal(edge_t[, 3], as.integer(edge_t[, 3])))
  assertthat::assert_that(assertthat::are_equal(edge_s[, 1], as.integer(edge_s[, 1])))
  assertthat::assert_that(assertthat::are_equal(edge_s[, 2], as.integer(edge_s[, 2])))
  assertthat::assert_that(assertthat::are_equal(edge_s[, 3], as.integer(edge_s[, 3])))
  assertthat::assert_that(all(edge_t[, 3] > 1 & edge_t[, 3] <= max(graph$stap$stap_id)))
  assertthat::assert_that(all(edge_s[, 3] >= 1 & edge_s[, 3] < max(graph$stap$stap_id)))
  assertthat::assert_that(all(edge_t[, 1] >= 1 & edge_t[, 1] <= g$dim[1]))
  assertthat::assert_that(all(edge_t[, 2] >= 1 & edge_t[, 2] <= g$dim[2]))
  assertthat::assert_that(all(edge_s[, 1] >= 1 & edge_s[, 1] <= g$dim[1]))
  assertthat::assert_that(all(edge_s[, 2] >= 1 & edge_s[, 2] <= g$dim[2]))

  # Keep only the ID for the file function, remove the rest to save memory
  graph <- list(param = list(id = graph$param$id))
  gc()

  # Prepare the matrix of speed to return
  if (return_averaged_variable) {
    var <- matrix(NA, nrow = nrow(edge_s), ncol = length(variable))
  } else {
    var <- list()
    for (var_i in seq_len(length(variable))) {
      var[[var_i]] <- vector("list", length(flight))
    }
  }

  # Start progress bar
  if (!quiet) {
    table_edge_s <- table(edge_s[, 3])
    i_stap <- 0
    cli::cli_progress_bar(
      "Compute wind speed for edges of stationary period:",
      format = "{cli::col_blue(cli::symbol$info)} {cli::pb_name} {i_stap}/{length(flight)} \\
      {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "{cli::col_green(cli::symbol$tick)} Compute wind speed for edges of \\
      stationary periods {cli::col_white('[', cli::pb_elapsed, ']')}",
      clear = FALSE,
      total = sum(table_edge_s)
    )
  }

  list_st_id <- split(seq(1, nrow(edge_s)), edge_s[, 3])

  # Loop through the stationary period kept in the graph
  for (i_stap in seq_len(length(flight))) {
    # Extract the flight information from the current stap to the next one considered in the graph.
    # It can be the next, or if some stap are skipped at construction, it can contains multiples
    # flights
    fl_s <- flight[[i_stap]]

    # Determine the id of edges of the graph corresponding to this/these flight(s).
    st_id <- list_st_id[[i_stap]]

    # We are assuming that the bird flight as a straight line between the source and the target node
    # of each edge. If multiple flights happen during this transition, we assume that the bird flew
    # with a constant groundspeed during each flight, thus considering its stopover position to be
    # spread according to the flight duration. This does not account for habitat, so that it would
    # assume a bird can stop over water. While we could improve this part of the code to assume
    # constant airspeed rather than groundspeed, we suggest to create the graph considering all
    # stopovers.
    ratio_stap <- as.matrix(c(0, cumsum(fl_s$duration) / sum(fl_s$duration)))

    if (return_averaged_variable) {
      # Prepare the u- and v- windspeed for each flight (row) and edge (col)
      var_stap <- list()
      for (var_i in seq_len(length(variable))) {
        var_stap[[var_i]] <- matrix(NA, nrow = length(fl_s$stap_s), ncol = length(st_id))
      }
    } else {
      for (var_i in seq_len(length(variable))) {
        var[[var_i]][[i_stap]] <- vector("list", nrow(fl_s))
      }
    }

    # Loop through each flight of the transition
    for (i_fl in seq_len(nrow(fl_s))) {
      # Find the stationary period ID from this specific flight (source)
      i_s <- fl_s$stap_s[i_fl]

      # Read the netCDF file
      nc <- ncdf4::nc_open(file(i_s, tag_id))

      # Read data from netCDF file and convert the time of data to posixt
      # Fix to use the correct time variable ("time" until the new CDS, then "valid_time")
      if ("time" %in% names(nc$dim)) {
        time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60,
          origin = "1900-01-01", tz = "UTC"
        )
      } else if ("valid_time" %in% names(nc$dim)) {
        time <- as.POSIXct(ncdf4::ncvar_get(nc, "valid_time"), origin = "1970-01-01", tz = "UTC")
      } else {
        cli::cli_abort(c(
          x = "Time variable not found in {.file {file(i_s, tag_id)}}",
          "i" = "Available variable{?s} {?is/are} {.var {names(nc$dim)}}."
        ))
      }
      pres_var <- names(nc$dim)[grepl("*level", names(nc$dim))]
      pres <- ncdf4::ncvar_get(nc, pres_var)
      lat <- ncdf4::ncvar_get(nc, "latitude")
      dlat <- abs(lat[2] - lat[1])
      lon <- ncdf4::ncvar_get(nc, "longitude")
      dlon <- lon[2] - lon[1]

      # Find the start and end latitude and longitude of each edge
      lat_s <- g$lat[edge_s[st_id, 1]] +
        ratio_stap[i_fl] * (g$lat[edge_t[st_id, 1]] - g$lat[edge_s[st_id, 1]])
      lon_s <- g$lon[edge_s[st_id, 2]] +
        ratio_stap[i_fl] * (g$lon[edge_t[st_id, 2]] - g$lon[edge_s[st_id, 2]])
      lat_e <- g$lat[edge_s[st_id, 1]] +
        ratio_stap[i_fl + 1] * (g$lat[edge_t[st_id, 1]] - g$lat[edge_s[st_id, 1]])
      lon_e <- g$lon[edge_s[st_id, 2]] +
        ratio_stap[i_fl + 1] * (g$lon[edge_t[st_id, 2]] - g$lon[edge_s[st_id, 2]])

      # As ERA5 data is available every hour, we build a one hour resolution timeserie including the
      # start and end time of the flight. Thus, we first round the start end end time.

      # Round down to the lower n-minute interval
      t_s <- as.POSIXct(trunc(as.numeric(fl_s$start[i_fl]) / (60 * rounding_interval)) *
        (60 * rounding_interval), origin = "1970-01-01", tz = "UTC")
      t_e <- as.POSIXct(ceiling(as.numeric(fl_s$end[i_fl]) / (60 * rounding_interval)) *
        (60 * rounding_interval), origin = "1970-01-01", tz = "UTC")
      t_q <- seq(from = t_s, to = t_e, by = 60 * rounding_interval)

      # We assume that the bird is moving with a constant groundspeed between `flight$start` and
      # `flight$end`. Using a linear interpolation, we extract the position (lat, lon) at every hour
      # on `t_q`. Extrapolation outside (before the bird departure or after he arrived) is with a
      # nearest neighbour.

      dlat_se <- (lat_e - lat_s) / fl_s$duration[i_fl]
      dlon_se <- (lon_e - lon_s) / fl_s$duration[i_fl]
      w <- pmax(pmin(as.numeric(
        difftime(t_q, fl_s$start[i_fl], units = "hours")
      ), fl_s$duration[i_fl]), 0)
      w2 <- matrix(w, nrow = length(dlat_se), ncol = length(w), byrow = TRUE)
      lat_int <- lat_s + w2 * replicate(length(w), dlat_se)
      lon_int <- lon_s + w2 * replicate(length(w), dlon_se)

      rm(w2, dlat_se, dlon_se, w, lat_s, lon_s, lat_e, lon_e)
      gc()

      if (TRUE) { # we use w for both return_averaged_variable TRUE and FALSE
        # As we are interesting in the average windspeed experienced during the entire flight, we
        # need to find the weights of each 1hr interval extracted from ERA5. We can estimate these
        # weight assuming a linear integration of the time (trapezoidal rule) or a step integration
        # (Riemann sum)

        # Linear integration
        w <- numeric(length(t_q))
        assertthat::assert_that(length(w) > 1)

        alpha <- 1 - as.numeric(difftime(fl_s$start[i_fl], t_q[1], units = "mins")) /
          rounding_interval
        assertthat::assert_that(alpha >= 0 & alpha <= 1)
        w[c(1, 2)] <- w[c(1, 2)] + c(alpha, 1 - alpha) * alpha

        alpha <- 1 - as.numeric(difftime(utils::tail(t_q, 1), fl_s$end[i_fl], units = "mins")) /
          rounding_interval
        assertthat::assert_that(alpha >= 0 & alpha <= 1)
        w[length(w) - c(1, 0)] <- w[length(w) - c(1, 0)] + c(1 - alpha, alpha) * alpha

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
        # w <- difftime(pmin(pmax(t_q+60*60/2,fl_s$start[i_fl]),fl_s$end[i_fl]),
        #               pmin(pmax(t_q-60*60/2,fl_s$start[i_fl]),fl_s$end[i_fl]),
        #               units = "hours")
      }

      # Prepare the interpolated variable for each flight
      var_fl <- list()
      for (var_i in seq_len(length(variable))) {
        var_fl[[var_i]] <- matrix(NA, nrow = length(t_q), ncol = length(st_id))
      }

      p_q <- numeric(length(t_q))

      # Find the index of lat and lon
      if (!interp_spatial_linear) {
        lat_int_ind <- matrix(match(as.vector(round(lat_int * 4) / 4), lat), nrow = nrow(lat_int))
        lon_int_ind <- matrix(match(as.vector(round(lon_int * 4) / 4), lon), nrow = nrow(lon_int))
      }

      # Loop through the 1hr interval
      for (i_time in seq_len(length(t_q))) {
        # find the two pressure level to query (one above, one under) based on the geolocator
        # pressure at this timestep
        p_q[i_time] <- stats::approx(pressure$date, pressure$value, t_q[i_time], rule = 2)$y
        tmp <- which(pres <= p_q[i_time])
        id_pres <- tmp[which.min(abs(pres[tmp] - p_q[i_time]))]
        # if the pressure is higher than the highest level (i.e. bird below the ground level
        # pressure), we extract only the last layer
        n_pres <- ifelse(id_pres == length(pres), 1, 2)

        # find the two time step before and after the time step to query in ERA5
        tmp <- which(time <= t_q[i_time])
        id_time <- tmp[which.min(abs(time[tmp] - t_q[i_time]))]
        n_time <- ifelse(id_time == length(time) | time[id_time] == t_q[i_time], 1, 2)

        # Find the index of lat and longitude necessary
        id_lon <- which(lon >= (min(lon_int[, i_time]) - dlon) &
          (max(lon_int[, i_time]) + dlon) >= lon)
        id_lat <- which(lat >= (min(lat_int[, i_time]) - dlat) &
          (max(lat_int[, i_time]) + dlat) >= lat)

        # get the two maps of u- and v-
        var_nc <- list()
        for (var_i in seq_len(length(variable))) {
          var_nc[[var_i]] <- ncdf4::ncvar_get(nc, variable[var_i],
            start = c(id_lon[1], id_lat[1], id_pres, id_time),
            count = c(length(id_lon), length(id_lat), n_pres, n_time),
            collapse_degen = FALSE
          )
        }

        # Interpolate linearly along time
        if (n_time == 2) {
          w_time <- as.numeric(difftime(t_q[i_time], time[id_time], units = "hours")) /
            as.numeric(difftime(time[id_time + 1], time[id_time], units = "hours"))
          for (var_i in seq_len(length(variable))) {
            var_nc[[var_i]] <- var_nc[[var_i]][, , , 1] +
              w_time * (var_nc[[var_i]][, , , 2] - var_nc[[var_i]][, , , 1])
          }
        } else {
          for (var_i in seq_len(length(variable))) {
            var_nc[[var_i]] <- var_nc[[var_i]][, , , 1]
          }
        }

        # Interpolate linearly along altitude/pressure.
        if (n_pres == 2) {
          w_pres <- (p_q[i_time] - pres[id_pres]) / (pres[id_pres + 1] - pres[id_pres])
          for (var_i in seq_len(length(variable))) {
            var_nc[[var_i]] <- var_nc[[var_i]][, , 1] +
              w_pres * (var_nc[[var_i]][, , 2] - var_nc[[var_i]][, , 1])
          }
        }

        if (interp_spatial_linear) {
          # Interpolation the u- and v- component at the interpolated position at the current time
          # step.
          # Because lat_int and lon_int are so big, we round their value and only interpolate on the
          # unique value that are needed. Then, we give the interpolated value back to all the
          # lat_int lon_int dimension
          # Convert the coordinate to 1d to have a more efficient unique.
          ll_int_1d <- (round(lat_int[, i_time], 1) + 90) * 10 * 10000 +
            (round(lon_int[, i_time], 1) + 180) * 10 + 1
          ll_int_1d_uniq <- unique(ll_int_1d)

          lat_int_uniq <- ((ll_int_1d_uniq - 1) %/% 10000) / 10 - 90
          lon_int_uniq <- ((ll_int_1d_uniq - 1) %% 10000) / 10 - 180
          # CHeck that the transofmration is correct with
          # cbind((round(lat_int[, i_time], 1)+90)*10, (ll_int_1d - 1) %/% 10000)
          # cbind((round(lon_int[, i_time],1)+180)*10, (ll_int_1d - 1) %% 10000)
          # cbind(lat_int_uniq, lon_int_uniq, lat_int[, i_time], lon_int[, i_time])

          id_uniq <- match(ll_int_1d, ll_int_1d_uniq)

          for (var_i in seq_len(length(variable))) {
            tmp <- pracma::interp2(rev(lat[id_lat]), lon[id_lon],
              var_nc[[var_i]][, rev(seq_len(ncol(var_nc[[var_i]])))],
              lat_int_uniq, lon_int_uniq,
              method = "linear"
            )
            assertthat::assert_that(all(!is.na(tmp)))
            var_fl[[var_i]][i_time, ] <- tmp[id_uniq]
          }
        } else {
          # Take the closest value
          for (var_i in seq_len(length(variable))) {
            # Compute the index of lat, lon in the spatial extent extracted for var_nc
            lon_int_ind_off <- lon_int_ind[, i_time] - id_lon[1] + 1
            lat_int_ind_off <- lat_int_ind[, i_time] - id_lat[1] + 1

            # compute the 2d index
            ind <- (lat_int_ind_off - 1) * nrow(var_nc[[var_i]]) + lon_int_ind_off

            # Extract variable
            var_fl[[var_i]][i_time, ] <- var_nc[[var_i]][ind]
          }
        }
      }

      if (return_averaged_variable) {
        # Compute the average wind component of the flight accounting for the weighting scheme
        for (var_i in seq_len(length(variable))) {
          var_stap[[var_i]][i_fl, ] <- colSums(var_fl[[var_i]] * w)
        }
      } else {
        for (var_i in seq_len(length(variable))) {
          var[[var_i]][[i_stap]][[i_fl]] <- data.frame(
            edge_id = rep(st_id, each = length(t_q)),
            val = as.vector(var_fl[[var_i]]),
            pressure = rep(p_q, length(st_id)),
            date = rep(t_q, length(st_id)),
            w = rep(w, length(st_id))
          )
          var[[var_i]][[i_stap]][[i_fl]]$var <- variable[var_i]
          if (interp_spatial_linear) {
            var[[var_i]][[i_stap]][[i_fl]]$lat <- as.vector(round(t(lat_int), 1))
            var[[var_i]][[i_stap]][[i_fl]]$lon <- as.vector(round(t(lon_int), 1))
          } else {
            var[[var_i]][[i_stap]][[i_fl]]$lat <- as.vector(lat[t(lat_int_ind)])
            var[[var_i]][[i_stap]][[i_fl]]$lon <- as.vector(lon[t(lon_int_ind)])
          }
        }
      }

      # Close the netCDF file
      ncdf4::nc_close(nc)

      rm(lat_int, lon_int)
      gc()
    }

    if (return_averaged_variable) {
      # Compute the average over all the flight of the transition accounting for the duration of the
      # flight.
      for (var_i in seq_len(length(variable))) {
        var[st_id, var_i] <- colSums(var_stap[[var_i]] * fl_s$duration / sum(fl_s$duration))
      }
    }
    if (!quiet) {
      cli::cli_progress_update(set = sum(table_edge_s[seq(1, i_stap)]), force = TRUE)
    }
  }

  # Final cleanup
  rm(list_st_id, flight, g, var_stap, fl_s, st_id, ratio_stap)
  gc()
  if (!quiet) {
    rm(table_edge_s)
  }
  gc()

  if (!return_averaged_variable) {
    var <- do.call(rbind, unlist(unlist(var, recursive = FALSE), recursive = FALSE))
  }

  return(var)
}


#' @noRd
edge_add_wind_check <- function(
    graph,
    pressure = NULL,
    variable = c("u", "v"),
    file = \(stap_id, tag_id) glue::glue( # nolint
      "./data/wind/{tag_id}/{tag_id}_{stap_id}.nc"
    )) {
  assertthat::assert_that(inherits(graph, "tag") | inherits(graph, "graph"))

  tag_id <- graph$param$id

  # Compute lat-lon coordinate of the grid
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)

  # Compute flight from stap
  flight <- stap2flight(graph$stap, format = "list")

  # Check pressure
  if (is.null(pressure) && inherits(graph, "tag")) {
    pressure <- graph$pressure
  }
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "value")))
  assertthat::assert_that(assertthat::is.time(pressure$date))
  assertthat::assert_that(is.numeric(pressure$value))

  # Check file
  assertthat::assert_that(is.function(file))

  # Check that all the files of wind_speed exist and match the data request
  for (i_stap in seq_len(length(flight))) {
    fl_s <- flight[[i_stap]]
    for (i_fl in seq_len(length(fl_s$stap_s))) {
      i_s <- fl_s$stap_s[i_fl]

      if (!file.exists(file(i_s, tag_id))) {
        cli::cli_abort(c(x = "No wind file {.file {file(i_s, tag_id)}}"))
      }
      nc <- ncdf4::nc_open(file(i_s, tag_id))

      # Check that the variables are present
      available_variable <- names(nc$var)
      tmp <- !(variable %in% available_variable)
      if (any(tmp)) {
        cli::cli_abort(c(
          x = "Wind file does not contains the variable{?s}: {.var {variable[tmp]}}.",
          "i" = "Available variable{?s} {?is/are} {.var {available_variable}}."
        ))
      }

      # Check that the time is matching
      if ("time" %in% names(nc$dim)) {
        time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60,
          origin = "1900-01-01", tz = "UTC"
        )
      } else if ("valid_time" %in% names(nc$dim)) {
        time <- as.POSIXct(ncdf4::ncvar_get(nc, "valid_time"), origin = "1970-01-01", tz = "UTC")
      } else {
        cli::cli_abort(c(
          x = "Time variable not found in {.file {file(i_s, tag_id)}}",
          "i" = "Available variable{?s} {?is/are} {.var {names(nc$dim)}}."
        ))
      }
      t_s <- as.POSIXct(format(fl_s$start[i_fl], "%Y-%m-%d %H:00:00"), tz = "UTC")
      t_e <- as.POSIXct(format(fl_s$end[i_fl] + 60 * 60, "%Y-%m-%d %H:00:00"), tz = "UTC")
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        cli::cli_abort(c(
          x = "Time between graph data and the wind file ({.file {file(i_s, tag_id)}}) does not
          match.",
          "!" = "You might have modified your stationary periods without updating your wind file? ",
          ">" = "If so, run {.run tag_download_wind(tag)}"
        ))
      }

      pres_var <- names(nc$dim)[grepl("*level", names(nc$dim))]
      pres <- ncdf4::ncvar_get(nc, pres_var)
      pres_value <- pressure$value[pressure$date > t_s & pressure$date < t_e]
      if (length(pres_value) == 0 ||
        !(min(pres) <= min(pres_value) &&
          max(pres) >= min(1000, max(pres_value)))) {
        cli::cli_abort(c(
          x = "Time between graph data and the wind file ({.file {file(i_s, tag_id)}}) does not
          match.",
          "!" = "You might have modified your stationary periods without updating your wind file? ",
          ">" = "If so, run {.run tag_download_wind(tag)}"
        ))
      }

      # Check if spatial extent match
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      nc_extent <- c(min(lon), max(lon), min(lat), max(lat)) # nolint
      if (min(g$lat) < min(lat) || max(g$lat) > max(lat) ||
        min(g$lon) < min(lon) || max(g$lon) > max(lon)) {
        cli::cli_abort(c(x = "Spatial extent of the grid ({graph$param$tag_set_map$extent}) is
        not included in the extent of {.file {file(i_s, tag_id)}} ({nc_extent})"))
      }

      # Check if flight duration is
      if (fl_s$start[i_fl] >= fl_s$end[i_fl]) {
        cli::cli_abort(c(
          x = "Flight starting on stap {fl_s$stap_s[i_fl]} has a start time equal or greater than
          the end time. Please review your labelling file."
        ))
      }
    }
  }
}
