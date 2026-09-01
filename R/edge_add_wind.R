#' Retrieve ERA5 variable along edge
#'
#' @description
#' Reads the NetCDF files and extracts the variable requested along each flight defined by the
#' edges.
#'
#' - Time: linear interpolation using the resolution requested with `rounding_interval`
#' - Space: nearest neighbour interpolation by default, or bi-linear with `pracma::interp2` if
#'   `interp_spatial_linear=TRUE`. Note: spatial interpolation is limited to 0.1° for computational
#'   reasons.
#' - Pressure/altitude: linear interpolation using the exact `pressure` values
#'
#' @param graph either a `tag` or a `graph` GeoPressureR object.
#' @param edge_s a index of the source node of the edge. Either a vector with 3D index or a matrix
#' of 3 columns, one for each dimension.
#' @param edge_t a index of the target node of the edge. Either a vector with 3D index or a matrix
#' of 3 columns, one for each dimension.
#' @param pressure pressure measurement of the associated `tag` data used to estimate the pressure
#' level (i.e., altitude) of the bird during the flights. This data.frame needs to contain `date` as
#' POSIXt and `value` in hPa. If not provided, uses `graph$pressure`, assuming that argument `graph`
#'  is a GeoPressureR `tag` object.
#' @param variable list of the variables to extract from [the ERA5 pressure level
#' ](https://confluence.ecmwf.int/display/CKB/ERA5:+data+documentation#ERA5:datadocumentation-Table9)
#' using the `shortName` notation: `"u"`, `"v"`,  `"t"`, `"cc"`, `"r"`,
#' `"w"`, `"ciwc"`, `"clwc"`, `"q"`, `"cswc"`, `"d"`, `"z"`, `"o3"`, `"pv"`, `"vo"`.
#' @param rounding_interval temporal resolution on which to query the variable (min). Default is to
#' match ERA5 native resolution (1hr).
#' @param interp_spatial_linear logical to interpolate the variable linearly over space, if `FALSE`
#' takes the nearest neighbour. ERA5 native resolution is 0.25°
#' @param return_averaged_variable `r lifecycle::badge("deprecated")` No longer used.
#' @param quiet logical to hide messages about the progress
#' @inheritParams tag_download_wind
#'
#' @return A data.frame with one row per time step, edge, and variable:
#' - `edge_id` edge index
#' - `val` value of the variable at each time step
#' - `pressure` pressure at each time step
#' - `date` datetime of each time step
#' - `w` weight for averaging
#' - `var` variable name
#' - `lat` latitude at each time step
#' - `lon` longitude at each time step
#'
#' @examples
#' \dontrun{
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(extent = c(-16, 23, 0, 50), scale = 1) |>
#'     geopressure_map(quiet = TRUE)
#' })
#' graph <- graph_create(tag, quiet = TRUE)
#' wind <- edge_add_wind(graph, edge_s = graph$s, edge_t = graph$t, quiet = TRUE)
#' }
#' @family edge
#' @seealso [GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/trajectory-with-wind.html)
#' @export
# nocov start
edge_add_wind <- function(
  graph,
  edge_s,
  edge_t,
  pressure = NULL,
  variable = c("u", "v"),
  rounding_interval = 60,
  interp_spatial_linear = FALSE,
  return_averaged_variable = lifecycle::deprecated(),
  file = \(stap_id, tag_id) {
    glue::glue(
      "./data/wind/{tag_id}/{tag_id}_{stap_id}.nc"
    )
  },
  quiet = FALSE
) {
  if (lifecycle::is_present(return_averaged_variable)) {
    lifecycle::deprecate_warn(
      "3.5.4",
      "edge_add_wind(return_averaged_variable)",
      details = "{.fun edge_add_wind} now always returns detailed edge/time/variable values. Use {.fun graph_add_wind} for memory-efficient graph-scale wind."
    )
  }

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ncdf4} is required for {.fun edge_add_wind}.",
      "i" = "Install it with {.run install.packages('ncdf4')}."
    ))
  }

  if (is.null(pressure) && inherits(graph, "tag")) {
    pressure <- graph$pressure
  }

  tag_id <- graph$param$id

  edge_add_wind_check(
    graph,
    pressure = pressure,
    variable = variable,
    file = file
  )

  # Build the spatial grid used by the graph nodes.
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)

  # Convert consecutive stationary periods into one or several flight segments.
  flight <- stap2flight(graph$stap, format = "list")
  stap_include <- if ("include" %in% names(graph$stap)) {
    graph$stap$stap_id[graph$stap$include]
  } else {
    graph$stap$stap_id
  }

  # Convert edge ids to array indices and group edges by source stationary period.
  edge_info <- edge_add_wind_prepare_edges(edge_s, edge_t, g, graph$stap)
  edge_s <- edge_info$edge_s
  edge_t <- edge_info$edge_t
  table_edge_s <- edge_info$table_edge_s
  list_st_id <- edge_info$list_st_id
  edge_stap <- names(list_st_id)

  # Keep only the ID for the file function, remove the rest to save memory.
  graph <- list(param = list(id = graph$param$id))
  gc()

  var <- vector("list", length(variable))
  for (var_i in seq_len(length(variable))) {
    var[[var_i]] <- vector("list", length(flight))
  }

  # Start progress bar
  if (!quiet) {
    i_stap <- 0
    cli::cli_progress_bar(
      "Extract variables for edges of stationary period:",
      format = "{cli::col_blue(cli::symbol$info)} {cli::pb_name} {i_stap}/{length(edge_stap)} {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "{cli::col_green(cli::symbol$tick)} Extract variables for edges of stationary periods {cli::col_white('[', cli::pb_elapsed, ']')}",
      clear = FALSE,
      total = sum(table_edge_s)
    )
  }

  # Loop through the stationary period kept in the graph
  n_edge_done <- 0L
  for (i_stap in seq_along(edge_stap)) {
    # Extract the flight information from the current stap to the next one considered in the graph.
    # It can be the next, or if some stap are skipped at construction, it can contains multiples
    # flights
    # Graph layers are compressed to included staps; flight names use original stap_id.
    stap_layer <- as.integer(edge_stap[i_stap])
    stap_id <- stap_include[stap_layer]
    fl_s <- flight[[as.character(stap_id)]]

    # Determine the id of edges of the graph corresponding to this/these flight(s).
    st_id <- list_st_id[[edge_stap[i_stap]]]
    if (length(st_id) == 0) {
      next
    }

    # We are assuming that the bird flight as a straight line between the source and the target node
    # of each edge. If multiple flights happen during this transition, we assume that the bird flew
    # with a constant groundspeed during each flight, thus considering its stopover position to be
    # spread according to the flight duration. This does not account for habitat, so that it would
    # assume a bird can stop over water. While we could improve this part of the code to assume
    # constant airspeed rather than groundspeed, we suggest to create the graph considering all
    # stopovers.
    ratio_stap <- as.matrix(c(0, cumsum(fl_s$duration) / sum(fl_s$duration)))

    for (var_i in seq_len(length(variable))) {
      var[[var_i]][[i_stap]] <- vector("list", nrow(fl_s))
    }

    # Loop through each flight of the transition
    for (i_fl in seq_len(nrow(fl_s))) {
      # Find the stationary period ID from this specific flight (source)
      i_s <- fl_s$stap_s[i_fl]
      file_path <- file(i_s, tag_id)

      # Read the netCDF file
      nc <- ncdf4::nc_open(file_path)

      nc_info <- edge_add_wind_nc_info(nc, file_path)
      time <- nc_info$time
      pres <- nc_info$pres
      lat <- nc_info$lat
      dlat <- nc_info$dlat
      lon <- nc_info$lon
      dlon <- nc_info$dlon

      coords <- edge_add_wind_segment_coords(edge_s, edge_t, g, st_id, ratio_stap, i_fl)
      lat_s <- coords$lat_s
      lon_s <- coords$lon_s
      lat_e <- coords$lat_e
      lon_e <- coords$lon_e

      # Interpolate the straight-line segment onto regular query times. The query path has one
      # coordinate per edge and time step.
      interp <- edge_add_wind_interp_path(
        fl_s,
        i_fl,
        lat_s,
        lon_s,
        lat_e,
        lon_e,
        rounding_interval
      )
      t_q <- interp$t_q
      lat_int <- interp$lat_int
      lon_int <- interp$lon_int
      w <- edge_add_wind_weights(fl_s, i_fl, t_q, rounding_interval)

      # For nearest-neighbour spatial interpolation, precompute ERA5 grid indices once for all
      # query positions of this flight segment.
      lat_int_ind <- NULL
      lon_int_ind <- NULL
      lat_int_int <- NULL
      lon_int_int <- NULL
      if (!interp_spatial_linear) {
        lat_int_int <- as.vector(round(lat_int * 4) / 4)
        lat_int_ind <- matrix(match(lat_int_int, lat), nrow = nrow(lat_int))
        lon_int_int <- as.vector(round(lon_int * 4) / 4)
        lon_int_ind <- matrix(match(lon_int_int, lon), nrow = nrow(lon_int))
      }

      out <- edge_add_wind_fill_flight(
        pressure,
        t_q,
        pres,
        time,
        lat,
        lon,
        dlat,
        dlon,
        lat_int,
        lon_int,
        lat_int_ind,
        lon_int_ind,
        interp_spatial_linear,
        nc,
        variable,
        i_s,
        i_fl,
        fl_s
      )
      var_fl <- out$var_fl
      p_q <- out$p_q

      for (var_i in seq_len(length(variable))) {
        # Expand the time-by-edge matrices into the detailed diagnostic output.
        var[[var_i]][[i_stap]][[i_fl]] <- data.frame(
          edge_id = rep(st_id, each = length(t_q)),
          val = as.vector(var_fl[[var_i]]),
          pressure = rep(p_q, length(st_id)),
          date = rep(t_q, length(st_id)),
          w = rep(w, length(st_id))
        )
        var[[var_i]][[i_stap]][[i_fl]]$var <- variable[var_i]

        # Add lat lon
        if (interp_spatial_linear) {
          var[[var_i]][[i_stap]][[i_fl]]$lat <- as.vector(lat_int)
          var[[var_i]][[i_stap]][[i_fl]]$lon <- as.vector(lon_int)
        } else {
          var[[var_i]][[i_stap]][[i_fl]]$lat <- lat_int_int
          var[[var_i]][[i_stap]][[i_fl]]$lon <- lon_int_int
        }
      }

      # Close the netCDF file
      ncdf4::nc_close(nc)

      rm(
        nc,
        nc_info,
        time,
        pres,
        lat,
        dlat,
        lon,
        dlon,
        coords,
        lat_s,
        lon_s,
        lat_e,
        lon_e,
        interp,
        t_q,
        lat_int,
        lon_int,
        w,
        lat_int_ind,
        lon_int_ind,
        lat_int_int,
        lon_int_int,
        file_path,
        out,
        var_fl,
        p_q
      )
      gc()
    }

    if (!quiet) {
      n_edge_done <- n_edge_done + length(st_id)
      cli::cli_progress_update(
        set = n_edge_done,
        force = TRUE
      )
    }
  }

  # Final cleanup: remove only objects that exist
  rm(
    list = intersect(
      c(
        "list_st_id",
        "flight",
        "g",
        "fl_s",
        "st_id",
        "ratio_stap",
        "table_edge_s"
      ),
      ls()
    )
  )
  gc()

  do.call(rbind, unlist(unlist(var, recursive = FALSE), recursive = FALSE))
}


#' @noRd
edge_add_wind_check <- function(
  graph,
  pressure = NULL,
  variable = c("u", "v"),
  file = \(stap_id, tag_id) {
    glue::glue(
      "./data/wind/{tag_id}/{tag_id}_{stap_id}.nc"
    )
  }
) {
  assertthat::assert_that(inherits(graph, "tag") | inherits(graph, "graph"))

  tag_id <- graph$param$id

  # Rebuild the same graph grid used by the edge indices.
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)

  # File coverage is checked flight-by-flight because one NetCDF file is expected per stap.
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
  for (i_flight in seq_len(length(flight))) {
    fl_s <- flight[[i_flight]]
    for (i_fl in seq_len(length(fl_s$stap_s))) {
      i_s <- fl_s$stap_s[i_fl]
      file_path <- file(i_s, tag_id)

      if (!file.exists(file_path)) {
        cli::cli_abort(c(x = "No wind file {.file {file_path}}"))
      }
      nc <- ncdf4::nc_open(file_path)

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
        time <- as.POSIXct(
          ncdf4::ncvar_get(nc, "time") * 60 * 60,
          origin = "1900-01-01",
          tz = "UTC"
        )
      } else if ("valid_time" %in% names(nc$dim)) {
        time <- as.POSIXct(
          ncdf4::ncvar_get(nc, "valid_time"),
          origin = "1970-01-01",
          tz = "UTC"
        )
      } else {
        cli::cli_abort(c(
          x = "Time variable not found in {.file {file_path}}",
          "i" = "Available variable{?s} {?is/are} {.var {names(nc$dim)}}."
        ))
      }
      t_s <- as.POSIXct(
        format(fl_s$start[i_fl], "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      t_e <- as.POSIXct(
        format(fl_s$end[i_fl] + 60 * 60, "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        cli::cli_abort(c(
          x = "Wind file ({.file {file_path}}) does not cover the flight time range.",
          "i" = "Wind file: {min(time)} to {max(time)}. Flight: {t_s} to {t_e}.",
          "!" = "You might have modified your stationary periods without updating your wind file? ",
          ">" = "If so, run {.run tag_download_wind(tag)}"
        ))
      }

      # Check that the pressure levels are matching
      pres_value <- pressure$value[pressure$date > t_s & pressure$date < t_e]
      pres_value <- pres_value[is.finite(pres_value)]
      if (length(pres_value) > 0) {
        pres_var <- names(nc$dim)[grepl("*level", names(nc$dim))]
        pres <- ncdf4::ncvar_get(nc, pres_var)

        if (
          !(min(pres) <= min(pres_value) &&
            max(pres) >= min(1000, max(pres_value)))
        ) {
          cli::cli_abort(c(
            x = "Wind file ({.file {file_path}}) does not cover the flight pressure range.",
            "i" = "Wind file: {min(pres)} to {max(pres)} hPa. Flight: {min(pres_value)} to
            {max(pres_value)} hPa.",
            "!" = "You might have modified your stationary periods without updating your wind
            file?",
            ">" = "If so, run {.run tag_download_wind(tag)}"
          ))
        }
      }

      # Check if spatial extent match
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      nc_extent <- c(min(lon), max(lon), min(lat), max(lat))
      if (
        min(g$lat) < min(lat) ||
          max(g$lat) > max(lat) ||
          min(g$lon) < min(lon) ||
          max(g$lon) > max(lon)
      ) {
        cli::cli_abort(c(
          x = "Spatial extent of the grid ({graph$param$tag_set_map$extent}) is
        not included in the extent of {.file {file_path}} ({nc_extent})"
        ))
      }

      # Check if flight duration is
      if (fl_s$start[i_fl] >= fl_s$end[i_fl]) {
        cli::cli_abort(c(
          x = "Flight starting on stap {fl_s$stap_s[i_fl]} has a start time equal or greater than
          the end time. Please review your labelling file."
        ))
      }

      ncdf4::nc_close(nc)
    }
  }
}


#' @noRd
edge_add_wind_prepare_edges <- function(edge_s, edge_t, g, stap) {
  # Accept either compact graph indices or already-expanded [lat, lon, stap] indices.
  if (!is.matrix(edge_s)) {
    edge_s <- arrayInd(edge_s, c(g$dim, nrow(stap)))
  }
  if (!is.matrix(edge_t)) {
    edge_t <- arrayInd(edge_t, c(g$dim, nrow(stap)))
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

  max_stap <- max(stap$stap_id)
  assertthat::assert_that(all(edge_t[, 3] > 1 & edge_t[, 3] <= max_stap))
  assertthat::assert_that(all(edge_s[, 3] >= 1 & edge_s[, 3] < max_stap))
  assertthat::assert_that(all(edge_t[, 1] >= 1 & edge_t[, 1] <= g$dim[1]))
  assertthat::assert_that(all(edge_t[, 2] >= 1 & edge_t[, 2] <= g$dim[2]))
  assertthat::assert_that(all(edge_s[, 1] >= 1 & edge_s[, 1] <= g$dim[1]))
  assertthat::assert_that(all(edge_s[, 2] >= 1 & edge_s[, 2] <= g$dim[2]))

  # Group edges by source stap so each ERA5 file is opened only for the relevant edges.
  list(
    edge_s = edge_s,
    edge_t = edge_t,
    table_edge_s = table(edge_s[, 3]),
    list_st_id = split(seq_len(nrow(edge_s)), edge_s[, 3])
  )
}


#' @noRd
edge_add_wind_nc_info <- function(nc, file_path) {
  # ERA5 files changed their time coordinate name in the CDS; support both layouts.
  if ("time" %in% names(nc$dim)) {
    time <- as.POSIXct(
      ncdf4::ncvar_get(nc, "time") * 60 * 60,
      origin = "1900-01-01",
      tz = "UTC"
    )
  } else if ("valid_time" %in% names(nc$dim)) {
    time <- as.POSIXct(
      ncdf4::ncvar_get(nc, "valid_time"),
      origin = "1970-01-01",
      tz = "UTC"
    )
  } else {
    cli::cli_abort(c(
      x = "Time variable not found in {.file {file_path}}",
      "i" = "Available variable{?s} {?is/are} {.var {names(nc$dim)}}."
    ))
  }
  pres_var <- names(nc$dim)[grepl("*level", names(nc$dim))]
  pres <- ncdf4::ncvar_get(nc, pres_var)

  # Keep only coordinate vectors and grid spacing; variable arrays are read later in small chunks.
  lat <- ncdf4::ncvar_get(nc, "latitude")
  dlat <- abs(lat[2] - lat[1])
  lon <- ncdf4::ncvar_get(nc, "longitude")
  dlon <- lon[2] - lon[1]

  list(time = time, pres = pres, lat = lat, lon = lon, dlat = dlat, dlon = dlon)
}


#' @noRd
edge_add_wind_segment_coords <- function(edge_s, edge_t, g, st_id, ratio_stap, i_fl) {
  # If several flights connect two graph staps, split the edge in proportion to flight duration.
  lat_s <- g$lat[edge_s[st_id, 1]] +
    ratio_stap[i_fl] * (g$lat[edge_t[st_id, 1]] - g$lat[edge_s[st_id, 1]])
  lon_s <- g$lon[edge_s[st_id, 2]] +
    ratio_stap[i_fl] * (g$lon[edge_t[st_id, 2]] - g$lon[edge_s[st_id, 2]])
  lat_e <- g$lat[edge_s[st_id, 1]] +
    ratio_stap[i_fl + 1] * (g$lat[edge_t[st_id, 1]] - g$lat[edge_s[st_id, 1]])
  lon_e <- g$lon[edge_s[st_id, 2]] +
    ratio_stap[i_fl + 1] * (g$lon[edge_t[st_id, 2]] - g$lon[edge_s[st_id, 2]])

  list(lat_s = lat_s, lon_s = lon_s, lat_e = lat_e, lon_e = lon_e)
}


#' @noRd
edge_add_wind_interp_path <- function(fl_s, i_fl, lat_s, lon_s, lat_e, lon_e, rounding_interval) {
  # Query times cover the whole flight by rounding start down and end up to the requested interval.
  t_s <- as.POSIXct(
    trunc(as.numeric(fl_s$start[i_fl]) / (60 * rounding_interval)) *
      (60 * rounding_interval),
    origin = "1970-01-01",
    tz = "UTC"
  )
  t_e <- as.POSIXct(
    ceiling(as.numeric(fl_s$end[i_fl]) / (60 * rounding_interval)) *
      (60 * rounding_interval),
    origin = "1970-01-01",
    tz = "UTC"
  )
  t_q <- seq(from = t_s, to = t_e, by = 60 * rounding_interval)

  # Linearly interpolate position along the straight segment at each query time.
  dlat_se <- (lat_e - lat_s) / fl_s$duration[i_fl]
  dlon_se <- (lon_e - lon_s) / fl_s$duration[i_fl]
  w <- pmax(
    pmin(
      as.numeric(difftime(t_q, fl_s$start[i_fl], units = "hours")),
      fl_s$duration[i_fl]
    ),
    0
  )
  lat_int <- lat_s + tcrossprod(dlat_se, w)
  lon_int <- lon_s + tcrossprod(dlon_se, w)

  rm(dlat_se, dlon_se, w)
  gc()

  list(t_q = t_q, lat_int = lat_int, lon_int = lon_int)
}


#' @noRd
edge_add_wind_weights <- function(fl_s, i_fl, t_q, rounding_interval) {
  # Weight query times by how much of the flight interval they represent. Boundary points receive
  # partial weights because the flight rarely starts or ends exactly on the query grid.
  w <- numeric(length(t_q))
  assertthat::assert_that(length(w) > 1)

  alpha <- 1 -
    as.numeric(difftime(fl_s$start[i_fl], t_q[1], units = "mins")) /
      rounding_interval
  assertthat::assert_that(alpha >= 0 & alpha <= 1)
  w[c(1, 2)] <- w[c(1, 2)] + c(alpha, 1 - alpha) * alpha

  alpha <- 1 -
    as.numeric(difftime(
      utils::tail(t_q, 1),
      fl_s$end[i_fl],
      units = "mins"
    )) /
      rounding_interval
  assertthat::assert_that(alpha >= 0 & alpha <= 1)
  w[length(w) - c(1, 0)] <- w[length(w) - c(1, 0)] + c(1 - alpha, alpha) * alpha

  if (length(w) >= 4) {
    w[c(2, length(w) - 1)] <- w[c(2, length(w) - 1)] + 0.5
  }
  if (length(w) >= 5) {
    w[seq(3, length(w) - 2)] <- w[seq(3, length(w) - 2)] + 1
  }
  w <- w / sum(w)

  assertthat::assert_that(!anyNA(w))

  w
}


#' @noRd
edge_add_wind_fill_flight <- function(
  pressure,
  t_q,
  pres,
  time,
  lat,
  lon,
  dlat,
  dlon,
  lat_int,
  lon_int,
  lat_int_ind,
  lon_int_ind,
  interp_spatial_linear,
  nc,
  variable,
  i_s,
  i_fl,
  fl_s
) {
  # Detailed output keeps one value per query time and edge, so allocate time-by-edge matrices here.
  var_fl <- vector("list", length(variable))
  for (var_i in seq_len(length(variable))) {
    var_fl[[var_i]] <- matrix(NA, nrow = length(t_q), ncol = nrow(lat_int))
  }
  p_q <- numeric(length(t_q))

  # Each query time reads the smallest NetCDF slab needed, then applies pressure/time/space
  # interpolation inside edge_add_wind_values_time().
  for (i_time in seq_len(length(t_q))) {
    lat_int_ind_i <- if (interp_spatial_linear) NULL else lat_int_ind[, i_time]
    lon_int_ind_i <- if (interp_spatial_linear) NULL else lon_int_ind[, i_time]
    out <- edge_add_wind_values_time(
      pressure,
      t_q[i_time],
      pres,
      time,
      lat,
      lon,
      dlat,
      dlon,
      lat_int[, i_time],
      lon_int[, i_time],
      lat_int_ind_i,
      lon_int_ind_i,
      interp_spatial_linear,
      nc,
      variable,
      i_s,
      i_fl,
      fl_s
    )
    p_q[i_time] <- out$pressure
    for (var_i in seq_len(length(variable))) {
      var_fl[[var_i]][i_time, ] <- out$value[[var_i]]
    }
  }

  list(var_fl = var_fl, p_q = p_q)
}


#' @noRd
edge_add_wind_values_time <- function(
  pressure,
  t_q_i,
  pres,
  time,
  lat,
  lon,
  dlat,
  dlon,
  lat_int_i,
  lon_int_i,
  lat_int_ind_i,
  lon_int_ind_i,
  interp_spatial_linear,
  nc,
  variable,
  i_s,
  i_fl,
  fl_s
) {
  # Interpolate bird pressure to the query time, then choose the one or two ERA5 levels bracketing
  # that pressure.
  p_q <- stats::approx(
    pressure$date,
    pressure$value,
    t_q_i,
    rule = 2
  )$y
  if (p_q >= pres[1]) {
    id_pres <- 1
    n_pres <- 1
    if (p_q > pres[1] && pres[1] != 1000) {
      cli::cli_warn(c(
        "!" = "Pressure is above the highest level while the highest level is not 1000hPa.",
        "i" = "Stationary period: {i_s}",
        "i" = "Flight index: {i_fl} of {nrow(fl_s)}",
        "i" = "Date: {format(t_q_i, '%Y-%m-%d %H:%M:%S')}",
        "i" = "Pressure: {round(p_q, 2)} hPa",
        "i" = "Highest available level: {pres[1]} hPa",
        "i" = "Pressure difference: {round(p_q - pres[1], 2)} hPa"
      ))
    }
  } else if (p_q <= pres[length(pres)]) {
    id_pres <- length(pres)
    n_pres <- 1
    if (p_q < pres[length(pres)]) {
      cli::cli_warn(
        "Pressure is below the lowest level. This should never happen!"
      )
    }
  } else {
    id_pres <- max(which(pres >= p_q))
    n_pres <- 2
  }

  # Choose the one or two ERA5 time slices bracketing this query time.
  id_time <- findInterval(t_q_i, time)
  n_time <- ifelse(
    id_time == length(time) | time[id_time] == t_q_i,
    1,
    2
  )

  # Read only the spatial bounding box needed for the edge positions at this query time.
  id_lon <- which(
    lon >= (min(lon_int_i) - dlon) &
      (max(lon_int_i) + dlon) >= lon
  )
  id_lat <- which(
    lat >= (min(lat_int_i) - dlat) &
      (max(lat_int_i) + dlat) >= lat
  )

  # Extract a small NetCDF slab: lon x lat x pressure x time for each requested variable.
  var_nc <- vector("list", length(variable))
  for (var_i in seq_len(length(variable))) {
    var_nc[[var_i]] <- ncdf4::ncvar_get(
      nc,
      variable[var_i],
      start = c(id_lon[1], id_lat[1], id_pres, id_time),
      count = c(length(id_lon), length(id_lat), n_pres, n_time),
      collapse_degen = FALSE
    )
  }

  # First interpolate linearly along time, reducing the slab to lon x lat x pressure.
  if (n_time == 2) {
    w_time <- as.numeric(difftime(
      t_q_i,
      time[id_time],
      units = "hours"
    )) /
      as.numeric(difftime(
        time[id_time + 1],
        time[id_time],
        units = "hours"
      ))
    for (var_i in seq_len(length(variable))) {
      var_nc[[var_i]] <- array(
        var_nc[[var_i]][,,, 1] +
          w_time * (var_nc[[var_i]][,,, 2] - var_nc[[var_i]][,,, 1]),
        dim = c(length(id_lon), length(id_lat), n_pres)
      )
    }
  } else {
    for (var_i in seq_len(length(variable))) {
      var_nc[[var_i]] <- array(
        var_nc[[var_i]][,,, 1],
        dim = c(length(id_lon), length(id_lat), n_pres)
      )
    }
  }

  # Then interpolate linearly along pressure, reducing the slab to lon x lat.
  if (n_pres == 2) {
    w_pres <- (p_q - pres[id_pres]) /
      (pres[id_pres + 1] - pres[id_pres])
    for (var_i in seq_len(length(variable))) {
      var_nc[[var_i]] <- array(
        var_nc[[var_i]][,, 1] +
          w_pres * (var_nc[[var_i]][,, 2] - var_nc[[var_i]][,, 1]),
        dim = c(length(id_lon), length(id_lat))
      )
    }
  } else {
    for (var_i in seq_len(length(variable))) {
      var_nc[[var_i]] <- array(
        var_nc[[var_i]][,, 1],
        dim = c(length(id_lon), length(id_lat))
      )
    }
  }

  if (interp_spatial_linear) {
    # Bilinear spatial interpolation: round positions to 0.1 degree, interpolate only unique
    # coordinates, then map values back to all edges.
    ll_int_1d <- (round(lat_int_i, 1) + 90) *
      10 *
      10000 +
      (round(lon_int_i, 1) + 180) * 10 +
      1
    ll_int_1d_uniq <- unique(ll_int_1d)

    lat_int_uniq <- ((ll_int_1d_uniq - 1) %/% 10000) / 10 - 90
    lon_int_uniq <- ((ll_int_1d_uniq - 1) %% 10000) / 10 - 180
    # Check that the transformation is correct with
    # cbind((round(lat_int[, i_time], 1)+90)*10, (ll_int_1d - 1) %/% 10000)
    # cbind((round(lon_int[, i_time],1)+180)*10, (ll_int_1d - 1) %% 10000)
    # cbind(lat_int_uniq, lon_int_uniq, lat_int[, i_time], lon_int[, i_time])

    id_uniq <- match(ll_int_1d, ll_int_1d_uniq)

    value <- vector("list", length(variable))
    for (var_i in seq_len(length(variable))) {
      tmp <- pracma::interp2(
        rev(lat[id_lat]),
        lon[id_lon],
        var_nc[[var_i]][, rev(seq_len(ncol(var_nc[[var_i]])))],
        lat_int_uniq,
        lon_int_uniq,
        method = "linear"
      )
      assertthat::assert_that(!anyNA(tmp))
      value[[var_i]] <- tmp[id_uniq]
    }
  } else {
    # Nearest-neighbour spatial interpolation: use precomputed global grid indices, offset them
    # into the current NetCDF slab, then extract values directly.
    value <- vector("list", length(variable))
    for (var_i in seq_len(length(variable))) {
      lon_int_ind_off <- lon_int_ind_i - id_lon[1] + 1
      lat_int_ind_off <- lat_int_ind_i - id_lat[1] + 1

      ind <- (lat_int_ind_off - 1) *
        nrow(var_nc[[var_i]]) +
        lon_int_ind_off

      value[[var_i]] <- var_nc[[var_i]][ind]
    }
  }
  list(value = value, pressure = p_q)
}
# nocov end
