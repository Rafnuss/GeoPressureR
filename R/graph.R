#' Create graph
#'
#' @description
#' This function return a graph representing the trajectory of a bird based on filtering and triming
#' the likelihood maps provided.
#'
#' The likelihood map list `likelihood` needs to contain all stationary periods in order to
#' build the adequate flight structure (account for all individual flights). Stationary periods with
#' no likelihood map will not be modeled. Use `stap` and `thr_duration` to furthere limit which
#' stationary periods to model.
#'
#' In the final graph, we only keep the most likely node (position in time) defined as: 1. those
#' which cumulative probability reach up to `thr_likelihood` for each stationary period. 2.
#' those which average ground speed is lower than `thr_gs` km/h.
#'
#' The graph returned is a list of the edges of the graph containing:

#'
#' The [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph) provided an
#' example how to prepare the
#' data for the function and the output of this function.
#'
#' @param likelihood List of likelihood map, ideally build with `geopressure_likelihood()` or
#' `geolight_likelihood()`.
#' @param thr_likelihood Threshold of percentile (see details).
#' @param thr_gs Threshold of groundspeed (km/h)  (see details).
#' @param thr_duration Threshold of the duration (in hours) for which stationary period are modeled
#' (default includes all).
#' @param stap_model Vector of the stationary period to model.
#' @param known Data.frame of the known positions. Need to includes a column `stap`, `lat` and
#' `lon`.
#' @return Graph as a list
#' - `s`:   source node (index in the 3d grid lat-lon-stap),
#' - `t`:   target node (index in the 3d grid lat-lon-stap),
#' - `gs`:  average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary.
#' - `obs`:   observation model, corresponding to the normalize likelihood in 3D matrix of size
#' `sz`,
#' - `sz`:  size of the 3d grid lat-lon-stap,
#' - `lat`: vector of the latitude in cell center,
#' - `lon`: vector of the longitude in cell center,
#' - `stap_model`: vector of the stationary period modeled,
#' - `stap`: data.frame of all stationary periods,
#' - `flight`: list of a data.frame of all flights included between subsequent stationary period,
#' - `flight_duration`: vector of the total flights duration (in hours),
#' - `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-sta),
#' - `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-sta),
#' - `extent`: vector of the geographical extent `c(xmin, xmax, ymin, ymax)`,
#' - `mask_water`: logical matrix of water-land
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#' @examples
#' # See `geopressure_mismatch()` for generating pressure_mismatch
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#' pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
#'
#' graph <- graph_create(
#'   pressure_likelihood,
#'   known = data.frame(
#'     stap = 1,
#'     lat = 48.9,
#'     lon = 17.05
#'   )
#' )
#'
#' str(graph)
#'
#' @export
graph_create <- function(likelihood,
                         thr_likelihood = .99,
                         thr_gs = 150,
                         thr_duration = 0,
                         stap_model = seq_len(length(likelihood)),
                         known = data.frame(
                           stap = integer(),
                           lat = double(),
                           lon = double()
                         )) {
  # Check input
  assertthat::assert_that(is.list(likelihood))
  assertthat::assert_that(is.list(likelihood[[1]]))
  assertthat::assert_that(assertthat::has_name(likelihood[[1]], c("stap", "start", "end")))
  assertthat::assert_that(is.numeric(thr_likelihood))
  assertthat::assert_that(length(thr_likelihood) == 1)
  assertthat::assert_that(thr_likelihood >= 0 & thr_likelihood <= 1)
  assertthat::assert_that(is.numeric(thr_gs))
  assertthat::assert_that(length(thr_gs) == 1)
  assertthat::assert_that(thr_gs >= 0)
  assertthat::assert_that(is.numeric(thr_duration))
  assertthat::assert_that(all(stap_model %in% seq_len(length(likelihood))))
  assertthat::assert_that(is.data.frame(known))
  assertthat::assert_that(assertthat::has_name(known, c("stap", "lat", "lon")))

  cli::cli_progress_step("Check data input")

  # construct stap data.frame (same as tag$stap, but tag not available here)
  stap <- do.call(rbind, lapply(likelihood, function(l) {
    data.frame(
      stap = l$stap,
      start = l$start,
      end = l$end
    )
  }))
  assertthat::assert_that(all(stap$stap == seq_len(nrow(stap))))

  # Define map parameters
  stap_map <- sapply(likelihood, function(l) {
    "likelihood" %in% names(l)
  })

  # Asset that the map are the same
  assertthat::assert_that(length(unique(lapply(likelihood[stap_map], function(l) {
    l$extent
  }))) == 1)
  extent <- unique(lapply(likelihood[stap_map], function(l) {
    l$extent
  }))[[1]]
  assertthat::assert_that(length(unique(lapply(likelihood[stap_map], function(l) {
    dim(l$likelihood)
  }))) == 1)
  map_dim <- unique(lapply(likelihood[stap_map], function(l) {
    dim(l$likelihood)
  }))[[1]]

  # Get latitude and longitude of the center of the pixel
  lat <- seq(extent[4], extent[3], length.out = map_dim[1] + 1)
  lat <- utils::head(lat, -1) + diff(lat[1:2]) / 2
  lon <- seq(extent[1], extent[2], length.out = map_dim[2] + 1)
  lon <- utils::head(lon, -1) + diff(lon[1:2]) / 2

  # Approximate resolution of the grid from ° to in km
  # Assume uniform grid in lat-lon
  # Use the smaller resolution assuming 111km/lon and 111*cos(lat)km/lat
  resolution <- mean(diff(lon)) * pmin(cos(lat * pi / 180) * 111.320, 110.574)

  # Define the mask of water
  mask_water <- is.na(likelihood[[stap_map[1]]]$likelihood)

  # Overwrite known position
  for (k in seq_len(nrow(known))) {
    lon_calib_id <- which.min(abs(known$lon[k] - lon))
    lat_calib_id <- which.min(abs(known$lat[k] - lat))
    tmp <- likelihood[[stap_map[1]]]$likelihood
    tmp[!is.na(tmp)] <- 0
    tmp[lat_calib_id, lon_calib_id] <- 1
    likelihood[[known$stap[k]]]$likelihood <- tmp
  }

  # Find stap to be included in the graph model
  # stap matching the thr_duration threshold.
  stap_model <- intersect(
    stap_model,
    stap$stap[difftime(stap$end, stap$start, units = "hours") >= thr_duration]
  )
  # stap with likelihood map or present as known value
  stap_model <- intersect(
    stap_model,
    stap$stap[sapply(likelihood, function(l) {
      "likelihood" %in% names(l)
    })]
  )

  assertthat::assert_that(length(stap_model) >= 2)

  # Construct flight
  # construct the full flight data.frame (completely defined by stap data.frame)
  flight_df <- data.frame(
    start = utils::head(stap$end, -1),
    end = utils::tail(stap$start, -1),
    stap_s = utils::head(stap$stap, -1),
    stap_t = utils::tail(stap$stap, -1)
  )

  # create the list of flight per stationary period modeled
  flight <- split(flight_df, stap_model[sapply(flight_df$stap_s, function(s) {
    sum(stap_model <= s)
  })])

  # compute flight duration for each stationary period
  flight_duration <- sapply(flight, function(f) {
    sum(difftime(f$end, f$start, units = "hours"))
  })
  names(flight_duration) <- NULL

  # Compute size
  sz <- c(map_dim[1], map_dim[2], length(stap_model))
  nll <- sz[1] * sz[2]

  # Process likelihood map
  # We use here the normalized likelihood assuming that the bird needs to be somewhere at each
  # stationary period. The log-linear pooling (`geopressure_likelihood`) is supposed to account
  # for the variation in stationary period duration.
  likelihood_norm <- lapply(likelihood[stap_model], function(x) {
    l <- x$likelihood

    # replace empty map with 1 everywhere
    if (sum(l, na.rm = TRUE) == 0) {
      l[l == 0] <- 1
    }

    # replace NA by 0
    l[is.na(l)] <- 0

    # Normalize
    l / sum(l, na.rm = TRUE)
  })

  # Check for invalid map
  stap_0 <- sapply(likelihood_norm, sum) == 0
  if (any(is.na(stap_0))) {
    cli::cli_abort(c(
      "x" = "{.val likelihood} is invalid for the stationary period: \\
      {stap_model[which(is.na(stap_0))]}"
    ))
  }
  if (any(stap_0)) {
    cli::cli_abort(c(
      "x" = "Using the {.val likelihood}  provided has an invalid probability map for the \\
      stationary period: {stap_model[which(stap_0)]}"
    ))
  }

  # find the pixels above to the percentile
  nds <- lapply(likelihood_norm, function(l) {
    # First, compute the threshold of prob corresponding to percentile
    ls <- sort(l)
    id_prob_percentile <- sum(cumsum(ls) <= (1 - thr_likelihood))
    thr_prob <- ls[id_prob_percentile + 1]

    # return matrix if the values are above the threshold
    return(l >= thr_prob)
  })

  # Check that there are still values
  nds_0 <- unlist(lapply(nds, sum)) == 0
  if (any(nds_0)) {
    cli::cli_abort(c(
      "x" = "Using the {.val thr_likelihood} of {thr_likelihood}, there are not any nodes left at \\
      stationary period: {stap_model[which(nds_0)]}"
    ))
  }

  cli::cli_progress_step("Create graph from maps")
  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  for (i_s in seq_len(sz[3] - 1)) {
    nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
      flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
    if (sum(nds[[i_s + 1]]) == 0) {
      cli::cli_abort(c(
        "x" = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left at stationary period {stap_model[i_s + 1]} from\\
        stationary period {stap_model[i_s]}"
      ))
    }
  }
  for (i_sr in seq_len(sz[3] - 1)) {
    i_s <- sz[3] - i_sr + 1
    nds[[i_s - 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
      flight_duration[i_s - 1] * thr_gs & nds[[i_s - 1]]
    if (sum(nds[[i_s - 1]]) == 0) {
      cli::cli_abort(c(
        "x" = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left at stationary period {stap_model[i_s - 1]} from\\
        stationary period {stap_model[i_s]}"
      ))
    }
  }

  # Check that there are still pixel present
  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    cli::cli_abort(c(
      "x" = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left."
    ))
  }

  # Create the graph from nds with the exact groundspeed

  # Run each transition in parallel with decreasing order of edges
  nds_sum <- unlist(lapply(nds, sum))
  nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
  nds_sorted_idx <- order(nds_expend_sum, decreasing = TRUE)
  nds_expend_sum <- sort(nds_expend_sum, decreasing = TRUE)
  future::plan(future::multisession, workers = future::availableCores() / 2)
  f <- list()

  cli::cli_progress_step(
    "Computing the groundspeed for {sum(nds_expend_sum)} edges for {length(nds_expend_sum)} \\
    stationary periods",
    spinner = TRUE
  )
  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
  p <- progressr::progressor(sum(nds_expend_sum))
  for (i in seq_len(length(nds_sorted_idx))) {
    i_s <- nds_sorted_idx[i]
    nds_i_s <- nds[[i_s]]
    nds_i_s_1 <- nds[[i_s + 1]]
    f[[i_s]] <- future::future(expr = {
      # find all the possible equipment and target based on nds and expand to all possible
      # combination
      grt <- expand.grid(
        s = as.integer(which(nds_i_s) + (i_s - 1) * nll),
        t = as.integer(which(nds_i_s_1) + i_s * nll)
      )

      # Find the index in lat, lon, stap of those equipment and target
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
        cli::cli_abort(c(
          "x" = "Using the {.val thr_g} of {thr_gs} km/h provided with the exact distance of \\
          edges, there are not any nodes left for the stationary period: {stap_model[i_s]}"
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

      if (sum(id) == 0) {
        cli::cli_abort(c(
          "x" = "Using the {.val thr_g} of {thr_gs} km/h provided with the exact distance of \\
          edges, there are not any nodes left for the stationary period: {stap_model[i_s]}"
        ))
      }
      p(amount = nds_expend_sum[i])
      return(grt)
    }, seed = TRUE)
  }

  # Retrieve the graph
  gr <- future::value(f)

  # Trim
  cli::cli_progress_step("Trim graph")
  gr <- graph_trim(gr)

  # Convert gr to a graph list
  graph <- as.list(do.call("rbind", gr))
  attr(graph, "out.attrs") <- NULL

  # Add observation model as matrix
  graph$obs <- do.call(c, likelihood_norm)
  dim(graph$obs) <- sz

  # Add metadata information
  graph$sz <- sz
  graph$lat <- lat
  graph$lon <- lon
  graph$stap_model <- stap_model
  graph$stap <- stap
  graph$flight <- flight
  graph$flight_duration <- flight_duration
  graph$equipment <- which(nds[[1]] == TRUE)
  graph$retrieval <- as.integer(which(nds[[sz[3]]] == TRUE) + (sz[3] - 1) * nll)
  graph$mask_water <- mask_water
  graph$extent <- extent
  return(graph)
}



#' Trim a graph
#'
#' Trimming consists in removing "dead branch" of a graph, that is removing the edges which are not
#' connected to both the source (i.e, equipment) or sink (i.e. retrieval site).
#'
#' @param gr graph constructed with [`graph_create()`].
#' @return graph trimmed
#' @seealso [`graph_create()`]
#' @noRd
graph_trim <- function(gr) {
  if (length(gr) < 2) {
    return(gr)
  }

  cli::cli_progress_bar(total = (length(gr) - 1) * 2, type = "task")

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
      cli::cli_abort(c(
        "x" =
          "Triming the graph killed it at stationary period {i_s} moving forward."
      ))
    }
    cli::cli_progress_update(force = TRUE)
  }
  # Then, trim the graph from retrieval to equipment
  for (i_s in seq(length(gr) - 1, 1)) {
    t <- unique(gr[[i_s]]$t)
    s_a <- unique(gr[[i_s + 1]]$s)
    unique_t_new <- t[t %in% s_a]

    id <- gr[[i_s]]$t %in% unique_t_new
    gr[[i_s]] <- gr[[i_s]][id, ]

    if (nrow(gr[[i_s]]) == 0) {
      cli::cli_abort(c(
        "x" =
          "Triming the graph killed it at stationary period {i_s} moving backward"
      ))
    }
    cli::cli_progress_update(force = TRUE)
  }
  return(gr)
}

#' Download wind data
#'
#' @description
#' This function download the wind data from [ERA5 hourly pressure level](
#' https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview)
#' with the [Climate Data Store (CDS)](https://cds.climate.copernicus.eu) and through the [`ecmwfr`
#' R package](https://bluegreen-labs.github.io/ecmwfr/index.html).
#'
#' The flight are determined from the stationary periods classified `tag$sta`
#' (see `tag_classify()`). It request a single file for each flight using the exact time
#' (hourly basis) and pressure (altitude). To make the download more efficient,
#' [`wf_request_batch()`](
#' https://bluegreen-labs.github.io/ecmwfr/articles/advanced_vignette.html#batch-parallel-requests)
#' is used to download all wind file at the same time (up to 20 requests in parallel).
#'
#' To be able to download data from the Climate Data Store (CDS), you will need to create an account
#' on [https://cds.climate.copernicus.eu](https://cds.climate.copernicus.eu). You can then save
#' your credential (`cds_key` and `cds_user`) in your `.Rprofile` (see
#' [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data)).
#'
#' @param tag data logger dataset list with `tag$sta` computed. See [`tag_read()`] and
#' [`tag_stap()`].
#' @param extent extent Geographical extent of the map on which the likelihood will be computed.
#' Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param stap Stationary period identifier of the start of the flight to query as defined in
#' `tag$sta`. Be default, download for all the flight.
#' @param cds_key User (email address) used to sign up for the ECMWF data service. See
#' [`wf_set_key()`].
#' @param cds_user Token provided by ECMWF. See [`wf_set_key()`].
#' @param directory Path were to store the downloaded data.
#' @return The path of the downloaded (requested file).
#' @seealso [`wf_request()`](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html),
#' [GeoPressureManual | Wind graph
#' ](https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data)
#' @export
graph_download_wind <- function(tag,
                                extent,
                                stap = seq_len(nrow(tag$stap) - 1),
                                cds_key = Sys.getenv("cds_key"),
                                cds_user = Sys.getenv("cds_user"),
                                directory = file.path("data/5_wind_graph/", tag$id)) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "value")))
  assertthat::assert_that(assertthat::has_name(tag, "sta"))
  assertthat::assert_that(is.data.frame(tag$sta))
  assertthat::assert_that(assertthat::has_name(tag$sta, c("end", "start")))
  assertthat::assert_that(length(extent) == 4)
  assertthat::assert_that(is.numeric(stap))
  assertthat::assert_that(all(stap %in% tag$stap$stap))

  ecmwfr::wf_set_key(user = cds_user, key = cds_key, service = "cds")

  if (!file.exists(directory)) {
    dir.create(directory, showWarnings = FALSE)
    cli::cli_warn(c(
      "!" = "The directory {.file {directory}} did not exist.",
      ">" = "We created the directory"
    ))
  }

  # see https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Levellistings
  possible_pressure <- c(
    1, 2, 3, 5, 7, 10, 20, 30, 50, 70,
    seq(100, 250, 25), seq(300, 750, 50), seq(775, 1000, 25)
  )

  # create list of request
  request_list <- list()

  for (i_id in stap) {
    # Find the index of stap
    i_s <- which(tag$stap$stap == i_id)

    # Get the timeserie of the flight on a 1 hour resolution
    flight_time <- seq(round.POSIXt(tag$stap$end[i_s] - 30 * 60, units = "hours"),
      round.POSIXt(tag$stap$start[i_s + 1] + 30 * 60, units = "hours"),
      by = 60 * 60
    )

    # Find the pressure level needed during this flight
    flight_id <- flight_time[1] <= tag$pressure$date &
      tag$pressure$date <= utils::tail(flight_time, 1)
    pres_id_min <- min(
      sum(!(min(tag$pressure$value[flight_id]) < possible_pressure)),
      length(possible_pressure) - 1
    )
    pres_id_max <- min(
      sum(max(tag$pressure$value[flight_id]) > possible_pressure) + 1,
      length(possible_pressure)
    )
    flight_pres_id <- seq(pres_id_min, pres_id_max)

    # Make some check
    assertthat::assert_that(length(possible_pressure[flight_pres_id]) > 1)
    assertthat::assert_that(length(flight_time) > 1)

    # Prepare the query
    request_list[[i_id]] <- list(
      dataset_short_name = "reanalysis-era5-pressure-levels",
      product_type = "reanalysis",
      format = "netcdf",
      variable = c("u_component_of_wind", "v_component_of_wind"),
      pressure_level = possible_pressure[flight_pres_id],
      year = sort(unique(format(flight_time, "%Y"))),
      month = sort(unique(format(flight_time, "%m"))),
      day = sort(unique(format(flight_time, "%d"))),
      time = sort(unique(format(flight_time, "%H:%M"))),
      extent = c(extent[4], extent[1], extent[3], extent[2]), # N, W, S, E
      target = glue::glue("{tag$id}_{i_s}.nc")
    )
  }

  ecmwfr::wf_request_batch(
    request_list,
    workers = 20,
    # user = ,
    path = directory,
    # time_out = 3600,
    # total_timeout = length(request_list) * time_out/workers
  )
}

#' Add windspeed and airspeed
#'
#' @description
#' Read NetCDF file downloaded on your computer and add the average windspeed experienced by the
#' bird and the corresponding airspeed for each edge of the graph.
#'
#' See the [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#download-wind-data) for
#' explanations and example on how to download the `NetCDF` files from ERA-5.
#'
#' @param graph graph constructed with [`graph_create()`]
#' @param pressure pressure data from a data logger. This data.frame needs to contains `date` as
#' POSIXt and `value` in hPa. It is best practice to use [`tag_read()`] and [`tag_stap()`] to build
#' this data.frame.
#' @param directory Character of the path where to find the netCDF file.
#' @param thr_as Threshold of airspeed (km/h).
#' @return Graph as a list with windspeed and airspeed as `ws` and `as` respectively (see
#' [`graph_create()`] for more detail on the graph returned).
#' @seealso [`graph_create()`], [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#add-wind-to-graph)
#' @export
graph_add_wind <- function(graph,
                           pressure,
                           directory,
                           filename_prefix = glue::glue("{graph$id}_"),
                           thr_as = Inf) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(
    graph, c("s", "t", "gs", "sz", "lat", "lon", "flight")
  ))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.data.frame(pressure))
  assertthat::assert_that(assertthat::has_name(pressure, c("date", "value")))
  assertthat::assert_that(inherits(pressure$date, "POSIXt"))
  assertthat::assert_that(is.numeric(pressure$value))
  assertthat::assert_that(is.character(directory))
  assertthat::assert_that(is.character(filename_prefix))
  assertthat::assert_that(file.exists(file.path(directory, glue::glue("{filename_prefix}1.nc"))))
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

      full_path <- file.path(directory, glue::glue("{filename_prefix}{i_s}.nc"))
      if (!file.exists(full_path)) {
        cli::cli_abort(c("x" = "No wind file {.file {full_path}}"))
      }
      nc <- ncdf4::nc_open(full_path)

      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60, origin = "1900-01-01", tz = "UTC")
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"), tz = "UTC")
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"), tz = "UTC")
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        cli::cli_abort(c("x" = "Time not matching for {.file {directory}{i_s}.nc}"))
      }

      pres <- ncdf4::ncvar_get(nc, "level")
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)
      pres_value <- pressure$value[pressure$date > t_s & pressure$date < t_e]
      if (length(pres_value) == 0 ||
        !(min(pres) <= min(pres_value) &&
          max(pres) >= min(1000, max(pres_value)))) {
        cli::cli_abort(c("x" = "Pressure not matching for {.file {directory}{i_s}.nc}"))
      }

      # Check if spatial extend match
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      if (min(graph$lat) < min(lat) || max(graph$lat) > max(lat) ||
        min(graph$lon) < min(lon) || max(graph$lon) > max(lon)) {
        cli::cli_abort(c("x" = "Spatial extend not matching for {.file {directory}{i_s}.nc}"))
      }

      # Check if flight duration is
      if (fl_s$start[i2] >= fl_s$end[i2]) {
        cli::cli_abort(c(
          "x" = "Flight starting on stap {fl_s$stap_s[i2]} has a start time equal or greater than \\
                         the end time. Please review your labeling file."
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
      "x" =
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

  return(graph)
}



#' Define the movement model
#'
#' Define the movement model used later by storing the parameter needed to build `flight_prob()`.
#'
#' @param graph graph constructed with `graph_create()`
#' @param type Groundspeed `"gs"` or airspeed `"as"`
#' @return graph list with a new list `graph$movement` storing all the parameters needed to compute
#' the transition probability
#' @seealso [`graph_create()`], [`graph_tran()`], [`graph_create()`],
#' [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @examples
#' # See `geopressure_mismatch()` for generating pressure_mismatch
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#' pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
#'
#' graph <- graph_create(pressure_likelihood,
#'   known = data.frame(
#'     stap = 1,
#'     lat = 48.9,
#'     lon = 17.05
#'   )
#' )
#'
#' graph <- graph_add_movement(graph, method = "logis", rate = 3)
#'
#' str(graph$movement)
#' @export
graph_add_movement <- function(graph,
                               type = ifelse("ws" %in% names(graph), "as", "gs"),
                               ...) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(type == "ws" | type == "gs")

  graph$movement <- list(
    type = type,
    ...
  )

  # Test that everything is correct
  graph_trans(graph)

  return(graph)
}

#' Compute transition probability of graph
#'
#' Use the movement model (see `graph_add_movement()`) to convert groundspeed `gs` or airspeed `as`
#' if available, to compute the transition probability of the edges of the graph.
#'
#' @param graph graph constructed with `graph_create()` and with movement (see
#' `graph_add_movement()`).
#' @return vector of transition probability for each edge.
#' @seealso [`graph_create()`], [`graph_add_movement()`]
#' @noRd
graph_trans <- function(graph) {
  assertthat::assert_that(is.list(graph))
  if (!assertthat::has_name(graph, "movement")) {
    cli::cli_abort(c(
      "x" = "The graph does not have a movement model.",
      "i" = "Make sure to call {.fn graph_add_movement} before."
    ))
  }
  assertthat::assert_that(assertthat::has_name(graph, c("movement", "gs")))

  if ("trans" %in% names(graph)) {
    trans <- graph$trans
  } else {
    if (graph$movement$type == "as") {
      assertthat::assert_that(assertthat::has_name(graph, c("ws")))
      trans <- do.call(flight_prob, c(graph$movement, list(speed = graph$gs - graph$ws)))
    } else if (graph$movement$type == "gs") {
      trans <- do.call(flight_prob, c(graph$movement, list(speed = graph$gs)))
    } else {
      throws_error("Invalid movement type :", graph$movement$type)
    }

    return(trans)
  }
}



#' Marginal probability map
#'
#' @description
#' Compute the marginal probability map from a graph. The graph needs to have a movement model
#' defined (see `graph_add_movement()`). The computation uses the forward-backward algorithm.
#'
#' @param graph graph constructed with [`graph_create()`]
#' #' @return A list for each stationary period in order 1, 2, ..., n containing:
#' - `stap` stationary period. Needs to be in continuous
#' - `start` POSIXct date time of the start of the stationary period
#' - `end` POSIXct date time of the end of the stationary period and start of the flight
#' - `marginal` matrix of the marginal map
#' - `extent` vector length 4 of the extent of the map `c(xmin, xmax, ymin, ymax)`
#' @seealso [`graph_create()`], [`graph_add_movement()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @examples
#' # See `geopressure_mismatch()` for generating pressure_mismatch
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds",
#'     package = "GeoPressureR"
#'   )
#' )
#' pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
#' graph <- graph_create(pressure_likelihood,
#'   known = data.frame(
#'     stap = 1,
#'     lat = 48.9,
#'     lon = 17.05
#'   )
#' )
#' graph <- graph_add_movement(graph)
#'
#' marginal <- graph_marginal(graph)
#'
#' str(marginal)
#'
#' terra::plot(
#'   c(
#'     terra::rast(pressure_likelihood[[3]]$likelihood, extent = pressure_likelihood[[3]]$extent),
#'     terra::rast(marginal[[3]]$marginal, extent = marginal[[3]]$extent),
#'   ),
#'   main = c("Likelihood", "Marginal likelihood"),
#'   xlim = c(5, 23), ylim = c(35, 50)
#' )
#' @export
graph_marginal <- function(graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "lat", "lon", "stap_model", "stap", "equipment", "retrieval",
    "mask_water", "extent"
  )))
  assertthat::assert_that(length(graph$s) > 0)

  # Compute the transition matrix (movement model)
  trans <- graph_trans(graph)

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # matrix of transition * observation
  trans_obs <- Matrix::sparseMatrix(graph$s, graph$t,
    x = trans * graph$obs[graph$t], dims = c(n, n)
  )

  # Initiate the forward probability vector (f_k^T in Nussbaumer et al. (2023) )
  map_f <- Matrix::sparseMatrix(1, 1, x = 0, dims = c(1, n))

  # Initiate the backward probability vector (b_k in Nussbaumer et al. (2023) )
  map_b <- Matrix::sparseMatrix(1, 1, x = 0, dims = c(n, 1))

  # build iteratively the marginal probability backward and forward by re-using the mapping
  # computed for previous stationary period. Set the equipment and retrieval site in each loop
  for (i_s in seq_len(graph$sz[3] - 1)) {
    map_f[1, graph$equipment] <- graph$obs[graph$equipment] # P_0^T O_0 with P_0=1
    map_f <- map_f %*% trans_obs # Eq. 3 in Nussbaumer et al. (2023)

    map_b[graph$retrieval, 1] <- 1 # equivalent to map_b[, 1] <- 1 but slower
    map_b <- trans_obs %*% map_b # Eq. 3 in Nussbaumer et al. (2023)
  }
  # add the retrieval and equipment at the end to finish it
  map_f[1, graph$equipment] <- graph$obs[graph$equipment]
  map_b[graph$retrieval, 1] <- 1

  # combine the forward and backward
  map_fb <- map_f * Matrix::t(map_b) # Eq. 5 in Nussbaumer et al. (2023)

  # reshape mapping as a full (non-sparce matrix of correct size)
  map_fb <- as.matrix(map_fb)
  dim(map_fb) <- graph$sz

  # return as list
  marginal <- lapply(split(graph$stap, graph$stap$stap), function(m) {
    m <- as.list(m)
    i_s <- which(m$stap == graph$stap_model)
    if (length(i_s) > 0) {
      map_fb_i <- map_fb[, , i_s]
      map_fb_i[graph$mask_water] <- NA
      if (sum(map_fb_i, na.rm = TRUE) == 0) {
        cli::cli_abort(
          "The probability of some transition are too small to find numerical solution. ",
          "Please check the data used to create the graph."
        )
      }
      m$marginal <- map_fb_i
      m$extent <- graph$extent
    }
    return(m)
  })
  return(marginal)
}




#' Simulation of trajectories
#'
#' This function simulates randomly multiple trajectories from a graph using the [forward filtering
#' backward sampling algorithm](https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm).
#' The trajectories consist of the set of positions of the bird at all stationary periods,
#' accounting for both the likely posit.
#'
#' @param graph Graph constructed with [`graph_create()`].
#' @param nj Number of simulation.
#' @return List of simulated paths:
#' - `id` Matrix of index in the graph (graph$sz) `nj x nstap`
#' - `lat` Matrix of latitude `nj x nstap`
#' - `lon` Matrix of longitude `nj x nstap`
#' - `stap` stationary period data.frame (same as `graph$stap` or `tag$stap`)
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @export
graph_simulation <- function(graph,
                             nj = 10) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "lat", "lon", "stap_model", "stap", "equipment", "retrieval",
    "mask_water", "extent"
  )))
  assertthat::assert_that(length(graph$s) > 0)

  # Compute the matrix TO
  trans_obs <- graph_trans(graph) * graph$obs[graph$t]

  # number of nodes in the 3d grid
  n <- prod(graph$sz)
  nll <- graph$sz[1] * graph$sz[2]

  # Find the stationary index of all the source so that only the edges from a specific stationary
  # period can be easily query
  s_id <- arrayInd(graph$s, graph$sz)

  # As we will simulate in forward chronological order, we will be able to create map_f inside the
  # simulation. However, map_b needs to be computed for all stationary period in advance, starting
  # by the last stationary period and moving backward in time as follow.
  # We store map_b as a list of vector of size lat x lon (instead of 3D with stap)
  map_b <- list()

  # Initiate map_b at the last stap with the retrieval node (b_n=1 in Nussbaumer et al. 2023)
  map_b[[graph$sz[3]]] <- Matrix::sparseMatrix(
    rep(1, length(graph$retrieval)),
    graph$retrieval,
    x = 1, dims = c(1, n)
  )

  # Build all map_b in backward order
  for (i_s in (graph$sz[3] - 1):1) {
    id <- s_id[, 3] == i_s
    map_b[[i_s]] <- map_b[[i_s + 1]] %*%
      Matrix::sparseMatrix(graph$t[id], graph$s[id], x = trans_obs[id], dims = c(n, n))
    # Same as Eq. 3 in Nussbaumer et al. (2023) but with b_k transpose thus TO * b_k instead
    # of b_k * TO
  }

  # Initialize the path
  path <- matrix(ncol = graph$sz[3], nrow = nj)

  # Sample the first position with map_b and map_f as f_0 = P_0 * O_0
  map_f_0 <- Matrix::sparseMatrix(1, 1, x = 0, dims = c(1, n))
  map_f_0[graph$equipment] <- graph$obs[graph$equipment]
  map_fb <- map_b[[1]][1:nll] * map_f_0[1:nll]

  for (i_j in seq_len(nj)) {
    path[i_j, 1] <- sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
  }

  # Loop through the simulation along chronological order
  cli::cli_progress_bar(total = graph$sz[3])
  for (i_s in seq(2, graph$sz[3])) {
    # find edges arriving to this stationary period
    id <- s_id[, 3] == (i_s - 1)

    # create the local trans_obs (only edges from previous stap to this stap
    trans_obs_l <- Matrix::sparseMatrix(graph$s[id], graph$t[id], x = trans_obs[id], dims = c(n, n))

    # build the forward mapping from the simulated nodes of the previous stationary period to the
    # current one using trans_obs_l
    map_f <- Matrix::sparseMatrix(seq_len(nj), path[, i_s - 1], x = 1, dims = c(nj, n)) %*%
      trans_obs_l

    # Combine forward and backward and samples
    if (nj > 1) {
      ids <- apply(map_f[, nll * (i_s - 1) + (1:nll)], 1, function(map_f_i) {
        map_fb <- map_f_i * map_b[[i_s]][nll * (i_s - 1) + (1:nll)]
        sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
      })
    } else {
      map_fb <- map_f[, nll * (i_s - 1) + (1:nll)] * map_b[[i_s]][nll * (i_s - 1) + (1:nll)]
      ids <- sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
    }

    # Convert ids into 3D coordinates
    path[, i_s] <- ids + nll * (i_s - 1)

    # Update progress bar
    cli::cli_progress_update(set = i_s, force = TRUE)
  }

  return(graph_path2lonlat(path, graph))
}


#' Most likely trajectory
#'
#' Compute the trajectory which maximize the overall probability using the [Viterbi algorithm](
#' https://en.wikipedia.org/wiki/Viterbi_algorithm) on the graph structure. The graph needs to have
#' a movement model defined (see `graph_add_movement()`).
#'
#' @param graph Graph constructed with [`graph_create()`].
#' @return List of simulated paths:
#' - `id` vector of index in the graph (graph$sz)
#' - `lat` vector of latitude
#' - `lon` vector of longitude
#' - `stap` stationary period data.frame (same as `graph$stap` or `tag$stap`)
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @export
graph_most_likely <- function(graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "lat", "lon", "stap_model", "stap", "equipment", "retrieval",
    "mask_water", "extent"
  )))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.numeric(nj))
  assertthat::assert_that(nj > 0)

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # Compute the matrix TO
  trans_obs <- graph_trans(graph) * graph$obs[graph$t]

  # Initiate the matrix providing for each node of the graph, the source id (index of the node)
  # with the most likely path to get there.
  path_s <- Matrix::sparseMatrix(
    rep(1, length(graph$equipment)),
    graph$equipment,
    x = 1, dims = c(1, n)
  )
  # Initiate the same matrix providing the total probability of the current path so far
  path_max <- Matrix::sparseMatrix(
    rep(1, length(graph$equipment)),
    graph$equipment,
    x = graph$obs[graph$equipment], dims = c(1, n)
  )

  # Create a data.frame of all edges information
  node <- data.frame(
    s = graph$s,
    t = graph$t,
    to = trans_obs,
    stap = arrayInd(graph$s, graph$sz)[, 3]
  )

  # Split this data.fram by stationary period (of the source)
  node_stap <- split(node, node$stap)

  n_edge <- sapply(node_stap, nrow)

  cli::cli_progress_bar(total = sum(n_edge))
  i_s <- 0

  for (node_i_s in node_stap) {
    # compute the probability of all possible transition
    node_i_s$p <- path_max[node_i_s$s] * node_i_s$to

    # Find the value of the maximum possible transition for each target node
    max_v <- sapply(split(node_i_s$p, node_i_s$t), max)
    max_t <- as.numeric(names(max_v))
    path_max[max_t] <- max_v

    # Find the source node of the maximum possible transition for each target node
    max_s <- sapply(split(node_i_s, node_i_s$t), function(x) {
      x$s[which.max(x$p)]
    })
    path_s[max_t] <- max_s

    # Update progress bar
    i_s <- i_s + 1
    cli::cli_progress_update(set = sum(n_edge[1:i_s]), force = TRUE)
  }

  # Construct the most likely path from path_max and path_s
  path <- c()
  # Initiate the last position as the maximum of all retrieval node
  path[graph$sz[3]] <- graph$retrieval[which.max(path_max[graph$retrieval])]
  # Iteratively find the previous node of the path
  for (i_s in (graph$sz[3] - 1):1) {
    path[i_s] <- path_s[path[i_s + 1]]
  }

  return(graph_path2lonlat(path, graph))
}


#' Find the latitude and longitude from a path index
#'
#' @param path_id List or matrix of node index.
#' @param graph Graph constructed with [`graph_create()`].
#' @return List of the path with latitude and longitude and index of the the path provided.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-1-shortest-path)
#' @export
graph_path2lonlat <- function(path_id,
                              graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c("s", "t", "sz", "lat", "lon")))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.numeric(path_id))
  assertthat::assert_that(all(path_id > 0 & path_id <= prod(graph$sz)))

  ind <- arrayInd(path_id, graph$sz)
  p <- list()
  p$id <- path_id
  p$lat <- graph$lat[ind[, 1]]
  dim(p$lat) <- dim(p$id)
  p$lon <- graph$lon[ind[, 2]]
  dim(p$lon) <- dim(p$id)
  p$stap <- graph$stap
  return(p)
}


#' Find the edge index from a path index
#'
#' Very inefficient way to find the edges...
#'
#' @param path_id vector or matrix of node index (`nj x nstap`).
#' @param graph graph constructed with [`graph_create()`].
#' @return List or matrix of the edge `nj x (nstap-1)`.
#' @seealso [`graph_create()`], [GeoPressureManual | Wind graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/wind-graph.html#energy)
#' @export
graph_path2edge <- function(path_id,
                            graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c("s", "t")))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.numeric(path_id))
  assertthat::assert_that(all(path_id > 0))
  assertthat::assert_that(all(path_id <= prod(graph$sz)))
  if (!is.matrix(path_id)) {
    path_id <- matrix(path_id, nrow = 1)
  }
  # Number of paths
  nj <- dim(path_id)[1]
  # number of stationary period
  nstap <- dim(path_id)[2]
  assertthat::assert_that(nstap == graph$sz[3])

  # construct the edge of the path as data.frame
  path_st <- data.frame(
    s = as.vector(utils::head(path_id, c(nj, -1))),
    t = as.vector(utils::tail(path_id, c(nj, -1)))
  )

  # Check that all sources and target exist in the graph
  assertthat::assert_that(all(path_st$s %in% graph$s),
    msg = "path_id is not compatible with the graph$s."
  )
  assertthat::assert_that(all(path_st$t %in% graph$t),
    msg = "path_id is not compatible with the graph$f."
  )

  # Build data.frame of the graph
  graph_st <- data.frame(
    edge = seq_len(length(graph$s)),
    s = graph$s,
    t = graph$t
  )

  # Find index of edge
  path_st <- merge(path_st, graph_st, all.x = TRUE, sort = FALSE)

  # reshape in original shape
  edge <- path_st$edge
  dim(edge) <- c(nj, nstap - 1)

  return(edge)
}
