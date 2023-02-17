#' Create graph
#'
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
#' * `s`:   source node (index in the 3d grid lat-lon-stap),
#' * `t`:   target node (index in the 3d grid lat-lon-stap),
#' * `gs`:  average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary.
#' * `O`:   observation likelihood of each node,
#' * `sz`:  size of the 3d grid lat-lon-stap,
#' * `lat`: vector of the latitude in cell center,
#' * `lon`: vector of the longitude in cell center,
#' * `stap`: vector of the stationary period modeled,
#' * `flight`: list of a data.frame of all flights included between subsequent stationary period,
#' * `flight_duration`: vector of the total flights duration (in hours),
#' * `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-sta),
#' * `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-sta),
#' * `extent`: vector of the geographical extent `c(xmin, xmax, ymin, ymax)`,
#' * `mask_water`: logical matrix of water-land
#'
#' The [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph) provided an
#' example how to prepare the
#' data for the function and the output of this function.
#'
#' @param likelihood list of likelihood map, ideally build with `geopressure_likelihood()` or
#' `geolight_likelihood()`.
#' @param thr_likelihood threshold of percentile (see details).
#' @param thr_gs threshold of groundspeed (km/h)  (see details).
#' @param thr_duration threshold of the duration (in hours) for which stationary period are modeled
#' (default includes all).
#' @param stap stationary period modeled.
#' @param known data.frame of the known positions. Need to includes a column `stap`, `lat` and
#' `lon`.
#' @return Graph as a list (see details).
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#' @example
#' # See `geopressure_mismatch()` for generating pressure_mismatch
#' # Load pre-computed pressure mismatch
#' pressure_mismatch <- readRDS(
#'   system.file(
#'     "extdata/1_pressure/18LX_pressure_mismatch.rds", package = "GeoPressureR"
#'   )
#' )
#' pressure_likelihood <- geopressure_likelihood(pressure_mismatch)
#'
#' graph <- graph_create(
#' pressure_likelihood,
#'    known = data.frame(
#'      stap = 1,
#'      lat = 48.9,
#'      lon = 17.05
#'    )
#' )
#'
#' str(graph)
#'
#' @export
graph_create <- function(likelihood,
                         thr_likelihood = .99,
                         thr_gs = 150,
                         thr_duration = 0,
                         stap = seq_len(length(likelihood)),
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
  assertthat::assert_that(all(stap %in% seq_len(length(likelihood))))
  assertthat::assert_that(is.data.frame(known))
  assertthat::assert_that(assertthat::has_name(known, c("stap", "lat", "lon")))

  # construct stap data.frame (same as tag$stap, but tag not available here)
  stap_df <- do.call(rbind, lapply(likelihood, function(l) {
    data.frame(
      stap = l$stap,
      start = l$start,
      end = l$end
    )
  }))
  assertthat::assert_that(all(stap_df$stap == seq_len(nrow(stap_df))))

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
    stap,
    stap_df$stap[difftime(stap_df$end, stap_df$start, units = "hours") >= thr_duration]
  )
  # stap with likelihood map or present as known value
  stap_model <- intersect(
    stap_model,
    stap_df$stap[sapply(likelihood, function(l) {
      "likelihood" %in% names(l)
    })]
  )

  assertthat::assert_that(length(stap_model) >= 2)

  # Construct flight
  # construct the full flight data.frame (completely defined by stap data.frame)
  flight_df <- data.frame(
    start = utils::head(stap_df$end, -1),
    end = utils::tail(stap_df$start, -1),
    stap_s = utils::head(stap_df$stap, -1),
    stap_t = utils::tail(stap_df$stap, -1)
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
  likelihood_n <- lapply(likelihood[stap_model], function(x) {
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
  stap_0 <- sapply(likelihood_n, sum) == 0
  if (any(is.na(stap_0))) {
    stop(paste0(
      "likelihood is invalid for stationary period: ",
      paste(stap_model[which(is.na(stap_0))], collapse = ", "),
      " (check that the probability map is not null/na)."
    ))
  }
  if (any(stap_0)) {
    stop(paste0(
      "The `likelihood` provided has an invalid probability map for the stationary period: ",
      stap_model[which(stap_0)], "."
    ))
  }

  # find the pixels above to the percentile
  nds <- lapply(likelihood_n, function(l) {
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
    stop(paste0(
      "Using the `thr_likelihood` of ", thr_likelihood, " provided, there are not any ",
      "nodes left for the stationary period: ", stap_model[which(nds_0)], "."
    ))
  }

  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  for (i_s in seq_len(sz[3] - 1)) {
    nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
      flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
    if (sum(nds[[i_s + 1]]) == 0) {
      stop(paste0(
        "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary distance, ",
        "there are not any nodes left at stationary period ", stap_model[i_s + 1],
        " from stationary period ", stap_model[i_s], "."
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
        "there are not any nodes left at stationary period ", stap_model[i_s - 1],
        " from stationary period ", stap_model[i_s], "."
      ))
    }
  }

  # Check that there are still pixel present
  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary distance, there are not ",
      "any nodes left"
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

  message(
    "Computing the groundspeed for ", sum(nds_expend_sum), " edges for ",
    length(nds_expend_sum), " stationary periods in parallel."
  )
  progress_bar(0, max = sum(nds_expend_sum))
  for (i in seq_len(length(nds_sorted_idx))) {
    i_s <- nds_sorted_idx[i]
    nds_i_s <- nds[[i_s]]
    nds_i_s_1 <- nds[[i_s + 1]]
    likelihood_n_i_s_1 <- likelihood_n[[i_s + 1]]
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
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the exact distance of ",
          "edges, there are not any nodes left for the stationary period: ", stap_model[i_s]
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

      # assign the observation model (i.e. likelihood) of the target node
      # We use here the normalized likelihood assuming that the bird needs to be somewhere at each
      # stationary period. The log-linear pooling (`geopressure_likelihood`) is supposed to account
      # for the variation in stationary period duration.
      # For un-normalized use likelihood[[i_s + 1]])
      grt$O <- likelihood_n_i_s_1[grt$t - i_s * nll]

      if (sum(id) == 0) {
        stop(paste0(
          "Using the `thr_gs` of ", thr_gs, " km/h provided with the exact distance of ",
          "edges, there are not any nodes left for the stationary period: ", stap_model[i_s]
        ))
      }
      return(grt)
    }, seed = TRUE)
    progress_bar(sum(nds_expend_sum[seq(1, i)]),
      max = sum(nds_expend_sum),
      text = paste0("| stap = ", i, "/", sz[3] - 1)
    )
  }

  # Retrieve the graph
  gr <- future::value(f)

  # Trim
  gr <- graph_trim(gr)

  # Convert gr to a graph list
  graph <- as.list(do.call("rbind", gr))
  attr(graph, "out.attrs") <- NULL

  # Add metadata information
  graph$sz <- sz
  graph$lat <- lat
  graph$lon <- lon
  graph$stap <- stap_model
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
#'
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
#' @param extent Geographical extent of the map to query. Either a raster (e.g. `likelihood`) or a
#' list ordered by North, West, South, East  (e.g. `c(50,-16,0,20)`).
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
                                directory = paste0("data/5_wind_graph/", tag$id, "/")) {
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
    warning(paste0("The directory ", directory, " did not exist, so we created it."))
    dir.create(directory, showWarnings = FALSE)
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
      extent = c(extent[4], extent[1], extent[3], extent[2]),
      target = paste0(tag$id, "_", i_s, ".nc")
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
  assertthat::assert_that(file.exists(file.path(directory, "1.nc")))
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

      if (!file.exists(file.path(directory, i_s, ".nc"))) {
        stop(paste0("No wind file: '", directory, i_s, ".nc'"))
      }
      nc <- ncdf4::nc_open(file.path(directory, i_s, ".nc"))

      time <- as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60, origin = "1900-01-01", tz = "UTC")
      t_s <- as.POSIXct(format(fl_s$start[i2], "%Y-%m-%d %H:00:00"), tz = "UTC")
      t_e <- as.POSIXct(format(fl_s$end[i2] + 60 * 60, "%Y-%m-%d %H:00:00"), tz = "UTC")
      if (!(min(time) <= t_e && max(time) >= t_s)) {
        stop(paste0("Time not matching for for '", directory, i_s, ".nc'"))
      }

      pres <- ncdf4::ncvar_get(nc, "level")
      t_q <- seq(from = t_s, to = t_e, by = 60 * 60)
      pres_value <- pressure$value[pressure$date > t_s & pressure$date < t_e]
      if (length(pres_value) == 0 ||
        !(min(pres) <= min(pres_value) &&
          max(pres) >= min(1000, max(pres_value)))) {
        stop(paste0("Pressure not matching for '", directory, i_s, ".nc'"))
      }

      # Check if spatial extend match
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      if (min(graph$lat) < min(lat) || max(graph$lat) > max(lat) ||
        min(graph$lon) < min(lon) || max(graph$lon) > max(lon)) {
        stop(paste0("Spatial extend not matching for '", directory, i_s, ".nc'"))
      }

      # Check if flight duration is
      if (fl_s$start[i2] >= fl_s$end[i2]) {
        stop(paste0(
          "Flight starting on stap ", fl_s$stap_s[i2], " has a start time equal or greater than ",
          "the end time. Please review your labeling file."
        ))
      }
    }
  }

  # Start progress bar
  nds_expend_sum <- table(s[, 3])
  progress_bar(0,
    max = sum(nds_expend_sum),
    text = paste0("| stap = ", 0, "/", graph$sz[3] - 1)
  )

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
    # with a constant groundspeed during each flight, thus considering its stop-over position to be
    # spread according to the flight duration. This does not account for habitat, so that it would
    # assume a bird can stop over water. While we could improve this part of the code to assume
    # constant airspeed rather than groundspeed, we suggest to create the graph considering all
    # stopovers.
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

      progress_bar(sum(nds_expend_sum[seq(1, i1)]),
        max = sum(nds_expend_sum),
        text = paste0("| stap = ", i1, "/", graph$sz[3] - 1)
      )
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
    stop(paste0(
      "Using the `thr_as` of ", thr_as,
      " km/h provided with the exact distance of edges, there are ",
      "not any nodes left for the stationary period: ", paste(sta_pass, collapse = ", "),
      " with a minimum airspeed of ", min(abs(as[s[, 3] == sta_pass])), " km/h"
    ))
  }

  graph$s <- graph$s[id]
  graph$t <- graph$t[id]
  graph$gs <- graph$gs[id]
  graph$ws <- graph$ws[id]
  graph$O <- graph$O[id]

  return(graph)
}



#' Marginal Probability Map
#'
#' This function return the marginal probability map as raster from a graph.
#'
#' @param graph graph constructed with [`graph_create()`]
#' @return list of map of the marginal probability at each stationary period
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @export
graph_marginal <- function(graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(
    graph, c(
      "s", "t", "sz", "lat", "lon", "mask_water", "extent", "flight", "stap"
    )
  ))
  assertthat::assert_that(length(graph$s) > 0)

  if ("TO" %in% names(graph)) {
    TO <- graph$TO
  } else if (all(c("T", "O") %in% names(graph))) {
    TO <- graph$T * graph$O
  } else {
    stop("graph needs to contains 'TO', 'T' and 'O' or 'O' and 'flight()' ")
  }

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # matrix of transition * observation
  TO <- Matrix::sparseMatrix(graph$s, graph$t, x = TO, dims = c(n, n))

  # forward mapping of marginal probability
  map_f <- Matrix::sparseMatrix(rep(1, length(graph$equipment)),
    graph$equipment,
    x = 1, dims = c(1, n)
  )

  # backward mapping of marginal probability
  map_b <- Matrix::sparseMatrix(graph$retrieval,
    rep(1, length(graph$retrieval)),
    x = 1, dims = c(n, 1)
  )

  # build iteratively the marginal probability backward and forward by re-using the mapping
  # computed for previous stationary period. Set the equipment and retrieval site in each loop
  for (i_s in seq_len(graph$sz[3] - 1)) {
    map_f[1, graph$equipment] <- 1
    map_f <- map_f %*% TO

    map_b[graph$retrieval, 1] <- 1
    map_b <- TO %*% map_b
  }
  # add the retrieval and equipment at the end to finish it
  map_f[1, graph$equipment] <- 1
  map_b[graph$retrieval, 1] <- 1

  # combine the forward and backward
  map <- map_f * Matrix::t(map_b)

  # reshape mapping as a full (non-sparce matrix of correct size)
  map <- as.matrix(map)
  dim(map) <- graph$sz

  # return as list
  likelihood_marginal <- list()
  for (i_s in seq_len(dim(map)[3])) {
    tmp <- map[, , i_s]
    tmp[graph$mask_water] <- NA
    if (sum(tmp, na.rm = TRUE) == 0) {
      stop(
        "The probability of some transition are too small to find numerical solution. ",
        "Please check the data used to create the graph."
      )
    }
    likelihood_marginal[[i_s]] <- list(
      marginal = tmp,
      stap = graph$stap[i_s],
      flight = graph$flight[[i_s]],
      extent = graph$extent
    )
  }

  return(likelihood_marginal)
}




#' Simulation of trajectory
#'
#' This function simulates multiple trajectory from a graph. The trajectories consist of the
#' positions at each stationary periods.
#'
#' @param graph Graph constructed with [`graph_create()`].
#' @param nj Number of simulation.
#' @return List of simulated paths.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @export
graph_simulation <- function(graph,
                             nj = 10) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(
    graph, c("s", "t", "p", "sz", "lat", "lon", "extent", "stap")
  ))
  assertthat::assert_that(length(graph$s) > 0)
  assertthat::assert_that(is.numeric(nj))
  assertthat::assert_that(nj > 0)

  # number of nodes in the 3d grid
  n <- prod(graph$sz)
  nll <- graph$sz[1] * graph$sz[2]

  # Find the stationary index of all the source so that only the edges from a specific stationary
  # period can be easily query
  s_id <- arrayInd(graph$s, graph$sz)

  # As we will simulate in forward chronological order, we will be able to create map_f inside the
  # simulation. However, map_b needs to be computed for all stationary period in advance, starting
  # by the last stationary period and moving backward in time as follow
  map_b <- list()
  map_b[[graph$sz[3]]] <- Matrix::sparseMatrix(rep(1, length(graph$retrieval)),
    graph$retrieval,
    x = 1, dims = c(1, n)
  )

  for (i_stap in (graph$sz[3] - 1):1) {
    id <- s_id[, 3] == i_stap
    map_b[[i_stap]] <- map_b[[i_stap + 1]] %*%
      Matrix::sparseMatrix(graph$t[id], graph$s[id], x = graph$p[id], dims = c(n, n))
  }

  # Initialize the path
  path <- matrix(ncol = graph$sz[3], nrow = nj)

  # Sample the first position with map_b assuming map_f to be uniform
  map <- map_b[[1]][1:nll]
  for (i_j in seq_len(nj)) {
    path[i_j, 1] <- sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
  }

  # Loop through the simulation along chronological order
  progress_bar(1, max = graph$sz[3])
  for (i_stap in seq(2, graph$sz[3])) {
    # find edges arriving to this stationary period
    id <- s_id[, 3] == (i_stap - 1)

    # create the local trans_f (only edges from previous stap to this sta
    trans_f <- Matrix::sparseMatrix(graph$s[id], graph$t[id],
      x = graph$p[id],
      dims = c(n, n)
    )

    # build the forward mapping from the simulated nodes of the previous stationary period to the
    # current one using trans_f
    map_f <- Matrix::sparseMatrix(seq_len(nj), path[, i_stap - 1],
      x = 1,
      dims = c(nj, n)
    ) %*% trans_f

    # Combine forward and backward and samples
    if (nj > 1) {
      ids <- apply(map_f[, nll * (i_stap - 1) + (1:nll)], 1, function(x) {
        map <- x * map_b[[i_stap]][nll * (i_stap - 1) + (1:nll)]
        sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
      })
    } else {
      map <- map_f[, nll * (i_stap - 1) + (1:nll)] * map_b[[i_stap]][nll * (i_stap - 1) + (1:nll)]
      ids <- sum(stats::runif(1) > cumsum(map) / sum(map)) + 1
    }

    #
    path[, i_stap] <- ids + nll * (i_stap - 1)

    # Update progress bar
    progress_bar(i_stap, max = graph$sz[3])
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
#' @param path_id list or matrix of node index (`nj x nstap`).
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
  assertthat::assert_that(all(path_id > 0 & path_id <= prod(graph$sz)))

  if (is.matrix(path_id)) {
    # Number of paths
    nj <- dim(path_id)[1]
    # number of stationary period
    nstap <- dim(path_id)[2]
    assertthat::assert_that(nstap == graph$sz[3])

    # Get the source and target
    path_s <- path_id[, 1:(nstap - 1)]
    path_t <- path_id[, 2:nstap]

    # put as vector
    dim(path_s) <- (nstap - 1) * nj
    dim(path_t) <- (nstap - 1) * nj
  } else {
    nstap <- length(path_id)
    nj <- 1
    assertthat::assert_that(nstap == graph$sz[3])

    path_s <- path_id[1:(nstap - 1)]
    path_t <- path_id[2:nstap]
  }


  # Use mapply to loop through each edge. THE WORST!
  edge <- mapply(function(s, t) {
    which(graph$s == s & graph$t == t)
  }, path_s, path_t)

  # reshape in original shapre
  dim(edge) <- c(nj, (nstap - 1))

  return(edge)
}
