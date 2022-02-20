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
#' - `sz`: size of the 3d grid lat-lon-sta
#' - `equipement`: node(s) of the first sta (index in the 3d grid lat-lon-sta)
#' - `retrival`: node(s) of the last sta (index in the 3d grid lat-lon-sta)
#' - `flight_duration`: list of flight duration to next sta in hours
#' - `lat`: list of the `raster_list` latitude in cell center
#' - `lon`: list of the `raster_list` longitude in cell center
#' - `extent`: raster extent of the `raster_list``
#' - `resolution`: raster res of the `raster_list`
#'
#'
#' The vignette `How to use the graph` provided an example how to prepare the
#' data for the function and the output of this function
#'
#' @param raster_list list of raster containing probability map of each
#' stationary period. The metadata of `raster_list` needs to include the flight
#' duration to the next stationary period in the variable
#' `next_flight_duration` as a numeric in hours.
#' @param thr_prob_percentile threshold of percentile (see explanation above)
#' @param thr_gs threashold of groundspeed (km/h)  (see explanation above)
#' @return graph as a list (see description above)
#' @export
geopressure_graph <- function(raster_list,
                              thr_prob_percentile = .99,
                              thr_gs = 150) {

  # Check input
  testthat::expect_type(raster_list, "list")
  testthat::expect_is(raster_list[[1]], "RasterLayer")
  testthat::expect_true("next_flight_duration" %in%
    names(raster::metadata(raster_list[[1]])))
  testthat::expect_is(thr_prob_percentile, c("integer", "numeric"))
  testthat::expect_length(thr_prob_percentile, 1)
  testthat::expect_true(thr_prob_percentile >= 0 & thr_prob_percentile <= 1)
  testthat::expect_is(thr_gs, c("integer", "numeric"))
  testthat::expect_length(thr_gs, 1)
  testthat::expect_true(thr_gs >= 0)

  # compute size
  nsta <- length(raster_list)
  sz <- c(nrow(raster_list[[1]]), ncol(raster_list[[1]]), nsta)
  nll <- sz[1] * sz[2]

  # convert raster into normalized matrix
  prob_list <- lapply(raster_list, function(x) {
    probt <- raster::as.matrix(x)
    probt[is.na(probt)] <- 0
    probt / sum(probt, na.rm = T)
  })

  tmp <- unlist(lapply(prob_list, sum)) == 0
  if (any(tmp)) {
    stop(paste0("The `raster_list provided` has a probability map equal to zero
                for the stationay period: ", which(tmp)))
  }

  # find the pixels above to the percentile
  nds <- lapply(prob_list, function(probi) {
    # First, compute the threshold of prob corresponding to percentile
    probis <- sort(probi)
    id_prob_percentile <- sum(cumsum(probis) <= (1 - thr_prob_percentile))
    thr_prob <- probis[id_prob_percentile + 1]

    # filter the pixels above the threashold
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
  lat <- seq(raster::ymax(raster_list[[1]]), raster::ymin(raster_list[[1]]),
    length.out = nrow(raster_list[[1]]) + 1
  )
  lat <- head(lat, -1) + diff(lat[1:2]) / 2
  lon <- seq(raster::xmin(raster_list[[1]]), raster::xmax(raster_list[[1]]),
    length.out = ncol(raster_list[[1]]) + 1
  )
  lon <- head(lon, -1) + diff(lon[1:2]) / 2

  # Approximate resolution of the grid in km assuming 111km/lat-lon
  resolution <- mean(diff(lon)) * 111

  # exctract the flight duration
  flight_duration <- unlist(lapply(raster_list, function(x) {
    raster::metadata(x)$next_flight_duration
  }))

  tmp <- (thr_gs * head(flight_duration, -1) / resolution) < 1
  if (any(tmp)) {
    stop(paste0("The flight duration provided is too small for the stationay
                period: ", which(tmp)))
  }

  # filter the pixels which are not in reach of any location of the previous
  # and next stationary period
  cond <- T
  while (cond) {
    n_old <- sum(unlist(lapply(nds, sum)))
    for (i_s in seq_len(nsta - 1)) {
      nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
    }
    for (i_sr in seq_len(nsta - 1)) {
      i_s <- nsta - i_sr + 1
      nds[[i_s - 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
        flight_duration[i_s - 1] * thr_gs & nds[[i_s - 1]]
    }
    n_new <- sum(unlist(lapply(nds, sum)))
    if (n_new == n_old) {
      cond <- F
    }
  }

  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
    stop(paste0(
      "Using the `thr_gs` of ", thr_gs, " km/h provided with the binary
      distance, there are not any nodes left for the stationay period: ",
      which(tmp)
    ))
  }

  # Identify equipement and retrival
  equipement <- which(nds[[1]] == T)
  retrival <- which(nds[[nsta]] == T) + (nsta - 1) * nll


  # Create the graph list from nds together with the exact groundspeed
  gr <- list()
  nds_sum <- unlist(lapply(nds, sum))
  nds_expend_sum <- head(nds_sum, -1) * tail(nds_sum, -1)
  progress_bar(0, max = sum(nds_expend_sum))
  for (i_s in seq_len(nsta - 1)) {
    # find all the possible equipement and target based on nds and expand to
    # all possible combinaison
    grt <- expand.grid(
      s = as.integer(which(nds[[i_s]]) + (i_s - 1) * nll),
      t = as.integer(which(nds[[i_s + 1]]) + i_s * nll)
    )

    # Find the index in lat,lon,sta of those equipement and target
    s_id <- arrayInd(grt$s, sz)
    t_id <- arrayInd(grt$t, sz)

    # compute the groundspeed for all transition
    grt$gs <- geosphere::distGeo(
      cbind(lon[s_id[, 2]], lat[s_id[, 1]]),
      cbind(lon[t_id[, 2]], lat[t_id[, 1]])
    ) / 1000 / flight_duration[i_s]

    # filter the transition based on the groundspeed
    id <- grt$gs < thr_gs
    grt <- grt[id, ]

    # assign the static probability of the target node (pressure * light)
    grt$ps <- prob_list[[i_s + 1]][grt$t - i_s * nll]

    # add the edges from this stationary period to all others
    gr <- rbind(gr, grt)

    if (sum(id) == 0) {
      stop(paste0("Using the `thr_gs` of ", thr_gs, " km/h provided with the
                  exact distance of edges, there are not any nodes left for
                  the stationay period: ", i_s))
    }
    progress_bar(sum(nds_expend_sum[seq(1, i_s)]),
      max = sum(nds_expend_sum),
      text = paste("| sta = ", i_s, "/", nsta - 1, sep = "")
    )
  }

  # Trim
  for (i in seq_len(nsta)) {
    unique_s <- c(retrival, unique(gr$s))
    unique_t <- c(equipement, unique(gr$t))

    unique_s_new <- unique_s[unique_t %in% unique_s]
    unique_t_new <- unique_t[unique_s %in% unique_t]

    id <- gr$s %in% unique_s_new & gr$t %in% unique_t_new
    if (all(id)) {
      break
    }
    gr <- gr[id, ]
  }

  if (nrow(gr) == 0) {
    stop(paste0("Triming in the graph resulted in an empty graph"))
  }

  # convert gr to a list to add other information
  grl <- as.list(gr)
  grl$sz <- sz
  grl$equipement <- equipement
  grl$retrival <- retrival
  grl$flight_duration <- flight_duration
  grl$lat <- lat
  grl$lon <- lon
  grl$extent <- raster::extent(raster_list[[1]])
  grl$resolution <- raster::res(raster_list[[1]])
  grl$extend_sample <- lapply(raster_list, function(x) {
    raster::metadata(x)$extend_sample
  })

  # return
  grl
}



#' Marginal Probability Map
#'
#' This function return the marginal proability map as raster from a graph.
#'
#' @param grl graph constructed with `geopressure_graph_create()`
#' @return list of raster of the marginal probability at each stationary period
#' @export
geopressure_graph_marginal <- function(grl) {

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
  raster_list_marginal <- list()
  for (i_s in seq_len(dim(map)[3])) {
    raster_list_marginal[[i_s]] <- raster::raster(grl$extent,
      resolution = grl$resolution,
      vals = map[, , i_s]
    )
    proj4string(raster_list_marginal[[i_s]]) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs")
  }

  # return
  raster_list_marginal
}




#' Simulation of trajectory
#'
#' This function generate simulated path from a graph
#'
#' @param grl graph constructed with `geopressure_graph_create()`
#' @param nj number of simulation
#' @return list of the simulated path
#' @export
geopressure_graph_simulation <- function(grl, nj = 100) {

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
      sum(runif(1) > cumsum(map) / sum(map)) + 1
    })

    #
    path[, i_sta] <- ids + nll * (i_sta - 1)

    # Update progress bar
    progress_bar(i_sta, max = grl$sz[3])
  }

  path2lonlat(path, grl)
}


#' Find the lattitude and longitude from a path index
#'
#' @param path_id list or matrix of node index
#' @param grl graph constructed with `geopressure_graph_create()`
#' @return list of the path with latitude and longitude
#' @export
path2lonlat <- function(path_id, grl) {
  ind <- arrayInd(path_id, grl$sz)
  p <- list()
  p$id <- path_id
  p$lat <- grl$lat[ind[, 1]]
  dim(p$lat) <- dim(p$id)
  p$lon <- grl$lon[ind[, 2]]
  dim(p$lon) <- dim(p$id)
  p
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
