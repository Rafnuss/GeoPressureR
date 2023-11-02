#' Create a `graph` object
#'
#' @description
#' This function returns a trellis graph representing the trajectory of a bird based on filtering
#' and pruning the likelihood maps provided.
#'
#' In the final graph, we only keep the most likely nodes (i.e., position of the bird at each
#' stationary periods) defined as (1) those whose likelihood value are within the threshold of
#' percentile `thr_likelihood` of the total likelihood map and (2) those which are connected to
#' at least one edge of the previous and next stationary periods requiring an average ground speed
#' lower than `thr_gs` (in km/h).
#'
#' For more details and illustration, see [section 2.2 of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0004-title)
#' and the [GeoPressureManual](https://bit.ly/3saLVqi)
#'
#' @param tag a GeoPressureR `tag` object.
#' @param thr_likelihood threshold of percentile (see details).
#' @param thr_gs threshold of groundspeed (km/h)  (see details).
#' @param geosphere_dist function to compute the distance. Usually, either
#' `geosphere::distHaversine` (fast) or `geosphere::distGeo` (precise but slow). See
#' https://rspatial.org/raster/sphere/2-distance.html for more options and details.
#' @param geosphere_bearing function to compute the bearing. Either `geosphere::bearing` (default)
#' or `geosphere::bearingRhumb`. See https://rspatial.org/raster/sphere/3-direction.html#bearing
#' for details.
#' @param workers number of workers used in the computation of edges ground speed. More workers
#' (up to the limit `future::availableCores()`) usually makes the computation faster, but because
#' the the number of edges is large, memory will often limit the computation.
#' @param quiet logical to hide messages about the progress.
#' @inheritParams tag2map
#'
#' @return Graph as a list
#' - `s`: source node (index in the 3d grid lat-lon-stap)
#' - `t`: target node (index in the 3d grid lat-lon-stap)
#' - `gs`: average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary
#' - `obs`: observation model, corresponding to the normalized likelihood in a 3D matrix of size
#' `sz`
#' - `sz`: size of the 3d grid lat-lon-stap
#' - `stap`: data.frame of all stationary periods (samme as `tag$stap`)
#' - `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-sta)
#' - `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-sta)
#' - `mask_water`: logical matrix of water-land
#' - `param`: list of parameters including `thr_likelihood` and `thr_gs` (same as `tag$param`)
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   twilight_create() |>
#'   twilight_label_read() |>
#'   tag_set_map(
#'     extent = c(-16, 23, 0, 50),
#'     known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
#'   ) |>
#'   geopressure_map(quiet = TRUE) |>
#'   geolight_map(quiet = TRUE)
#'
#' # Create graph
#' graph <- graph_create(tag, thr_likelihood = 0.95, thr_gs = 100, quiet = TRUE)
#'
#' print(graph)
#'
#' @seealso [GeoPressureManual](https://bit.ly/3saLVqi)
#' @family graph
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_create <- function(tag,
                         thr_likelihood = .99,
                         thr_gs = 150,
                         likelihood = NULL,
                         geosphere_dist = geosphere::distHaversine,
                         geosphere_bearing = geosphere::bearing,
                         workers = 1,
                         quiet = FALSE) {
  if (!quiet) {
    cli::cli_progress_step("Check data input")
  }

  # Construct the likelihood map
  lk <- tag2map(tag, likelihood = likelihood)

  assertthat::assert_that(is.numeric(thr_likelihood))
  assertthat::assert_that(length(thr_likelihood) == 1)
  assertthat::assert_that(thr_likelihood >= 0 & thr_likelihood <= 1)
  assertthat::assert_that(is.numeric(thr_gs))
  assertthat::assert_that(length(thr_gs) == 1)
  assertthat::assert_that(thr_gs >= 0)

  # Extract info from tag for simplicity
  stap <- tag$stap
  stap_model <- which(stap$include)

  # Select only the map for the stap to model
  lk <- lk[stap_model]

  lk_null <- sapply(lk, is.null)
  if (any(lk_null)) {
    cli::cli_abort(c(
      x = "The {.field {likelihood}} in {.var tag} is/are null for stationary periods \\
       {.var {stap_model[lk_null]}} while those stationary period are required in \\
      {.var stap$include}",
      i = "Check your input and re-run {.fun geopressure_map} if necessary."
    ))
  }

  if (length(stap_model) < 2) {
    cli::cli_abort(c(
      x = "There are only {.var {length(stap_model)}} stationary period{?s} to be modeled \\
      according to {.var stap$include}.",
      i = "You need at least 3 stationary periods."
    ))
  }

  g <- map_expand(tag$param$extent, tag$param$scale)

  # Approximate resolution of the grid from ° to in km
  # Assume uniform grid in lat-lon
  # Use the smaller resolution assuming 111km/lon and 111*cos(lat)km/lat
  resolution <- mean(diff(g$lon)) * pmin(cos(g$lat * pi / 180) * 111.320, 110.574)

  # Construct flight
  flight <- stap2flight(stap)
  flight_duration <- as.numeric(flight$duration)
  assertthat::assert_that(length(flight_duration) == length(stap_model) - 1)

  # Compute size
  sz <- c(g$dim[1], g$dim[2], length(stap_model))
  nll <- sz[1] * sz[2]

  # Process likelihood map
  # We use here the normalized likelihood assuming that the bird needs to be somewhere at each
  # stationary period. The log-linear pooling (`geopressure_map_likelihood`) is supposed to account
  # for the variation in stationary period duration.
  lk_norm <- lapply(lk, function(l) {
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
  stap_id_0 <- sapply(lk_norm, sum) == 0
  if (any(is.na(stap_id_0))) {
    cli::cli_abort(c(
      x = "{.var likelihood} is invalid for the stationary period: \\
      {stap_model[which(is.na(stap_id_0))]}"
    ))
  }
  if (any(stap_id_0)) {
    cli::cli_abort(c(
      x = "Using the {.var likelihood}  provided has an invalid probability map for the \\
      stationary period: {stap_model[which(stap_id_0)]}"
    ))
  }

  # find the pixels above to the percentile
  nds <- lapply(lk_norm, function(l) {
    # First, compute the threshold of prob corresponding to percentile
    ls <- sort(l)
    id_prob_percentile <- sum(cumsum(ls) < (1 - thr_likelihood))
    thr_prob <- ls[id_prob_percentile + 1]

    # return matrix if the values are above the threshold
    return(l >= thr_prob)
  })

  # Check that there are still values
  nds_0 <- unlist(lapply(nds, sum)) == 0
  if (any(nds_0)) {
    cli::cli_abort(c(
      x = "Using the {.var thr_likelihood} of {.val {thr_likelihood}}, there are not any nodes \\
      left at stationary period: {.val {stap_model[which(nds_0)]}}"
    ))
  }

  if (!quiet) {
    cli::cli_progress_step("Create graph from maps")
  }

  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  # The "-1" of distmap accounts for the fact that the shortest distance between two grid cell is
  # not the center of the cell but the side. This should only impact short flight distance/duration.
  for (i_s in seq_len(sz[3] - 1)) {
    nds[[i_s + 1]] <- (EBImage::distmap(!nds[[i_s]]) - 1) * resolution <
      flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
    if (sum(nds[[i_s + 1]]) == 0) {
      cli::cli_abort(c(
        x = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the binary distance \\
          edges, there are not any nodes left at stationary period {.val {stap_model[i_s + 1]}} \\
        from stationary period {.val {stap_model[i_s]}}"
      ))
    }
  }
  for (i_sr in seq_len(sz[3] - 1)) {
    i_s <- sz[3] - i_sr + 1
    nds[[i_s - 1]] <- (EBImage::distmap(!nds[[i_s]]) - 1) * resolution <
      flight_duration[i_s - 1] * thr_gs & nds[[i_s - 1]]
    if (sum(nds[[i_s - 1]]) == 0) {
      cli::cli_abort(c(
        x = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left at stationary period {.val {stap_model[i_s - 1]}} \\
        from stationary period {.val {stap_model[i_s]}}"
      ))
    }
  }

  # Check that there are still pixel present
  if (any(unlist(lapply(nds, sum)) == 0)) {
    cli::cli_abort(c(
      x = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left."
    ))
  }

  # Create the graph from nds with the exact groundspeed

  # Run each transition in parallel with decreasing order of edges
  nds_sum <- unlist(lapply(nds, sum))
  nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
  nds_sorted_idx <- order(nds_expend_sum, decreasing = TRUE)
  nds_expend_sum <- sort(nds_expend_sum, decreasing = TRUE)
  assertthat::assert_that(workers > 0 & workers <= future::availableCores())
  future::plan(future::multisession, workers = workers)
  f <- list()

  if (!quiet) {
    i <- 0
    cli::cli_progress_step(
      "Compute the groundspeed for stationary period {i}/{length(nds_expend_sum)} \\
      ({ round(sum(nds_expend_sum[seq_len(i)])/sum(nds_expend_sum)*100)}% of nodes)",
      msg_done = "Compute the groundspeed"
    )
  }
  for (i in seq_len(length(nds_sorted_idx))) {
    i_s <- nds_sorted_idx[i]
    nds_i_s <- which(nds[[i_s]])
    nds_i_s_1 <- which(nds[[i_s + 1]])
    f[[i_s]] <- future::future(expr = {
      # find all the possible equipment and target based on nds and expand to all possible
      # combination
      grt <- expand.grid(
        s = as.integer(nds_i_s + (i_s - 1) * nll),
        t = as.integer(nds_i_s_1 + i_s * nll)
      )

      # Find the index in lat, lon, stap of those equipment and target
      s_id <- arrayInd(grt$s, sz)
      t_id <- arrayInd(grt$t, sz)

      # compute the groundspeed for all transition
      dist <- geosphere_dist(
        cbind(g$lon[s_id[, 2]], g$lat[s_id[, 1]]),
        cbind(g$lon[t_id[, 2]], g$lat[t_id[, 1]])
      ) / 1000 # m -> km

      # The minimal distance between grid cell is not from the center of the cell, but from one edge
      # to the other (opposite) edge. So the minimal distance between cell should be reduce by the
      # grid resolution.
      dist <- pmax(dist - resolution[s_id[, 1]], 0)

      # Compute groundspeed
      gs_abs <- dist / flight_duration[i_s]

      # filter the transition based on the groundspeed
      id <- gs_abs < thr_gs
      if (sum(id) == 0) {
        cli::cli_abort(c(
          x = "Using the {.var thr_g} of {.val {thr_gs}} km/h provided with the exact distance of \\
          edges, there are not any nodes left for the stationary period: {.val stap_model[i_s]}"
        ))
      }
      grt <- grt[id, ]

      # Compute the bearing of the trajectory
      gs_bearing <- geosphere_bearing(
        cbind(g$lon[s_id[id, 2]], g$lat[s_id[id, 1]]),
        cbind(g$lon[t_id[id, 2]], g$lat[t_id[id, 1]])
      )
      # bearing is NA if gs==0, fix for computing the complex representation
      gs_bearing[is.na(gs_bearing)] <- 0

      # save groundspeed in complex notation
      gs_arg <- (450 - gs_bearing) %% 360
      grt$gs <- gs_abs[id] * cos(gs_arg * pi / 180) +
        1i * gs_abs[id] * sin(gs_arg * pi / 180)

      if (sum(id) == 0) {
        cli::cli_abort(c(
          x = "Using the {.var thr_g} of {.val {thr_gs}} km/h provided with the exact distance of \\
          edges, there are not any nodes left for the stationary period: {.val {stap_model[i_s]}}"
        ))
      }

      return(grt)
    }, seed = TRUE)

    # Update progress bar
    cli::cli_progress_update()
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  # Retrieve the graph
  gr <- future::value(f)

  # Explicitly close multisession workers by switching plan
  future::plan(future::sequential)

  # Prune
  gr <- graph_create_prune(gr, quiet = quiet)

  if (!quiet) {
    cli::cli_progress_step("Format graph output")
  }

  # Convert gr to a graph list
  graph <- as.list(do.call("rbind", gr))
  # nolint start
  attr(graph, "out.attrs") <- NULL
  # nolint end
  graph <- structure(graph, class = "graph")

  # Add observation model as matrix
  graph$obs <- do.call(c, lk_norm)
  dim(graph$obs) <- sz

  # Add metadata information
  graph$sz <- sz
  graph$stap <- tag$stap
  graph$equipment <- which(nds[[1]] == TRUE)
  graph$retrieval <- as.integer(which(nds[[sz[3]]] == TRUE) + (sz[3] - 1) * nll)
  graph$mask_water <- tag$map_pressure$mask_water

  # Create the param from tag
  graph$param <- tag$param
  graph$param$thr_likelihood <- thr_likelihood
  graph$param$thr_gs <- thr_gs

  return(graph)
}



#' Prune a graph
#'
#' Pruning consists in removing "dead branch" of a graph, that is removing the edges which are not
#' connected to both the source (i.e, equipment) or sink (i.e. retrieval site).
#'
#' @param gr graph constructed with [`graph_create()`].
#' @return graph prunned
#' @family graph
#' @noRd
graph_create_prune <- function(gr, quiet = FALSE) {
  if (length(gr) < 2) {
    return(gr)
  }

  if (!quiet) {
    i <- 0
    cli::cli_progress_step(
      "Prune the graph {i}/{(length(gr) - 1) * 2} ",
      msg_done = "Prune the graph"
    )
  }

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
        "x" = "Triming the graph killed it at stationary period {.val {i_s}} moving forward."
      ))
    }
    if (!quiet) {
      i <- i_s
      cli::cli_progress_update()
    }
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
        "x" = "Triming the graph killed it at stationary period {.val {i_s}} moving backward"
      ))
    }
    if (!quiet) {
      i <- length(gr) * 2 - i_s
      cli::cli_progress_update()
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  return(gr)
}
