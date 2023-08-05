#' Create graph
#'
#' @description
#' This function returns a trellis graph representing the trajectory of a bird based on filtering and
#' prunning the likelihood maps provided.
#'
#' In the final graph, we only keep the most likely nodes (i.e., position of the bird at each
#' stationary periods) defined as (1) those whose likelihood value are within the threshold of
#' percentile `thr_likelihood` of the total likelihood map and (2) those which are connected to
#' at least one edge of the previous and next stationary periods requireing an average ground speed
#' lower than `thr_gs` (in km/h).
#'
#' For more details and illustration, see Section 2.2 of @Nussbaumer2022b and
#' [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#'
#' @param tag A GeoPressureR `tag` object.
#' @param thr_likelihood Threshold of percentile (see details).
#' @param thr_gs Threshold of groundspeed (km/h)  (see details).
#' @inheritParams tag2map
#' @return Graph as a list
#' - `id`:
#' - `s`: source node (index in the 3d grid lat-lon-stap)
#' - `t`: target node (index in the 3d grid lat-lon-stap)
#' - `gs`: average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary
#' - `obs`: observation model, corresponding to the normalized likelihood in a 3D matrix of size
#' `sz`
#' - `sz`: size of the 3d grid lat-lon-stap
#' - `stap`: data.frame of all stationary periods
#' - `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-sta)
#' - `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-sta)
#' - `extent`: same as `tag$param$extent`
#' - `scale`: same as `tag$param$scale`
#' - `mask_water`: logical matrix of water-land
#' - `param`: parameter used to create the graph, including `thr_likelihood` and `thr_gs`
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#' @family graph
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_create <- function(tag,
                         thr_likelihood = .99,
                         thr_gs = 150,
                         likelihood = NULL) {
  # Construct the likelihood map
  lk <- tag2map(tag, likelihood = likelihood)

  assertthat::assert_that(is.numeric(thr_likelihood))
  assertthat::assert_that(length(thr_likelihood) == 1)
  assertthat::assert_that(thr_likelihood >= 0 & thr_likelihood <= 1)
  assertthat::assert_that(is.numeric(thr_gs))
  assertthat::assert_that(length(thr_gs) == 1)
  assertthat::assert_that(thr_gs >= 0)

  cli::cli_progress_step("Check data input")

  # Extract info from tag for simplicity
  stap <- tag$stap
  stap_model <- which(stap$include)

  # Select only the map for the stap to model
  lk <- lk[stap_model]

  tmp <- sapply(lk, is.null)
  if (any(tmp)) {
    cli::cli_abort(c(
      x = "The {.field {likelihood}} in {.var tag} is/are null for stationary periods \\
       {.var {stap_model[tmp]}} while those stationary period are required in {.var stap$include}",
      i = "Check your input and re-run {.fun geopressure_map} if necessary."
    ))
  }

  if (length(stap_model) < 2) {
    cli::cli_abort(c(
      x = "There are only {.var {length(stap_model)}} stationary period{?s} to be modeled according \\
      to {.var stap$include}.",
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
    id_prob_percentile <- sum(cumsum(ls) <= (1 - thr_likelihood))
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

  cli::cli_progress_step("Create graph from maps")
  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  for (i_s in seq_len(sz[3] - 1)) {
    nds[[i_s + 1]] <- EBImage::distmap(!nds[[i_s]]) * resolution <
      flight_duration[i_s] * thr_gs & nds[[i_s + 1]]
    if (sum(nds[[i_s + 1]]) == 0) {
      cli::cli_abort(c(
        x = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the binary distance \\
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
        x = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance \\
          edges, there are not any nodes left at stationary period {.val {stap_model[i_s - 1]}} \\
        from stationary period {.val {stap_model[i_s]}}"
      ))
    }
  }

  # Check that there are still pixel present
  tmp <- unlist(lapply(nds, sum)) == 0
  if (any(tmp)) {
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
  future::plan(future::multisession, workers = future::availableCores() / 2)
  f <- list()

  cli::cli_progress_step(
    "Computing the groundspeed for {sum(nds_expend_sum)} edges for {length(nds_expend_sum)} \\
    stationary periods",
    spinner = TRUE
  )
  # progressr::handlers(global = TRUE)
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
        cbind(g$lon[s_id[, 2]], g$lat[s_id[, 1]]),
        cbind(g$lon[t_id[, 2]], g$lat[t_id[, 1]])
      ) / 1000 / flight_duration[i_s]

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
      gs_bearing <- geosphere::bearingRhumb(
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
      p(amount = nds_expend_sum[i])
      return(grt)
    }, seed = TRUE)
  }

  # Retrieve the graph
  gr <- future::value(f)

  # Prune
  cli::cli_progress_step("Prune graph")
  gr <- graph_create_prune(gr)

  # Convert gr to a graph list
  graph <- as.list(do.call("rbind", gr))
  attr(graph, "out.attrs") <- NULL
  graph <- structure(graph, class = "graph")

  # Add observation model as matrix
  graph$obs <- do.call(c, lk_norm)
  dim(graph$obs) <- sz

  # Add metadata information

  graph$sz <- sz
  graph$stap <- tag$stap
  graph$equipment <- which(nds[[1]] == TRUE)
  graph$retrieval <- as.integer(which(nds[[sz[3]]] == TRUE) + (sz[3] - 1) * nll)
  graph$mask_water <- tag$mask_water
  graph$extent <- tag$param$extent
  graph$scale <- tag$param$scale

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
graph_create_prune <- function(gr) {
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
        x =
          "Triming the graph killed it at stationary period {.val {i_s}} moving forward."
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
        x =
          "Triming the graph killed it at stationary period {.val {i_s}} moving backward"
      ))
    }
    cli::cli_progress_update(force = TRUE)
  }
  return(gr)
}
