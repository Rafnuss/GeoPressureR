#' Create graph
#'
#' @description
#' This function returns a graph representing the trajectory of a bird based on filtering and
#' trimming the likelihood maps provided.
#'
#' In the final graph, we only keep the most likely nodes (position in time) defined as (1) those
#' whose cumulative probability reaches up to `thr_likelihood` for each stationary period and (2)
#' those which are connected to edges whose average ground speed is lower than `thr_gs` km/h.
#'
#'
#' @inheritParams geopressure_map
#' @param thr_likelihood Threshold of percentile (see details).
#' @param thr_gs Threshold of groundspeed (km/h)  (see details).
#' @param likelihood Field of the `tag` list containing the likelihood map (character). Default
#' `NA` is to take the product of `map_pressure` and `map_light` if available.
#' @return Graph as a list
#' - `s`:   source node (index in the 3d grid lat-lon-stap)
#' - `t`:   target node (index in the 3d grid lat-lon-stap)
#' - `gs`:  average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary
#' - `obs`:   observation model, corresponding to the normalized likelihood in a 3D matrix of size
#' `sz`
#' - `sz`:  size of the 3d grid lat-lon-stap
#' - `stap`: data.frame of all stationary periods
#' - `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-sta)
#' - `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-sta)
#' - `extent`: same as `tag$extent`
#' - `scale`: same as `tag$scale`
#' - `mask_water`: logical matrix of water-land
#' - `param`: parameter used to create the graph, including `thr_likelihood` and `thr_gs`
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#create-the-graph)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_create <- function(tag,
                         thr_likelihood = .99,
                         thr_gs = 150,
                         likelihood = NA) {
  # Check input
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "stap"))
  assertthat::assert_that(is.data.frame(tag$stap))
  assertthat::assert_that(assertthat::has_name(tag$stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "start"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "end"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "known_lat"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "known_lon"))
  assertthat::assert_that(assertthat::has_name(tag, "scale"))
  assertthat::assert_that(assertthat::has_name(tag, "extent"))

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

  # Construct the likelihood map
  if (all(is.na(likelihood))) {
    likelihood <- c("map_pressure", "map_light")
    tmp <- likelihood %in% names(tag)
    if (all(tmp)) {
      lk <- mapply(\(p, l) {
        if (is.null(p) | is.null(l)) {
          return(NULL)
        } else {
          return(p * l)
        }
      }, tag$map_pressure, tag$map_light, SIMPLIFY = FALSE)
    } else if (any(tmp)) {
      likelihood <- likelihood[tmp]
      lk <- tag[[likelihood]]
    } else {
      cli::cli_abort(c(
        x = "None of {.field {likelihood}} are present in {.var tag}",
        i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}"
      ))
    }
  } else {
    assertthat::assert_that(assertthat::has_name(tag, likelihood))
    lk <- tag[[likelihood]]
  }

  # Select only the map for the stap to model
  lk <- lk[stap_model]

  tmp <- sapply(lk, is.null)
  if (any(tmp)) {
    cli::cli_abort(c(
      x = "The {.field {likelihood}} in {.var tag} is/are null for stationary periods \\
       {.var {stap_model[tmp]}}  while those stationary period are required in {.var stap$include}",
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

  g <- geo_expand(tag$extent, tag$scale)

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

  # Trim
  cli::cli_progress_step("Trim graph")
  gr <- graph_create_trim(gr)

  # Convert gr to a graph list
  graph <- as.list(do.call("rbind", gr))
  attr(graph, "out.attrs") <- NULL
  graph <- structure(graph, class = "graph")

  # Add observation model as matrix
  graph$obs <- do.call(c, lk_norm)
  dim(graph$obs) <- sz

  # Add metadata information
  graph$id <- tag$id
  graph$sz <- sz
  graph$stap <- tag$stap
  graph$equipment <- which(nds[[1]] == TRUE)
  graph$retrieval <- as.integer(which(nds[[sz[3]]] == TRUE) + (sz[3] - 1) * nll)
  graph$mask_water <- tag$mask_water
  graph$extent <- tag$extent
  graph$scale <- tag$scale
  graph$param <- list(
    thr_likelihood = thr_likelihood,
    thr_gs = thr_gs
  )
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
graph_create_trim <- function(gr) {
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