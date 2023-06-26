#' Marginal probability map
#'
#' Compute the marginal probability map from a graph. The graph needs to have a movement model
#' defined (see `graph_add_movement()`). The computation uses the [forward
#' backward algorithm](https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm).
#'
#' @param graph graph constructed with [`graph_create()`].
#' @return A list of the marginal maps for each stationary period (even those not modelled). Best to
#' include within `tag`.
#' @seealso [`graph_create()`], [`graph_add_movement()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_marginal <- function(graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "stap", "equipment", "retrieval", "mask_water", "extent", "scale"
  )))
  assertthat::assert_that(length(graph$s) > 0)

  # Compute the transition matrix (movement model)
  transition <- graph_transition(graph)

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # matrix of transition * observation
  trans_obs <- Matrix::sparseMatrix(graph$s, graph$t,
    x = transition * graph$obs[graph$t], dims = c(n, n)
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
  marginal <- vector("list", nrow(graph$stap))
  stap_includeed <- graph$stap$stap_id[graph$stap$include]
  for (i_s in seq_len(graph$sz[3])) {
    map_fb_i <- map_fb[, , i_s]
    map_fb_i[graph$mask_water] <- NA
    if (sum(map_fb_i, na.rm = TRUE) == 0) {
      cli::cli_abort(c(
        x = "The probability of some transition are too small to find numerical solution.",
        i = "Please check the data used to create the graph."
      ))
    }
    marginal[[stap_includeed[i_s]]] <- map_fb_i
  }
  return(marginal)
}




#' Simulation of trajectories
#'
#' This function randomly simulates multiple trajectories from a graph using the forward filtering
#' backward sampling algorithm.
#'
#' @param graph Graph constructed with [`graph_create()`].
#' @param nj Number of simulations.
#' @return Data.frame of simulated paths. See [`ind2path()`] for the structure.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_simulation <- function(graph,
                             nj = 10) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "stap", "equipment", "retrieval", "mask_water", "extent", "scale"
  )))
  assertthat::assert_that(length(graph$s) > 0)

  # Compute the matrix TO
  trans_obs <- graph_transition(graph) * graph$obs[graph$t]

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

  # Initialize the path, stored as index of the 3D grid
  path_ind3d <- matrix(ncol = graph$sz[3], nrow = nj)

  # Sample the first position with map_b and map_f as f_0 = P_0 * O_0
  map_f_0 <- Matrix::sparseMatrix(1, 1, x = 0, dims = c(1, n))
  map_f_0[graph$equipment] <- graph$obs[graph$equipment]
  map_fb <- map_b[[1]][1:nll] * map_f_0[1:nll]

  for (i_j in seq_len(nj)) {
    path_ind3d[i_j, 1] <- sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
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
    map_f <- Matrix::sparseMatrix(seq_len(nj), path_ind3d[, i_s - 1], x = 1, dims = c(nj, n)) %*%
      trans_obs_l

    # Combine forward and backward and samples
    if (nj > 1) {
      path_ind2d <- apply(map_f[, nll * (i_s - 1) + (1:nll)], 1, function(map_f_i) {
        map_fb <- map_f_i * map_b[[i_s]][nll * (i_s - 1) + (1:nll)]
        sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
      })
    } else {
      map_fb <- map_f[, nll * (i_s - 1) + (1:nll)] * map_b[[i_s]][nll * (i_s - 1) + (1:nll)]
      path_ind2d <- sum(stats::runif(1) > cumsum(map_fb) / sum(map_fb)) + 1
    }

    # Convert ids into 3D coordinates
    path_ind3d[, i_s] <- path_ind2d + nll * (i_s - 1)

    # Update progress bar
    cli::cli_progress_update(set = i_s, force = TRUE)
  }

  # convert 3D to 2D grid
  path_ind2d <- path_ind3d - t(replicate(nj, nll * (seq_len(graph$sz[3]) - 1)))

  # Path was defined on the modeled stationary period which might be different than the full stap,
  # but we want to generate path at the level of all stationary periods
  path_ind2d_full <- matrix(ncol = nrow(graph$stap), nrow = nj)
  # find the stap_id of the model
  stap_includeed <- graph$stap$stap_id[graph$stap$include]
  path_ind2d_full[, stap_includeed] <- path_ind2d

  # Convert the index of the path in a path data.frame
  path <- ind2path(path_ind2d_full, graph)

  return(path)
}


#' Most likely trajectory
#'
#' Compute the trajectory which maximizes the overall probability using the [Viterbi algorithm](
#' https://en.wikipedia.org/wiki/Viterbi_algorithm) on the graph structure. The graph needs to have
#' a movement model defined (see `graph_add_movement()`).
#'
#' @param graph Graph constructed with [`graph_create()`].
#' @return Path data.frame. See [`ind2path()`] for the structure.
#' @seealso [`graph_create()`], [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-3-simulate-path)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_most_likely <- function(graph) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(assertthat::has_name(graph, c(
    "s", "t", "obs", "sz", "stap", "equipment", "retrieval", "mask_water", "extent", "scale"
  )))
  assertthat::assert_that(length(graph$s) > 0)

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # Compute the matrix TO
  trans_obs <- graph_transition(graph) * graph$obs[graph$t]

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
  path_ind3d <- c()
  # Initiate the last position as the maximum of all retrieval node
  path_ind3d[graph$sz[3]] <- graph$retrieval[which.max(path_max[graph$retrieval])]
  # Iteratively find the previous node of the path
  for (i_s in (graph$sz[3] - 1):1) {
    path_ind3d[i_s] <- path_s[path_ind3d[i_s + 1]]
  }

  # convert 3D to 2D grid
  path_ind2d <- path_ind3d - prod(graph$sz[c(1, 2)]) * (seq_len(graph$sz[3]) - 1)

  # path_ind2d was defined on the modeled stationary period which might be different than the full stap,
  # but we want to generate path at the level of all stationary periods
  path_ind2d_full <- rep(NA, nrow(graph$stap))
  # find the stap_id of the model
  stap_includeed <- graph$stap$stap_id[graph$stap$include]
  path_ind2d_full[stap_includeed] <- path_ind2d

  # Convert the index of the path in a path data.frame
  path <- ind2path(path_ind2d_full, graph)

  return(path)
}
