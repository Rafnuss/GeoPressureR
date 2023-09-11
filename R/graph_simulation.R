#' Simulate randomly multiple trajectories
#'
#' @description
#' This function randomly simulates multiple trajectories from a graph using the forward filtering
#' backward sampling algorithm. For more
#' details, see [section 2.3.3 of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0013-title)
#' and the [GeoPressureManual](https://bit.ly/3YE83Wn).
#'
#' @param graph a graph object.
#' @param nj Number of simulations.
#' @param quiet logical to hide messages about the progress.
#'
#' @return Path data.frame containing the columns
#' -`stap_id` stationary period
#' - `j` unique ID for each simulation.
#' - `ind` indices of the coordinate in the 2D grid. Useful to retrieve map or graph information
#' - `lat` latitude,
#' - `lon` longitude
#' - `start` datetime of the start of the stationary period (same as in `stap`)
#' - `end` datetime of the end of the stationary period (same as in `stap`)
#' - `include` logical if stationary period was modeled (same as in `stap`)
#' - `nb_sample known` number of datapoint used to compute pressure (same as in `stap`)
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
#' graph <- graph_create(tag, quiet = TRUE)
#'
#' # Define movement model
#' graph <- graph_set_movement(graph)
#'
#' # Compute simulations
#' path_simulation <- graph_simulation(graph, quiet = TRUE)
#'
#' plot_path(path_simulation, plot_leaflet = FALSE)
#'
#' @seealso [GeoPressureManual](https://bit.ly/3YE83Wn)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @family graph
#' @export
graph_simulation <- function(graph,
                             nj = 10,
                             quiet = FALSE) {
  graph_assert(graph, "full")

  # Compute the matrix TO
  if (!quiet) {
    cli::cli_progress_step("Compute movement model")
  }
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
  if (!quiet) {
    cli::cli_progress_step("Build backward probability vector")
  }
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
  if (!quiet) {
    i_s <- 1
    cli::cli_progress_bar(
      "Simulate positions for stationary period:",
      format = "{cli::pb_name} {i_s}/{graph$sz[3]} {cli::pb_bar} {cli::pb_percent} | \\
      {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "Simulate positions for stationary periods [{cli::pb_elapsed}]",
      clear = FALSE,
      total = graph$sz[3]
    )
  }
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
    if (!quiet) {
      cli::cli_progress_update(set = i_s, force = TRUE)
    }
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

  if (!quiet) {
    cli::cli_alert_success("All done")
  }

  return(path)
}
