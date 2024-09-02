#' Compute the most likely trajectory
#'
#' @description
#' Compute the trajectory which maximizes the joint probability using the [Viterbi algorithm](
#' https://en.wikipedia.org/wiki/Viterbi_algorithm) on the graph structure. For more
#' details, see [section 2.3.1 of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0011-title)
#' and the [GeoPressureManual](https://bit.ly/4578D12).
#'
#' @param graph a graph object.
#' @param quiet logical to hide messages about the progress.
#'
#' @return Path data.frame containing the columns
#' -`stap_id` stationary period
#' - `j` unique ID for each path, here always 1 as there is a single path.
#' - `ind` indices of the coordinate in the 2D grid. Useful to retrieve map or graph information
#' - `lat` latitude,
#' - `lon` longitude
#' - `start` datetime of the start of the stationary period (same as in `stap`)
#' - `end` datetime of the end of the stationary period (same as in `stap`)
#' - `include` logical if stationary period was modelled (same as in `stap`)
#'
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
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
#' setwd(owd)
#'
#' # Create graph
#' graph <- graph_create(tag, quiet = TRUE)
#'
#' # Define movement model
#' graph <- graph_set_movement(graph)
#'
#' # Compute most likely path
#' path_most_likely <- graph_most_likely(graph, quiet = TRUE)
#'
#' plot_path(path_most_likely)
#'
#' @seealso [GeoPressureManual](https://bit.ly/4578D12)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @family graph
#' @export
graph_most_likely <- function(graph, quiet = FALSE) {
  graph_assert(graph, "full")

  # number of nodes in the 3d grid
  n <- prod(graph$sz)

  # Compute the matrix TO
  if (!quiet) {
    cli::cli_progress_step("Compute movement model")
  }
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
  if (!quiet) {
    cli::cli_progress_step("Create edge data.frame")
  }
  node <- data.frame(
    s = graph$s,
    t = graph$t,
    to = trans_obs,
    stap = arrayInd(graph$s, graph$sz)[, 3]
  )

  # Split this data.fram by stationary period (of the source)
  node_stap <- split(node, node$stap)

  n_edge <- sapply(node_stap, nrow)

  if (!quiet) {
    i_s <- 0
    cli::cli_progress_bar(
      "Compute most likely position for stationary period:",
      format = "{cli::pb_name} {i_s}/{length(node_stap)} {cli::pb_bar} {cli::pb_percent} | \\
      {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "Compute most likely position for stationary periods [{cli::pb_elapsed}]",
      clear = FALSE,
      total = sum(n_edge)
    )
  }

  for (i_s in seq_len(length(node_stap))) {
    node_i_s <- node_stap[[i_s]]

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

    if (!quiet) {
      cli::cli_progress_update(set = sum(n_edge[1:i_s]), force = TRUE)
    }
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

  # path_ind2d was defined on the modelled stationary period which might be different than the full
  # stap, but we want to generate path at the level of all stationary periods
  path_ind2d_full <- rep(NA, nrow(graph$stap))
  # find the stap_id of the model
  stap_include <- graph$stap$stap_id[graph$stap$include]
  path_ind2d_full[stap_include] <- path_ind2d

  # Convert the index of the path in a path data.frame
  path <- ind2path(path_ind2d_full, graph)

  # Assign the type of path
  attr(path, "type") <- "most_likely"

  if (!quiet) {
    cli::cli_alert_success("All done")
  }

  return(path)
}
