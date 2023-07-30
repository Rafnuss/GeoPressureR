#' Marginal probability map
#'
#' Compute the marginal probability map from a graph. The computation uses the [forward
#' backward algorithm](https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm). See
#' section 2.3.2 in Nussbaumer (2023) for more information.
#'
#' @param graph graph constructed with [`graph_create()`].
#' @return A list of the marginal maps for each stationary period (even those not modelled). Best to
#' include within `tag`.
#'
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. “Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model.” *Methods in Ecology and Evolution*.
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @family graph
#' @export
graph_marginal <- function(graph) {
  graph_assert(graph, "full")

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

  # Add attribute
  attr(marginal, "id") <- graph$id
  attr(marginal, "extent") <- graph$extent
  attr(marginal, "scale") <- graph$scale
  attr(marginal, "stap") <- graph$stap

  return(marginal)
}
