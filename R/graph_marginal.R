#' Compute marginal probability map
#'
#' Compute the marginal probability map from a graph. The computation uses the [forward
#' backward algorithm](https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm). For more
#' details, see [section 2.3.2 of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0012-title)
#' and the [GeoPressureManual](https://bit.ly/3sd20vC).
#'
#' @param graph a GeoPressureR `graph` with defined movement model `graph_set_movement()`.
#'
#' @return A list of the marginal maps for each stationary period (even those not modelled). Best to
#' include within `tag`.
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
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
#' # Compute marginal
#' marginal <- graph_marginal(graph)
#'
#' plot(marginal)
#'
#' @seealso [GeoPressureManual](https://bit.ly/3sd20vC)
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
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
  marginal_data <- vector("list", nrow(graph$stap))
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
    marginal_data[[stap_includeed[i_s]]] <- map_fb_i
  }

  marginal <- map_create(
    data = marginal_data,
    extent = graph$param$extent,
    scale = graph$param$scale,
    stap = graph$stap,
    id = graph$param$id,
    type = "marginal"
  )

  return(marginal)
}
