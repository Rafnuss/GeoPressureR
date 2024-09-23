#' Compute transition probabilities of a `graph`
#'
#' Use the movement model (see `graph_set_movement()`) to convert ground speed `gs` (or airspeed
#' `as` if available) into the transition probability of the edges of the graph.
#'
#' The vector return correspond to the elements of the transition matrices
#' \eqn{T_k \forall k \in [1,n]} extracted for all edges considered in the graph. Each of these
#' values thus corresponds to the probability \eqn{P(X_k \mid X_{k-1})}, where \eqn{X_k} is the
#' random variable of the position of the bird at time \eqn{k}.
#'
#' To create a generic function, we define `speed2prob` which converts the speed of an edge into the
#' transition probability.
#'
#' @param graph Graph constructed with `graph_create()` and with a movement (see
#' `graph_set_movement()`).
#' @return Vector of transition probability for each edge.
#'
#' @family movement
#' @export
graph_transition <- function(graph) {
  graph_assert(graph)

  # The full transition vector can be specify manually as graph$transition
  if ("transition" %in% names(graph)) {
    return(graph$transition)
  }

  # Check that movement has been defined
  graph_assert(graph, "movement")


  if (graph$param$graph_set_movement$type == "as") {
    transition <- speed2prob(graph$gs - graph$ws, graph$param$graph_set_movement)
  } else if (graph$param$graph_set_movement$type == "gs") {
    transition <- speed2prob(graph$gs, graph$param$graph_set_movement)
  } else {
    cli::cli_abort(c(
      "x" = "Movement type not correct.",
      "i" = "{.code graph$param$graph_set_movement$type} needs to be {.val as} or {.val gs}"
    ))
  }

  return(transition)
}
