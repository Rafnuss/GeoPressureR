#' Compute transition probability of graph
#'
#' Use the movement model (see `graph_add_movement()`) to convert ground speed `gs` (or airspeed `as`
#' if available) into the transition probability of the edges of the graph.
#'
#' The vector return correspond to the elements of the transition matrices
#' \eqn{T_k \forall k \in [1,n]} extracted for all edges considered in the graph. Each of these
#' values thus corresponds to the probability \eqn{P(X_k \mid X_{k-1})}, where \eqn{X_k} is the random
#' variable of the position of the bird at time \eqn{k}.
#'
#' To create a generic function, we define `speed2prob` which converts the speed of an edge into the
#' transition probability.
#'
#'
#' @param graph Graph constructed with `graph_create()` and with a movement (see
#' `graph_add_movement()`).
#' @return Vector of transition probability for each edge.
#' @seealso [`graph_create()`], [`graph_add_movement()`]
#' @export
graph_transition <- function(graph) {
  assertthat::assert_that(is.graph(graph))
  if (!assertthat::has_name(graph, "movement")) {
    cli::cli_abort(c(
      x = "The graph does not have a movement model.",
      i = "Make sure to call {.fn graph_add_movement} before."
    ))
  }
  assertthat::assert_that(assertthat::has_name(graph, c("movement", "gs")))

  if ("transition" %in% names(graph)) {
    transition <- graph$trans
  } else {
    speed2prob_param <- graph$movement[!(names(graph$movement) == "type")]
    if (graph$movement$type == "as") {
      assertthat::assert_that(assertthat::has_name(graph, c("ws")))
      transition <- do.call(speed2prob, c(speed2prob_param, list(speed = graph$gs - graph$ws)))
    } else if (graph$movement$type == "gs") {
      transition <- do.call(speed2prob, c(speed2prob_param, list(speed = graph$gs)))
    } else {
      cli::cli_abort("Invalid movement type : {.val {graph$movement$type}}")
    }
    return(transition)
  }
}
