#' Define the movement model
#'
#' Define the movement model used later by storing the parameters needed to build [`speed2prob()`].
#'
#' @param graph graph constructed with [`graph_create()`]
#' @param type Ground speed `"gs"` or airspeed `"as"`
#' @param ... 	Arguments to be passed to [`speed2prob()`]
#' @return Graph list with a new list `graph$movement` storing all the parameters needed to compute
#' the transition probability
#' @seealso [`graph_create()`], [`graph_transition()`], [`graph_create()`],
#' [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @export
graph_add_movement <- function(graph,
                               type = ifelse("ws" %in% names(graph), "as", "gs"),
                               ...) {
  assertthat::assert_that(is.list(graph))
  assertthat::assert_that(type == "as" | type == "gs")

  graph$movement <- list(
    type = type,
    ...
  )

  # Test that everything is correct
  graph_transition(graph)

  return(graph)
}
