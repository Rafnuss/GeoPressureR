#' Define the movement model
#'
#' Define the movement model used later by storing the parameters needed to build [`speed2prob()`].
#'
#' @param graph graph constructed with [`graph_create()`]
#' @param type Ground speed `"gs"` or airspeed `"as"`
#' @param method method used to convert the speed to probability ("gamma", "logis" or "power")
#' @param shape parameter of the gamma distribution
#' @param scale  parameter of the gamma and logistic distribution
#' @param location parameter for the logistic distribution
#' @param bird list of basic morphological traits necessary: mass, wing span, wing aspect ratio, and
#'   body frontal area. It is good practice to create a bird with [`bird_create()`].
#' @param power2prob function taking power as a single argument and returning a probability
#' @param low_speed_fix speed below which the probability remains the same. This parameter is used
#'   to allow short flights covering small distances.
#' @return Graph list with a new list `graph$movement` storing all the parameters needed to compute
#' the transition probability
#' @family graph
#' @seealso [GeoPressureManual | Basic graph](
#' https://raphaelnussbaumer.com/GeoPressureManual/basic-graph.html#output-2-marginal-probability-map)
#' @export
graph_add_movement <- function(graph,
                               type = ifelse("ws" %in% names(graph), "as", "gs"),
                               method = ifelse("ws" %in% names(graph), "power", "gamma"),
                               shape = 7,
                               scale = 7,
                               location = 40,
                               bird = NULL,
                               power2prob = \(power) (1 / power)^3,
                               low_speed_fix = 15) {
  graph_assert(graph)

  assertthat::assert_that(type == "as" | type == "gs")
  assertthat::assert_that(is.character(method))
  assertthat::assert_that(any(c("gamma", "power", "logis") == method))
  assertthat::assert_that(is.function(power2prob))

  mvt <- list(
    type = type,
    method = method,
    low_speed_fix = low_speed_fix
  )

  if (method == "gamma") {
    mvt$shape <- shape
    mvt$scale <- scale
  } else if (method == "logis") {
    mvt$scale <- scale
    mvt$location <- location
  } else if (method == "power") {
    assertthat::assert_that(inherits(bird, "bird"))
    assertthat::assert_that(is.function(power2prob))
    mvt$bird <- bird
    mvt$power2prob <- power2prob
  }

  # Add the movement list to the graph
  graph$movement <- mvt

  # Test that everything is correct
  graph_transition(graph)

  return(graph)
}
