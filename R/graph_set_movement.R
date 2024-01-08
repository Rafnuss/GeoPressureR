#' Define the movement model of a `graph`
#'
#' @description
#' Configure the movement model of a `graph` by defining the value of the parameters needed to build
#' the transition `graph_transition()` through `speed2prob()`.
#'
#' Three methods are currently implemented with two parametric function `"gamma"` and `"logis"`
#' suitable when wind data is not available and are thus defining the probability of a groundspeed.
#'
#' If wind data is available, it is recommended to use the `"power"` method which rely on the power
#' curve equation (energy vs airspeed) to estimate the probability of a airspeed. Read more about
#' this approach in [section 2.2.5. of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0009-title)
#'
#' @param graph a GeoPressureR `graph` object.
#' @param type Ground speed `"gs"` or airspeed `"as"`
#' @param method method used to convert the speed to probability ("gamma", "logis" or "power")
#' @param shape parameter of the gamma distribution
#' @param scale  parameter of the gamma and logistic distribution
#' @param location parameter for the logistic distribution
#' @param bird A GeoPressureR `bird` object containing the basic morphological traits necessary:
#'  mass, wing span, wing aspect ratio, and body frontal area. See `bird_create()`.
#' @param power2prob function taking power as a single argument and returning a probability
#' @param low_speed_fix speed below which the probability remains the same. This parameter is used
#'   to allow short flights covering small distances.
#'
#' @return Graph list with a new list `graph$movement` storing all the parameters needed to compute
#' the transition probability
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
#' graph <- graph_create(tag, quiet = TRUE)
#'
#' graph <- graph_set_movement(graph,
#'   method = "gamma",
#'   shape = 4,
#'   scale = 6,
#'   low_speed_fix = 10
#' )
#' plot_graph_movement(graph)
#'
#' graph <- graph_set_movement(graph,
#'   method = "logis",
#'   shape = 4,
#'   location = 60,
#'   low_speed_fix = 10
#' )
#' plot_graph_movement(graph)
#'
#' @family graph
#' @family movement
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @export
graph_set_movement <- function(graph,
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
  graph$param$movement <- mvt

  # Test that everything is correct
  graph_transition(graph)

  return(graph)
}
