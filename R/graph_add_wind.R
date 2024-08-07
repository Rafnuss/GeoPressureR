#' Compute windspeed and airspeed on a `graph`
#'
#' @description
#' Reads the NetCDF files downloaded and interpolate the average windspeed experienced by the
#' bird on each possible edge, as well as the corresponding airspeed.
#'
#' In addition, the graph can be further pruned based on a threshold of airspeed `thr_as`.
#'
#' See section [2.2.4 in Nussbaumer (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0008-title)
#' for more technical details and the [GeoPressureManual](
#' https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html) for an
#' illustration on how to use it.
#'
#' @param graph a GeoPressureR graph object.
#' @param thr_as threshold of airspeed (km/h).
#' @inheritParams edge_add_wind
#'
#' @return a `graph` object with windspeed and airspeed as `ws` and `as` respectively.
#'
#' @family graph, movement
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' <https://doi.org/10.1111/2041-210X.14082>.}
#' @seealso [GeoPressureManual](
#' https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
#' @export
graph_add_wind <- function(
    graph,
    pressure,
    rounding_interval = 60,
    interp_spatial_linear = FALSE,
    file = \(stap_id) glue::glue("./data/wind/{graph$param$id}/{graph$param$id}_{stap_id}.nc"),
    thr_as = Inf,
    quiet = FALSE) {
  graph_assert(graph, "full")
  assertthat::assert_that(is.numeric(thr_as))
  assertthat::assert_that(length(thr_as) == 1)
  assertthat::assert_that(thr_as >= 0)

  # Check that all the files of wind_speed exist and match the data request
  uv <- edge_add_wind(graph,
    edge_s = graph$s,
    edge_t = graph$t,
    pressure = pressure,
    variable = c("u", "v"),
    rounding_interval = rounding_interval,
    interp_spatial_linear = interp_spatial_linear,
    return_averaged_variable = TRUE,
    file = file,
    quiet = quiet
  )

  # save windspeed in complex notation and convert from m/s to km/h
  graph$ws <- (uv[, 1] + 1i * uv[, 2]) / 1000 * 60 * 60

  # filter edges based on airspeed
  id <- abs(graph$gs - graph$ws) <= thr_as

  # Check that there are always at least one node left by stap
  g <- map_expand(graph$param$extent, graph$param$scale)
  edge_s <- arrayInd(graph$s[id], c(g$dim, nrow(graph$stap)))
  sta_pass <- which(!(seq_len(graph$sz[3] - 1) %in% unique(edge_s[, 3])))
  if (length(sta_pass) > 0) {
    cli::cli_abort(c(
      x = "Using the {.val thr_as} of {thr_as} km/h provided with the exact distance of edges, \\
      there are not any nodes left for the stationary period: {sta_pass} with a minimum airspeed \\
      of {min(abs(as[edge_s[, 3] == sta_pass]))} km/h."
    ))
  }

  graph$s <- graph$s[id]
  graph$t <- graph$t[id]
  graph$gs <- graph$gs[id]
  graph$ws <- graph$ws[id]
  graph$param$thr_as <- thr_as
  attr(file, "srcref") <- NULL
  attr(file, "srcfile") <- NULL
  environment(file) <- baseenv()
  graph$param$wind_file <- file

  return(graph)
}
