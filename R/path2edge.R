#' Extract the edges of a `path` from a `graph`
#'
#' @description
#' Retrieve the edges in a `graph` corresponding to the flight transition defined by a `path`. These
#' edges can be useful to extract flight information specific to a path.
#'
#' @param path a GeoPressureR `path` data.frame
#' @param graph a GeoPressureR `graph` object
#' @param add_flight compute flight data.frame and merge with the return data.frame
#'
#' @return Data.frame of the edge containing:
#' - `stap_s` : stationary period of the origin (source).
#' - `stap_t` : stationary period of the destination (target).
#' - `gs` : groundspeed vector expressed as a complex number. You can compute the groundspeed value
#' (km/h) with `abs(gs)`, the W-E and S-N component of the flight with `Re(gs)` and `Im(gs)`, and
#'  the angle/direction with `Arg(gs)`.
#' - `distance` : Distance (in km) of the flight.
#' - `ws`: if computed with `graph_add_wind()`, same value as `gs`. Airspeed is computed with
#' `as = gs - ws` in complex number to keep the vectorial additive properties.
#' - `start` end of the flight. If `add_flight=T`.
#' - `end` start of the flight. If `add_flight=T`.
#' - `duration` duration of the flight. If `add_flight=T`.
#' @family path
#' @seealso [GeoPressureManual](https://bit.ly/47MhQxN)
#' @export
path2edge <- function(path, graph, add_flight = TRUE) {
  graph_assert(graph)

  g <- map_expand(graph$param$extent, graph$param$scale)
  nll <- prod(g$dim)

  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("stap_id", "ind")))

  # number of stationary period
  assertthat::assert_that(all(unique(path$stap_id) == graph$stap$stap_id))

  # Number of paths
  nj <- length(unique(path$j))

  ind2d <- matrix(path$ind[!is.na(path$ind)], nrow = nj)
  lat <- matrix(path$lat[!is.na(path$ind)], nrow = nj)
  lon <- matrix(path$lon[!is.na(path$ind)], nrow = nj)

  ind3d <- ind2d + t(replicate(nj, nll * (seq_len(graph$sz[3]) - 1)))

  assertthat::assert_that(all(ind3d > 0))
  assertthat::assert_that(all(ind3d <= prod(graph$sz)))

  # construct the edge of the path as data.frame
  path_st <- data.frame(
    s = as.vector(utils::head(ind3d, c(nj, -1))),
    t = as.vector(utils::tail(ind3d, c(nj, -1))),
    lat_s = as.vector(utils::head(lat, c(nj, -1))),
    lat_t = as.vector(utils::tail(lat, c(nj, -1))),
    lon_s = as.vector(utils::head(lon, c(nj, -1))),
    lon_t = as.vector(utils::tail(lon, c(nj, -1)))
  )

  # Check that all sources and target exist in the graph
  assertthat::assert_that(all(path_st$s %in% graph$s),
    msg = "path_id is not compatible with the graph$s."
  )
  assertthat::assert_that(all(path_st$t %in% graph$t),
    msg = "path_id is not compatible with the graph$t."
  )

  # Build data.frame of the graph
  graph_st <- data.frame(
    edge = seq_len(length(graph$s)),
    s = graph$s,
    t = graph$t
  )

  # Shorten graph to only node of interest
  graph_st_s <- graph_st[graph_st$s %in% path_st$s & graph_st$t %in% path_st$t, ]

  # Find index of edge
  e <- merge(path_st, graph_st_s, all.x = TRUE, sort = FALSE)

  edge <- data.frame(
    stap_s = ceiling(e$s / prod(graph$sz[c(1, 2)])),
    stap_t = ceiling(e$t / prod(graph$sz[c(1, 2)])),
    gs = graph$gs[e$edge],
    distance = geosphere::distGeo(cbind(e$lon_s, e$lat_s), cbind(e$lon_t, e$lat_t)) / 1000
  )

  if ("ws" %in% names(graph)) {
    edge$ws <- graph$ws[e$edge]
  }

  if (add_flight) {
    flight <- stap2flight(graph$stap)
    edge <- merge(edge, flight)
  }

  # Sort by stap_s
  edge <- edge[order(edge$stap_s), ]

  return(edge)
}
