#' Extract the edges of a `path` from a `graph`
#'
#' @description
#' Retrieve the edges in a `graph` corresponding to the flight transition defined by a `path`. These
#' edges can be useful to extract flight information specific to a path.
#'
#' @param path a GeoPressureR `path` data.frame
#' @param graph either a `tag` or a `graph` GeoPressureR object.
#'
#' @return Data.frame of the edge containing:
#' - `s`: index in 3D (lat-lon-stap) of the origin (source).
#' - `t`: index in 3D (lat-lon-stap) of the destination (target).
#' - `lat_s`: latitude of the origin (source).
#' - `lat_t`: latitude of the destination (target).
#' - `lon_s`: longitude of the origin (source).
#' - `lon_t`: longitude of the destination (target).
#' - `stap_s`: stationary period of the origin (source).
#' - `stap_t`: stationary period of the destination (target).
#' - `distance`: Distance (in km) of the flight.
#' - `start`: end of the flight.
#' - `end`: start of the flight.
#' - `duration`: duration of the flight.
#' - `n`: number of flight.
#' - `gs`: groundspeed vector expressed as a complex number. You can compute the groundspeed value
#' (km/h) with `abs(gs)`, the W-E and S-N component of the flight with `Re(gs)` and `Im(gs)`, and
#'  the angle/direction with `Arg(gs)`. If graph provided.
#' - `ws`: if computed with `graph_add_wind()`, same value as `gs`. Airspeed is computed with
#' `as = gs - ws` in complex number to keep the vectorial additive properties. If graph provided.
#' @family path
#' @seealso [GeoPressureManual](https://bit.ly/47MhQxN)
#' @export
path2edge <- function(path, tag_graph) {
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("stap_id", "ind")))

  assertthat::assert_that(inherits(tag_graph, "tag") | inherits(tag_graph, "graph"))
  stap <- tag_graph$stap
  assertthat::assert_that(all(unique(path$stap_id) == stap$stap_id))

  g <- map_expand(tag_graph$param$extent, tag_graph$param$scale)
  nll <- prod(g$dim)

  # Number of paths
  nj <- length(unique(path$j))

  # Find stap included
  stap_id_included <- unique(path$stap_id[!is.na(path$ind)])
  # should be similar to stap$include in most case.

  ind2d <- matrix(path$ind[path$stap_id %in% stap_id_included], nrow = nj)
  lat <- matrix(path$lat[path$stap_id %in% stap_id_included], nrow = nj)
  lon <- matrix(path$lon[path$stap_id %in% stap_id_included], nrow = nj)

  ind3d <- ind2d + t(replicate(nj, nll * (seq_len(ncol(ind2d)) - 1)))

  stap3d <- t(replicate(nj, stap_id_included))

  # construct the edge of the path as data.frame
  edge <- data.frame(
    stap_s = as.vector(utils::head(stap3d, c(nj, -1))),
    stap_t = as.vector(utils::tail(stap3d, c(nj, -1))),
    s = as.vector(utils::head(ind3d, c(nj, -1))),
    t = as.vector(utils::tail(ind3d, c(nj, -1))),
    lat_s = as.vector(utils::head(lat, c(nj, -1))),
    lat_t = as.vector(utils::tail(lat, c(nj, -1))),
    lon_s = as.vector(utils::head(lon, c(nj, -1))),
    lon_t = as.vector(utils::tail(lon, c(nj, -1)))
  )

  edge <- merge(edge, stap2flight(tag_graph$stap, include_stap_id = stap_id_included))

  edge$distance <- geosphere::distGeo(cbind(edge$lon_s, edge$lat_s), cbind(edge$lon_t, edge$lat_t)) / 1000

  # Compute the bearing of the trajectory
  edge$bearing <- geosphere::bearing(cbind(edge$lon_s, edge$lat_s), cbind(edge$lon_t, edge$lat_t))
  # bearing is NA if gs==0, fix for computing the complex representation
  edge$bearing[is.na(edge$bearing) & !is.na(edge$distance)] <- 0

  # save groundspeed in complex notation
  gs_abs <- edge$distance / edge$duration
  gs_arg <- (450 - edge$bearing) %% 360
  edge$gs <- gs_abs * cos(gs_arg * pi / 180) + 1i * gs_abs * sin(gs_arg * pi / 180)

  if (inherits(tag_graph, "graph")) {
    # Check that all sources and target exist in the graph
    assertthat::assert_that(all(edge$s %in% tag_graph$s),
      msg = "path$ind is not compatible with the graph$s."
    )
    assertthat::assert_that(all(edge$t %in% tag_graph$t),
      msg = "path$ind is not compatible with the graph$t."
    )

    # Build data.frame of the graph
    graph_st <- data.frame(
      edge_id = seq_len(length(tag_graph$s)),
      s = tag_graph$s,
      t = tag_graph$t
    )

    # Shorten graph to only node of interest
    graph_st <- graph_st[graph_st$s %in% edge$s & graph_st$t %in% edge$t, ]

    # Find index of edge
    tmp <- merge(edge, graph_st, all.x = TRUE, sort = FALSE)

    edge$gs <- tag_graph$gs[tmp$edge_id]
    if ("ws" %in% names(tag_graph)) {
      edge$ws <- tag_graph$ws[tmp$edge_id]
    }
  }

  # Sort by stap_s
  edge <- edge[order(edge$stap_s), ]

  return(edge)
}
