#' Create a `map`
#'
#' This function create a GeoPressureR `map` object
#
#' @param data List of matrices of the same size, one for each stationary period
#' @inheritParams tag_setmap
#' @param stap A stationary period data.frame (see [`tag_label_stap()`])
#' @inheritParams tag_create
#' @param type Type of data one of "unknown","pressure", "light", "pressure_mse", "water_mask",
#' "pressure_mask", "marginal"
#'
#' @return A GeoPressure `map` object is returned
#'
#' @family map
#' @export
map_create <- function(data, extent, scale, stap, id = NA, type = "unknown") {

  g <- map_expand(extent, scale)

  assertthat::assert_that(is.list(data))
  stap_id_null <- sapply(data, is.null)
  lapply(data[!stap_id_null], \(x) assertthat::assert_that(is.matrix(x)))
  data_dim <- sapply(data[!stap_id_null], \(x) dim(x))
  assertthat::assert_that(length(unique(data_dim[1,])) == 1 & length(unique(data_dim[2,])) == 1,
                          msg = "All matrices of data don't have the same size")
  assertthat::assert_that(assertthat::are_equal(length(g$lat), data_dim[1]))
  assertthat::assert_that(assertthat::are_equal(length(g$lon), data_dim[2]))
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, c("stap_id", "start", "end")))
  assertthat::assert_that(assertthat::are_equal(nrow(stap), length(data)))

  assertthat::assert_that(is.character(type))
  assertthat::assert_that(type %in% c("unknown","pressure", "light", "pressure_mse", "water_mask",
                                      "pressure_mask", "marginal"))

  map <- structure(list(
    id = id,
    data = data,
    extent = extent,
    scale = scale,
    lat = g$lat,
    lon = g$lon,
    stap = stap,
    type = type), class ="map")

  return(map)
}

#' @rdname map_create
#' @export
"[.map" <- function(x, i, ...) {
  x$data[i]
}

#' @rdname map_create
#' @export
"[[.map" <- function(x, i, ...) {
  x$data[[i]]
}

#' @rdname map_create
#' @export
length.map <- function(x) {
  length(x$data)
}

#' @rdname map_create
#' @export
dim.map <- function(x) {
  c(length(x$lat), length(x$lon), length(x$data))
}


#' @rdname map_create
#' @export
`*.map` <- function(x, y) {
  assertthat::assert_that(inherits(x, "map"))
  assertthat::assert_that(inherits(y, "map"))
  assertthat::assert_that(assertthat::are_equal(x$scale, y$scale))
  assertthat::assert_that(assertthat::are_equal(x$extent, y$extent))
  assertthat::assert_that(assertthat::are_equal(x$id, y$id))
  assertthat::assert_that(assertthat::are_equal(dim(x), dim(y)))

  # Compute value
  x$data <- mapply(\(p, l) p * l, x$data, y$data, SIMPLIFY = FALSE)

  x$stap <- merge(x$stap, y$stap)

  x$type = glue::glue("{x$type} x {y$type}")

  return(x)
}
