#' Create a `map` object
#'
#' @description
#' This function create a GeoPressureR `map` object from a spatio-temporal dataset.
#' The data needs to be discretized according to `scale`, `extend` (space) and `stap` (time).
#'
#' This functions is used by `geopressure_map` and `graph_marginal`.
#
#' @param data list of matrices of the same size, one for each stationary period.
#' @inheritParams tag_set_map
#' @param stap a data.frame of stationary periods.
#' @inheritParams tag_create
#' @param type type of data one of `"unknown"`,`"pressure"`, `"light"`, `"pressure_mse"`,
#' `"water_mask"`, `"pressure_mask"`, `"marginal"`. Allows for custom colour palette on plot.
#'
#' @return A GeoPressure `map` object is returned
#'
#'
#' @examples
#' data <- lapply(1:10, \(x) matrix(runif(5000), nrow = 50, ncol = 100))
#' scale <- 10
#' extent <- c(0, 10, 0, 5)
#' seq(as.Date("2023-01-01"), as.Date("2023-01-10"), by = "day")
#' stap <- data.frame(
#'   stap_id = 1:10,
#'   start = seq(as.POSIXct("2023-01-01", tz = "UTC"),
#'     as.POSIXct("2023-01-10 UTC", tz = "UTC"),
#'     by = "day"
#'   ),
#'   include = TRUE
#' )
#' stap$end <- stap$start + sample(1:10) * 10000
#'
#' # Create the map
#' map <- map_create(
#'   data = data,
#'   extent = extent,
#'   scale = scale,
#'   stap = stap,
#'   id = "18LX",
#'   type = "pressure"
#' )
#'
#' print(map)
#'
#' plot(map)
#'
#' @family map
#' @export
map_create <- function(data,
                       extent,
                       scale,
                       stap,
                       id = NA,
                       type = "unknown") {
  g <- map_expand(extent, scale)

  assertthat::assert_that(is.list(data))
  stap_id_null <- sapply(data, is.null)
  lapply(data[!stap_id_null], \(x) assertthat::assert_that(is.matrix(x)))
  data_dim <- sapply(data[!stap_id_null], \(x) dim(x))
  assertthat::assert_that(length(unique(data_dim[1, ])) == 1 & length(unique(data_dim[2, ])) == 1,
    msg = "All matrices of data don't have the same size"
  )
  assertthat::assert_that(assertthat::are_equal(length(g$lat), data_dim[1]))
  assertthat::assert_that(assertthat::are_equal(length(g$lon), data_dim[2]))
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, c("stap_id", "start", "end")))
  assertthat::assert_that(assertthat::are_equal(nrow(stap), length(data)))

  assertthat::assert_that(is.character(type))
  assertthat::assert_that(type %in% c(
    "unknown", "pressure", "light", "magnetic", "pressure_mse", "water_mask",
    "pressure_mask", "magnetic_intensity", "magnetic_inclination", "marginal"
  ))

  # Define the mask of water
  tmp <- data[[which(!sapply(data, is.null))[1]]]
  mask_water <- tmp < -1.5 | is.na(tmp)

  # Replace negative value (-1|not computed or -2|water) by NA
  for (istap in which(!sapply(data, is.null))) {
    data[[istap]][data[[istap]] < 0] <- NA
  }

  map <- structure(list(
    id = id,
    data = data,
    mask_water = mask_water,
    extent = extent,
    scale = scale,
    lat = g$lat,
    lon = g$lon,
    stap = stap,
    type = type
  ), class = "map")

  return(map)
}

#' @noRd
#' @export
"[.map" <- function(x, i, ...) {
  x$data[i]
}

#' @noRd
#' @export
"[[.map" <- function(x, i, ...) {
  x$data[[i]]
}

#' @noRd
#' @export
length.map <- function(x) {
  length(x$data)
}

#' @noRd
#' @export
dim.map <- function(x) {
  c(length(x$lat), length(x$lon), length(x$data))
}


#' @noRd
#' @export
`*.map` <- function(x, y) {
  assertthat::assert_that(inherits(x, "map"))
  assertthat::assert_that(inherits(y, "map"))
  assertthat::assert_that(assertthat::are_equal(x$scale, y$scale))
  assertthat::assert_that(assertthat::are_equal(x$extent, y$extent))
  assertthat::assert_that(assertthat::are_equal(x$id, y$id))
  assertthat::assert_that(assertthat::are_equal(dim(x), dim(y)))

  # Return a map similar to x
  z <- x

  # Compute value
  z$data <- mapply(\(p, l) {
    if (is.null(p) && is.null(l)) {
      return(NULL)
    } else if (is.null(p)) {
      return(l)
    } else if (is.null(l)) {
      return(p)
    } else {
      return(p * l)
    }
  }, x$data, y$data, SIMPLIFY = FALSE)

  # Merge the two stap, should have the same nrow
  z$stap <- merge(x$stap, y$stap, all = TRUE)

  # Preserve order of stap
  z$stap <- z$stap[order(z$stap$stap_id), ]

  z$type <- glue::glue("{x$type} x {y$type}")

  return(z)
}
