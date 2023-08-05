#' Compute grid from `extent` and `scale`
#'
#' Take the two GEOgraphic parameters used to define the grid in `tag` and `graph` and return
#' all variable of interest for the spatial grid (e.g., grid dimension, latitude and longitude...)
#'
#' @inheritParams tag_setmap
#' @return A list containing:
#' - `extent` same as input
#' - `scale` same as input
#' - `lat` vector of latitude
#' - `lon` vector of longitude
#' - `dim` vector of length 2 of the dimension of the map (number of pixel in lat and lon)
#' @examples
#' str(map_expand(extent = c(0, 10, 0, 5), scale = 1))
#'
#' str(map_expand(extent = c(-16, 23, 0, 50), scale = 10))
#' @export
map_expand <- function(extent, scale) {
  assertthat::assert_that(is.numeric(extent))
  assertthat::assert_that(length(extent) == 4)
  assertthat::assert_that(
    extent[1] >= -180 & extent[1] <= 180,
    msg = "extent[1] needs to be between -180 and 180. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(
    extent[2] >= -180 & extent[2] <= 180,
    msg = "extent[2] needs to be between -180 and 180. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(
    extent[3] >= -90 & extent[3] <= 90,
    msg = "extent[3] needs to be between -90 and 90. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(
    extent[4] >= -90 & extent[4] <= 90,
    msg = "extent[4] needs to be between -90 and 90. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(
    extent[1] < extent[2],
    msg = "extent[1] needs to be smaller than extent[2]. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(
    extent[3] < extent[4],
    msg = "extent[3] needs to be smaller than extent[4]. Make sure extent follows `c(W, E, S, N)`.")
  assertthat::assert_that(is.numeric(scale))
  assertthat::assert_that(0 < scale)
  assertthat::assert_that(scale <= 10)

  # Compute dimension
  dim <- c(
    (extent[4] - extent[3]) * scale,
    (extent[2] - extent[1]) * scale
  )
  # Check that dimension is correct
  if (!all(round(dim) == dim)) {
    cli::cli_abort(c(
      x = "A valid map configuration requires (E-W)*scale and (N-S)*scale to be an integer.",
      i = "Change {.val E}, {.val W} or {.val scale} to match this requirement"
    ))
  }

  # Compute cell center vector of latitude and longitude
  lat <- seq(extent[4], extent[3], length.out = dim[1] + 1)
  lat <- utils::head(lat, -1) + diff(lat[1:2]) / 2
  lon <- seq(extent[1], extent[2], length.out = dim[2] + 1)
  lon <- utils::head(lon, -1) + diff(lon[1:2]) / 2

  grid <- list(
    extent = extent,
    scale = scale,
    lat = lat,
    lon = lon,
    dim = dim
  )
  return(grid)
}
