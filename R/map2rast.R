#' Construct a SpatRaster from a likelihood map of `tag`
#'
#' This function return a [`terra::SpatRaster`] with each stationary periods as a different layer.
#'
#' @inheritParams tag_label
#' @inheritParams tag2map
#' @param time time of the SpatRaster layers. Default is to take the starting date of the stationary
#' period. See [`terra::time`].
#' @param names names of the SpatRaster layers. See [`terra::names`].
#' @inheritDotParams terra::rast
#' @export
map2rast <- function(map, names = glue::glue("#{seq_len(length(map))}"), ...) {
  assertthat::assert_that(assertthat::has_attr(map, "extent"))
  assertthat::assert_that(assertthat::has_attr(map, "scale"))

  # Replace stap with NULL value in `map` with a matrix of NA (this should only happen with
  # map_pressure_mse or map_pressure_mask)
  stap_id_null <- which(sapply(map, is.null))
  if (length(stap_id_null) > 0) {
    g <- geo_expand(attr(map, "extent"), attr(map, "scale"))
    map[[stap_id_null]] <- matrix(NA, nrow = g$dim[1], ncol = g$dim[2])
  }

  r <- terra::rast(simplify2array(map), extent = attr(map, "extent"), crs = "epsg:4326", ...)

  # add starting date as time
  # terra::time(r) <- stap$time

  # Add stap_id as name
  names(r) <- names

  return(r)
}
