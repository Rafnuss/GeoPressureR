#' Construct a SpatRaster from a `map`
#'
#' This function return a [`terra::SpatRaster`] with each stationary periods as a different layer.
#'
#' @param x A GeoPressureR `map` object
#' @param names names of the SpatRaster layers created (see [`terra::names`]).
#' @inheritParams terra::rast
#' @param ... Additional parameters for `terra::rast`
#'
#' @importMethodsFrom terra rast
#' @return A [terra::SpatRaster] object.
#' @export
rast.map <- function(x,
                     names = glue::glue("#{map$stap$stap_id}"),
                     crs = "epsg:4326",
                     ...) {

  map <- x

    # Replace stap with NULL value in `map` with a matrix of NA (this should only happen with
  # map_pressure_mse or map_pressure_mask)
  stap_id_null <- which(sapply(map$data, is.null))
  if (length(stap_id_null) > 0) {
    map$data[[stap_id_null]] <- matrix(NA, nrow = dim(map)[1], ncol = dim(map)[2])
  }

  r <- terra::rast(simplify2array(map$data), extent = map$extent, crs = crs, ...)

  # add starting date as time
  terra::time(r) <- map$stap$start

  # Add stap_id as name
  names(r) <- names

  return(r)
}

# create generic function rast
# #' @export
#rast <- function(x, ...) {
#  UseMethod("rast")
#}

methods::setMethod(rast, "map", rast.map)
# #' @importMethodsFrom terra rast
