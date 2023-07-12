#' Construct a SpatRaster from a likelihood map of `tag`
#'
#' This function return a [`terra::SpatRater`] with each stationary periods as a different layer.
#'
#' @inheritParams tag_label
#' @inheritParams tag2likelihood
#' @param time time of the SpatRaster layers. Default is to take the starting date of the stationary
#' period. See [`terra::time`].
#' @param names names of the SpatRaster layers. See [`terra::names`].
#' @inheritDotParams terra::rast
#' @export
tag2rast <- function(tag, likelihood = NA, time = tag$stap$start, names = tag$stap$stap_id, ...){

  # Construct the likelihood map
  lk <- tag2likelihood(tag, likelihood = likelihood)

  r = terra::rast(simplify2array(lk), extent = tag$extent, crs = "epsg:4326", ...)

  # add starting date as time
  terra::time(r) <- time

  # Add stap_id as name
  names(r) <- names

  return(r)
}
