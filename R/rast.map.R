#' Construct a SpatRaster from a `map`
#'
#' This function convert a GeoPressureR `map` object into a [`terra::SpatRaster`] with the data of
#' each stationary periods stored in a different layer.
#'
#' @param x a GeoPressureR `map` object
#' @param names names of the SpatRaster layers created. See [`terra::names`].
#' @inheritParams terra::rast
#' @param ... additional parameters for `terra::rast`
#'
#' @return A [terra::SpatRaster] object.
#'
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_set_map(
#'     extent = c(-16, 23, 0, 50),
#'     scale = 4
#'   ) |>
#'   geopressure_map(quiet = TRUE)
#'
#' rast.map(tag$map_pressure)
#'
#'
#' @family map
#' @export
rast.map <- function(x,
                     names = glue::glue("#{map$stap$stap_id}"),
                     crs = "epsg:4326",
                     ...) {
  map <- x

  assertthat::assert_that(inherits(map, "map"))

  # Replace stap with NULL value in `map` with a matrix of NA (this should only happen with
  # map_pressure_mse or map_pressure_mask)
  stap_id_null <- which(sapply(map$data, is.null))
  for (i in stap_id_null) {
    map$data[[i]] <- matrix(NA, nrow = dim(map)[1], ncol = dim(map)[2])
  }

  r <- terra::rast(simplify2array(map$data), extent = map$extent, crs = crs, ...)

  # add starting date as time
  terra::time(r) <- map$stap$start

  # Add stap_id as name
  names(r) <- names

  return(r)
}
