#' Export GeoPressure Variable
#'
#' Set of functions to export the core variable of the process:
#'
#' - `tag`: export all sensors data.frame and `stap` data.frame, thus containing the `stap_id` and
#' `label` information.
#' - `map`: export map (e.g., `tag$map_pressure` or `tag$map_marginal`)
#' - `path`: Export a `path` data.frame into a csv.
#' - `edge`:
#'
#' @inheritParams tag_create
#' @inheritParams graph_create
#' @inheritParams map2path
#' @param path Data.frame containing.
#' @param directory Folder in which to export the variable
#' @param file Name of the file for the exported variable
#' @param edge See [`path2edge()`]
#' @export
geopressure_export <- function(tag = NA,
                               path = NA,
                               directory = glue::glue("./data/processed/{tag$id}")) {
  assertthat::assert_that(assertthat::is.dir(directory))

  if (!is.na(tag)) {
    assertthat::assert_that(assertthat::is.dir(directory))
    for (field in c("pressure", "acceleration", "light", "twilight", "stap")) {
      if (field %in% tag) {
        utils::write.csv(tag[[field]], file.path(directory, glue::glue("{field}.csv")))
      }
    }
  }
  if (!is.na(tag)) {
    r <- lapply(tag[[likelihood]], function(l) {
      terra::rast(l, extent = tag$extent)
    })

    terra::writeRaster(terra::rast(r), file.path(directory, glue::glue("{likelihood}.geotiff")))
  }
  if (!is.na(tag)) {
    geopressure_export_tag(tag)
  }
}
