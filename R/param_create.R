#' Create a `param` list
#'
#' @description
#' Create the list of parameter for GeoPressureR `tag` and `graph` objects.
#'
#' `param` list are mostly used to archived the actual value of parameters used to create a `tag`
#' and/or a `graph`, thus allowing for examination of parameters post-creation. This function should
#' therefore not be used to set/define parameters ahead of computation. In reality, there are very
#' few external case of use for this function.
#'
#' @param id Unique identifier of a tag.
#' @param default logical to initiate param with default value of the package.
#' @param ... arguments passed to other methods.
#'
#' @return A GeoPressureR `param` list
#'
#' @examples
#' param_create("18LX", extent = c(0, 0, 1, 1))
#'
#' @family param
#' @export
param_create <- function(id, default = FALSE, ...) {
  assertthat::assert_that(is.character(id))

  if (default) {
    param <- list(
      id = id,
      sensor_file_directory = formals(tag_create)$directory,
      pressure_file = formals(tag_create)$pressure_file,
      light_file = formals(tag_create)$light_file,
      acceleration_file = formals(tag_create)$acceleration_file,
      crop_start = formals(tag_create)$crop_start,
      crop_end = formals(tag_create)$crop_end,
      label_file = formals(tag_label)$file,
      extent = NULL,
      scale = formals(tag_set_map)$scale,
      known = formals(tag_set_map)$known,
      include_stap_id = formals(tag_set_map)$include_stap_id,
      include_min_duration = formals(tag_set_map)$include_min_duration,
      max_sample = formals(geopressure_map)$max_sample,
      margin = formals(geopressure_map)$margin,
      sd = formals(geopressure_map)$sd,
      thr_mask = formals(geopressure_map)$thr_mask,
      log_linear_pooling_weight = formals(geopressure_map)$log_linear_pooling_weight,
      compute_known = formals(geopressure_map)$compute_known,
      twl_thr = formals(twilight_create)$twl_thr,
      twl_offset = formals(twilight_create)$twl_offset,
      twilight_file = formals(twilight_label_read)$file,
      twl_calib_adjust = formals(geolight_map)$twl_calib_adjust,
      twl_llp = formals(geolight_map)$twl_llp,
      thr_likelihood = formals(graph_create)$thr_likelihood,
      thr_gs = formals(graph_create)$thr_gs,
      thr_as = formals(graph_add_wind)$thr_as,
      wind_file = formals(graph_add_wind)$file,
      type = formals(graph_set_movement)$type,
      method = formals(graph_set_movement)$method,
      shape = formals(graph_set_movement)$shape,
      scale = formals(graph_set_movement)$scale,
      location = formals(graph_set_movement)$location,
      bird = formals(graph_set_movement)$bird,
      power2prob = formals(graph_set_movement)$power2prob,
      low_speed_fix = formals(graph_set_movement)$low_speed_fix,
      GeoPressureR_version = utils::packageVersion("GeoPressureR")
    )

    # Overwrite default value with input value
    param_overwrite <- list(...)
    print(param_overwrite)
    common_names <- intersect(names(param), names(param_overwrite))
    for (name in common_names) {
      param[[name]] <- param_overwrite[[name]]
    }
  } else {
    param <- list(
      id = id,
      GeoPressureR_version = utils::packageVersion("GeoPressureR"),
      ...
    )
  }

  return(structure(param, class = "param"))
}
