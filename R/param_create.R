#' Create a `param` list
#'
#' Configure/add paramger a GeoPressureR `tag` object
#'
#' @param id Unique identifier of a tag.
#' @param default Logical to initiate param with default value of the package.
#' @param ... arguments passed from other methods
#'
#' @return A GeoPressureR `param` list
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
      scale = formals(tag_setmap)$scale,
      known = formals(tag_setmap)$known,
      include_stap_id = formals(tag_setmap)$include_stap_id,
      include_min_duration = formals(tag_setmap)$include_min_duration,
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
      type = formals(graph_add_movement)$type,
      method = formals(graph_add_movement)$method,
      shape = formals(graph_add_movement)$shape,
      scale = formals(graph_add_movement)$scale,
      location = formals(graph_add_movement)$location,
      bird = formals(graph_add_movement)$bird,
      power2prob = formals(graph_add_movement)$power2prob,
      low_speed_fix = formals(graph_add_movement)$low_speed_fix,
      GeoPressureR_version = utils::packageVersion("GeoPressureR")
    )
  } else {
    param <- list(
      id = id
    )
  }

  # Overwrite default value with input value
  param_overwrite <- list(...)
  common_names <- intersect(names(param), names(param_overwrite))
  for (name in common_names) {
    param[[name]] <- param_overwrite[[name]]
  }

  return(param)
}
