#' Create a `param` list
#'
#' Configure/add paramger a GeoPressureR `tag` object
#'
#' @param id Unique identifier of a tag.
#' @param default Logical to initiate param with default value of the package.
#' @return A GeoPressureR `param` list
#'
#' @family param
#' @export
param_create <- function(id , default = FALSE, ...){
  assertthat::assert_that(is.character(id))

  if (default){
    param0 <- list(
      id = id,
      pressure_file = formals(tag_read)$pressure_file,
      light_file = formals(tag_read)$light_file,
      acceleration_file = formals(tag_read)$acceleration_file,
      crop_start = formals(tag_read)$crop_start,
      crop_end = formals(tag_read)$crop_end,
      label_file = formals(tag_label)$file,
      extent = NULL,
      scale = 5,
      known = data.frame(
        stap_id = integer(),
        known_lat = double(),
        known_lon = double()
      ),
      exclude_stap_id = NA,
      include_min_duration = 0,
      max_sample = formals(geopressure_map)$max_sample,
      margin = formals(geopressure_map)$margin,
      sd = formals(geopressure_map)$sd,
      thr_mask = formals(geopressure_map)$thr_mask,
      log_linear_pooling_weight = formals(geopressure_map)$thr_mask,
      compute_known = formals(geopressure_map)$compute_known,
      twl_thr = formals(twilight_create)$twl_thr,
      twl_offset = formals(twilight_create)$twl_offset,
      twilight_file = formals(twilight_label_read)$twilight_file,
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
    param0 <-list(
      id = id
    )
  }

  # Overwrite default value with input value
  param <- structure(utils::modifyList(param0, list(...), keep.null = TRUE), class="param")

  return(param)
}


param_write <- function(param, file, ...){
  write(jsonlite::toJSON(param, ...), file = file)
}

param_read <- function(file,...){
  return(structure(jsonlite::fromJSON(file),...), class="param")
}
