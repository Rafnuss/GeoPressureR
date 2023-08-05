#' Plot `tag`
#'
#' This function plot a `tag`.
#
#' @param x A GeoPressureR `tag` object
#' @param ... arguments passed from other methods
#'
#' @return `tag` is returned invisibly and unchanged
#'
#' @examples
#' param_create("18LX")
#'
#' @family param
#' @method print param
#' @export
print.param <- function(x, ...) {
  param <- x
  cli::cli_h1("GeoPressureR `param` object for {.field id}={.val {param$id}}")
  cli::cli_text("GeoPressureR version: {param$GeoPressureR_version}")

  cli::cli_h3("Sensors data {.fun tag_read}")
  cli::cli_bullets("{.field pressure_file}: {.val {param$pressure_file}}")
  cli::cli_bullets("{.field acceleration_file}: {.val {param$acceleration_file}}")
  cli::cli_bullets("{.field crop_start}: {.val {param$crop_start}}")
  cli::cli_bullets("{.field crop_end}: {.val {param$crop_end}}")

  cli::cli_h3("tag label {.fun tag_label}")
  cli::cli_bullets("{.field label_file}: {.val {deparse(param$label_file)}}")

  cli::cli_h3("Stationary period definition {.fun tag2stap}")
  cli::cli_bullets("{.field known}:")
  print(param$known)
  cli::cli_bullets("{.field exclude_stap_id}: {.val {param$exclude_stap_id}}")
  cli::cli_bullets("{.field include_min_duration}: {.val {param$include_min_duration}}")

  cli::cli_h3("Geographical parameters {.fun geo_expend}")
  cli::cli_bullets("{.field extent}: {.val {param$extent}}")
  cli::cli_bullets("{.field scale}: {.val {param$scale}}")

  cli::cli_h3("Geopressure {.fun geopressure_map}")
  cli::cli_bullets("{.field max_sample}: {.val {param$max_sample}}")
  cli::cli_bullets("{.field margin}: {.val {param$margin}}")
  cli::cli_bullets("{.field sd}: {.val {param$sd}}")
  cli::cli_bullets("{.field thr_mask}: {.val {param$thr_mask}}")
  cli::cli_bullets("{.field log_linear_pooling_weight}: {.val {deparse(param$log_linear_pooling_weight)}}")

  cli::cli_h3("Twilight & Geolight{.fun twilight_create}")
  cli::cli_bullets("{.field twl_thr}: {.val {param$twl_thr}}")
  cli::cli_bullets("{.field twl_offset}: {.val {param$twl_offset}}")
  cli::cli_bullets("{.field twilight_file}: {.val {param$twilight_file}}")
  cli::cli_bullets("{.field twl_calib_adjust}: {.val {param$twl_calib_adjust}}")
  cli::cli_bullets("{.field twl_llp}: {.val {deparse(param$twl_llp)}}")

  cli::cli_h3("Graph {.fun graph_create}")
  cli::cli_bullets("{.field thr_likelihood}: {.val {param$thr_likelihood}}")
  cli::cli_bullets("{.field thr_gs}: {.val {param$thr_gs}}")

  cli::cli_h3("Movement model & wind {.fun graph_add_wind} {.fun graph_movement}")
  cli::cli_bullets("{.field thr_as}: {.val {param$thr_as}}")
  cli::cli_bullets("{.field wind_file}: {.val {deparse(param$wind_file)}}")
  cli::cli_bullets("{.field type}: {.val {param$type}}")
  cli::cli_bullets("{.field method}: {.val {param$method}}")
  cli::cli_bullets("{.field shape}: {.val {param$shape}}")
  cli::cli_bullets("{.field scale}: {.val {param$scale}}")
  cli::cli_bullets("{.field location}: {.val {param$location}}")
  cli::cli_bullets("{.field bird}: {.val {param$bird}}")
  cli::cli_bullets("{.field power2prob}: {.val {deparse(param$power2prob)}}")
  cli::cli_bullets("{.field low_speed_fix}: {.val {param$low_speed_fix}}")


  return(invisible(tag))
}
