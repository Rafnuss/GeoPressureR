#' Print a `param` list
#'
#' This function displays the information of a `param` list.
#
#' @param x a GeoPressureR `param` list.
#' @param ... arguments passed to other methods
#'
#' @return `param` is returned invisibly and unchanged
#'
#' @examples
#' # Display the default parameters used in the package.
#' param <- param_create("18LX", default = T)
#' print(param)
#'
#' @family param
#' @method print param
#' @export
print.param <- function(x, ...) {
  param <- x

  cli::cli_h1("GeoPressureR `param` object for {.field id}={.val {param$id}}")
  cli::cli_text("GeoPressureR version: {param$GeoPressureR_version}")

  cli::cli_h3("Sensors data {.fun tag_create}")
  cli::cli_bullets(c(
    "*" = "{.field directory}: {.val {call2deparse(param$sensor_file_directory)}}",
    "*" = "{.field pressure_file}: {.val {param$pressure_file}}",
    "*" = "{.field light_file}: {.val {param$light_file}}",
    "*" = "{.field acceleration_file}: {.val {param$acceleration_file}}",
    "*" = "{.field crop_start}: {.val {param$crop_start}}",
    "*" = "{.field crop_end}: {.val {param$crop_end}}"
  ))

  cli::cli_h3("tag label {.fun tag_label}")
  cli::cli_bullets(c("*" = "{.field label_file}: {.val {call2deparse(param$label_file)}}"))

  cli::cli_h3("Stationary period definition {.fun tag2stap}")
  cli::cli_bullets(c(
    "*" = "{.field known}:",
    print(param$known),
    "*" = "{.field include_stap_id}: {.val {call2deparse(param$include_stap_id)}}",
    "*" = "{.field include_min_duration}: {.val {param$include_min_duration}}"
  ))

  cli::cli_h3("Geographical parameters {.fun geo_expend}")
  cli::cli_bullets(c(
    "*" = "{.field extent}: {.val {param$extent}}",
    "*" = "{.field scale}: {.val {param$scale}}"
  ))

  cli::cli_h3("Geopressure {.fun geopressure_map}")
  cli::cli_bullets(c(
    "*" = "{.field max_sample}: {.val {param$max_sample}}",
    "*" = "{.field margin}: {.val {param$margin}}",
    "*" = "{.field sd}: {.val {param$sd}}",
    "*" = "{.field thr_mask}: {.val {param$thr_mask}}",
    "*" = "{.field log_linear_pooling_weight}: {.val {call2deparse(param$log_linear_pooling_weight)}}"
  ))

  cli::cli_h3("Twilight & Geolight{.fun twilight_create}")
  cli::cli_bullets(c(
    "*" = "{.field twl_thr}: {.val {param$twl_thr}}",
    "*" = "{.field twl_offset}: {.val {param$twl_offset}}",
    "*" = "{.field twilight_file}: {.val {param$twilight_file}}",
    "*" = "{.field twl_calib_adjust}: {.val {param$twl_calib_adjust}}",
    "*" = "{.field twl_llp}: {.val {call2deparse(param$twl_llp)}}"
  ))

  cli::cli_h3("Graph {.fun graph_create}")
  cli::cli_bullets(c(
    "*" = "{.field thr_likelihood}: {.val {param$thr_likelihood}}",
    "*" = "{.field thr_gs}: {.val {param$thr_gs}}"
  ))

  cli::cli_h3("Movement model & wind {.fun graph_add_wind} {.fun graph_movement}")
  cli::cli_bullets(c(
    "*" = "{.field thr_as}: {.val {param$thr_as}}",
    "*" = "{.field wind_file}: {.val {call2deparse(param$wind_file)}}",
    "*" = "{.field type}: {.val {param$movement$type}}",
    "*" = "{.field method}: {.val {param$movement$method}}",
    "*" = "{.field shape}: {.val {param$movement$shape}}",
    "*" = "{.field scale}: {.val {param$movement$scale}}",
    "*" = "{.field location}: {.val {param$movement$location}}",
    "*" = "{.field bird}: {.val {param$movement$bird}}",
    "*" = "{.field power2prob}: {.val {call2deparse(param$movement$power2prob)}}",
    "*" = "{.field low_speed_fix}: {.val {param$movement$low_speed_fix}}"
  ))

  return(invisible(param))
}

call2deparse <- function(x) {
  if (is.call(x) | is.function(x)) {
    deparse(x)
  } else {
    x
  }
}
