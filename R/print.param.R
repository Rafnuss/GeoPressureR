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
#' param <- param_create("18LX", default = TRUE)
#'
#' print(param)
#'
#' @family param
#' @method print param
#' @export
print.param <- function(x, ...) {
  param <- x

  cli::cli_h1("GeoPressureR `param` object for {.field id}: {param$id}")
  cli::cli_text(cli::col_grey(
    "{.strong Note}: All {.field green} texts are fields of `param` (i.e., `param${.field field}`)."
  ))

  bullets(param, "GeoPressureR_version")

  cli::cli_h3("Sensors data {.fun tag_create}")
  bullets(param, "crop_start")
  bullets(param, "crop_end")
  bullets(param, "directory")
  bullets(param, "pressure_file")
  bullets(param, "light_file")
  bullets(param, "acceleration_file")
  bullets(param, "temperature_file")
  bullets(param, "airtemperature_file")
  bullets(param, "magnetic_file")


  cli::cli_h3("Tag label {.fun tag_label}")
  bullets(param, "label_file")

  cli::cli_h3("Stationary period definition {.fun tag_set_map}")
  bullets(param, "extent")
  bullets(param, "scale")
  bullets(param, "known")

  bullets(param, "include_stap_id")
  bullets(param, "include_min_duration")


  cli::cli_h3("Geopressure {.fun geopressure_map}")

  bullets(param, "max_sample")
  bullets(param, "margin")
  bullets(param, "sd")
  bullets(param, "thr_mask")
  bullets(param, "log_linear_pooling_weight")

  cli::cli_h3("Twilight & Geolight {.fun twilight_create} {.fun geolight_map}")

  bullets(param, "twl_thr")
  bullets(param, "twl_offset")
  bullets(param, "twilight_file")
  bullets(param, "twl_calib_adjust")
  bullets(param, "twl_llp")

  cli::cli_h3("Graph {.fun graph_create}")

  bullets(param, "thr_likelihood")
  bullets(param, "thr_gs")

  cli::cli_h3("Movement model & wind {.fun graph_add_wind} {.fun graph_movement}")
  bullets(param, "thr_as")
  bullets(param, "wind_file")
  bullets(param$movement, "type")
  bullets(param$movement, "method")
  bullets(param$movement, "shape")
  bullets(param$movement, "scale")
  bullets(param$movement, "location")
  bullets(param$movement, "bird")
  bullets(param$movement, "power2prob")
  bullets(param$movement, "low_speed_fix")

  return(invisible(param))
}

bullets <- function(param, x) {
  val <- param[[x]]
  if (x == "extent") {
    cli::cli_bullets(c("*" = "{.field {x}}: [W:{.val {val[1]}}, E:{.val {val[2]}}, \\
                       S:{.val {val[3]}}, N:{.val {val[4]}}]"))
  } else if (is.call(val) || is.function(val)) {
    cli::cli_bullets(c("*" = "{.field {x}}: {.code {glue::glue_collapse(deparse(val))}}"))
  } else if (is.data.frame(val)) {
    cli::cli_bullets(c("*" = "{.field {x}}:"))
    cli::cat_print(val)
  } else if (is.list(val) & length(val) > 1) {
    cli::cli_bullets(c("*" = "{.field {x}}:"))
    for (n in names(val)) {
      cli::cli_bullets(c(" " = "{.field {n}}: {.val {val[[n]]}}"))
    }
  } else {
    cli::cli_bullets(c("*" = "{.field {x}}: {.val {val}}"))
  }
}
