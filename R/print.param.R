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
  bullets(param$tag_create, "manufacturer")
  bullets(param$tag_create, "crop_start")
  bullets(param$tag_create, "crop_end")
  bullets(param$tag_create, "directory")
  bullets(param$tag_create, "pressure_file")
  bullets(param$tag_create, "light_file")
  bullets(param$tag_create, "acceleration_file")
  bullets(param$tag_create, "temperature_external_file")
  bullets(param$tag_create, "temperature_internal_file")
  bullets(param$tag_create, "magnetic_file")

  cli::cli_h3("Tag label {.fun tag_label}")
  bullets(param$tag_label, "file")

  cli::cli_h3("Stationary period definition {.fun tag_set_map}")
  bullets(param$tag_set_map, "extent")
  bullets(param$tag_set_map, "scale")
  bullets(param$tag_set_map, "known")

  bullets(param$tag_set_map, "include_stap_id")
  bullets(param$tag_set_map, "include_min_duration")

  cli::cli_h3("Geopressure {.fun geopressure_map}")

  bullets(param$geopressure_map, "max_sample")
  bullets(param$geopressure_map, "margin")
  bullets(param$geopressure_map, "sd")
  bullets(param$geopressure_map, "thr_mask")
  bullets(param$geopressure_map, "log_linear_pooling_weight")

  cli::cli_h3("Twilight & Geolight {.fun twilight_create} {.fun geolight_map}")

  bullets(param$twilight_create, "twl_thr")
  bullets(param$twilight_create, "twl_offset")
  bullets(param$twilight_create, "twilight_file")
  bullets(param$geolight_map, "twl_calib_adjust")
  bullets(param$geolight_map, "twl_llp")

  cli::cli_h3("Graph {.fun graph_create}")

  bullets(param$graph_create, "thr_likelihood")
  bullets(param$graph_create, "thr_gs")

  cli::cli_h3(
    "Movement model & wind {.fun graph_add_wind} {.fun graph_movement}"
  )
  bullets(param$graph_add_wind, "thr_as")
  bullets(param$graph_add_wind, "file")
  bullets(param$graph_set_movement, "type")
  bullets(param$graph_set_movement, "method")
  bullets(param$graph_set_movement, "shape")
  bullets(param$graph_set_movement, "scale")
  bullets(param$graph_set_movement, "location")
  bullets(param, "bird_create")
  bullets(param$graph_set_movement, "power2prob")
  bullets(param$graph_set_movement, "low_speed_fix")
  bullets(param$graph_set_movement, "zero_speed_ratio")

  cli::cli_h3("Outputs {.fun graph_simulation} {.fun pressurepath_create}")
  bullets(param$graph_simulation, "nj")
  bullets(param$pressurepath_create, "variable")
  bullets(param$pressurepath_create, "solar_dep")
  bullets(param$pressurepath_create, "era5_dataset")

  if ("geopressuretemplate" %in% names(param)) {
    cli::cli_h3("GeoPressureTemplate {.fun geopressuretemplate}")
    bullets(param$geopressuretemplate, "likelihood")
    bullets(param$geopressuretemplate, "outputs")
    bullets(param$geopressuretemplate, "pressurepath")
  }

  invisible(param)
}

bullets <- function(param, x) {
  val <- param[[x]]
  if (x == "extent") {
    cli::cli_bullets(c(
      "*" = "{.field {x}}: [W:{.val {val[1]}}, E:{.val {val[2]}}, \\
                       S:{.val {val[3]}}, N:{.val {val[4]}}]"
    ))
  } else if (is.call(val) || is.function(val)) {
    cli::cli_bullets(c(
      "*" = "{.field {x}}: {.code {glue::glue_collapse(deparse(val))}}"
    ))
  } else if (is.data.frame(val)) {
    cli::cli_bullets(c("*" = "{.field {x}}:"))
    cli::cat_print(val)
  } else if (is.list(val) && length(val) > 1) {
    cli::cli_bullets(c("*" = "{.field {x}}:"))
    for (n in names(val)) {
      cli::cli_bullets(c(" " = "{.field {n}}: {.val {as.character(val[[n]])}}"))
    }
  } else {
    cli::cli_bullets(c("*" = "{.field {x}}: {.val {val}}"))
  }
}
