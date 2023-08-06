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
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX")
#' print(tag)
#'
#' @family tag
#' @method print tag
#' @export
print.tag <- function(x, ...) {
  tag <- x
  cli::cli_h1("GeoPressureR `tag` object for {.field id}={.val {tag$param$id}}")

  status <- GeoPressureR:::tag_status(tag)

  if (! ("read" %in% status)){
    cli::cli_alert_danger("Sensors data not yet read. Use {.fun tag_create}")
    return(invisible(tag))
  }


  cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")

  cli::cli_h3("Sensors data")
  cli::cli_bullets("{.field pressure}: {nrow(tag$pressure)} datapoints")
  if ("acceleration" %in% status) {
    cli::cli_bullets("{.field acceleration}: {nrow(tag$acceleration)} datapoints")
  }
  if ("light" %in% status) {
    cli::cli_bullets("{.field light}: {nrow(tag$light)} datapoints")
  }

  # Stationary periods
  cli::cli_h3("Stationary periods {.field stap}")
  if ("stap" %in% status) {
    cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
    print(utils::head(tag$stap))
    if (nrow(tag$stap) > 6) {
      cli::cli_text("Run {.code )} to display full table")
    }
  } else {
    cli::cli_alert_danger("Not stationary periods defined yet labeled. Use {.fun tag_label}")
    return(invisible(tag))
  }

  # Geographical
  cli::cli_h3("Geographical parameters ({.field scale} and {.field extent})")
  if ("setmap" %in% status) {
    geo <- map_expand(tag$param$extent, tag$param$scale)
    cli::cli_text("Extent W-E: {.val {tag$param$extent[1]}}\u00b0 to {.val {tag$param$extent[2]}}\u00b0")
    cli::cli_text("Extent S-N: {.val {tag$param$extent[3]}}\u00b0 to {.val {tag$param$extent[4]}}\u00b0")
    cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}\u00b0")
    cli::cli_text("Resolution lat-lon: {.val {1/tag$param$scale}}\u00b0")
  } else {
    cli::cli_alert_danger("No geographical parameters defined yet. Use {.fun tag_setmap}")
    return(invisible(tag))
  }

  # Map
  cli::cli_h3("Map")
  if ("map_pressure" %in% status) {
    cli::cli_alert_success("Pressure likelihood map {.field map_pressure} computed!")
    if ("map_pressure_mse" %in% status) {
      cli::cli_alert_info("Pressure mismatched maps {.field map_pressure_mse} and {.field map_pressure_mask} are also available.")
    }
  } else {
    if ("map_pressure_mse" %in% status) {
      cli::cli_alert_warning("Pressure mismatched maps {.field map_pressure_mse} computed and {.field map_pressure_mask}, but not the likelihood map. Use {.fun geopressure_map_likelihood}.")
    } else {
      cli::cli_alert_danger("No pressure likelihood computed. Use {.fun geopressure_map}.")
    }
  }
  if ("map_light" %in% status) {
    cli::cli_alert_success("Light likelihood {.field map_light} computed!")
  }

  # Param
  cli::cli_h3("Parameter {.field param}")
  cli::cli_text("Run {.code tag$param} to display full table")

  return(invisible(tag))
}
