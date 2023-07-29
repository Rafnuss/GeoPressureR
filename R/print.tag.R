#' Print `tag`
#'
#' This function display the basic information on a `tag`
#
#' @param x A GeoPressureR `tag` object
#' @param ... arguments passed from other methods
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @method print tag
#' @export
print.tag <- function(x,...) {
  tag <- x
  cli::cli_h1("GeoPressureR `tag` object for {.field id}={.val {tag$id}}")

  cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")

  cli::cli_h3("Sensors data")
  cli::cli_bullets("{.field pressure}: {nrow(tag$pressure)} datapoints")
  if ("acceleration" %in% names(tag)) {
    cli::cli_bullets("{.field acceleration}: {nrow(tag$acceleration)} datapoints")
  }
  if ("light" %in% names(tag)) {
    cli::cli_bullets("{.field light}: {nrow(tag$light)} datapoints")
  }

  # Stationary periods
  cli::cli_h3("Stationary periods {.field stap}")
  if (!("stap" %in% names(tag))) {
    cli::cli_alert_danger("Not stationary periods defined yet labeled. Use {.fun tag_label}")
    return(invisible(tag))
  } else {
    cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
    print(head(tag$stap))
    if (nrow(tag$stap)>6){
      cli::cli_text("Run {.code tag$stap} to display full table")
    }
  }

  # Geographical
  cli::cli_h3("Geographical parameters ({.field scale} and {.field extent})")
  if (!("extent" %in% names(tag) & "scale" %in% names(tag))) {
    cli::cli_alert_danger("No geographical parameters defined yet. Use {.fun tag_geostap}")
    return(invisible(tag))
  } else {
    geo <- geo_expand(tag$extent, tag$scale)
    cli::cli_text("Extent W-E: {.val {tag$extent[1]}}\u00b0 to {.val {tag$extent[2]}}\u00b0")
    cli::cli_text("Extent S-N: {.val {tag$extent[3]}}\u00b0 to {.val {tag$extent[4]}}\u00b0")
    cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}\u00b0")
    cli::cli_text("Resolution lat-lon: {.val {1/tag$scale}}\u00b0")
  }

  # Map
  cli::cli_h3("Map")
  if ("map_pressure" %in% names(tag)) {
    cli::cli_alert_success("Pressure likelihood map {.field map_pressure} computed!")
  } else {
    if ("map_pressure_mse" %in% names(tag)) {
      cli::cli_alert_warning("Pressure mismatched map {.field map_pressure_mse} computed, but not the likelihood map. Use {.fun geopressure_map_likelihood}.")
    } else {
      cli::cli_alert_danger("No pressure likelihood computed. Use {.fun geopressure_map}.")
    }
  }
  if ("map_light" %in% names(tag)) {
    cli::cli_alert_success("Light likelihood {.field map_light} computed!")
  }

  # Param
  # cli::cli_h3("Parameter {.field param}")
  # str(tag$param)

  return(invisible(tag))
}
