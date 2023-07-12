#' Print `tag`
#'
#' This function display the basic information on a `tag`
#
#' @param x A `tag` object
#' @param ... arguments passed from other methods
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @method print tag
#' @export
print.tag <- function(x,...) {
  tag <- x
  cli::cli_text("`tag` of {.field {tag$id}}")

  cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")
  cli::cli_text("Sensors data.frame:")
  cli::cli_ul()
  cli::cli_li("{.field pressure}: {nrow(tag$pressure)} datapoints")
  if ("acceleration" %in% names(tag)) {
    cli::cli_li("{.field acceleration}: {nrow(tag$acceleration)} datapoints")
  }
  if ("light" %in% names(tag)) {
    cli::cli_li("{.field light}: {nrow(tag$light)} datapoints")
  }

  # Stationary periods
  cli::cli_h3("Stationary periods")
  if (!("stap" %in% names(tag))) {
    cli::cli_alert_danger("Not yet labeled. Use {.fun tag_label} to define the stationary periods")
    return(invisible(tag))
  } else {
    cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
    print(head(tag$stap))
  }

  # Geographical
  cli::cli_h3("Geographical parameters")
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

  # Likelihood
  cli::cli_h3("Likelihood")
  if ("map_pressure" %in% names(tag)) {
    cli::cli_alert_success("Pressure likelihood computed!")
  } else {
    if ("mse" %in% names(tag)) {
      cli::cli_alert_warning("Pressure mismatched computed, but not likelihood. Use {.fun geopressure_map_likelihood}.")
    } else {
      cli::cli_alert_danger("No pressure likelihood computed. Use {.fun geopressure_map}.")
    }
  }
  if ("map_light" %in% names(tag)) {
    cli::cli_alert_success("Light likelihood computed!")
  }

  return(invisible(tag))
}
