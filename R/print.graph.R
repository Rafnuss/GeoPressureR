#' Print `graph`
#'
#' This function display the basic information on a `graph` object.
#
#' @param x A `graph` list
#' @param ... arguments passed from other methods
#'
#' @return `graph` is returned invisibly and unchanged
#' @family graph
#' @method print graph
#' @export
print.graph <- function(x, ...) {
  graph <- x
  cli::cli_h1("GeoPressureR `graph` object for {.field id}={.val {graph$param$id}}")


  cli::cli_h3("Stationary periods {.field stap}")
  cli::cli_text("{.val {nrow(graph$stap)}} stationary period{?s}")
  print(utils::head(graph$stap))
  if (nrow(graph$stap) > 6) {
    cli::cli_text("Run {.code graph$stap} to display full table")
  }

  cli::cli_h3("Geographical parameters ({.field scale} and {.field extent})")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_text("Extent W-E: {.val {graph$extent[1]}}\u00b0 to {.val {graph$extent[2]}}\u00b0")
  cli::cli_text("Extent S-N: {.val {graph$extent[3]}}\u00b0 to {.val {graph$extent[4]}}\u00b0")
  cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}\u00b0")
  cli::cli_text("Resolution lat-lon: {.val {1/graph$scale}}\u00b0")

  cli::cli_h3("Graph size")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_li("{.val {length(graph$s)}} edge{?s}")
  cli::cli_li("{.val {length(graph$equipment)}} equipement node{?s}")
  cli::cli_li("{.val {length(graph$retrieval)}} retrieval node{?s}")

  cli::cli_h3("Movement model")
  if ("ws" %in% names(graph)) {
    cli::cli_alert_success("Windspeed computed!")
  } else {
    cli::cli_alert_warning("Windspeed not computed. Use {.fun graph_add_wind}")
  }

  if ("movement" %in% names(graph)) {
    cli::cli_alert_success("Movement model defined for {.field {graph$movement$type}}")
  } else {
    cli::cli_alert_danger("No movement model defined. Use {.fun graph_add_movement}")
  }

  return(invisible(graph))
}
