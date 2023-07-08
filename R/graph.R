#' Print `graph`
#'
#' This function display the basic information on a `graph` list.
#
#' @param x A `graph` list
#'
#' @return `graph` is returned invisibly and unchanged
#' @seealso graph_create
#' @export
print.graph <- function(x,...) {
  graph <- x
  cli::cli_text("Graph of {.field {graph$id}}")
  cli::cli_h3("Stationary periods {.field stap}")
  cli::cli_text("{.val {nrow(tag$stap)}} stationary periods")
  print(head(graph$stap))

  cli::cli_h3("Geographical {.field geo}")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_text("Extent W-E: {.val {graph$extent[1]}}\u00b0 to {.val {graph$extent[2]}}\u00b0")
  cli::cli_text("Extent S-N: {.val {graph$extent[3]}}\u00b0 to {.val {graph$extent[4]}}\u00b0")
  cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}\u00b0")
  cli::cli_text("Resolution lat-lon: {.val {1/graph$scale}}\u00b0")

  cli::cli_h3("Graph built")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_li("{.val {length(graph$s)}} edges")
  cli::cli_li("{.val {length(graph$equipment)}} equipements nodes")
  cli::cli_li("{.val {length(graph$retrieval)}} retrieval nodes")

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

  invisible(graph)
}


#' Check if an object is of class `graph`
#'
#' @param x A `graph` object.
#'
#' @return `TRUE` for an object of class `graph`, otherwise `FALSE`.
#'
#' @export
is.graph <- function(x) {
  inherits(x, "graph")
}
