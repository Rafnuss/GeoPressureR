#' Check if an object is of class `graph`
#'
#' @param x A `graph` object.
#'
#' @return `TRUE` for an object of class `graph`, otherwise `FALSE`.
#' @family graph
#' @method is graph
#' @export
is.graph <- function(x) {
  inherits(x, "graph")
}

assertthat::on_failure(is.graph) <- function(call, env) {
  paste0(deparse(call$x), " is not a graph object.")
}
