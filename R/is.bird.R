#' Check if an object is of class `bird`
#'
#' @param x A `bird` object.
#'
#' @return `TRUE` for an object of class `bird`, otherwise `FALSE`.
#' @family bird
#' @method is bird
#' @export
is.bird <- function(x) {
  inherits(x, "bird")
}
