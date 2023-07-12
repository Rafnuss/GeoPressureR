#' Print `bird`
#'
#' This function display the basic information on a `bird`
#
#' @param x A `bird` object
#' @param ... arguments passed from other methods
#'
#' @return `bird` is returned invisibly and unchanged
#' @family bird
#' @method print bird
#' @export
print.bird <- function(x,...) {
  bird <- x
  cli::cli_text("GeoPressure bird")
  print(bird)
  return(invisible(bird))
}


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
