#' Check if an object is of class `tag`
#'
#' @param x A GeoPressureR `tag` object
#'
#' @return `TRUE` for an object of class `tag`, otherwise `FALSE`.
#' @family tag
#' @method is tag
#' @export
is.tag <- function(x) {
  inherits(x, "tag")
}

assertthat::on_failure(is.tag) <- function(call, env) {
  paste0(deparse(call$x), " is not a tag object.")
}
