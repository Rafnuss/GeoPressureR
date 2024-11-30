#' Print a `bird` object
#'
#' This function displays the information of a `bird` object.
#
#' @param x a GeoPressureR `bird` object.
#' @param ... arguments passed to other methods
#'
#' @return `bird` is returned invisibly and unchanged
#'
#' @examples
#' # Using AVONET dataset
#' bird_create("Acrocephalus arundinaceus")
#'
#' @family bird
#' @method print bird
#' @export
print.bird <- function(x, ...) {
  bird <- x
  cli::cli_h1("GeoPressureR `bird` object")
  cli::cli_bullets(c(
    "*" = "Scientific name: {bird_create$scientific_name}",
    "*" = "Mass: {round(bird_create$mass,2)} (kg).",
    "*" = "Body frontal area: {round(bird_create$body_frontal_area,1)} (m^2).",
    "*" = "Wing span: {round(bird_create$wing_span,1)} (m).",
    "*" = "Wing aspect: {round(bird_create$wing_aspect,1)} (-)."
  ))
  return(invisible(bird))
}
