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
    "*" = "Species: {bird$species_name}",
    "*" = "Mass: {round(bird$mass,2)} (kg).",
    "*" = "Body frontal area: {round(bird$body_frontal_area,1)} (m^2).",
    "*" = "Wing span: {round(bird$wing_span,1)} (m).",
    "*" = "Wing aspect: {round(bird$wing_aspect,1)} (-)."
  ))

  return(invisible(bird))
}
