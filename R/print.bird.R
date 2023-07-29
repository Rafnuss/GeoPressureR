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
  cli::cli_h1("GeoPressureR `bird` object")
  cli::cli_bullets(c("*" = "Species: {bird$species_name}"))
  cli::cli_bullets(c("*" = "Mass: {round(bird$mass,2)} kg."))
  cli::cli_bullets(c("*" = "Body frontal area: {round(bird$body_frontal_area,1)} m^2."))
  cli::cli_bullets(c("*" = "Wing span: {round(bird$wing_span,1)} m."))
  cli::cli_bullets(c("*" = "Wing aspect: {round(bird$wing_aspect,1)} "))

  return(invisible(bird))
}
