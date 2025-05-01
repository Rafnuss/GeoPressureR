#' @importFrom ggplot2 .data
NULL

#' @importMethodsFrom terra rast
NULL

#  setOldClass("map") allows S4 dispatch on S3 map objects.
#' @importFrom methods setOldClass
methods::setOldClass("map")

#' @importFrom methods setMethod
methods::setMethod(rast, "map", rast.map)

#' @noRd
format_minutes <- function(mins) {
  mins <- round(mins)
  years <- mins %/% (365 * 24 * 60)
  days <- (mins %% (365 * 24 * 60)) %/% (24 * 60)
  hours <- (mins %% (24 * 60)) %/% 60
  minutes <- mins %% 60

  glue::glue(
    "{if (years > 0) paste0(years, 'y ') else ''}",
    "{if (days > 0) paste0(days, 'd ') else ''}",
    "{if (hours > 0 || days > 0 || years > 0) paste0(hours, 'h ') else ''}",
    "{minutes}min"
  )
}
