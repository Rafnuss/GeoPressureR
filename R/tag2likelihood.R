#' Return a likelihood map from a `tag`
#'
#'
#
#' @inheritParams tag_create
#' @param likelihood Field of the `tag` list containing the likelihood map (character). Default
#' `NA` is to take the product of `map_pressure` and `map_light`, or only `map_pressure` if
#' `map_light` is not available.
#'
#' @return likelihood map
#' @export
tag2likelihood <- function(tag, likelihood = NA){
  tag_assert(tag)

  if (all(is.na(likelihood))) {
    likelihood <- c("map_pressure", "map_light")
    tmp <- likelihood %in% names(tag)
    if (all(tmp)) {
      lk <- mapply(\(p, l) {
        if (is.null(p) | is.null(l)) {
          return(NULL)
        } else {
          return(p * l)
        }
      }, tag$map_pressure, tag$map_light, SIMPLIFY = FALSE)
    } else if (any(tmp)) {
      likelihood <- likelihood[tmp]
      lk <- tag[[likelihood]]
    } else {
      cli::cli_abort(c(
        x = "None of {.field {likelihood}} are present in {.var tag}",
        i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}"
      ))
    }
  } else {
    assertthat::assert_that(assertthat::has_name(tag, likelihood))
    lk <- tag[[likelihood]]
  }
  return(lk)
}
