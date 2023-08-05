#' Create `tag` object
#'
#' Create a GeoPressureR `tag` object
#'
#' @param id Unique identifier of a tag.
#' @return A GeoPressureR `tag` object
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#'
#' tag_read("18LX")
#'
#' @family tag
#' @export
tag_create <- function(id, ...){
  param <- param_create(id = id, ...)

  tag = structure(list(
    param = param
  ), class="tag")

  return(tag)
}
