#' @importFrom ggplot2 .data
NULL

#' @importMethodsFrom terra rast
NULL

#  setOldClass("map") allows S4 dispatch on S3 map objects.
methods::setOldClass("map")

methods::setMethod(rast, "map", rast.map)
