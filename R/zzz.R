#' @importFrom ggplot2 .data
NULL

#' @importMethodsFrom terra rast
NULL

#  setOldClass("map") allows S4 dispatch on S3 map objects.
#' @importFrom methods setOldClass
methods::setOldClass("map")

#' @importFrom methods setMethod
methods::setMethod(rast, "map", rast.map)
