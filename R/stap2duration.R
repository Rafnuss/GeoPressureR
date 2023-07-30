#' Compute duration
#'
#' This function compute and add the duration column to a `stap` or `path` data.frame based on the
#' start and end time.
#'
#' @param x A `stap` or `path` data.frame
#' @param units character string. Units in which the results are desired. Can be abbreviated.
#' See [`difftime`]
#'
#' @export
stap2duration <- function(x,
                          units = "days",
                          numeric = TRUE) {
  dur <- difftime(x$end, x$start, units = units)
  if (numeric) {
    dur <- as.numeric(dur)
  }
  return(dur)
}
