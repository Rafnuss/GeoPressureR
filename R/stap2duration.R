#' Compute duration of stationary periods
#'
#' @description
#' This function returns the duration between `stap$start` and `stap$end`.
#'
#' The function can be used with any data.frame containing `start` and `end` as POSIXct (e.g.,
#' `flight` or `path`).
#'
#' @param stap_path A `stap` data.frame.
#' @param units character string. Units in which the results are desired. Can be abbreviated.
#' See `difftime()`
#' @param return_numeric logical to return the duration as a numeric rather than with a duration
#' format.
#'
#' @return Vector of duration
#'
#' @examples
#' # Create fake stap
#' stap <- data.frame(
#'   start = seq(as.POSIXct("1990-01-01"), as.POSIXct("1991-01-01"), length.out = 13)
#' )
#' stap$end <- stap$start + 60 * 60 * 24 * 30
#' stap$stap_id <- seq_len(nrow(stap))
#'
#' stap2duration(stap)
#'
#' stap2duration(stap, units = "mins", return_numeric = FALSE)
#'
#' @export
stap2duration <- function(stap_path, units = "days", return_numeric = TRUE) {
  assertthat::assert_that(is.data.frame(stap_path))
  assertthat::assert_that(assertthat::has_name(stap_path, c("start", "end")))
  dur <- difftime(stap_path$end, stap_path$start, units = units)
  if (return_numeric) {
    dur <- as.numeric(dur)
  }
  return(dur)
}
