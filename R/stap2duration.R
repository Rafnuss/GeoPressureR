#' Compute duration
#'
#' This function compute and add the duration column to a `stap` or `path` data.frame based on the
#' start and end time.
#'
#' @param stap_path A `stap` or `path` data.frame
#' @param units character string. Units in which the results are desired. Can be abbreviated.
#' See [`difftime`]
#' @param return_numeric Logical to return the duration as a numeric rather than with a format
#'
#' @examples
#' # Create fake stap
#' stap <- data.frame(
#' start = seq(as.POSIXct("1990-01-01"), as.POSIXct("1991-01-01"), length.out = 13)
#' )
#' stap$end <- stap$start + 60 * 60 * 24 * 30
#' stap$stap_id <- seq_len(nrow(stap))
#'
#' stap2duration(stap)
#'
#' stap2duration(stap, units = "mins", return_numeric = FALSE)
#'
#' @export
stap2duration <- function(stap_path,
                          units = "days",
                          return_numeric = TRUE) {
  assertthat::assert_that(is.data.frame(stap_path))
  assertthat::assert_that(assertthat::has_name(stap_path, c("start", "end")))
  dur <- difftime(stap_path$end, stap_path$start, units = units)
  if (return_numeric) {
    dur <- as.numeric(dur)
  }
  return(dur)
}
