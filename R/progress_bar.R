#' Progress bar function
#' @param x value of the current status
#' @param max maximum value that x will reach
#' @param text text to display
#' @noRd

progress_bar <- function(x, max = 100, text = "") {
  percent <- x / max * 100
  cat(sprintf(
    "\r[%-50s] %d / %d %s",
    paste(rep("=", percent / 2), collapse = ""),
    x, max, text
  ))
  if (x == max) {
    cat("\n")
  }
}
