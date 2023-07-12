#' Convert light data in matrix format
#'
#' @inheritParams twilight_create
#' @param twl_offset Shift of the middle of the night compared to 00:00 UTC (in seconds). If not
#' provided, it uses the middle of all nights.
#' @return A dataframe with columns `date` and `value`.
#' @export
light2mat <- function(light, twl_offset = 0) {
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(assertthat::has_name(light, c("date", "value")))
  assertthat::assert_that(assertthat::is.time(light$date))
  assertthat::assert_that(is.numeric(light$value))
  assertthat::assert_that(is.numeric(twl_offset))

  res <- difftime(utils::tail(light$date, -1), utils::head(light$date, -1), units = "secs")
  if (length(unique(res)) != 1) {
    cli::cli_abort(c(
      x = "Temporal resolution of the light data is not constant.",
      i = "Use {.fun TwGeos::FindTwilight} instead."
    ))
  }
  res <- as.numeric(res[1])

  # Pad time to start and finish at 00:00
  date <- seq(
    from = as.POSIXct(format(light$date[1] - twl_offset * 60 * 60, "%Y-%m-%d"), tz = "UTC"),
    to = as.POSIXct(format(light$date[length(light$date)] - twl_offset * 60 * 60, "%Y-%m-%d"),
                    tz = "UTC"
    ) + 60 * 60 * 24 - res,
    by = res
  )
  date <- date + twl_offset * 60 * 60

  # if light$date is not measuring at 00:00 exactly, we need to move date
  closest <- which.min(abs(date - light$date[1]))
  date <- date - (date[closest] - light$date[1])

  # Match the observation on the new grid
  value <- rep(NA, length(date))
  id <- date %in% light$date
  assertthat::assert_that(any(id))
  value[id] <- light$value

  # reshape in matrix format
  mat <- list(
    value = matrix(value, nrow = 24 * 60 * 60 / res),
    date = matrix(date, nrow = 24 * 60 * 60 / res),
    res = res
  )

  # image(mat$value)
  mat$day <- as.Date(as.POSIXct(colMeans(mat$date), origin = "1970-01-01", tz = "UTC"))
  mat$time <- format(as.POSIXct(mat$date[,1], origin = "1970-01-01", tz = "UTC"), "%H:%M")

  return(mat)
}
