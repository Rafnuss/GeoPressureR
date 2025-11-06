#' Format timeserie data.frame into a matrix
#'
#' @param ts data.frame of a `tag`, containing at least `date` and `value`.
#' @param value column name to extract
#' @inheritParams twilight_create
#' @return A data.frame with columns `date` and `value`.
#' @export
ts2mat <- function(ts, twl_offset = 0, value = "value") {
  assertthat::assert_that(is.data.frame(ts))
  assertthat::assert_that(assertthat::has_name(ts, "date"))
  assertthat::assert_that(assertthat::is.time(ts$date))
  assertthat::assert_that(assertthat::has_name(ts, value))
  assertthat::assert_that(is.numeric(ts$value))
  assertthat::assert_that(is.numeric(twl_offset))

  res_vec <- as.numeric(diff(ts$date), units = "secs")
  res <- stats::median(res_vec)
  if (length(unique(res_vec)) != 1) {
    cli::cli_warn(c(
      x = "Temporal resolution of the ts data is not constant. We will use a regular \\
      resolution of {.val {res}} seconds."
    ))
  }

  # Pad time to start and finish at 00:00
  date <- seq(
    from = as.POSIXct(
      format(ts$date[1] - twl_offset * 60 * 60, "%Y-%m-%d"),
      tz = "UTC"
    ),
    to = as.POSIXct(
      format(ts$date[length(ts$date)] - twl_offset * 60 * 60, "%Y-%m-%d"),
      tz = "UTC"
    ) +
      60 * 60 * 24 -
      res,
    by = res
  )
  date <- date + twl_offset * 60 * 60

  # if ts$date is not measuring at 00:00 exactly, we need to move date
  closest <- which.min(abs(date - ts$date[1]))
  date <- date - (date[closest] - ts$date[1])

  # Match the observation on the new grid
  # Convert to numeric for faster computation
  date_num <- as.numeric(date)
  ts_date_num <- as.numeric(ts$date)

  # Sort the ts data (if not already sorted)
  ord <- order(ts_date_num)
  ts_date_num <- ts_date_num[ord]
  ts_value <- ts$value[ord]

  # Approximate index positions for all date_num
  idx <- findInterval(date_num, ts_date_num)

  # Ensure idx is within bounds (1 to length-1)
  idx[idx < 1] <- 1
  idx[idx >= length(ts_date_num)] <- length(ts_date_num) - 1

  # Compare both idx and idx + 1 for closeness
  delta1 <- abs(ts_date_num[idx] - date_num)
  delta2 <- abs(ts_date_num[idx + 1] - date_num)

  # Pick the closest one, but only if within 30s
  use_next <- delta2 < delta1
  closest_idx <- ifelse(use_next, idx + 1, idx)

  # Mask values beyond 30s
  closest_idx[(pmin(delta1, delta2) > 30)] <- NA

  # Final values
  value <- rep(NA, length(date_num))
  value[!is.na(closest_idx)] <- ts_value[closest_idx[!is.na(closest_idx)]]

  # reshape in matrix format
  mat <- list(
    value = matrix(value, nrow = 24 * 60 * 60 / res),
    date = matrix(date, nrow = 24 * 60 * 60 / res),
    res = res
  )

  # image(mat$value)
  mat$day <- as.Date(as.POSIXct(
    colMeans(mat$date),
    origin = "1970-01-01",
    tz = "UTC"
  ))
  mat$time <- format(
    as.POSIXct(mat$date[, 1], origin = "1970-01-01", tz = "UTC"),
    "%H:%M"
  )

  return(mat)
}
