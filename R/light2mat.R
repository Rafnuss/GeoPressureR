#' Format light data into a matrix to enable the retrieval of twilights
#'
#' @inheritParams twilight_create
#' @param light data.frame of a `tag`, containing at least `date` and `value`.
#' @return A data.frame with columns `date` and `value`.
#' @family twilight
#' @export
light2mat <- function(light, twl_offset = 0) {
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(assertthat::has_name(light, c("date", "value")))
  assertthat::assert_that(assertthat::is.time(light$date))
  assertthat::assert_that(is.numeric(light$value))
  assertthat::assert_that(is.numeric(twl_offset))

  res_vec <- as.numeric(diff(light$date), units = "secs")
  res <- stats::median(res_vec)
  if (length(unique(res_vec)) != 1) {
    cli::cli_warn(c(
      x = "Temporal resolution of the light data is not constant. We will use a regular \\
      resolution of {.val {res}} seconds."
    ))
  }

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
  # Convert to numeric for faster computation
  date_num <- as.numeric(date)
  light_date_num <- as.numeric(light$date)

  # Sort the light data (if not already sorted)
  ord <- order(light_date_num)
  light_date_num <- light_date_num[ord]
  light_value <- light$value[ord]

  # Approximate index positions for all date_num
  idx <- findInterval(date_num, light_date_num)

  # Ensure idx is within bounds (1 to length-1)
  idx[idx < 1] <- 1
  idx[idx >= length(light_date_num)] <- length(light_date_num) - 1

  # Compare both idx and idx + 1 for closeness
  delta1 <- abs(light_date_num[idx] - date_num)
  delta2 <- abs(light_date_num[idx + 1] - date_num)

  # Pick the closest one, but only if within 30s
  use_next <- delta2 < delta1
  closest_idx <- ifelse(use_next, idx + 1, idx)

  # Mask values beyond 30s
  closest_idx[(pmin(delta1, delta2) > 30)] <- NA

  # Final values
  value <- rep(NA, length(date_num))
  value[!is.na(closest_idx)] <- light_value[closest_idx[!is.na(closest_idx)]]

  # reshape in matrix format
  mat <- list(
    value = matrix(value, nrow = 24 * 60 * 60 / res),
    date = matrix(date, nrow = 24 * 60 * 60 / res),
    res = res
  )

  # image(mat$value)
  mat$day <- as.Date(as.POSIXct(colMeans(mat$date), origin = "1970-01-01", tz = "UTC"))
  mat$time <- format(as.POSIXct(mat$date[, 1], origin = "1970-01-01", tz = "UTC"), "%H:%M")

  return(mat)
}
