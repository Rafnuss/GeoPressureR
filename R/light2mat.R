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
  res <- median(res_vec)
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

  # Function to find the closest value within ±30s
  closest_value <- function(t) {
    idx <- findInterval(t, light_date_num) # Approximate closest index

    # Get candidate indices (current and adjacent)
    candidates <- c(idx, idx + 1)
    candidates <- candidates[candidates > 0 & candidates <= length(light_date_num)]

    # Filter to those within ±30s
    candidates <- candidates[abs(light_date_num[candidates] - t) <= 30]

    if (length(candidates) > 0) {
      closest_idx <- candidates[which.min(abs(light_date_num[candidates] - t))]
      return(light_value[closest_idx])
    } else {
      return(NA)
    }
  }

  # Vectorized lookup
  value <- sapply(date_num, closest_value)

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
