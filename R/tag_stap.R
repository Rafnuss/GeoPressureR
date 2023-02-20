#' Compute stationary periods
#'
#' @description
#' This function computes the stationary periods from classified pressure and/or acceleration data.
#'
#' If acceleration data.frame is present and contains a colum `label`, the stationary period will be
#' computed from it, otherwise, uses pressure data.frame.
#'
#' Uses the label `"flight"` to separate stationary periods. flight of any duration will be
#' considered.
#'
#' @param tag Data logger list with label (see [`tag_read()`] and [`trainset_read()`])
#' @return Same data logger list as input `tag` but with (1) a new data.frame of stationary periods
#' `tag$stap` and (2) a new column `stap` for pressure, light and acceleration data.
#'
#' @examples
#' tag <- tag_read(
#'   directory = system.file("extdata/0_tag/18LX", package = "GeoPressureR")
#' )
#' tag <- trainset_read(tag,
#'   directory = system.file("extdata/1_pressure/labels", package = "GeoPressureR")
#' )
#' tag <- tag_stap(tag)
#' str(tag)
#' @seealso [`tag_read()`], [`tag_classify()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#identify-stationary-periods)
#' @export
tag_stap <- function(tag) {
  # Perform test
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "label")))

  # If acceleration is present, use acceleration, otherwise pressure
  if (assertthat::has_name(tag, "acceleration") &&
      assertthat::has_name(tag$acceleration, "label")) {
    sensor <- tag$acceleration
    if ("flight" %in% tag$pressure$label) {
      warning(paste0(
        "The stationary periods will be estimated from acceleration data and the ",
        "label 'flight' from pressure will be ignored. It is best practise to remove 'flight' in ",
        "pressure data if you are using acceleration. Remove label column in acceleration data to ",
        "use pressure label."
      ))
    }
  } else {
    sensor <- tag$pressure
  }

  # Create a table of activities (migration or stationary)
  tmp <- c(1, cumsum(diff(as.numeric(sensor$label == "flight")) == 1) + 1)
  tmp[sensor$label == "flight"] <- NA

  # construct stationary period table
  tag$stap <- data.frame(
    stap = unique(tmp[!is.na(tmp)]),
    start = do.call(c, lapply(split(sensor$date, tmp), min)),
    end = do.call("c", lapply(split(sensor$date, tmp), max))
  )

  # Assign to each sensor the stationary period to which it belong to.
  for (sensor_df in c("pressure", "acceleration", "light")) {
    if (assertthat::has_name(tag, sensor_df)) {
      assertthat::assert_that(is.data.frame(tag[[sensor_df]]))
      assertthat::assert_that(assertthat::has_name(tag[[sensor_df]], "date"))
      tmp <- mapply(function(start, end) {
        start <= tag[[sensor_df]]$date & tag[[sensor_df]]$date <= end
      }, tag$stap$start, tag$stap$end)
      tmp <- which(tmp, arr.ind = TRUE)
      tag[[sensor_df]]$stap <- 0
      tag[[sensor_df]]$stap[tmp[, 1]] <- tmp[, 2]
    }
  }

  return(tag)
}
