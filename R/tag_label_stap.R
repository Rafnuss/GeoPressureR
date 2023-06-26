#' Create stationary periods from tag label
#'
#' @description
#' This function computes the stationary periods from the pressure and/or acceleration label data.
#'
#' If an acceleration data.frame is present and contains a column `label`, the stationary period will
#' be computed from it, otherwise, it uses the pressure data.frame.
#'
#' Stationary periods are defined by being separated by a flight (label `"flight"`). Flights of any
#' duration will be considered.
#'
#' The function also adds the corresponding `stap_id` column to each sensor data.
#'
#' @inheritParams tag_label
#' @return Same data logger list as input `tag` but with (1) a new data.frame of stationary periods
#' `tag$stap` and (2) a new column `stap_id` for each sensor data.
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |>
#'   tag_label_read()
#'
#' tag <- tag_label_stap(tag)
#' str(tag)
#' str(tag$stap)
#'
#' @seealso [`tag_create()`], [`tag_label()`], [`tag_label_read()`], [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#identify-stationary-periods)
#' @export
tag_label_stap <- function(tag) {
  # Perform test
  assertthat::assert_that(inherits(tag, "tag"))
  assertthat::assert_that(assertthat::has_name(tag, "pressure"))
  assertthat::assert_that(is.data.frame(tag$pressure))
  assertthat::assert_that(assertthat::has_name(tag$pressure, c("date", "label")))

  # If acceleration is present, use acceleration, otherwise pressure
  if (assertthat::has_name(tag, "acceleration") &&
    assertthat::has_name(tag$acceleration, "label")) {
    sensor <- tag$acceleration
    if ("flight" %in% tag$pressure$label) {
      cli::cli_warn("The stationary periods will be estimated from acceleration data and the
        label {.val flight} from pressure will be ignored. It is best practise to remove
        {.val flight} in  pressure data if you are using acceleration. Remove label column in
        acceleration data to use pressure label.")
    }
  } else {
    sensor <- tag$pressure
  }

  # Create a table of activities (migration or stationary)
  tmp <- c(1, cumsum(diff(as.numeric(sensor$label == "flight")) == 1) + 1)
  tmp[sensor$label == "flight"] <- NA

  # construct stationary period table
  tag$stap <- data.frame(
    stap_id = unique(tmp[!is.na(tmp)]),
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
      tag[[sensor_df]]$stap_id <- 0
      tag[[sensor_df]]$stap_id[tmp[, 1]] <- tmp[, 2]
    }
  }

  # Display warning and danger based on stap duration
  stap <- tag$stap
  stap_duration_warning <- 6
  stap_duration_danger <- 2
  if (nrow(stap) == 1) {
    cli::cli_alert_warning(c(
      "!" = "There is only a single stationary period.",
      i = "Check that you are using {.val flight} in the label file and labelling the correct \\
      series ({.field pressure} or {.field acceleration})."
    ))
  }

  stap$duration <- difftime(stap$end, stap$start, units = "hours")
  if (any(stap$duration <= stap_duration_warning)) {
    for (i in seq_len(nrow(stap))) {
      stap <- stap[i, ]
      if (stap$duration <= stap_duration_danger) {
        cli::cli_alert_danger("Stap {stap$stap} ({stap$start} - {stap$end}) : \\
                              {prettyunits::pretty_dt(stap$duration)}")
      } else if (stap$duration <= stap_duration_warning) {
        cli::cli_alert_warning("Stap {stap$stap} ({stap$start} - {stap$end}) : \\
                               {prettyunits::pretty_dt(stap$duration)}")
      }
    }
  } else {
    cli::cli_alert_success("All {nrow(stap)} stationary period{?s} duration are above \\
                           {stap_duration_warning} hour{?s}.")
  }

  # cli::cli_rule("Flight duration")
  flight_duration_warning <- 2
  flight_duration_danger <- 1
  flight <- stap2flight(stap)
  if (any(flight$duration <= flight_duration_warning)) {
    for (i in seq_len(nrow(flight))) {
      flight_i <- flight[i, ]
      if (flight_i$duration <= flight_duration_danger) {
        cli::cli_alert_danger("Flight {flight_i$stap_s} -> {flight_i$stap_t} ({flight_i$start} - \\
                              {flight_i$end}) : {prettyunits::pretty_dt(flight_i$duration)}")
        pass <- FALSE
      } else if (flight_i$duration <= flight_duration_warning) {
        cli::cli_alert_warning("Flight {flight_i$stap_s} -> {flight_i$stap_t} ({flight_i$start} - \\
                               {flight_i$end}) : {prettyunits::pretty_dt(flight_i$duration)}")
      }
    }
  } else {
    cli::cli_alert_success("All {nrow(flight)} flight{?s} duration are above \\
                            {flight_duration_warning} hour{?s}.")
  }

  return(tag)
}