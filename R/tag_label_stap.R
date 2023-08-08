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
#' @param warning_flight_duration Threshold of flight duration to display warning for (hours)
#' @param warning_stap_duration Threshold of stationary period duration to display warning for
#' (hours)
#' @param quiet  Logical to display warning
#' @return Same data logger list as input `tag` but with (1) a new data.frame of stationary periods
#' `tag$stap` and (2) a new column `stap_id` for each sensor data.
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = T) |>
#'   tag_label_read()
#'
#' tag <- tag_label_stap(tag)
#' tag
#' str(tag$stap)
#'
#' @family tag_label
#' @seealso  [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#identify-stationary-periods)
#' @export
tag_label_stap <- function(tag,
                           quiet = FALSE,
                           warning_flight_duration = 2,
                           warning_stap_duration = 6) {
  if ("setmap" %in% tag_status(tag)) {
    cli::cli_abort(c(
      "x" = "{.fun setmap} has already been run on this {.var tag}.",
      ">" = "It is best practice to start from your raw data again using {.fun tag_create}.",
      "i" = "You can also use {.fun tag_update} to only change the what needs to be updated in {.var tag}."
    ))
  }

  # Perform test
  tag_assert(tag, "label")

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

  if (!quiet) {
    # Display warning based on stap duration
    stap <- tag$stap
    if (nrow(stap) == 1) {
      cli::cli_warn(c(
        "!" = "There is only a single stationary period.",
        i = "Check that you are using {.val flight} in the label file and labelling the correct \\
        series ({.field pressure} or {.field acceleration})."
      ))
    }

    stap$duration_num <- stap2duration(stap, units = "hours")
    stap$duration_time <- stap2duration(stap, return_numeric = FALSE)

    stap_warning <- stap[stap$duration_num <= warning_stap_duration, ]
    cli::cli_h3("Short stationary periods:")
    if (nrow(stap_warning) > 0) {
      for (i in seq_len(nrow(stap_warning))) {
        s <- stap_warning[i, ]
        cli::cli_inform(c(
          "!" =
            "Stap {s$stap} ({s$start} - {s$end}) : {prettyunits::pretty_dt(s$duration_time)}\f"
        ))
      }
    } else {
      cli::cli_inform(c("v" = "All {nrow(stap)} stationary period{?s} duration are above \\
                             {warning_stap_duration} hour{?s}.\f"))
    }

    # Flight
    flight <- stap2flight(stap, units = "hours", return_numeric = FALSE)
    flight_warning <- flight[as.numeric(flight$duration, units = "hours") <= warning_flight_duration, ]
    cli::cli_h3("Short flights:")
    if (nrow(flight_warning) > 0) {
      for (i in seq_len(nrow(flight_warning))) {
        f <- flight_warning[i, ]
        cli::cli_inform(c(
          "!" =
            "Flight {f$stap_s} -> {f$stap_t} ({f$start} - {f$end}) : \\
          {prettyunits::pretty_dt(as.difftime(f$duration, units = 'hours'))}\f"
        ))
      }
    } else {
      cli::cli_inform(c("v" = "All {nrow(flight)} flight{?s} duration are above \\
                              {warning_flight_duration} hour{?s}.\f"))
    }
  }
  return(tag)
}
