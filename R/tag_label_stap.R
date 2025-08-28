#' Create stationary periods from a tag label
#'
#' @description
#' This function computes the stationary periods from the pressure and/or acceleration label data.
#' In most case, this function should be run directly after `tag_label_read()` in order to update
#' `tag` to correspond to the new label file.
#'
#' A stationary period is a period during which the bird is considered static relative to the
#' spatial resolution of interest (~10-50km). They are defined by being separated by a flight of any
#' duration (label `"flight"`). The `stap_id` is an integer value for stationary periods and decimal
#' value for flight. The `stap_id` is added as a new column to each sensor data.
#'
#' If an acceleration data.frame is present and contains a column `label`, the stationary period
#' will be computed from it, otherwise, it uses the pressure data.frame.
#'
#'
#' @inheritParams tag_label
#' @param warning_flight_duration Threshold of flight duration to display warning for (hours)
#' @param warning_stap_duration Threshold of stationary period duration to display warning for
#' (hours)
#' @param quiet logical to display warning message.
#' @return `tag` is return with (1) a new data.frame of stationary periods `tag$stap` and (2) a new
#'  column `stap_id` for each sensor data.
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label_read()
#'
#'   tag <- tag_label_stap(tag)
#'
#'   str(tag)
#'
#'   str(tag$stap)
#' })
#' @family tag_label
#' @seealso [GeoPressureManual](https://bit.ly/45gwcVu)
#' @export
tag_label_stap <- function(tag,
                           quiet = FALSE,
                           warning_flight_duration = 2,
                           warning_stap_duration = 6) {
  if (tag_assert(tag, "setmap", "")) {
    cli::cli_abort(c(
      "x" = "{.fun setmap} has already been run on this {.var tag}.",
      ">" = "It is best practice to start from your raw data again using {.fun tag_create}."
    ))
  }

  # Perform test
  tag_assert(tag, "label")

  # Build activity from acceleration when available
  if (assertthat::has_name(tag, "acceleration") &&
    assertthat::has_name(tag$acceleration, "label")) {
    sensor <- tag$acceleration[, c("date", "label")]

    # Append pressure label when acceleration ends
    if (assertthat::has_name(tag, "pressure")) {
      # Only use pressure data fire the first or after the last acceleration timestamp
      out <- tag$pressure$date < min(sensor$date, na.rm = TRUE) |
        tag$pressure$date > max(sensor$date, na.rm = TRUE)
      pres_outside_acc <- tag$pressure[out, ]

      if (nrow(pres_outside_acc) > 0) {
        # combine both data
        sensor <- rbind(sensor, pres_outside_acc[, c("date", "label")])
        # order by date
        sensor <- sensor[order(sensor$date), ]
      }
      if ("flight" %in% tag$pressure$label[!out]) {
        cli::cli_warn(c(
          "!" = "The label {.val flight} is detected on {.field pressure} while
        {.field acceleration} is also present.",
          "i" = "The stationary periods will be estimated from {.field acceleration} data and the
        {.val flight} label from pressure will be ignored. ",
          ">" = "It is best practise to remove {.val flight} label in {.field pressure} data when
        {.field acceleration} is available."
        ))
      }
    }
  } else {
    sensor <- tag$pressure[, c("date", "label")]
  }

  # Create a table of activities (migration or stationary)
  tmp <- c(1, cumsum(diff(as.numeric(sensor$label == "flight")) == 1) + 1)
  tmp[sensor$label == "flight"] <- NA

  # As we label "in flight" pressure/acceleration, the taking-off and landing happened before and
  # after the labelling respectively. To account for this. We estimate that the bird took off
  # between the previous and first flight label, and landed between the last flight label and next
  # one. We use the temporal resolution to account for this.
  sensor$dt_prev <- c(as.difftime(0, units = "mins"), diff(sensor$date)) / 2
  sensor$dt_next <- c(diff(sensor$date), as.difftime(0, units = "mins")) / 2

  # construct stationary period table
  tag$stap <- data.frame(
    stap_id = unique(tmp[!is.na(tmp)]),
    start = do.call(c, lapply(split(sensor$date, tmp), min)) -
      do.call(c, lapply(split(sensor$dt_prev, tmp), function(x) x[1])),
    end = do.call("c", lapply(split(sensor$date, tmp), max)) +
      do.call(c, lapply(split(sensor$dt_next, tmp), function(x) x[1]))
  )

  # Assign to each sensor the stationary period to which it belong to.
  for (sensor_df in c("pressure", "acceleration", "light", "twilight", "magnetic")) {
    if (assertthat::has_name(tag, sensor_df)) {
      assertthat::assert_that(is.data.frame(tag[[sensor_df]]))
      if ("date" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$date
      } else if ("twilight" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$twilight
      } else {
        cli::cli_abort(c("{.field {sensor_df}} needs to have a column {.field date} or \\
                         {.field twilight}."))
      }
      tag[[sensor_df]]$stap_id <- find_stap(tag$stap, date)
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
    cli::cli_rule("Short stationary periods ({.strong <{warning_stap_duration}hr}):")
    if (nrow(stap_warning) > 0) {
      for (i in seq_len(nrow(stap_warning))) {
        # nolint start
        s <- stap_warning[i, ]
        cli::cli_bullets(c(
          "!" =
            "Stap {s$stap} ({format(s$start, format='%Y-%m-%d %H:%M')} - \\
          {format(s$end, format='%Y-%m-%d %H:%M')}) : {pretty_dt(s$duration_time)}"
        ))
        # nolint end
      }
    } else {
      cli::cli_bullets(c("v" = "All {nrow(stap)} stationary period{?s} duration are above \\
                             {warning_stap_duration} hour{?s}."))
    }

    # Flight
    flight <- stap2flight(stap, units = "hours", return_numeric = FALSE)
    flight_warning <- flight[as.numeric(flight$duration, units = "hours") <=
      warning_flight_duration, ]
    cli::cli_rule("Short flights ({.strong <{warning_flight_duration}hr}):")
    if (nrow(flight_warning) > 0) {
      for (i in seq_len(nrow(flight_warning))) {
        # nolint start
        f <- flight_warning[i, ]
        cli::cli_bullets(c(
          "!" =
            "Flight {f$stap_s} -> {f$stap_t} ({format(f$start, format='%Y-%m-%d %H:%M')} - \\
            {format(f$end, format='%Y-%m-%d %H:%M')}) : \\
          {pretty_dt(as.difftime(f$duration, units = 'hours'))}"
        ))
        # nolint end
      }
    } else {
      cli::cli_bullets(c("v" = "All {nrow(flight)} flight{?s} duration are above \\
                              {warning_flight_duration} hour{?s}."))
    }
  }

  tag$param$tag_label$warning_flight_duration <- warning_flight_duration
  tag$param$tag_label$warning_stap_duration <- warning_stap_duration

  return(tag)
}

#' Format difftime in d, h, m and s
#'
#' @param tim character string or numeric value specifying a time interval.
#' @noRd
pretty_dt <- function(tim) {
  # Ensure the difftime object is in seconds
  seconds <- as.numeric(as.difftime(tim), units = "secs")

  # Calculate days, hours, minutes, and seconds
  days <- floor(seconds / (24 * 60 * 60))
  seconds <- seconds %% (24 * 60 * 60)
  hrs <- floor(seconds / (60 * 60))
  seconds <- seconds %% (60 * 60)
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)

  # Format the string
  duration_str <- ""
  if (days > 0) {
    duration_str <- paste0(duration_str, days, "d ")
  }
  if (hrs > 0) {
    duration_str <- paste0(duration_str, hrs, "h ")
  }
  if (mins > 0) {
    duration_str <- paste0(duration_str, mins, "m ")
  }
  if (secs > 0 || duration_str == "") {
    duration_str <- paste0(duration_str, secs, "s")
  }

  # Trim and return
  trimws(duration_str)
}

#' Find the stationary period corresponding to a date
#'
#' @noRd
find_stap <- function(stap, date) {
  # Find stap for each date
  tmp <- mapply(function(start, end) {
    start <= date & date <= end
  }, stap$start, stap$end)

  # Find index
  tmp <- which(tmp, arr.ind = TRUE)

  # Initiate with 0
  stap_id <- rep(0, length(date))

  # Add known stap_id
  stap_id[tmp[, 1]] <- tmp[, 2]

  # Interpolate linearly in between
  sequence <- seq_len(length(stap_id))
  id <- stap_id == 0
  stap_id[id] <- stats::approx(sequence[!id], stap_id[!id], sequence[id], rule = 2)$y

  assertthat::assert_that(all(!is.na(stap_id)))

  stap_id
}
