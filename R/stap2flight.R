#' Compute flights from stationary periods
#'
#' @description
#' Convert a stationary period data.frame `stap` into a flight data.frame or list. Flight are
#' computed as the difference between the end of a stationary period to the start of the next one.
#' Because the pressure/acceleration is labelled for "in flight", the bird was already in flight
#' before the first label and after the last label. We account for this by adding to all flights
#' duration half the temporal resolution of the sensor.
#'
#' You can compute the flight between specific stationary periods using `include_stap_id`. In this
#' case, the flight duration is computed as the sum of individual flights in between.
#'
#' You can return the flight as a data.frame or as a list if you want to retrieve the information
#' of all individual flight between the `include_stap_id`.
#'
#' @param stap a stationary period data.frame (see [`tag_label_stap()`]).
#' @param include_stap_id vector of the stationary period `stap_id` to consider in the flight.
#' Default is to use `stap$stap_id[stap$include]` or `stap$stap_id` if `include` is not available in
#' `stap`.
#' @param format character to return a list `"list"` or a data.frame `"df"` (see description)
#' @inheritParams stap2duration
#' @return A list or a data.frame (see description) containing
#' - `start`: Start time of the (first) flight
#' - `end`: End time of the (last) flight
#' - `stap_id_s`: Source stap_id (i.e, start)
#' - `stap_id_t`: Target stap_id (i.e, end)
#' - `duration`: (Sum of the) duration of flight(s)
#' - (`n`: Number of flights)
#' The value in brackets are only for the data.frame
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' # By default, return a data.frame of all individual flights
#' knitr::kable(stap2flight(tag$stap))
#'
#' # Compute the total flight between stap 1,3 and 5. Sum flight duration in between.
#' knitr::kable(stap2flight(tag$stap, include_stap_id = c(1, 3, 5)))
#'
#' # Can also return as a list of data.frame to access individual flights information.
#' knitr::kable(stap2flight(tag$stap,
#'   include_stap_id = c(1, 3, 5), format = "list",
#'   units = "secs"
#' ))
#'
#' @export
stap2flight <- function(
  stap,
  include_stap_id = NULL,
  format = "df",
  units = "hours",
  return_numeric = TRUE
) {
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(stap, "start"))
  assertthat::assert_that(assertthat::has_name(stap, "end"))
  assertthat::assert_that(all(stap$stap_id == seq_len(nrow(stap))))
  if (is.null(include_stap_id)) {
    if ("include" %in% names(stap)) {
      include_stap_id <- stap$stap_id[stap$include]
    } else {
      include_stap_id <- stap$stap_id
    }
  }
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))
  assertthat::assert_that(format %in% c("list", "df"))
  assertthat::assert_that(is.logical(return_numeric))

  if (length(include_stap_id) == 1) {
    if (format == "list") {
      return(list())
    } else {
      return(data.frame())
    }
  }

  flight_all <- data.frame(
    start = utils::head(stap$end, -1),
    end = utils::tail(stap$start, -1),
    stap_s = utils::head(stap$stap_id, -1),
    stap_t = utils::tail(stap$stap_id, -1)
  )
  flight_all$duration <- stap2duration(
    flight_all,
    units = units,
    return_numeric = return_numeric
  )

  # Filter flight_all to remove flight before the first include_stap_id or after the last
  # include_stap_id
  flight_all <- flight_all[
    min(include_stap_id) <= flight_all$stap_s &
      max(include_stap_id) >= flight_all$stap_t,
  ]

  # create the list of flight per stationary period modelled
  stap_flight_group <- sapply(flight_all$stap_s, function(s) {
    sum(include_stap_id <= s)
  })
  flight_list <- split(flight_all, include_stap_id[stap_flight_group])

  if (format == "list") {
    return(flight_list)
  }

  # compute flight duration for each stationary period
  flight_df <- do.call(
    rbind,
    lapply(flight_list, function(f) {
      data.frame(
        start = f$start[1],
        end = utils::tail(f$end, 1),
        stap_s = f$stap_s[1],
        stap_t = utils::tail(f$stap_t, 1),
        duration = sum(f$duration),
        n = nrow(f)
      )
    })
  )
  return(flight_df)
}
