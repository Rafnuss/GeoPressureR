#' Compute flights from stationary periods
#'
#' @description
#' Convert a stationary period data.frame `stap` into a flight data.frame or list. Flight are
#' computed as the difference between the end of a stationary period to the start of the next one.
#'
#' You can compute the flight between specific stationary periods using `stap_include`. In this
#' case, the flight duration is computed as the sum of individual flights in between.
#'
#' You can return the flight as a data.frame or as a list if you want to retrieve the information
#' of all individual flight between the `stap_include`.
#'
#' @param stap A stationary period data.frame (see [`tag_label_stap()`]).
#' @param stap_include Vecto of the stationary period `stap_id` to consider in the flight. Default
#' is to use `stap$stap_id[stap$include]` or `stap$stap_id` if `model` is not available in `stap`.
#' @param format Character to return a list `"list"` or a data.frame `"df"` (see description)
#' @inheritParams stap2duration
#' @return A list or a data.frame (see description) containing
#' - `start`: Start time of the (first) flight
#' - `end`: End time of the (last) flight
#' - `stap_id_s`: Source stap_id (i.e, start)
#' - `stap_id_t`: Target stap_id (i.e, end)
#' - `duration`: (Sum of the) duration of flight(s)
#' - (`n`: Numer of flights)
#' The value in brackets are only for the data.frame
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#'
#' # By default, return a data.frame of all individual flights
#' knitr::kable(stap2flight(tag$stap))
#'
#' # Compute the total flight between stap 1,3 and 5. Sum flight duration in between.
#' knitr::kable(stap2flight(tag$stap, stap_include = c(1, 3, 5)))
#'
#' # Can also return as a list of data.frame to access individual flights information.
#' knitr::kable(stap2flight(tag$stap, stap_include = c(1, 3, 5), format = "list", units = "secs"))
#'
#' @export
stap2flight <- function(stap,
                        stap_include = NULL,
                        format = "df",
                        units = "hours",
                        return_numeric = TRUE) {
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(stap, "start"))
  assertthat::assert_that(assertthat::has_name(stap, "end"))
  assertthat::assert_that(all(stap$stap_id == seq_len(nrow(stap))))
  if (is.null(stap_include)) {
    if ("include" %in% names(stap)) {
      stap_include <- stap$stap_id[stap$include]
    } else {
      stap_include <- stap$stap_id
    }
  }
  assertthat::assert_that(all(stap_include %in% stap$stap_id))
  assertthat::assert_that(format %in% c("list", "df"))

  if (length(stap_include) == 1) {
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
  flight_all$duration <- stap2duration(flight_all, units = units, return_numeric = return_numeric)

  # Filter flight_all to remove flight before the first stap_include or after the last stap_include
  flight_all <- flight_all[
    min(stap_include) <= flight_all$stap_s &
      max(stap_include) >= flight_all$stap_t,
  ]

  # create the list of flight per stationary period modeled
  stap_flight_group <- sapply(flight_all$stap_s, function(s) {
    sum(stap_include <= s)
  })
  flight_list <- split(flight_all, stap_include[stap_flight_group])

  if (format == "list") {
    return(flight_list)
  }

  # compute flight duration for each stationary period
  flight_df <- do.call(rbind, lapply(flight_list, function(f) {
    data.frame(
      start = f$start[1],
      end = utils::tail(f$end, 1),
      stap_s = f$stap_s[1],
      stap_t = utils::tail(f$stap_t, 1),
      duration = sum(f$duration),
      n = nrow(f)
    )
  }))
  return(flight_df)
}
