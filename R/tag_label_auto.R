#' Automatic labelling acceleration data of a `tag`
#'
#' @description
#' This function uses acceleration data to classify migratory flights. The function uses a
#' `k=2` mean clustering ([`kmeans()`]) to identify high activity periods. Periods of high activity
#'  lasting more than `min_duration` are then considered to be migratory flight.
#'
#' Additionally, we perform a post-processing step to improve the classification of low-activity
#' period happening during a migration flight (e.g. gliding phase):
#' 1. Mid-activity surrounded by flights. The kmeans classification classify high-activity all
#' values above 50% of the distance between the two clusters. Here, we classify as mid-activity
#' non-flight activity data which value are greater than `thr_reclassify` of the distance between
#' the two kmeans clusters. Any periods of mid-activity surrounded by flight are also considered
#' as flights.
#' 2. To avoid stap lasting only a few datapoints, we also classify any datapoint
#' which is surrounded by a flight before and after, within a window of +/- `post_proc_window`
#' points. As a result, no stap will be shorter than `(2 * post_proc_window + 1) * dt`.
#'
#' This function is inspired by the function `classify_flap` from the
#' [PAMlr package](https://github.com/KiranLDA/PAMlr).
#'
#' @inheritParams tag_label
#' @param min_duration Minimal duration (in minutes) to consider a high activity as migratory
#' flight.
#' @param thr_reclassify Post-processing threshold of activity considered for re-classification.
#' Typically between `0` and `0.5` (no effect). See post-processing for details.
#' @param post_proc_window Post-processing windows considered for re-classification. Typically `0`
#' (no effect) to `5` in unit of the temporal resolution. See post-processing for details.
#' @return Same data logger list than input `tag`, but with the column `label` filled with
#' `"flight"` in the acceleration data.frame when a sustained high-activity period is detected.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE)
#'
#'   tag <- tag_label_auto(tag, min_duration = 15)
#'   str(tag$acceleration)
#' })
#'
#' @family tag_label
#' @seealso [GeoPressureManual](https://bit.ly/45bthNt)
#' @export
tag_label_auto <- function(
  tag,
  min_duration = 30,
  thr_reclassify = 0.1,
  post_proc_window = 2
) {
  tag_assert(tag)
  if (!assertthat::has_name(tag$pressure, "label")) {
    tag$pressure$label <- ""
  }

  if (assertthat::has_name(tag, "acceleration")) {
    assertthat::assert_that(is.data.frame(tag$acceleration))
    assertthat::assert_that(assertthat::has_name(
      tag$acceleration,
      c("value", "date")
    ))
    assertthat::assert_that(is.numeric(min_duration))
    assertthat::assert_that(min_duration > 0)

    if (all(tag$acceleration$value == 0)) {
      warning("All acceleration values are zero. No flight detected.")
      tag$acceleration$label <- ""
      return(tag)
    }

    # Run a 2 class k mean clustering
    km <- stats::kmeans(
      tag$acceleration$value[tag$acceleration$value > 0],
      centers = 2
    )

    # classify all datapoints belonging to the high value cluster
    act_mig <- tag$acceleration$value > mean(km$centers)

    # group continous activites (low or high) with and ID
    act_id <- c(1, cumsum(diff(as.numeric(act_mig)) != 0) + 1)

    # compute the time resolution of the datset
    dt <- as.double(
      tag$acceleration$date[2] - tag$acceleration$date[1],
      units = "mins"
    )

    # Search all activity with high activity and with a duration above
    # min_duration
    tmp <- sapply(split(act_mig, act_id), unique) &
      table(act_id) * dt > min_duration

    is_flight <- as.vector(tmp[act_id])

    # Post-processing to improve classification of low activity migration, typically occurring when
    # bird glides in between a long flight.

    # Find a threashold of activity from which a low activity could actually be migration.
    centers <- as.vector(km$centers)
    thr_0 <- min(centers) + thr_reclassify * abs(diff(centers))

    # Classify these as NA for now
    is_flight[thr_0 < tag$acceleration$value & !is_flight] <- NA

    # Find if the previous and next (non-NA) is a flight or not.
    prev_isna <- stats::approx(
      seq_along(is_flight)[!is.na(is_flight)],
      is_flight[!is.na(is_flight)],
      seq_along(is_flight)[is.na(is_flight)],
      method = "constant",
      rule = 2,
      f = 0
    )$y

    next_isna <- stats::approx(
      seq_along(is_flight)[!is.na(is_flight)],
      is_flight[!is.na(is_flight)],
      seq_along(is_flight)[is.na(is_flight)],
      method = "constant",
      rule = 2,
      f = 1
    )$y

    # Classify as migratory flight if next or previous is migration.
    is_flight[is.na(is_flight)] <- prev_isna | next_isna

    # In addition, consider in flight if there is previous and next is in flight
    for (i in seq_len(length(is_flight))) {
      if (!is_flight[i]) {
        # Define the range of neighbours
        start_idx <- max(1, i - post_proc_window)
        end_idx <- min(length(is_flight), i + post_proc_window)

        # Check if any of the neighbours are TRUE
        if (any(is_flight[start_idx:i]) && any(is_flight[i:end_idx])) {
          is_flight[i] <- TRUE
        }
      }
    }

    # Classify acceleration accordingly
    tag$acceleration$label <- ifelse(is_flight, "flight", "")
  }
  return(tag)
}
