#' Automatic labelling of tag from acceleration data
#'
#' This function uses acceleration data to classify migratory flights. The function uses a
#' `k=2` mean clustering ([`kmeans()`]) to identify high activity periods. Periods lasting more than
#' `min_duration` are then considered to be migratory flight.
#'
#' This function is inspired by the function `classify_flap` from the
#' [PAMlr package](https://github.com/KiranLDA/PAMlr).
#'
#' @inheritParams tag_label
#' @param min_duration Minimal duration (in minutes) to consider a high activity as migratory
#' flight.
#' @return Same data logger list than input `tag`, but with the column `label` filled with
#' `"flight"` in the acceleration data.frame when a sustained high-activity period is detected.
#' @seealso [flapping chapter of the tagLr
#' manual](https://kiranlda.github.io/tagLrManual/flapping.html), [GeoPressureManual | Pressure Map
#' ](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#automatic-classification-of-activity)
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX")
#'
#' tag <- tag_label_auto(tag, min_duration = 15)
#' str(tag$acceleration)
#'
#' @family tag_label
#' @export
tag_label_auto <- function(tag,
                           min_duration = 30) {
  tag_assert(tag)
  if (!assertthat::has_name(tag$pressure, "label")) {
    tag$pressure$label <- ""
  }

  if (assertthat::has_name(tag, "acceleration")) {
    assertthat::assert_that(is.data.frame(tag$acceleration))
    assertthat::assert_that(assertthat::has_name(tag$acceleration, c("value", "date")))
    assertthat::assert_that(is.numeric(min_duration))
    assertthat::assert_that(min_duration > 0)

    # Run a 2 class k mean clustering
    km <- stats::kmeans(tag$acceleration$value[tag$acceleration$value > 0], centers = 2)

    # classify all datapoints belonging to the high value cluster
    act_mig <- tag$acceleration$value > mean(km$centers)

    # group continous activites (low or high) with and ID
    act_id <- c(1, cumsum(diff(as.numeric(act_mig)) != 0) + 1)

    # compute the time resolution of the datset
    dt <- as.double(tag$acceleration$date[2] - tag$acceleration$date[1], units = "mins")

    # Search all activity with high activity and with a duration above
    # min_duration
    tmp <- sapply(split(act_mig, act_id), unique) & table(act_id) * dt > min_duration

    # Classify acceleration accordingly
    tag$acceleration$label <- ifelse(tmp[act_id], "flight", "")
  }
  return(tag)
}
