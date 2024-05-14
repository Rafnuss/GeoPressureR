#' Estimate twilight from light data
#'
#' @description
#' This function estimate twilight (i.e., datetime of sunrise and sunset) by searching for the first
#' and last light of the day which is defined by the light exceeds or
#' falls below the light threshold `twl_thr`.
#'
#' @param tag a GeoPressureR `tag` object
#' @param twl_thr Light threshold that defines twilight. By default (`NULL`), it uses the smallest
#' value of light (i.e, first and last light of day).
#' @param twl_offset Shift of the middle of the night compared to 00:00 UTC (in hours). If not
#' provided, it uses the middle of all nights.
#' @param transform_light logical to use a log transformation of light
#'
#' @return a `tag` list containing a new data.frame `twilight` with columns:
#' - `twilight` (date-time of twilight)
#' - `rise` (logical) indicating sunrise (`TRUE`) or sunset (`FALSE`).
#' - `stap_id` if `stap_id` is present in `light`.
#'
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' setwd(owd)
#'
#' # Create twilight data.frame
#' tag <- twilight_create(tag)
#'
#' str(tag$twilight)
#'
#' plot(tag, type = "twilight")
#'
#' @family geolight
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html), [`TwGeos::findTwilights()`](
#' https://rdrr.io/github/slisovski/TwGeos/man/findTwilights.html)
#' @export
twilight_create <- function(tag,
                            twl_thr = NULL,
                            twl_offset = NULL,
                            transform_light = TRUE) {
  tag_assert(tag)

  light <- tag$light
  assertthat::assert_that(is.data.frame(light))
  assertthat::assert_that(assertthat::has_name(light, c("date", "value")))
  assertthat::assert_that(assertthat::is.time(light$date))
  assertthat::assert_that(is.numeric(light$value))

  if (transform_light) {
    light$value <- log(light$value + 0.0001) + abs(min(log(light$value + 0.0001)))
  }

  if (is.null(twl_thr)) {
    twl_thr <- min(light$value[light$value > 0])
  }
  assertthat::assert_that(is.numeric(twl_thr))

  # add padding of time to center if night are not at 00:00 UTC
  if (is.null(twl_offset)) {
    mat <- light2mat(light, twl_offset = 0)
    l <- mat$value >= twl_thr
    tmp <- rowMeans(l, na.rm = TRUE)
    offset_id <- round(sum(tmp * seq_len(dim(mat$value)[1])) / sum(tmp))
    twl_offset <- (mat$res * offset_id - 60 * 60 * 12) / 60 / 60
  }

  # Use light2mat() to reshape light into a matrix
  mat <- light2mat(light, twl_offset)
  # image(mat$value)

  # Compute exceed of light
  l <- mat$value >= twl_thr
  # image(l)

  # Find the first light
  id_sr <- apply(l, 2, which.max)
  id_sr_r <- id_sr + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  # check that this value was measured and above the threshold
  id <- mat$value[id_sr_r] >= twl_thr
  id_sr <- id_sr[id]
  id_sr_r <- id_sr_r[id]
  if (any(id_sr == 1)) {
    cli::cli_warn(c(
      "!" = "{sum(id_sr == 1)} twilights are set at midnight (relative to {.var twl_offset}).",
      "i" = "There is likely a problem with {.var twl_offset = {twl_offset}}.\f"
    ))
  }
  sr <- as.POSIXct(mat$date[id_sr_r], origin = "1970-01-01", tz = "UTC")

  # Find the last light
  id_ss <- dim(l)[1] - apply(l[nrow(l):1, ], 2, which.max)
  id_ss_s <- id_ss + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  # check that this value was measured and above the threshold
  id <- mat$value[id_ss_s + 1] >= twl_thr
  id_ss_s <- id_ss_s[id]
  id_ss <- id_ss[id]
  if (any(id_ss == dim(l)[1])) {
    cli::cli_warn(c(
      "!" = "{sum(id_ss == 1)} twilights are set at midnight (relative to {.var twl_offset}).",
      "i" = "There is likely a problem with {.var twl_offset = {twl_offset}}.\f"
    ))
  }
  ss <- as.POSIXct(mat$date[id_ss_s + 1], origin = "1970-01-01", tz = "UTC")

  twilight <- data.frame(
    twilight = c(ss, sr),
    rise = c(logical(length(ss)), !logical(length(sr)))
  )

  # order by time
  twilight <- twilight[order(twilight$twilight), ]

  # Add stap_id if present
  if ("stap_id" %in% names(tag$stap)) {
    twilight$stap_id <- find_stap(tag$stap, twilight$twilight)
  }

  tag$twilight <- twilight
  tag$param$twl_offset <- twl_offset
  tag$param$twl_thr <- twl_thr

  return(tag)
}
