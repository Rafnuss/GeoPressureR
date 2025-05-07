#' Estimate twilights from light data
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
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
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
    light$value <- twilight_create_transform(light$value)
  }

  if (is.null(twl_thr)) {
    twl_thr <- min(light$value[light$value > 0], na.rm = TRUE)
  }
  assertthat::assert_that(is.numeric(twl_thr))

  # add padding of time to center if night are not at 00:00 UTC
  if (is.null(twl_offset)) {
    twl_offset <- twilight_create_guess_offset(light, twl_thr = twl_thr)
  }

  # Use ts2mat() to reshape light into a matrix
  mat <- ts2mat(light, twl_offset)
  # image(mat$value)

  # Compute exceed of light
  l <- mat$value >= twl_thr
  # image(l)

  # Find the first light
  id_sr <- apply(l, 2, function(x) {
    if (all(is.na(x))) NA else which.max(x)
  })
  id_sr_r <- id_sr + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  # check that this value was measured and above the threshold
  id <- mat$value[id_sr_r] >= twl_thr
  id_sr <- id_sr[id]
  id_sr_r <- id_sr_r[id]
  if (any(stats::na.omit(id_sr) == 1)) {
    cli::cli_warn(c(
      "!" = "{sum(id_sr == 1)} twilights are set at midnight (relative to {.var twl_offset}).",
      "i" = "There is likely a problem with {.var twl_offset = {twl_offset}}."
    ))
  }
  sr <- as.POSIXct(mat$date[id_sr_r], origin = "1970-01-01", tz = "UTC")

  # Find the last light
  id_ss <- apply(l[rev(seq_len(nrow(l))), ], 2, function(x) {
    if (all(is.na(x))) NA else dim(l)[1] - which.max(x)
  })

  id_ss_s <- id_ss + (seq_len(dim(l)[2]) - 1) * dim(l)[1]
  # check that this value was measured and above the threshold
  id <- mat$value[id_ss_s + 1] >= twl_thr
  id_ss_s <- id_ss_s[id]
  id_ss <- id_ss[id]
  if (any(stats::na.omit(id_ss) == dim(l)[1])) {
    cli::cli_warn(c(
      "!" = "{sum(id_ss == 1)} twilights are set at midnight (relative to {.var twl_offset}).",
      "i" = "There is likely a problem with {.var twl_offset = {twl_offset}}."
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
  tag$param$twl_transform_light <- transform_light
  tag$param$twilight_create$twl_offset <- twl_offset
  tag$param$twilight_create$twl_thr <- twl_thr

  return(tag)
}

#' @noRd
twilight_create_transform <- function(value) {
  log(value + 0.0001) + abs(min(log(value + 0.0001), na.rm = TRUE))
}

#' @noRd
twilight_create_guess_offset <- function(light, twl_thr = NULL) {
  if (is.null(twl_thr)) {
    twl_thr <- min(light$value[light$value > 0], na.rm = TRUE)
  }

  mat <- ts2mat(light, twl_offset = 0)
  l <- mat$value >= twl_thr
  tmp <- rowMeans(l, na.rm = TRUE)
  offset_id <- round(sum(tmp * seq_len(dim(mat$value)[1]), na.rm = TRUE) / sum(tmp, na.rm = TRUE))
  twl_offset <- (mat$res * offset_id - 60 * 60 * 12) / 60 / 60

  return(twl_offset)
}
