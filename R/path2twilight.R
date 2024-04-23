#' Compute astronomical twilight from positions and dates
#'
#' @description
#' This function compute the theorical twilight (i.e., datetime of sunrise and sunset) at given
#' locations and for specific date.
#'
#' We use the [suntools](https://github.com/adokter/suntools) package for this.
#'
#' @param path a GeoPressureR `path` or `pressurepath` data.frame
#' @param date a vector of POSIXt datetime.
#'
#' @return a `twilight` data.frame with columns:
#' - `twilight` (date-time of twilight)
#' - `rise` (logical) indicating sunrise (`TRUE`) or sunset (`FALSE`).
#' - `stap_id`.
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
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html), [suntools
#' ](https://github.com/adokter/suntools)
#' @export
path2twilight <- function(path,
                          date = NULL) {
  assertthat::assert_that(is.data.frame(path))

  # pressurepath
  if ("date" %in% names(path)) {
    assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "date")))

    if (!is.null(date)) {
      cli::cli_warn(c(
        "!" = "You cannot provide a {.var pressurepath} and a {.var date} varible.",
        "i" = "{.var date} is already included in {.var pressurepath}.",
        ">" = "{.var date} will be ignored."
      ))
    }
    twl <- data.frame(
      date = path$date,
      lon = path$lon,
      lat = path$lat
    )
  } else {
    # path
    assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "start", "end")))

    # if date is not provided
    if (is.null(date)) {
      date <- seq.POSIXt(
        from = trunc.POSIXt(min(path$start), units = "days") + 60 * 60 * 24 / 2,
        to = trunc.POSIXt(max(path$end), units = "days") + 60 * 60 * 24 / 2,
        by = "day"
      )
    }

    stap_id <- GeoPressureR:::find_stap(path, date)

    twl <- data.frame(
      date = date,
      lon = path$lon[round(stap_id)],
      lat = path$lat[round(stap_id)],
      stap_id = stap_id
    )
  }

  twl$sunset <- suntools::sunriset(
    crds = matrix(c(twl$lon, twl$lat), ncol = 2),
    dateTime = twl$date,
    direction = "sunset",
    POSIXct.out = TRUE
  )$time

  twl$sunrise <- suntools::sunriset(
    crds = matrix(c(twl$lon, twl$lat), ncol = 2),
    dateTime = twl$date,
    direction = "sunrise",
    POSIXct.out = TRUE
  )$time

  out <- data.frame(
    twilight = c(twl$sunrise, twl$sunset),
    rise = c(rep(TRUE, length(twl$sunrise)), rep(FALSE, length(twl$sunset))),
    stap_id = c(stap_id, stap_id),
    lon = c(twl$lon, twl$lon),
    lat = c(twl$lat, twl$lat),
    date = c(twl$date, twl$date)
  )
  return(out)
}
