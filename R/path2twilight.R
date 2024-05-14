#' Compute astronomical twilight from positions and dates
#'
#' @description
#' This function compute the theorical twilight (i.e., datetime of sunrise and sunset) at given
#' locations and for specific date.
#'
#' We use the [suntools](https://github.com/adokter/suntools) package for this.
#'
#' By default (`solarDep=0`), the computation return sunrise and sunset. But, it is also possible
#' to compute different twilights by setting depression angle value greater than 0 (6° for civil,
#' 12° for nautical and 18° for astronomical).
#'
#' @param path a GeoPressureR `path` or `pressurepath` data.frame
#' @param date a vector of POSIXt datetime for which sunrise and sunset are computed. Be default,
#' uses the range of `path$date` provided.
#' @param solarDep a numerical value representing the solar depression angle used to compute sunrise
#' and sunset.
#'
#' @return a `twilight` data.frame with columns:
#' - `twilight` (date-time of twilight)
#' - `rise` (logical) indicating sunrise (`TRUE`) or sunset (`FALSE`).
#' - `stap_id`.
#'
#' @examples
#' path <- data.frame(
#'   stap_id = c(1, 2, 3, 4, 5),
#'   j = c(1L, 1L, 1L, 1L, 1L),
#'   ind = c(1652L, 1603L, 1505L, 1609L, 1463L),
#'   lat = c(48.9, 47.5, 45.5, 41.5, 37.5),
#'   lon = c(17.05, 16.5, 14.5, 16.5, 13.5),
#'   start = as.POSIXct(c(1501113450, 1501888650, 1501987950, 1502075550, 1502151150), tzone = "UTC"),
#'   end = as.POSIXct(c(1501876050, 1501961250, 1502046750, 1502133150, 1502323050), tzone = "UTC"),
#'   include = c(TRUE, TRUE, TRUE, TRUE, TRUE),
#'   known = c(TRUE, FALSE, FALSE, FALSE, FALSE),
#'   interp = c(FALSE, FALSE, FALSE, FALSE, FALSE)
#' )
#'
#' twl <- path2twilight(path)
#'
#' @family geolight
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html), [suntools
#' ](https://github.com/adokter/suntools)
#' @export
path2twilight <- function(path,
                          date = NULL,
                          solarDep = 0) {
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

  if (solarDep == 0) {
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
  } else {
    twl$sunrise <- suntools::crepuscule(
      crds = matrix(c(twl$lon, twl$lat), ncol = 2),
      dateTime = twl$date,
      solarDep = solarDep,
      direction = "dawn",
      POSIXct.out = TRUE
    )$time

    twl$sunset <- suntools::crepuscule(
      crds = matrix(c(twl$lon, twl$lat), ncol = 2),
      dateTime = twl$date,
      solarDep = solarDep,
      direction = "dusk",
      POSIXct.out = TRUE
    )$time
  }

  return(data.frame(
    twilight = c(twl$sunrise, twl$sunset),
    rise = c(rep(TRUE, length(twl$sunrise)), rep(FALSE, length(twl$sunset))),
    stap_id = c(stap_id, stap_id),
    lon = c(twl$lon, twl$lon),
    lat = c(twl$lat, twl$lat),
    date = c(twl$date, twl$date)
  ))
}
