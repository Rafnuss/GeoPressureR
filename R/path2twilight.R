#' Compute astronomical twilight from positions and dates
#'
#' @description
#' This function compute the theoretical twilight (i.e., datetime of sunrise and sunset) at given
#' locations and for specific date.
#'
#' We use the [suntools](https://github.com/adokter/suntools) package for this.
#'
#' By default (`solar_dep=0`), the computation return sunrise and sunset. But, it is also possible
#' to compute different twilights by setting depression angle value greater than 0 (6° for civil,
#' 12° for nautical and 18° for astronomical).
#'
#' @param path a GeoPressureR `path` or `pressurepath` data.frame
#' @param date a vector of POSIXt datetime for which sunrise and sunset are computed. Be default,
#' uses the range of `path$date` provided.
#' @param solar_dep a numerical value representing the solar depression angle used to compute
#' sunrise and sunset.
#' @param return_long logical defining the format of the data.frame returned. If `TRUE`, returns the
#' long format identical to `twilight_create()`. If `FALSE`, return the sunrise and sunset as
#' different column, making the data.frame the same size as `date`.
#'
#' @return if `return_long == TRUE`, a `twilight` data.frame (same as `twilight_create()`) with
#' columns:
#' - `date` same as input `date`
#' - `twilight` date-time of twilight
#' - `rise` logical indicating sunrise (`TRUE`) or sunset (`FALSE`).
#' - `stap_id` same as `path$stap_id`
#' - `lat` same as `path$lat`
#' - `lon` same as `path$lon`
#'
#' if `return_long == FALSE`, a data.frame with the same size of `date` with columns:
#' - `date` same as input `date`
#' - `sunrise` date-time of sunrise
#' - `sunset` date-time of sunset
#' - `stap_id` same as `path$stap_id`
#' - `lat` same as `path$lat`
#' - `lon` same as `path$lon`
#'
#' @examples
#' path <- data.frame(
#'   stap_id = c(1, 2, 3, 4, 5),
#'   j = c(1L, 1L, 1L, 1L, 1L),
#'   ind = c(1652L, 1603L, 1505L, 1609L, 1463L),
#'   lat = c(48.9, 47.5, 45.5, 41.5, 37.5),
#'   lon = c(17.05, 16.5, 14.5, 16.5, 13.5),
#'   start = as.POSIXct(
#'     c(1501113450, 1501888650, 1501987950, 1502075550, 1502151150),
#'     tzone = "UTC"
#'   ),
#'   end = as.POSIXct(c(1501876050, 1501961250, 1502046750, 1502133150, 1502323050), tzone = "UTC"),
#'   include = c(TRUE, TRUE, TRUE, TRUE, TRUE),
#'   known = c(TRUE, FALSE, FALSE, FALSE, FALSE),
#'   interp = c(FALSE, FALSE, FALSE, FALSE, FALSE)
#' )
#'
#' twl <- path2twilight(path)
#'
#' @family path, pressurepath
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html), [suntools
#' ](https://github.com/adokter/suntools)
#' @export
path2twilight <- function(path,
                          date = NULL,
                          solar_dep = 0,
                          return_long = TRUE) {
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
      lat = path$lat,
      stap_id = path$stap_id
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

      # Filtering dates for only those in path
      date <- date[sapply(date, function(d) {
        any(d >= path$start & d <= path$end)
      })]
    }

    stap_id <- find_stap(path, date)

    twl <- data.frame(
      date = date,
      lon = path$lon[round(stap_id)],
      lat = path$lat[round(stap_id)],
      stap_id = stap_id
    )
  }

  if (is.null(attr(twl$date, "tzone")) || attr(twl$date, "tzone") != "UTC") {
    cli::cli_warn("The {.val date} is not in UTC which might lead to undesired effect")
  }

  if (solar_dep == 0) {
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
      solarDep = solar_dep,
      direction = "dawn",
      POSIXct.out = TRUE
    )$time

    twl$sunset <- suntools::crepuscule(
      crds = matrix(c(twl$lon, twl$lat), ncol = 2),
      dateTime = twl$date,
      solarDep = solar_dep,
      direction = "dusk",
      POSIXct.out = TRUE
    )$time
  }

  if (return_long) {
    out <- data.frame(
      date = c(twl$date, twl$date),
      twilight = c(twl$sunrise, twl$sunset),
      rise = c(rep(TRUE, length(twl$sunrise)), rep(FALSE, length(twl$sunset))),
      stap_id = c(twl$stap_id, twl$stap_id),
      lon = c(twl$lon, twl$lon),
      lat = c(twl$lat, twl$lat)
    )

    # Sort by twilight date/time
    out <- out[order(out$twilight), ]

    return(out)
  } else {
    return(twl)
  }
}
