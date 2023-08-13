#' Extract altitude from a `pressurepath`
#'
#' @description
#' This function compute the timeseries of altitude from a pressurepath.
#'
#' As the altitude is estimated both at the arrival and departure stationary period location, we
#' need to find a way to correct for the altitude in between.
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame
#'
#' @return A data.frame with columns:
#' - `date` same as `pressure$date`
#' - `altitude`
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#'
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#'
#' pressurepath <- pressurepath_create(tag, path = path, quiet = TRUE)
#'
#' pressurepath2altitude(pressurepath)
#'
#' @family pressurepath
#' @export
pressurepath2altitude <- function(pressurepath) {
  pp <- pressurepath

  # Interpolate stap_id for flight period so that, a flight between stap_id 2 and 3 will have a
  # `stap_interp` between 2 and 3.
  id_0 <- pp$stap_id == 0
  pp$stap_interp <- pp$stap_id
  pp$stap_interp[id_0] <- stats::approx(which(!id_0),
    pp$stap_id[!id_0], which(id_0),
    rule = 2
  )$y

  # Compute the weight
  pp$w <- ifelse(pp$stap_interp == pp$stap_ref, 1, 1 - abs((pp$stap_interp - pp$stap_ref) * 2))

  # Weight the altitude
  pp$altitude_w <- pp$w * pp$altitude

  # Sum the altitude by date, thus weighting average
  pp_alt <- data.frame(
    date = sapply(split(pp$date, pp$date), stats::median),
    stap_id = sapply(split(pp$stap_id, pp$date), stats::median),
    altitude = sapply(split(pp$altitude_w, pp$date), sum),
    stap_s = sapply(split(pp$stap_ref, pp$date), min),
    stap_t = sapply(split(pp$stap_ref, pp$date), max)
  )

  # Because sum of weight is not exactly 1 all the time, we normalized by the sum
  pp_alt$altitude <- pp_alt$altitude / sapply(split(pp$w, pp$date), sum)

  # nolint start
  if (FALSE) {
    p <- ggplot2::ggplot(pp_alt) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data$date, y = .data$altitude),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pp[pp$w != 1, ],
        ggplot2::aes(x = .data$date, y = .data$altitude),
        color = "red"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

    plotly::ggplotly(p, dynamicTicks = TRUE)
  }
  # nolint end

  return(pp_alt)
}
