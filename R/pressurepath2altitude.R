#' Altitude
#'
#' @description
#' This function compute the timeseries of altitude from a pressurepath.
#'
#' As the altitude is estimated both at the arrival and departure stationary period location, we
#' need to find a way to correct for the altitude in between.
#'
#' @param pressurepath A `pressurepath` data.frame
#'
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
  pp_m <- stats::aggregate(list(altitude = pp$altitude_w), list(date = pp$date), \(x) sum(x))

  # Because sum of weight is not exactly 1 all the time, we normalized by the sum
  sum_w <- stats::aggregate(list(sum_w = pp$w), list(date = pp$date), \(x) sum(x))
  pp_m$altitude <- pp_m$altitude / sum_w$sum_w

  if (FALSE) {
    p <- ggplot2::ggplot(pp_m) +
      ggplot2::geom_line(
        ggplot2::aes_string(x = "date", y = "altitude"),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pp[pp$w != 1, ],
        ggplot2::aes_string(x = "date", y = "altitude"),
        color = "red"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

    plotly::ggplotly(p, dynamicTicks = TRUE)
  }

  return(pp_m)
}
