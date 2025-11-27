#' Compute wind support and drift from a wind and ground speed vectors
#'
#' @description
#' Wind support, or wind profit, is the projection of the wind vector \eqn{\vec{v}_w} onto the
#' groundspeed vector \eqn{\vec{v}_g},
#' \deqn{\text{Wind Support} = \frac{\vec{v}_g \cdot \vec{v}_w}{|\vec{v}_g|}},
#' with \eqn{\cdot} being the [cross product](https://en.wikipedia.org/wiki/Dot_product).
#'
#' A positive value indicates that wind was blowing in the direction of movement, while a negative
#' one indicates a head wind.
#'
#' `windspeed` and `groundspeed` should be expressed as complex value where the real part
#' corresponds to the east-west component and imaginary part to the north-south component.
#'
#' Alternatively to wind support, you can compute the drift (or crosswind component) value with
#' `drift = TRUE`,
#' \deqn{\text{Drift} = \frac{|\vec{v}_g \times \vec{v}_w|}{|\vec{v}_g|}},
#' with \eqn{\times} being the [cross product](https://en.wikipedia.org/wiki/Cross_product).
#'
#' A positive value of drift indicates that the wind is pushing the bird to the right of its
#' intended path, while a negative indicates that the wind is pushing the bird to the left.
#'
#' You can use `abs()` (or `Mod()`) to compute the norm (or absolute value) of the speed vector and
#' `speed_to_bearing()` to compute the bearing/orientation of the speed vector. The latter
#' computes the trigonometric angle of the speed vector with `Arg()` and convert this angle to a
#' bearing (North = 0° instead of 90°).
#'
#' @param windspeed windspeed as complex value
#' @param groundspeed groundspeed as complex value
#' @param drift return drift instead of windsupport
#'
#' @return wind support as scalar value (same unit as `gs` and `ws`)
#'
#' @export
windsupport <- function(windspeed, groundspeed, drift = FALSE) {
  # Rename to shorter name
  ws <- windspeed
  gs <- groundspeed

  w_support <- (Re(ws) * Re(gs) + Im(ws) * Im(gs)) / abs(gs)

  # Alternate 1
  # Re(ws * Conj(gs)) / abs(gs)

  # Alternate 2
  # w_support = (gs* Re(ws) - 1i * ws * Im(gs)) / abs(gs)

  # Alternate 3
  # aas = abs(gs-ws)
  # aws = abs(ws)
  # ags = abs(gs)
  # gam = acos((aws^2+ags^2-aas^2)/(2.*aws*ags))
  # aw_support = aws*cos(gam)
  # aw_drift = (1i*2-1)*aws*sin(gam)

  if (drift) {
    w_drift <- (Re(ws) * Re(gs) + Im(ws) * Im(gs)) / abs(ws)
    return(w_drift)
  }

  return(w_support)
}
