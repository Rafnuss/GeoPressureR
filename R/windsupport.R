#' Compute wind support and drift
#'
#' @description
#' Compute the wind support (or wind profit) from groundspeed and windspeed.
#'
#' All speed vectors are expressed as complex value where the real part corresponds to the east-west
#' component and imaginary part to the north-south component. You can use `abs()` (or `Mod()`) to
#' compute the norm (or absolute value) of the speed vector and `speed_to_bearing()` to compute the
#' bearing/orientation of the speed vector. The latter simply compute the trigonometric angle of the
#' speed vector with `Arg()` and convert this angle to a bearing (North = 0° instead of 90°).
#'
#' Alternatively, if `drift = TRUE`, the function return the drift instead of wind support.
#'
#' @param windspeed windspeed as complex value
#' @param groundspeed groundspeed as complex value
#' @param drift return drift instead of windsupport
#'
#' @return wind support vector as complex value (same unit as `gs` and `ws`)
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
