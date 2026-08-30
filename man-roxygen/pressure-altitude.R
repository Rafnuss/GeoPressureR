#' @section Pressure-derived altitude:
#' When altitude is produced, GeoPressureR uses the same barometric relation as
#' [GeoPressureAPI](https://github.com/GeoPressure/GeoPressureAPI):
#' \deqn{z_{tag}=z_{ERA5}+\frac{T_{ERA5}}{L_b}\left[\left(
#' \frac{P_{tag}}{P_{ERA5}}\right)^{-\frac{R L_b}{g M}}-1\right].}
#' Here, \eqn{P_{tag}} is tag pressure, while \eqn{P_{ERA5}}, \eqn{T_{ERA5}}, and \eqn{z_{ERA5}}
#' are ERA5 surface pressure, 2 m temperature, and model-surface elevation. Model-surface elevation
#' is obtained from ERA5 surface geopotential divided by standard gravity. The constants are the
#' standard temperature lapse rate \eqn{L_b=-0.0065} K/m, universal gas constant
#' \eqn{R=8.31432} J/(mol K), standard gravity \eqn{g=9.80665} m/s^2, and molar mass of dry air
#' \eqn{M=0.0289644} kg/mol. Pressure is converted to Pa internally and altitude is returned in
#' metres above mean sea level.
