pressure_to_altitude <- function(pressure_tag, surface_pressure, temperature, elevation) {
  elevation +
    temperature /
      -0.0065 *
      ((pressure_tag / surface_pressure)^(-8.31432 * -0.0065 / 9.80665 / 0.0289644) - 1)
}
