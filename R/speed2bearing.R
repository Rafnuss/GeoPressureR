#' Compute the bearing of a speed
#'
#' @description
#' This function convert a speed vector represented in complex number into a bearing angle (0°=N ;
#' 90°=E...). This conversion is needed because trigonomtric angle are different than bearing.
#'
#' @param speed speed as complex value
#' @param speed_ref reference vector of the angle. Default is the North in order to return bearing.
#' Normal trigonometriy should be `speed_ref = 1`
#' @param positive logical to ensure the bearing is positive between 0-360 degree.
#'
#' @return bearing angle in degree
#'
#' @export
speed2bearing <- function(speed, speed_ref = 0 + 1i, positive = TRUE) {
  # Compute the angle of the complex notation
  angle_rad <- Arg(speed_ref * Conj(speed))
  # Convert from radian to degree
  angle_deg <- angle_rad * 180 / pi
  # Ensure the result is positive
  if (positive) {
    angle_deg[angle_deg < 0] <- angle_deg[angle_deg < 0] + 360
  }
  return(angle_deg)
}
