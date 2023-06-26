#' Convert mismatch map into likelihood map
#'
#' This function converts the mismatch maps (MSE and mask) into a likelihood map.
#'
#' We convert the map of the mean square error \eqn{MSE} and altitude mask \eqn{z_{mask}} computed
#' by [`geopressure_map_mismatch()`] into a likelihood map with,
#'
#' \deqn{L = \exp \left(-w \frac{MSE}{\sigma} \right) \left[z_{mask}>T \right],}
#'
#' where \eqn{\sigma} is the standard deviation of pressure and \eqn{T} is the mask threshold.
#'
#' Because the auto-correlation of the timeseries is not accounted for in this equation, we use a
#' log-linear pooling weight of \eqn{w=\log(n)/n} by default, where \eqn{n} is the number of samples
#' in the timeserie (i.e., data points used to compute the MSE).
#'
#' For more background and details on the algorithm, please read the [associated scientific publication
#' ]( https://doi.org/10.1111/2041-210X.14043) and the [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html).
#' @inheritParams geopressure_map
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' “Global Positioning with Animal‐borne Pressure Sensors.” *Methods in Ecology and Evolution*.
#'  <https://doi.org/10.1111/2041-210X.14043>.}
#' @family geopressure_map
#' @export
geopressure_map_likelihood <- function(tag,
                                       sd = 1,
                                       thr_mask = 0.9,
                                       log_linear_pooling_weight = \(n) log(n) / n) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "stap"))
  assertthat::assert_that(assertthat::has_name(tag, "scale"))
  assertthat::assert_that(assertthat::has_name(tag, "extent"))
  assertthat::assert_that(assertthat::has_name(tag, "mse"))
  assertthat::assert_that(assertthat::has_name(tag, "mask"))
  assertthat::assert_that(is.data.frame(tag$stap))
  assertthat::assert_that(assertthat::has_name(tag$stap, "stap_id"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "known_lat"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "known_lon"))
  assertthat::assert_that(assertthat::has_name(tag$stap, "nb_sample"))

  assertthat::assert_that(is.numeric(sd))
  assertthat::assert_that(sd >= 0)
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.function(log_linear_pooling_weight))

  tag$map_pressure <- vector("list", nrow(tag$stap))

  for (istap in which(!sapply(tag$mse, is.null))) {
    # Number of sample
    n <- tag$stap$nb_sample[istap]

    # Log-linear pooling weight
    w <- log_linear_pooling_weight(n)

    # get MSE layer
    mse <- tag$mse[[istap]]
    # change 0 (water) in NA
    mse[mse == 0] <- NA

    # compute likelihood assume gaussian error distribution
    likelihood <- (1 / (2 * pi * sd^2))^(n * w / 2) * exp(-w * n / 2 / (sd^2) * mse)

    # mask value of threshold
    tag$map_pressure[[istap]] <- likelihood * (tag$mask[[istap]] >= thr_mask)
  }

  # Find water mask
  # Define the mask of water
  tag$mask_water <- is.na(tag$map_pressure[[which(!sapply(tag$mse, is.null))[1]]])

  # Add known location
  # compute latitude, longitude and dimension
  g <- geo_expand(tag$extent, tag$scale)
  for (stap_id in which(!is.na(tag$stap$known_lat))) {
    # Initiate an empty map
    tag$map_pressure[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
    tag$map_pressure[[stap_id]][tag$mask_water] <- NA
    # Compute the index of the known position
    known_lon_id <- which.min(abs(tag$stap$known_lon[stap_id] - g$lon))
    known_lat_id <- which.min(abs(tag$stap$known_lat[stap_id] - g$lat))
    # Assign a likelihood of 1 for that position
    tag$map_pressure[[stap_id]][known_lat_id, known_lon_id] <- 1
  }

  tag$param$sd <- sd
  tag$param$thr_mask <- thr_mask
  tag$param$log_linear_pooling_weight <- log_linear_pooling_weight
  attr(tag$param$log_linear_pooling_weight, "srcref") <- NULL

  return(tag)
}
