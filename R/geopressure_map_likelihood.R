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
#' For more background and details on the algorithm, please read Nussbaumer et al. ([2023a
#' ]( https://doi.org/10.1111/2041-210X.14043)) and the [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html).
#' @inheritParams geopressure_map
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' Global Positioning with Animal‐borne Pressure Sensors. *Methods in Ecology and Evolution*, 14,
#' 1118–1129 <https://doi.org/10.1111/2041-210X.14043>.}
#' @family geopressure_map
#' @seealso [GeoPressureManual](https://bit.ly/3saRNQu)
#' @export
geopressure_map_likelihood <- function(tag,
                                       sd = 1,
                                       thr_mask = 0.9,
                                       log_linear_pooling_weight = \(n) log(n) / n) {
  # Check tag status
  tag_assert(tag, "map_pressure_mismatch")

  # Check sd
  assertthat::assert_that(is.numeric(sd))
  if (length(sd) == 1) {
    sd <- rep(sd, times = nrow(tag$stap))
  } else if (length(sd) != nrow(tag$stap)) {
    cli::cli_abort(c(
      "x" = "{.var sd} is of length {.val {length(sd)}}.",
      ">" = "{.var sd} needs to be of length {.val {1}} or {.val {nrow(tag$stap)}} \\
      ({.code nrow(tag$stap)})."
    ))
  }
  assertthat::assert_that(all(sd >= 0))
  if (any(sd < 0.3) || any(sd > 5)) {
    cli::cli_warn(c(
      "!" = "{.var sd} has values {.val {unique(sd)}}.",
      "i" = "It is generally not recommended to have a standard deviation between {.val {0.3}} \\
      and {.val {5}}.\f"
    ))
  }
  assertthat::assert_that(is.numeric(thr_mask))
  assertthat::assert_that(thr_mask >= 0 & thr_mask <= 1)
  assertthat::assert_that(is.function(log_linear_pooling_weight))

  map_pressure <- vector("list", nrow(tag$stap))

  for (istap in which(!sapply(tag$map_pressure_mse$data, is.null))) {
    # Number of sample
    n <- tag$stap$nb_sample[istap]

    # Log-linear pooling weight
    w <- log_linear_pooling_weight(n)

    # get MSE layer
    mse <- tag$map_pressure_mse$data[[istap]]
    # change 0 (water) in NA
    mse[mse == 0] <- NA

    # compute likelihood assume gaussian error distribution
    likelihood <- (1 / (2 * pi * sd[istap]^2))^(n * w / 2) * exp(-w * n / 2 / (sd[istap]^2) * mse)

    # mask value of threshold
    map_pressure[[istap]] <- likelihood * (tag$map_pressure_mask$data[[istap]] >= thr_mask)
  }

  # Find water mask
  # Define the mask of water
  tag$mask_water <- is.na(map_pressure[[which(!sapply(tag$map_pressure_mse$data, is.null))[1]]])

  # Add known location
  # compute latitude, longitude and dimension
  g <- map_expand(tag$param$extent, tag$param$scale)
  # Add known location only if map_pressure_mse is null
  # (ie., if .known_compute = TRUE in geopressure_map_mse)
  for (stap_id in which(!is.na(tag$stap$known_lat) & sapply(tag$map_pressure_mse$data, is.null))) {
    # Initiate an empty map
    map_pressure[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
    map_pressure[[stap_id]][tag$mask_water] <- NA
    # Compute the index of the known position
    known_lon_id <- which.min(abs(tag$stap$known_lon[stap_id] - g$lon))
    known_lat_id <- which.min(abs(tag$stap$known_lat[stap_id] - g$lat))
    # Assign a likelihood of 1 for that position
    map_pressure[[stap_id]][known_lat_id, known_lon_id] <- 1
  }

  # Create map object
  tag$map_pressure <- map_create(
    data = map_pressure,
    extent = tag$param$extent,
    scale = tag$param$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "pressure"
  )

  tag$param$sd <- sd
  tag$param$thr_mask <- thr_mask
  tag$param$log_linear_pooling_weight <- log_linear_pooling_weight
  attr(tag$param$log_linear_pooling_weight, "srcref") <- NULL

  return(tag)
}
