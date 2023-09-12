#' @family geopressure_map
#' @rdname geopressure_map
#' @export
geopressure_map_likelihood <- function(tag,
                                       sd = 1,
                                       log_linear_pooling_weight = \(n) log(n) / n,
                                       keep_mse = TRUE) {
  # Check tag status
  tag_assert(tag, "map_pressure_mismatch")

  # Check sd
  assertthat::assert_that(is.numeric(sd))
  sd0 <- sd
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
  assertthat::assert_that(is.function(log_linear_pooling_weight))

  map_pressure <- vector("list", nrow(tag$stap))

  for (istap in which(!sapply(tag$map_pressure_mse$data, is.null))) {
    # Number of sample
    n <- tag$stap$nb_sample[istap]

    # Log-linear pooling weight
    w <- log_linear_pooling_weight(n)

    # get MSE layer
    mse <-
      # compute likelihood assume gaussian error distribution
      likelihood <- (1 / (2 * pi * sd[istap]^2))^(n * w / 2) *
        exp(-w * n / 2 / (sd[istap]^2) * tag$map_pressure_mse$data[[istap]])

    # change water in NA
    likelihood[is.na(likelihood)] <- 0
    likelihood[tag$map_pressure_mse$mask_water] <- NA

    # mask value of threshold
    map_pressure[[istap]] <- likelihood
  }

  # Find water mask
  # Define the mask of water
  # tag$mask_water <- is.na(map_pressure[[which(!sapply(tag$map_pressure_mse$data, is.null))[1]]])

  # Add known location
  # compute latitude, longitude and dimension
  g <- map_expand(tag$param$extent, tag$param$scale)
  # Add known location only if map_pressure_mse is null
  # (ie., if .known_compute = TRUE in geopressure_map_mse)
  for (stap_id in which(!is.na(tag$stap$known_lat) & sapply(tag$map_pressure_mse$data, is.null))) {
    # Initiate an empty map
    map_pressure[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
    map_pressure[[stap_id]][tag$map_pressure_mse$mask_water] <- NA
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

  tag$param$sd <- sd0
  tag$param$log_linear_pooling_weight <- log_linear_pooling_weight
  attr(tag$param$log_linear_pooling_weight, "srcref") <- NULL

  # remove mse maps computed by geopressure_map_mismatch()
  if (!keep_mse) {
    tag[names(tag) %in% c("map_pressure_mse", "map_pressure_mask")] <- NULL
    tag$stap <- tag$stap[names(tag$stap) != "nb_sample"]
  }

  return(tag)
}
