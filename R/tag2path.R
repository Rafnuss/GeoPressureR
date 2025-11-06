#' Build a `path` from the likelihood maps of a `tag`
#'
#' @description
#' Find the position of the highest value in a map, typically most probable value in a likelihood
#' map.
#'
#' Note that this path is the most likely, considering only the observation model and ignoring the
#' movement model. Prefer to use `graph_most_likely()` for the most realistic path accounting for
#' flight duration.
#'
#' `interp` can be used to interpolate unrealistic position from short stationary periods based on
#' the position of the longer ones. In order to preserve a reference to the grid, the interpolation
#' is only performed at the centre of the grid cell (defined by the `likelihood` map). Note also
#' that it is not possible to interpolate the first and last stationary period.
#'
#' @param tag a GeoPressureR `tag` object
#' @inheritParams tag2map
#' @inheritParams ind2path
#' @param interp the position of the stationary period shorter than `interp` will be replace by a
#' linear average from other position accounting for flight duration (in days).
#'
#' @return A path data.frame
#' - `stap_id` stationary period
#' - `ind` indices of the coordinate in the 2D grid. Useful to retrieve map or graph information.
#' - `lat` Latitude,
#' - `lon` longitude
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 2
#'     ) |>
#'     geopressure_map(quiet = TRUE)
#' })
#'
#' # Extract a path from pressure map
#' path <- tag2path(tag)
#' plot_path(path)
#'
#' # Short stationary periods (e.g. 1 day) can be unreliably
#' # estimated, so interpolating them is often better
#' path <- tag2path(tag, interp = 1)
#' plot_path(path)
#'
#' @family path
#' @export
tag2path <- function(tag, likelihood = NULL, interp = FALSE, use_known = TRUE) {
  # Construct the likelihood map
  map <- tag2map(tag, likelihood = likelihood)

  # find the index in the 2D grid
  ind <- rep(NA, length(map))
  stap_id <- which(!sapply(map$data, is.null))
  ind[stap_id] <- sapply(map[stap_id], which.max)

  # Interpolation for short stationary period is only performed if interp>0

  if (interp) {
    if (!is.numeric(interp)) {
      cli::cli_abort(c(
        x = "{.var interp} needs to be a numeric or {.val {FALSE}}.",
        ">" = "Indicate the maximum duration of stationary periods for which the position is \\
        interpolated"
      ))
    }

    # Find the stap to be interpolated
    path_interp <- stap2duration(tag$stap) <= interp

    # If known, the stap will not be interpolated
    path_interp[!is.na(tag$stap$known_lon) & use_known] <- FALSE

    # Compute the grid information used for known or interp
    g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

    # Compute the latitude and longitude ind
    lat_ind <- arrayInd(ind, g$dim)[, 1]
    lon_ind <- arrayInd(ind, g$dim)[, 2]

    # Enforce first and last stap constant
    fal <- c(1, length(path_interp))
    if (any(path_interp[fal])) {
      path_interp[fal] <- FALSE
      if (any(is.na(lat_ind[fal]))) {
        cli::cli_abort(c(
          x = "First and/or last modelled stationary periods ({.val
          {tag$stap$stap_id[fal[is.na(lat_ind[fal])]]}}) don't have a likelihood map."
        ))
      } else {
        cli::cli_warn(c(
          "!" = "First and/or last modelled stationary periods ({.val {tag$stap$stap_id[fal]}}) \\
         are shorter than {.val {interp}} day{?s} but cannot be interpolated.",
          ">" = "They will not be interpolated."
        ))
      }
    }

    # Compute flight duration for all stap (even the non-included)
    flight <- stap2flight(tag$stap, include_stap_id = tag$stap$stap_id)

    # Cummulate the flight duration to get a proxy of the over distance covered
    total_flight <- cumsum(as.numeric(c(0, flight$duration)))

    # Interpolate the lat and lon indices separately using `total_flight` as a spacing between
    # position
    lon_ind[path_interp] <- round(
      stats::approx(
        total_flight[!path_interp],
        lon_ind[!path_interp],
        total_flight[path_interp]
      )$y
    )
    lat_ind[path_interp] <- round(
      stats::approx(
        total_flight[!path_interp],
        lat_ind[!path_interp],
        total_flight[path_interp]
      )$y
    )

    # Move to the closest non-water position
    # Find the index of lat-lon for all non-water position
    mask_water_ind2 <- which(!tag$map_pressure$mask_water)
    mask_water_ind_lat <- (mask_water_ind2 %% g$dim[1])
    mask_water_ind_lon <- (mask_water_ind2 - mask_water_ind_lat) / g$dim[1] + 1

    for (i in seq_len(length(lat_ind))) {
      if (
        !is.na(lat_ind[i]) &&
          !is.na(lon_ind[i]) &&
          tag$map_pressure$mask_water[lat_ind[i], lon_ind[i]]
      ) {
        closest_ind2 <- which.min(
          (mask_water_ind_lat - lat_ind[i])^2 +
            (mask_water_ind_lon - lon_ind[i])^2
        )
        lat_ind[i] <- mask_water_ind_lat[closest_ind2]
        lon_ind[i] <- mask_water_ind_lon[closest_ind2]
      }
    }

    # Update in 2d
    ind[path_interp] <- (lon_ind[path_interp] - 1) *
      g$dim[1] +
      lat_ind[path_interp]
  } else {
    path_interp <- FALSE
  }

  # Convert the index of the path in a path data.frame
  path <- ind2path(ind, tag, use_known = use_known)

  path$interp <- path_interp

  # Assign the type of path
  attr(path, "type") <- "tag"
  attr(path, "likelihood") <- map$type

  return(path)
}
