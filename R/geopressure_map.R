#' Compute likelihood map from pressure data
#'
#' @description
#' This function computes a likelihood map for each stationary period based on pressure measurements.
#' It performs the following operations:
#'
#' 1. Compute the mismatch map between the pressure sensor measurements and the ERA5 reanalysis
#' database. See [`geopressure_map_mismatch()`] for details.
#' 2. Convert the mismatch map into a likelihood map with [`geopressure_map_likelihood()`].
#'
#' A map will only be computed for the stationary periods included in `tag$stap$include` and without a
#' known position `tag$stap$known_l**`. If the position is known, the function will
#' create a likelihood map with a single 1 value at the grid cell closest to the known position.
#'
#' For more background and details on the method behind these functions, please refer to the [associated scientific
#' publication]( https://doi.org/10.1111/2041-210X.14043).
#'
#' @param tag List of the geographical and stationary period information. See
#' [`tag_create()`] for details.
#' @param max_sample The computation of the maps is only performed on `max_sample` datapoints of
#' pressure to reduce computational time. The samples are randomly (uniformly) selected on the
#' timeseries.
#' @param margin The margin is used in the mask map to accept measurement errors, small-scale
#' topography, and vertical movements of the bird (unit in meters, 1hPa~10m).
#' @param sd Standard deviation of the pressure error . numeric of lenght 1 or number of stationary periods.
#' @param thr_mask Threshold of the percentage of data points outside the elevation range to be considered
#' not possible.
#' @param log_linear_pooling_weight Weighting function of the log-linear pooling, taking the number of samples of the
#' stationary periods used and returning the weight of the aggregation. See
#' [GeoPressureManual | Probability aggregation
#' ](https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html) for more details.
#' @param keep_mse_mask Logical defining if the matrix of the MSE and mask are returned in the list.
#' @param timeout Duration before the code is interrupted both for the request on
#' GeoPressureAPI and GEE (in seconds, see [`httr::timeout()`]).
#' @param workers Number of parallel requests on GEE. Integer between 1 and 99.
#' @param compute_known Logical defining if the map(s) for known stationary period should be
#' estimated based on twilight or hard defined by the known location `stap$known_l**`
#' @return Same list as parameter `tag` but with a `likelihood` field containing a list of maps
#' for each stationary period stored as matrix. Note that stationary periods not marked as model in
#' `tag$stap$include` will be included with a `NULL`value in `likelihood`.
#' `tag$param` is a list of all the parameters used to compute the likelihood map.
#' If `keep_mse_mask` is
#' true, the `mse` and `mask` maps computed with `geopressure_map_mismatch` will also be kept, as
#' well as `tag$stap$nb_sample`, indicating the number of datapoints used to compute the MSE.
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023.
#' “Global Positioning with Animal‐borne Pressure Sensors.” *Methods in Ecology and Evolution*.
#'  <https://doi.org/10.1111/2041-210X.14043>.}
#' @family geopressure_map
#' @examples
#' temp_dir <- getwd()
#' print(temp_dir)
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = T) |>
#' tag_label(quiet = T) |>
#' tag_geostap(
#'   extent = c(-16, 23, 0, 50),
#'   scale = 4,
#'   include_stap_id = 1
#' )
#'
#' tag <- geopressure_map(tag,
#'   max_sample = 50,
#'   sd = 0.7,
#'   keep_mse_mask = TRUE
#' )
#'
#' plot(tag, type = "map_pressure_mse", plot_leaflet = F)
#' plot(tag, type = "map_pressure_mask", plot_leaflet = F)
#' @export
geopressure_map <- function(tag,
                            max_sample = 250,
                            margin = 30,
                            timeout = 60 * 5,
                            workers = "auto",
                            sd = 1,
                            thr_mask = 0.9,
                            log_linear_pooling_weight = \(n) log(n) / n,
                            keep_mse_mask = FALSE,
                            compute_known = FALSE) {
  # Compute mean square error maps
  tag <- geopressure_map_mismatch(tag,
    max_sample = max_sample,
    margin = margin,
    timeout = timeout,
    workers = workers,
    compute_known = compute_known
  )

  # Compute likelihood maps from the MSE maps
  tag <- geopressure_map_likelihood(tag,
    sd = sd,
    thr_mask = thr_mask,
    log_linear_pooling_weight = log_linear_pooling_weight
  )

  # remove intermediate maps computed by geopressure_map_mismatch()
  if (!keep_mse_mask) {
    tag[names(tag) %in% c("map_pressure_mse", "map_pressure_mask")] <- NULL
    tag$stap <- tag$stap[names(tag$stap) != "nb_sample"]
  }

  return(tag)
}
