#' Define grid for the trajectory
#'
#' @description
#' This function adds the parameters defining the 3D grid of the maps. The spatial parameters
#' (`extent` and `scale`) defines the GEOgraphical dimension. The temporal dimension is
#' defined based on the stationary periods build from the label. `include_stap_id` and
#' `include_min_duration` can be used to limit which stationary periods are computed and model in
#' the rest of the analysis. By default, all stationary periods are included.
#'
#' In addition, `tag` also includes the ability to define `known` locations (e.g., equipment or
#' retrieval site). These can only be defined at the level of a stationary period (i.e., assuming
#' constant position during the whole stationary period) but you can define as many known stationary
#' periods as you wish. No likelihood map will be computed for these stationary periods, thus saving
#' computational time.
#'
#' @param tag Data logger list with label information. See [`tag_label()`] for the required input.
#' @param extent Geographical extent of the map on which the likelihood and graph model will be
#' computed. Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param scale Number of pixels per 1° latitude-longitude. For instance, `scale = 10` for a
#' resolution of 0.1° (~10km) and `scale=4` for a resolution of 0.25° (~30km). To avoid
#' interpolating the ERA5 data, the scale should be smaller than 10. Read more about scale on the [Google
#' earth Engine documentation](https://developers.google.com/earth-engine/guides/scale).
#' @param include_stap_id Vector of `stap_id` defining which stationary period to model, that is,
#' to compute in the likelihood map and use in the graph.
#' @param include_min_duration Numeric defining the minimum threshold of stationary periods duration
#' (in hours) to includes.
#' @param known Data.frame containing the known positions of the bird (e.g., equipment or retrieval
#' site). This information can only be attached at the level of a stationary period.
#' @return A `tag` object with:
#' - `stap`: Data.frame of all stationary periods with three new columns: `known_lat` and
#' `known_lon` define the known position during these stationary periods, and `model` defines
#' whether the likelihood map of this stationary period should be computed and later used in the
#' graph.
#' - `extent` same as input parameter `extent`
#' - `scale` same as input parameter `scale`
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_create("18LX") |>
#'   tag_label()
#'
#' # Default tag
#' tag <- tag_geostap(tag, c(-16, 23, 0, 50))
#' str(tag)
#'
#' # Customized tag, with coarse grid scale, known position for the first stationary period and
#' # considering only the stationary periods lasting more than 20hours.
#' tag <- tag_geostap(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 1,
#'   include_min_duration = 20,
#'   known = data.frame(
#'     stap_id = 1,
#'     known_lon = 17.05,
#'     known_lat = 48.9
#'   ))
#' )
#' str(tag)
#' @export
tag_geostap <- function(tag,
                           extent,
                           scale = 10,
                           include_stap_id = NA,
                           include_min_duration = NA,
                        known = data.frame(
                          stap_id = integer(),
                          known_lat = double(),
                          known_lon = double()
                        ),) {
  assertthat::assert_that(inherits(tag,"tag"))
  assertthat::assert_that(assertthat::has_name(tag, "id"))
  if (!("stap" %in% names(tag))) {
    cli::cli_abort(c(
      x = "{.var tag} does not contains {.var stap}",
      i = "Make sure to run {.fn tag_label} or {.fn tag_label_stap} before using {.fn tag_create}"
    ))
  }
  stap <- tag$stap
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(all(stap$stap_id == seq_len(nrow(stap))))

  # Check extent and scale
  geo_expand(extent, scale)

  # Check known
  assertthat::assert_that(is.data.frame(known))
  assertthat::assert_that(assertthat::has_name(known, "stap_id"))
  assertthat::assert_that(assertthat::has_name(known, "known_lat"))
  assertthat::assert_that(assertthat::has_name(known, "known_lon"))
  if (!all(known$known_lon >= extent[1] & known$known_lon <= extent[2] &
    known$known_lat >= extent[3] & known$known_lat <= extent[4])) {
    cli::cli_abort(c(
      x = "The known latitude and longitude are not inside the extent of the map",
      i = "Modify {.var extent} or {.var known} to match this requirement."
    ))
  }
  assertthat::assert_that(all(known$stap_id %in% stap$stap_id))
  assertthat::assert_that(all(unique(known$stap_id) == known$stap_id))
  # Add known to stap
  stap <- merge(stap, known, by = "stap_id", all.x = TRUE)

  # Define which stationary periods to include
  stap$include <- FALSE

  if (is.na(include_stap_id)){
    include_stap_id <- tag$stap$stap_id
  }
  assertthat::assert_that(all(include_stap_id %in% tag$stap$stap_id))

  if (is.na(include_min_duration)){
    include_min_duration <- 0
  }
  assertthat::assert_that(is.numeric(include_min_duration))
  include_min_duration_id <- which(difftime(tag$stap$end, tag$stap$start, units = "hours")
                               > include_min_duration)

  # Include stap which are matching both include constrains
  stap$include[include_stap_id & include_min_duration_id] <- TRUE

  # Display warning
  if (any(!stap$include)) {
    cli::cli_warn(c(
      "!" = "The {.var tag} is setup to model {.val {sum(stap$include)}} out of \\
      {.val {nrow(stap)}} stationary periods: {.var {stap$stap_id[stap$include]}}."
    ))
  }

  # Add parameters
  tag$stap <- stap
  tag$scale <- scale
  tag$extent <- extent

  return(tag)
}
