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
#' periods as you wish. No likelihood map will be computed for these stationary periods and the
#' trajectory model will be more constrain, thus saving significant computational time.
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
#' site).
#' @return A GeoPressureR `tag` object with:
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
#'   )
#' )
#' str(tag)
#' @export
tag_geostap <- function(tag,
                        extent,
                        scale = 10,
                        known = data.frame(
                          stap_id = integer(),
                          known_lat = double(),
                          known_lon = double()
                        ),
                        include_stap_id = tag$stap$stap_id,
                        include_min_duration = 0) {
  tag_assert(tag, "stap")

  # define stap for convenience
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

  # Define which stationary periods to include
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))
  assertthat::assert_that(is.numeric(include_min_duration))
  include_min_duration_id <- stap$stap_id[stap2duration(stap, units = "hours") > include_min_duration]

  # Include stap which are matching the three include constrains
  stap_include <- rep(FALSE, nrow(stap))
  stap_include[intersect(include_stap_id, include_min_duration_id)] <- TRUE

  # Check if value are already defined and if they are changing
  # Check if geostap has already been run before (all these condition should always be the same)
  if ("extent" %in% names(tag) | "known_lat" %in% names(stap) |
      "scale" %in% names(tag) | "include" %in% names(stap)){

    # Check if value are changing
    chg_known = any(stap$known_lon[known$stap_id] != known$known_lon)
    chg_include = any(stap$include != stap_include)
    chg_extent = any(extent != tag$extent)
    chg_scale = scale != tag$scale

    # Check if known has changed
    if ( chg_known | chg_extent | chg_scale | chg_include ){

      # Only provide option to stop the process if map are already defined
      if (any(c("map_pressure", "map_light") %in% names(tag))){
        cli::cli_inform(c(
          "!" = "{.fun geostap} has already been run on this {.var tag} object, the input \\
          parameters ({.var scale}, {.var extent}, {.var tag$known} or {.var tag$include}) are \\
          different and the likelihood map ({.var map_pressure} and/or {.var map_light}) \\
          have already been computed."
        ))
        res <- utils::askYesNo(
          "Do you want to overwrite the parameters and delete the likelihood maps?")
        if (res){
          # If yes, remove existing likelihood map and carry on the overwrite of parameter
          tag$map_pressure <- NULL
          tag$map_light <- NULL
          tag$mask_water <- NULL
          tag$param <- NULL
          cli::cli_warn(c(
            "!" = "The old parameters have been overwitten with the new ones and the likelihood \\
            map have been deleted.",
            ">" = "Run {.fun geopressure_map} and/or {.fun geolight_map} again to create the new \\
            likelihood maps"
          ))
        } else {
          cli::cli_warn(c(
            "x" = "No modification were made.",
            ">" = "This function return the original (unmodified) {.var tag}."
          ))
          # If no, stop and return the existing tag
          return(tag)
        }
      } else {
        cli::cli_warn(c(
          "!" = "{.fun geostap} has already been run on this {.var tag} object and the input \\
          parameters are different.",
          ">" = "The old parameters ({.var scale}, {.var extent}, {.var tag$known} or \\
          {.var tag$include}) will be overwitten with the new ones."
        ))
      }
    }
  }

  # Add known to stap
  # remove first known_lat and lon if they exist to be able to merge the table without duplicate
  stap <- stap[ , !(names(stap) %in% c("known_lat", "known_lon"))]
  stap <- merge(stap, known, by = "stap_id", all.x = TRUE)

  # Add the vector of stap to include
  stap$include <- stap_include

  # Add parameters to stap
  tag$stap <- stap
  tag$scale <- scale
  tag$extent <- extent

  return(tag)
}
