#' Configure the `map` of a `tag` object
#'
#' @description
#' This function adds to `tag` the parameters defining the 3D grid of the map. The spatial
#' parameters (`extent` and `scale`) define the **geographical dimensions of the map**, and the
#' **temporal dimension** is defined based on the stationary periods built using the labels.
#' `include_stap_id` and `include_min_duration` can be used to limit which stationary periods are
#' computed and modelled in the analysis. By default, all stationary periods are included.
#'
#' In addition, `tag` offers the possibility to define `known` locations (e.g., equipment or
#' retrieval site). These can only be defined at the level of a stationary period (i.e., assuming
#' constant position during the whole stationary period) but you can define as many known stationary
#' periods as you wish. Because the index of the last stationary period is generally unknown, you
#' can use negative indexing in `known`, i.e., `known$stap_id = -1` will be converted to
#' `nrow(tag$stap)`.
#'
#' By default, no likelihood map will be computed for these stationary periods and the trajectory
#' model will be more constrained, saving significant computational time. You can change this
#' using the `compute_known` parameter in `geopressure_map()`.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param extent geographical extent of the map on which the likelihood and graph model will be
#' computed. Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param scale number of pixels per 1° latitude-longitude. For instance, `scale = 10` for a
#' resolution of 0.1° (~10km) and `scale=4` for a resolution of 0.25° (~30km). To avoid
#' interpolating the ERA5 data, the scale should be equal to or smaller than 10. Read more about
#' scale on the [Google earth Engine documentation
#' ](https://developers.google.com/earth-engine/guides/scale).
#' @param include_stap_id vector of `stap_id` defining which stationary period to model, that is,
#' to compute in the likelihood map and use in the graph.
#' @param include_min_duration minimum duration threshold of stationary periods to include (in
#' hours).
#' @param known data.frame containing the known positions of the bird (e.g., equipment or retrieval
#' site) with columns `stap_id`, `known_lat` and `known_lon`. You can set position of the last
#' stationary period using `stap_id = -1`. Also accept list which are converted as data.frame.
#' @return A GeoPressureR `tag` object with:
#' - `stap`: Data.frame of all stationary periods with three new columns: `known_lat` and
#' `known_lon` define the known position during these stationary periods, and `include` defines
#' whether the likelihood map of this stationary period should be computed and later used in the
#' graph.
#' - `extent` same as input parameter `extent`
#' - `scale` same as input parameter `scale`
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' # Default tag
#' tag_default <- tag_set_map(tag, c(-16, 23, 0, 50))
#'
#' print(tag_default)
#'
#' # Customized tag, with coarse grid scale, known position for the first stationary
#' #  period and considering only the stationary periods lasting more than 20hours.
#' tag_custom <- tag_set_map(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 1,
#'   include_min_duration = 20,
#'   known = data.frame(
#'     stap_id = 1,
#'     known_lon = 17.05,
#'     known_lat = 48.9
#'   )
#' )
#'
#' print(tag_custom)
#'
#' @family tag
#' @seealso [GeoPressureManual](https://bit.ly/3QGkf6N)
#' @export
tag_set_map <- function(tag,
                        extent,
                        scale = 10,
                        known = data.frame(
                          stap_id = integer(),
                          known_lat = double(),
                          known_lon = double()
                        ),
                        include_stap_id = NULL,
                        include_min_duration = 0) {
  tag_assert(tag, "stap")

  # define stap for convenience
  stap <- tag$stap
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(all(stap$stap_id == seq_len(nrow(stap))))

  # Check extent and scale
  if (is.list(extent)) {
    extent <- unlist(extent)
  }
  map_expand(extent, scale)

  # Check known
  if (is.list(known) && !is.data.frame(known)) {
    known <- as.data.frame(known)
  }
  assertthat::assert_that(is.data.frame(known))
  assertthat::assert_that(assertthat::has_name(known, "stap_id"))
  assertthat::assert_that(assertthat::has_name(known, "known_lat"))
  assertthat::assert_that(assertthat::has_name(known, "known_lon"))
  # Only use the required column. Other names can cause issue later...
  unexpected_cols <- setdiff(names(known), c("stap_id", "known_lat", "known_lon"))
  if (length(unexpected_cols)) {
    cli::cli_warn("Unexpected columns found in {.var known}: \\
                  {paste(unexpected_cols, collapse = ', ')}")
    known <- known[, c("stap_id", "known_lat", "known_lon")]
  }
  known <- known[, c("stap_id", "known_lat", "known_lon")]
  if (!all(known$known_lon >= extent[1] & known$known_lon <= extent[2] &
    known$known_lat >= extent[3] & known$known_lat <= extent[4])) {
    cli::cli_abort(c(
      x = "The known latitude and longitude are not inside the extent of the map",
      i = "Modify {.var extent} or {.var known} to match this requirement."
    ))
  }
  if (anyDuplicated(known$stap_id)) {
    cli::cli_abort("{.var known} contains duplicate {.field stap_id} values.")
  }
  # Keep a copy of the original known to keep in param. Useful to keep negative indexing
  known0 <- known
  # Use negative indexing: e.g. replace known$stap_id = -1 to the last stap
  known$stap_id[known$stap_id < 0] <- nrow(stap) - 1 - known$stap_id[known$stap_id < 0]
  assertthat::assert_that(all(known$stap_id %in% stap$stap_id))
  assertthat::assert_that(all(unique(known$stap_id) == known$stap_id))

  # Define which stationary periods to include
  if (is.null(include_stap_id)) {
    include_stap_id <- tag$stap$stap_id
  }
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))
  assertthat::assert_that(is.numeric(include_min_duration))
  include_min_duration_id <- stap$stap_id[stap2duration(stap, units = "hours") >
    include_min_duration]

  # Include stap which are matching the three include constrains
  stap_include <- rep(FALSE, nrow(stap))
  stap_include[intersect(include_stap_id, include_min_duration_id)] <- TRUE

  # Check if value are already defined and if they are changing
  # Check if setmap has already been run before (all these condition should always be the same)
  if ("extent" %in% names(tag) || "known_lat" %in% names(stap) || "scale" %in% names(tag) ||
    "include" %in% names(stap)) {
    # Check if value are changing
    chg_known <- nrow(known0) != nrow(tag$param$tag_set_map$known) ||
      any(known0 != tag$param$tag_set_map$known)
    chg_include <- any(stap$include != stap_include)
    chg_extent <- any(extent != tag$param$tag_set_map$extent)
    chg_scale <- scale != tag$param$tag_set_map$scale

    # Check if known has changed
    if (chg_known || chg_extent || chg_scale || chg_include) {
      # Only provide option to stop the process if map are already defined
      if (any(c("map_pressure", "map_light") %in% names(tag))) {
        cli::cli_bullets(
          c("!" = "The likelihood map ({.var map_pressure} and/or {.var map_light}) \\
          have already been computed on this {.var tag} object with different setmap parameters.")
        )
        res <- utils::askYesNo(
          "Do you want to overwrite the parameters and delete the likelihood maps?"
        )
        if (res) {
          # If yes, remove existing likelihood map and carry on the overwrite of parameter
          tag$map_pressure <- NULL
          tag$map_light <- NULL
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
          "!" = "{.fun setmap} has already been run on this {.var tag} object and the input \\
          parameters are different.",
          ">" = "The old parameters ({.var scale}, {.var extent}, {.var tag$known} or \\
          {.var tag$include}) will be overwitten with the new ones."
        ))
      }
    }
  }

  # Add known to stap
  # remove all duplicate names to merge the table without duplicate
  stap <- stap[, !(names(stap) %in% c("known_lat", "known_lon", "include"))]
  known <- known[, !(names(known) %in% c("start", "end"))]
  stap <- merge(stap, known, all.x = TRUE)

  # Add the vector of stap to include
  stap$include <- stap_include

  if (all(!stap$include)) {
    cli::cli_warn(c(
      "x" = "All stationary periods have been excluded from the computation",
      ">" = "Check {.var include_stap_id} {.var include_min_duration}."
    ))
  }

  # Add parameters to stap
  tag$stap <- stap
  tag$param$tag_set_map <- list(
    extent = extent,
    scale = scale,
    known = known0,
    include_stap_id = NULL,
    include_min_duration = include_min_duration
  )
  if (length(include_stap_id) != length(tag$stap$stap_id) ||
    any(include_stap_id != tag$stap$stap_id)) {
    tag$param$tag_set_map$include_stap_id <- include_stap_id
  }

  return(tag)
}
