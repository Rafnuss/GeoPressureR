#' Create a `geostap`
#'
#' @description
#' This function creates a `geostap` list, which defines all the essential information needed to construct the
#' likelihood map and later create the graph.
#'
#' `geostap` stands for **geo**graphical and **sta**tionary **p**eriod, as these are the two
#' key dimensions (space-time) which are defined here for the rest of the analysis.
#'
#' In addition, `geostap` also includes the ability to define `known` locations (e.g., equipment or
#' retrieval site). These can only be defined at the level of a stationary period (i.e., assuming
#' constant position during the whole stationary period) but you can define as many known stationary
#' periods as you wish. No likelihood map will be computed for these stationary periods, thus saving
#' computational time.
#'
#' Furthermore, it is possible to compute (and later model) a subset of the stationary periods.
#' This is done with `stap_include`. By default, `stap_include` includes all stationary periods.
#'
#'
#' @param tag Data logger list with label information. See [`tag_label()`] for the required input.
#' @param extent Geographical extent of the map on which the likelihood and graph model will be
#' computed. Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param scale Number of pixels per 1° latitude-longitude. For instance, `scale = 10` for a
#' resolution of 0.1° (~10km) and `scale=4` for a resolution of 0.25° (~30km). To avoid
#' interpolating the ERA5 data, the scale should be smaller than 10. Read more about scale on the [Google
#' earth Engine documentation](https://developers.google.com/earth-engine/guides/scale).
#' @param known Data.frame containing the known positions of the bird (e.g., equipment or retrieval
#' site). This information can only be attached at the level of a stationary period.
#' @param stap_include Vector of the stationary period to model, that is, to compute in the likelihood
#' map and use in the graph.
#' @return List of the misfit map for each stationary period, containing:
#' - `ìd`: Tag identifier, same as `tag$id`
#' - `stap`: Data.frame of all stationary periods. Same as `tag$stap` but with three new columns:
#' `known_lat` and `known_lon` define the known position during these stationary periods, and `model`
#' defines whether the likelihood map of this stationary period should be computed and
#' later used in the graph.
#' - `extent` same as parameter `extent`
#' - `scale` same as parameter `scale`
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
#' tag <- tag_read("18LX") |>
#'   tag_label()
#'
#' # Default geostap
#' geostap <- geostap_create(tag, c(-16, 23, 0, 50))
#' str(geostap)
#'
#' # Customized geostap, with coarse grid scale, known position for the first stationary period and
#' # considering only the stationary periods lasting more than 20hours.
#' geostap <- geostap_create(tag,
#'   extent = c(-16, 23, 0, 50),
#'   scale = 1,
#'   known = data.frame(
#'     stap_id = 1,
#'     known_lon = 17.05,
#'     known_lat = 48.9
#'   ),
#'   stap_include = which(difftime(tag$stap$end, tag$stap$start, units = "hours") > 20)
#' )
#' str(geostap)
#' @export
geostap_create <- function(tag,
                           extent,
                           scale = 10,
                           known = data.frame(
                             stap_id = integer(),
                             known_lat = double(),
                             known_lon = double()
                           ),
                           stap_include = tag$stap$stap_id) {
  assertthat::assert_that(is.list(tag))
  assertthat::assert_that(assertthat::has_name(tag, "id"))
  if (!("stap" %in% names(tag))) {
    cli::cli_abort(c(
      x = "{.var tag} does not contains {.var stap}",
      i = "Make sure to run {.fn tag_label} or {.fn tag_label_stap} before using {.fn geostap_create}"
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

  # Check stap_include
  assertthat::assert_that(all(stap_include %in% tag$stap$stap_id))
  # Add stap_id to stap
  stap$include <- FALSE
  stap$include[stap_include] <- TRUE
  if (any(!stap$include)) {
    cli::cli_warn(c(
      "!" = "The {.var geostap} is setup to model {.val {sum(stap$include)}} out of \\
      {.val {nrow(stap)}} stationary periods: {.var {stap$stap_id[stap$include]}}."
    ))
  }

  # Copy
  geostap <- list(
    id = tag$id,
    stap = stap,
    scale = scale,
    extent = extent
  )

  return(geostap)
}
