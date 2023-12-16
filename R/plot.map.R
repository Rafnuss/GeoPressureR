#' Plot a `map` object
#'
#' @description
#' This function plot a GeoPressureR `map` object.
#'
#' You can plot on top of the `map` a `path`, this uses the `plot_path()` function.
#'
#' Our maps are defined in lat-lon (i.e., EPSG:4326), but the display of maps on web map are
#' nearly always in [web mercator](https://en.wikipedia.org/wiki/Web_Mercator_projection) (i.e.,
#' EPSG:3857). We therefore need to reproject our map for display.
#' However, we don't really want to interpolate the map as each pixel might be important to
#' visualize. We therefore re-project with a near-neighbor interpolation (`method = "near"`
#' in [`terra::project()`]). Yet to avoid having pixel misplaced, we generally need to use a
#' projection with a finer resolution. The argument `fac_res_proj` controls the relative change of
#' resolution between the orginal map to the projected map.
#'
#' @param map a GeoPressureR `map` object
#' @param plot_leaflet logical to use an interactive `leaflet` map instead of `terra::plot`
#' @param path a GeoPressureR `path` data.frame
#' @param provider_options tile options. See leaflet::addProviderTiles() and
#' leaflet::providerTileOptions()
#' @inheritParams leaflet::addProviderTiles
#' @inheritParams leaflet::colorNumeric
#' @inheritParams leaflet::addRasterImage
#' @inheritParams terra::plot
#' @inheritParams graph_create
#' @param fac_res_proj Factor of the resolution of the reprojection (see details above). A value of
#' `1` will roughly reproject on a map with similar size resulting in relatively high inaccuracy of
#' the pixel displayed. Increasing this factor will reduce the uncerstainty but might also increase
#' the computational cost of the reprojection.
#'
#' @return a plot or leaflet object.
#'
#' @examples
#' owd <- setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_set_map(
#'     extent = c(-16, 23, 0, 50),
#'     scale = 4
#'   ) |>
#'   geopressure_map(quiet = TRUE)
#' setwd(owd)
#'
#' plot(tag$map_pressure)
#'
#' plot(tag$map_pressure, plot_leaflet = FALSE)
#'
#' # `thr_likelihood` can be used to visualize its effect in `graph_create`
#' plot(tag$map_pressure,
#'   thr_likelihood = 0.9,
#'   palette = "viridis",
#'   opacity = 1,
#'   provider = "CartoDB.DarkMatterNoLabels"
#' )
#'
#' @family map plot_tag
#' @seealso [plot_path()]
#' @method plot map
#' @export
plot.map <- function(x,
                     thr_likelihood = 1,
                     path = NULL,
                     plot_leaflet = TRUE,
                     provider = "Esri.WorldTopoMap",
                     provider_options = leaflet::providerTileOptions(),
                     palette = "auto",
                     opacity = 0.8,
                     legend = FALSE,
                     fac_res_proj = 4,
                     ...) {
  map <- x

  # Eliminate unlikely pixel, same as in the creation of graph
  map$data <- lapply(map$data, function(m) {
    if (!is.null(m)) {
      # Normalize
      m <- m / sum(m, na.rm = TRUE)

      # Find threshold of percentile
      ms <- sort(m)
      id_prob_percentile <- sum(cumsum(ms) < (1 - thr_likelihood))
      thr_prob <- ms[id_prob_percentile + 1]

      # Set to NA all value below this threshold
      m[m < thr_prob] <- NA
    }
    return(m)
  })

  # Convert GeoPressureR map to terra rast object
  r <- rast.map(map)

  if (plot_leaflet) {
    grp <- glue::glue("#{map$stap$stap_id} | {format(map$stap$start , format = '%d %b %H:%M')} - \\
                      {format(map$stap$end , format = '%d %b %H:%M')}")

    lmap <- leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider, options = provider_options)

    if (palette == "auto") {
      if ("pressure" == map$type) {
        palette <- "GnBu"
      } else if ("light" == map$type) {
        palette <- "OrRd"
      } else if ("pressure_mse" == map$type) {
        palette <- "BuPu"
      } else if ("pressure_mask" == map$type) {
        palette <- "YlOrBr"
      } else if ("mask_water" == map$type) {
        palette <- "Greys"
      } else if ("marginal" == map$type) {
        palette <- "plasma"
      } else {
        palette <- "plasma"
      }
    }

    # Compute the resolution for the projection to web Mercator
    g <- map_expand(map$extent, map$scale)
    lon_epsg3857 <- (g$lon * 20037508.34 / 180)
    lat_epsg3857 <- (log(tan((90 + g$lat) * pi / 360)) / (pi / 180)) * (20037508.34 / 180)

    # Define the default resolution of the projection as the median value of the difference of the
    # actual position
    res_proj <- c(
      stats::median(diff(lon_epsg3857)),
      stats::median(abs(diff(lat_epsg3857))) / fac_res_proj
    )

    for (i in map$stap$stap_id[map$stap$include]) {
      # Project from lat-lon to web Mercator using the nearest neighbor interpolation
      r_i_proj <- terra::project(
        r[[i]],
        "epsg:3857",
        method = "near",
        # res = res(r[[i]]) * c(110.574, 111.320) * 1000,
        res = res_proj,
        origin = c(stats::median(lon_epsg3857), stats::median(lat_epsg3857))
      )

      lmap <- leaflet::addRasterImage(
        lmap,
        r_i_proj,
        opacity = opacity,
        group = grp[i],
        project = FALSE,
        colors = leaflet::colorNumeric(
          palette = palette,
          domain = NULL,
          na.color = "#00000000",
          alpha = TRUE
        )
      )
    }

    # Known location
    id_known <- which(!is.na(map$stap$known_lat))
    for (i in seq_len(length(id_known))) {
      lmap <- leaflet::addCircleMarkers(
        lmap,
        lng = map$stap$known_lon[id_known[i]],
        lat = map$stap$known_lat[id_known[i]],
        fillOpacity = 1,
        weight = 2,
        color = "white",
        fillColor = "red"
      )
    }

    # path
    if (!is.null(path)) {
      lmap <- plot_path_leaflet(lmap, path)

      for (i in seq_len(nrow(path))) {
        if (!is.na(path$lon[i])) {
          lmap <- leaflet::addCircleMarkers(
            lmap,
            lng = path$lon[i],
            lat = path$lat[i],
            group = grp[i],
            radius = stap2duration(path[i, ])^(0.25) * 6,
            stroke = TRUE,
            color = "white",
            weight = 2,
            opacity = 1,
            fill = TRUE,
            fillColor = "black",
            fillOpacity = 1,
            label = glue::glue("#{path$stap_id[i]}, {round(stap2duration(path)[i], 1)} days")
          )
        }
      }
    }

    lmap <- leaflet::addLayersControl(
      lmap,
      baseGroups = grp[map$stap$include],
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

    return(lmap)
  } else {
    terra::plot(r[[map$stap$include]], legend = legend, ...)
  }
}
