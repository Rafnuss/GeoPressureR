#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight timeseries recorded by a tag
#'
#' @param map A GeoPressureR `map` matrix
#' @param stap A stationary period data.frame. By default, use the `stap` attached to `map`, but
#' can be overwritten
#' @param plot_leaflet Logical to use an interactive `leaflet` map instead of `terra::plot`
#' @param path A GeoPressureR `path` data.frame
#' @inheritParams leaflet::addProviderTiles
#' @inheritParams leaflet::colorNumeric
#' @inheritParams leaflet::addRasterImage
#' @inheritParams terra::plot
#' @export
plot_map <- function(map,
                     stap = NULL,
                     path = NULL,
                     plot_leaflet = TRUE,
                     provider = "Stamen.TerrainBackground",
                     palette = "OrRd",
                     opacity = 0.8,
                     legend = FALSE,
                     ...) {
  if (inherits(map, "SpatRaster")) {
    r <- map
  } else {
    # Compute the raster from the map
    r <- map2rast(map)
  }

  # If r is very small, the plot is not working, this is a small trick to solve it.
  r <- r / max(terra::minmax(r)[2, ], na.rm = TRUE)

  if (plot_leaflet) {
    require("terra") # require to attach has.RGB which has missing dependancy

    if (is.null(stap) & assertthat::has_attr(map, "stap")) {
      stap <- attr(map, "stap")
    }

    if (is.null(stap)) {
      grp <- glue::glue("#{seq_len(length(map))}")
    } else {
      grp <- glue::glue("#{stap$stap_id} | {format(stap$start , format = '%d %b %H:%M')} - {format(stap$end , format = '%d %b %H:%M')}")
    }

    lmap <- leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider)


    for (i in seq_len(dim(r)[3])) {
      lmap <- leaflet::addRasterImage(
        lmap,
        r[[i]],
        opacity = opacity,
        group = grp[i],
        method = "ngb",
        colors = leaflet::colorNumeric(
          palette = palette,
          domain = NULL,
          na.color = "#00000000",
          alpha = TRUE
        )
      )
    }

    # Known location
    if (!is.null(stap)) {
      id_known <- which(!is.na(stap$known_lat))
      for (i in seq_len(length(id_known))) {
        lmap <- leaflet::addCircleMarkers(
          lmap,
          lng = stap$known_lon[id_known[i]],
          lat = stap$known_lat[id_known[i]],
          fillOpacity = 1,
          weight = 2,
          color = "white",
          fillColor = "red"
        )
      }
    }

    # path
    if (!is.null(path)) {
      lmap <- plot_path_leaflet(lmap, path)

      for (i in seq_len(nrow(path))) {
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
          label = glue::glue("#{path$stap_id}, {round(stap2duration(path), 1)} days")
        )
      }
    }

    lmap <- leaflet::addLayersControl(
      lmap,
      baseGroups = grp,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

    return(lmap)
  } else {
    terra::plot(r, legend = legend, ...)
  }
}
