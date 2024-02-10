#' Plot a `path`
#'
#' @description
#' This function plots a `path` data.frame. This function is used in [plot.map()].
#
#' @param path a GeoPressureR `path` data.frame.
#' @param plot_leaflet logical defining if the plot is an interactive `leaflet` map or a static
#' basic plot.
#' @inherit leaflet::addProviderTiles
#' @param provider_options tile options. See leaflet::addProviderTiles() and
#' leaflet::providerTileOptions()
#' @param pad padding of the map in degree lat-lon (only for `plot_leaflet = FALSE`).
#' @param ... additional parameters for `plot_path_leaflet()`
#'
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_set_map(c(-16, 23, 0, 50), scale = 1)
#' path <- ind2path(c(1652, 1603, 1755, 1708, 1607), tag)
#'
#' plot_path(path)
#'
#' @family path
#' @seealso [plot.map()]
#' @export
plot_path <- function(path,
                      plot_leaflet = TRUE,
                      provider = "Esri.WorldTopoMap",
                      provider_options = leaflet::providerTileOptions(),
                      pad = 3,
                      ...) {
  if (all(c("start", "end") %in% names(path))) {
    path$duration <- stap2duration(path)
  } else {
    path$duration <- 1
  }

  if (!("j" %in% names(path))) path$j <- 1

  if (plot_leaflet) {
    leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider, options = provider_options) |>
      plot_path_leaflet(path, ...)
  } else {
    bbox <- list(
      min_lon = min(path$lon, na.rm = TRUE) - pad,
      max_lon = max(path$lon, na.rm = TRUE) + pad,
      min_lat = min(path$lat, na.rm = TRUE) - pad,
      max_lat = max(path$lat, na.rm = TRUE) + pad
    )

    world <- ggplot2::map_data("world")
    intersecting_countries <- unique(world[
      world$long >= bbox$min_lon &
        world$long <= bbox$max_lon &
        world$lat >= bbox$min_lat &
        world$lat <= bbox$max_lat,
      "region"
    ])
    map_data_countries <- ggplot2::map_data("world", region = intersecting_countries)



    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(
        data = map_data_countries,
        ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
        fill = "#f7f7f7", color = "#e0e0e0", linewidth = 0.2
      )

    path_full <- path[!is.na(path$lat), ]
    if (nrow(path_full) < nrow(path)) {
      p <- p + ggplot2::geom_path(
        data = path_full,
        ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$j),
        color = "black", linewidth = 1, alpha = 0.2
      )
    }

    p <- p + ggplot2::geom_path(
      data = path,
      ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$j),
      color = "black", linewidth = 1
    ) +
      ggplot2::geom_point(
        data = path_full,
        ggplot2::aes(x = .data$lon, y = .data$lat, size = log(.data$duration)),
        fill = "#e74c3c", color = "black", shape = 21
      ) +
      ggplot2::coord_fixed(
        ratio = 1.3,
        xlim = c(bbox$min_lon, bbox$max_lon),
        ylim = c(bbox$min_lat, bbox$max_lat)
      ) +
      ggplot2::theme_minimal()



    return(p)
  }
}


#' @noRd
plot_path_leaflet <- function(
    map,
    path,
    polyline = list(
      stroke = TRUE,
      color = "black",
      weight = 5,
      opacity = 0.7,
      dashArray = NULL
    ),
    circle = list(
      radius = stap2duration(path)^(0.25) * 6,
      stroke = TRUE,
      color = "white",
      weight = 2,
      opacity = 1,
      fill = ifelse(is.null(path$interp), TRUE, !path$interp),
      fillColor = "grey",
      fillOpacity = 0.8,
      label = glue::glue("#{path$stap_id}, {round(stap2duration(path), 1)} days")
    )) {
  # Remove position of stap not included/not available
  path_full <- path[!is.na(path$lat), ]

  # Plot trajectory of all available point in grey
  if (nrow(path_full) < nrow(path)) {
    polyline_full <- polyline
    # polyline_full$weight <- polyline_full$weight/2
    polyline_full$opacity <- polyline_full$opacity / 2
    polyline_full$color <- "grey"
    map <- do.call(leaflet::addPolylines, c(
      list(
        map = map,
        lng = path_full$lon,
        lat = path_full$lat,
        group = path_full$j
      ),
      polyline_full
    ))
  }

  # Overlay with trajectory of consecutive position in black.
  map <- do.call(leaflet::addPolylines, c(
    list(
      map = map,
      lng = path$lon,
      lat = path$lat,
      group = path$j
    ),
    polyline
  ))

  suppressWarnings({
    map <- do.call(leaflet::addCircleMarkers, c(
      list(
        map = map,
        lng = path$lon,
        lat = path$lat,
        group = path$j
      ),
      circle
    ))
  })
  # Legend
  # nolint start
  # https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
  # nolint end
  return(map)
}
