#' Plot a `path`
#'
#' @description
#' This function plots a `path` data.frame. This function is used in [plot.map()].
#
#' @param path a GeoPressureR `path` data.frame.
#' @param plot_leaflet logical defining if the plot is an interactive `leaflet` map or a static
#' basic plot.
#' @inherit leaflet::addProviderTiles
#' @param pad padding of the map in degree lat-lon (only for `plot_leaflet = FALSE`).
#' @param ... additional parameters for `plot_path_leaflet()`
#'
#' @examples
#' setwd(system.file("extdata/", package = "GeoPressureR"))
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
                      provider = "Stamen.TerrainBackground",
                      pad = 3,
                      ...) {
  if (plot_leaflet) {
    leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider) |>
      plot_path_leaflet(path, ...)
  } else {
    bbox <- list(
      min_lon = min(path$lon) - pad,
      max_lon = max(path$lon) + pad,
      min_lat = min(path$lat) - pad,
      max_lat = max(path$lat) + pad
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

    path$duration <- log(stap2duration(path))

    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(
        data = map_data_countries,
        ggplot2::aes_(x = .data$long, y = .data$lat, group = .data$group),
        fill = "#f7f7f7", color = "#e0e0e0", size = 0.2
      ) +
      ggplot2::geom_path(
        data = path,
        ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$j),
        color = "#3498db", linewidth = 1
      ) +
      ggplot2::geom_point(
        data = path,
        ggplot2::aes(x = .data$lon, y = .data$lat, size = .data$duration),
        fill = "#e74c3c", color = "black", shape = 21
      ) +
      ggplot2::coord_fixed(ratio = 1.3,
                           xlim = c(bbox$min_lon, bbox$max_lon),
                           ylim = c(bbox$min_lat, bbox$max_lat)) +
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
  map <- do.call(leaflet::addPolylines, c(
    list(
      map = map,
      lng = path$lon,
      lat = path$lat,
      group = path$j
    ),
    polyline
  ))

  map <- do.call(leaflet::addCircleMarkers, c(
    list(
      map = map,
      lng = path$lon,
      lat = path$lat,
      group = path$j
    ),
    circle
  ))
  # Legend
  # https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny

  return(map)
}
