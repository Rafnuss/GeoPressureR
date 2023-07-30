#' Plot `path`
#'
#' This function plot a map with a `path`.
#
#' @inheritParams plot_map
#' @inheritParams pressurepath_create
#' @inheritDotParams plot_path_leaflet
#'
#' @export
plot_path <- function(path,
                      plot_leaflet = TRUE,
                      provider = "Stamen.TerrainBackground",
                      ...) {
  if (plot_leaflet) {
    leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider) |>
      plot_path_leaflet(path, ...)
  } else {
    plot(path$lon, path$lat, ...)
  }
}

#' Plot `path` with leaflet
#'
#' This function plot a leaflet map with a `path`
#
#' @param path A `path` list
#'
plot_path_leaflet <- function(map,
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
