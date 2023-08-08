#' Plot twilight data of a `tag`
#'
#' This function display a plot of twilight timeseries recorded by a tag
#'
#' @param map A GeoPressureR `map` matrix
#' @param plot_leaflet Logical to use an interactive `leaflet` map instead of `terra::plot`
#' @param path A GeoPressureR `path` data.frame
#' @inheritParams leaflet::addProviderTiles
#' @inheritParams leaflet::colorNumeric
#' @inheritParams leaflet::addRasterImage
#' @inheritParams terra::plot
#' @inheritParams graph_create
#'
#' @family map
#' @method plot map
#' @export
plot.map <- function(x,
                     thr_likelihood = 1,
                     path = NULL,
                     plot_leaflet = TRUE,
                     provider = "Stamen.TerrainBackground",
                     palette = "auto",
                     opacity = 0.8,
                     legend = FALSE,
                     ...) {

  map <- x

  # Eliminate unlikely pixel, same as in the creation of graph
  map$data <- lapply(map$data, function(m) {
    # Normalize
    m <- m / sum(m, na.rm = TRUE)

    ms <- sort(m)
    id_prob_percentile <- sum(cumsum(ms) < (1 - thr_likelihood))
    thr_prob <- ms[id_prob_percentile + 1]

    m[m < thr_prob] <- NA

    return(m)
  })

  # Convert map into rast
  r <- rast(map)

  # If r is very small, the plot is not working, this is a small trick to solve it.
  # r <- r / max(terra::minmax(r)[2, ], na.rm = TRUE)


  if (plot_leaflet) {
    #require("terra") required to attach has.RGB which has missing dependancy

    grp <- glue::glue("#{map$stap$stap_id} | {format(map$stap$start , format = '%d %b %H:%M')} - \\
                      {format(map$stap$end , format = '%d %b %H:%M')}")

    lmap <- leaflet::leaflet(width = "100%") |>
      leaflet::addProviderTiles(provider = provider)

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
