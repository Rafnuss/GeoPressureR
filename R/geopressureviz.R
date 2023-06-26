#' Start the GeoPressureViz shiny app
#'
#' GeoPressureViz is a shiny app designed to help you visualize the overall trajectory of the bird
#' as well as each step-by-step move. Learn more about GeoPressureViz in the [GeoPressureManual |
#' GeoPressureViz](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html) or with
#' this [demo of the Great Reed Warbler (18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).
#'
#' @inheritParams tag_label_update
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on Rstudio.
#' @return The updated path visualized in the app.
#' @seealso [GeoPressureManual | GeoPressureViz
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
#' @export
geopressureviz <- function(geostap,
                           path_pres = NA,
                           launch_browser = TRUE) {
  if (all(c("map_pressure", "map_light") %in% names(geostap))) {
    geostap$map_preslight <- mapply(\(p, l) p * l, geostap$map_pressure, geostap$map_light, SIMPLIFY = FALSE)
  }
  stopifnot(require("shiny"), msg="")

  # Add possible map to display
  maps_choices <- c("Light", "Pres. MSE", "Pres. mask", "Pressure", "Pres.&Light", "Marginal")
  maps_field <- c("map_light", "mse", "mask", "map_pressure", "map_preslight", "map_marginal")
  tmp <- maps_field %in% names(geostap)
  maps <- geostap[maps_field[tmp]]
  names(maps) <- maps_choices[tmp]

  # Get stationary period information
  stap <- geostap$stap

  # Set color of each stationary period
  col <- rep(RColorBrewer::brewer.pal(8, "Dark2"), times = ceiling(nrow(stap) / 8))
  stap$col <- col[stap$stap_id]
  stap$duration <- as.numeric(difftime(stap$end, stap$start, units = "days"))


  # Get the pressure timeserie
  if (any(!is.na(path_pres))) {
    path0 <- path_pres[, c("stap_id", "lat", "lon")]

    ts0 <- path_pres
  } else {
    ts0 <- list()

    # Set the initial path to the most likely from static prob
    path0 <- geostap2path(geostap)
  }



  # PEROSENVIR <- new.env(parent=emptyenv())
  .GlobalEnv$.tag_id <- geostap$id
  .GlobalEnv$.stap <- stap
  .GlobalEnv$.pressure <- pressure
  .GlobalEnv$.maps <- maps
  .GlobalEnv$.extent <- geostap$extent
  .GlobalEnv$.ts0 <- ts0
  .GlobalEnv$.path0 <- path0

  # delete variable when removed
  on.exit(
    rm(
      list = c(".map_choices", ".maps", ".stap", ".pressure", ".ts0", ".path0", ".tag_id"),
      envir = .GlobalEnv
    )
  )

  if (launch_browser) {
    launch_browser <- getOption("browser")
  } else {
    launch_browser <- getOption("shiny.launch.browser", interactive())
  }

  # Start the app
  shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"),
    launch.browser = launch_browser
  )
  return(.GlobalEnv$.path0)
}
