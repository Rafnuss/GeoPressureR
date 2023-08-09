#' Start the GeoPressureViz shiny app
#'
#' GeoPressureViz is a shiny app designed to help you visualize the overall trajectory of the bird
#' as well as each step-by-step move. Learn more about GeoPressureViz in the [GeoPressureManual |
#' GeoPressureViz](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html) or with
#' this [demo of the Great Reed Warbler (18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).
#'
#' @param tag a GeoPressureR `tag` object.
#' @param id Unique identifier of a tag.
#' @param pressurepath a pressure path computed with `create_pressurepath()`
#' @param marginal map of the marginal probability computed with `graph_marginal()`
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on Rstudio.
#' @return The updated path visualized in the app.
#'
#' @seealso [GeoPressureManual | GeoPressureViz
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
#' @export
geopressureviz <- function(tag = NULL,
                           pressurepath = NULL,
                           marginal = NULL,
                           id = NULL,
                           launch_browser = TRUE) {
  if (!is.null(id)) {
    # Make of copy of the arguement so that they don't get overwritten
    tag0 <- tag
    pressurepath0 <- pressurepath
    marginal0 <- marginal
    # Load interim
    load(glue::glue("./data/interim/{id}.RData"))
    # Overwrite loaded variable with arguments if provided
    if (!is.null(tag0))
      tag <- tag0
    if (!is.null(pressurepath0))
      pressurepath <- pressurepath0
    if (!is.null(marginal0))
      marginal <- marginal0
  }

  tag_assert(tag, "setmap")

  if (all(c("map_pressure", "map_light") %in% names(tag))) {
    tag$map_preslight <- tag$map_pressure * tag$map_light
  }

  if (is.null(marginal)) {
    if (!is.null(id)) {
      # load marginal from id
    }
  } else {
    tag$map_marginal <- marginal
  }

  # Add possible map to display
  maps_choices <- list(
    "Pres. MSE" = "map_pressure_mse",
    "Pres. mask" = "map_pressure_mask",
    "Pressure" = "map_pressure",
    "Light" = "map_light",
    "Pres.&Light" = c("map_pressure", "map_light"),
    "Marginal" = "map_marginal"
  )
  maps_is_available <- sapply(maps_choices, \(x) all(x %in% names(tag)))

  maps <- lapply(maps_choices[maps_is_available], \(likelihood){
    rast(tag2map(tag, likelihood = likelihood))
  })

  names(maps) <- names(maps_choices[maps_is_available])

  # Get stationary period information
  stap <- tag$stap

  # Set color of each stationary period
  col <- rep(RColorBrewer::brewer.pal(8, "Dark2"), times = ceiling(nrow(stap) / 8))
  stap$col <- col[stap$stap_id]
  stap$duration <- stap2duration(stap)


  # Get the pressure timeserie
  if (is.null(pressurepath)) {
    pressurepath <- list()

    # Set the initial path with tag2path
    path <- tag2path(tag)
  } else {
    path <- unique(pressurepath[, c("stap_id", "lat", "lon")])
  }
  pressurepath$linetype <- as.factor(1)
  pressurepath <- merge(pressurepath, stap[, names(stap) %in% c("stap_id", "col")], by = "stap_id")


  # PEROSENVIR <- new.env(parent=emptyenv())
  .GlobalEnv$.tag_id <- tag$param$id
  .GlobalEnv$.stap <- stap
  .GlobalEnv$.pressure <- tag$pressure
  .GlobalEnv$.maps <- maps
  .GlobalEnv$.extent <- tag$param$extent
  .GlobalEnv$.pressurepath <- pressurepath
  .GlobalEnv$.path <- path

  # delete variable when removed
  # on.exit(
  #   rm(
  #     list = c(".tag_id",".stap", ".pressure", ".maps", ".extent", ".ts0", ".path0"),
  #     envir = .GlobalEnv
  #  )
  # )

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
