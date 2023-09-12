#' Start the GeoPressureViz shiny app
#'
#' GeoPressureViz is a shiny app designed to help you visualize the overall trajectory of the bird
#' as well as each step-by-step move. This app is particularly useful to check the correspondance
#' between pressure map, light map and flight distance. You can edit the path and query pressure
#' timeserie for individual stationary period to test manual what seems the optimal path.
#'
#' You can retrieved the edited path from the global envirenmental variable `geopressureviz_path`.
#'
#' Learn more about GeoPressureViz in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html) or with
#' this [demo of the Great Reed Warbler (18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).
#'
#'
#' @param x a GeoPressureR `tag` object or an unique identifier `id`.
#' @param pressurepath a GeoPressureR `pressurepath` data.frame.
#' @param marginal map of the marginal probability computed with `graph_marginal()`.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @return The updated path visualized in the app. Can also be retrieved with
#' `.GlobalEnv$geopressureviz_path`
#'
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
#' @export
geopressureviz <- function(x,
                           pressurepath = NULL,
                           marginal = NULL,
                           launch_browser = TRUE) {
  if (!inherits(x, "tag")) {
    if (is.character(x) && file.exists(x)) {
      file <- x
    } else if (is.character(x)) {
      file <- glue::glue("./data/interim/{x}.RData")
    } else {
      file <- NULL
    }

    if (is.character(file) && file.exists(file)) {
      # Make of copy of the arguement so that they don't get overwritten
      if (!is.null(pressurepath)) {
        pressurepath0 <- pressurepath
      }
      if (!is.null(marginal)) {
        marginal0 <- marginal
      }
      # Load interim
      load(file)
      # Overwrite loaded variable with arguments if provided
      if (exists("pressurepath0")) {
        pressurepath <- pressurepath0
      }
      if (exists("marginal0")) {
        marginal <- marginal0
      }
    } else {
      cli::cli_abort("The first arguement {.var x} needs to be a {.cls tag}, a {.field file} or \\
                     an {.field id}")
    }
  } else {
    tag <- x
  }


  tag_assert(tag, "setmap")

  if (all(c("map_pressure", "map_light") %in% names(tag))) {
    tag$map_preslight <- tag$map_pressure * tag$map_light
  }

  if (!is.null(marginal)) {
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

  maps <- lapply(maps_choices[maps_is_available], \(likelihood) {
    tag2map(tag, likelihood = likelihood)
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
    pressurepath <- data.frame()

    if (exists("path_most_likely")) {
      path <- path_most_likely
    } else {
      # Set the initial path with tag2path
      path <- tag2path(tag)
    }
  } else {
    path <- merge(tag$stap, unique(pressurepath[, c("stap_id", "lat", "lon")]), all = TRUE)
    pressurepath$linetype <- as.factor(1)
    pressurepath <- merge(
      pressurepath,
      stap[, names(stap) %in% c("stap_id", "col")],
      by = "stap_id"
    )
  }

  # nolint start
  .GlobalEnv$.tag_id <- tag$param$id
  .GlobalEnv$.stap <- stap
  .GlobalEnv$.pressure <- tag$pressure
  .GlobalEnv$.maps <- maps
  .GlobalEnv$.extent <- tag$param$extent
  .GlobalEnv$.pressurepath <- pressurepath
  .GlobalEnv$.path <- path
  # nolint end

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

  return(.GlobalEnv$geopressureviz_path)
}
