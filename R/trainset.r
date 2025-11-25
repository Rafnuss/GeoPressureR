#' Start the GeoPressure Trainset shiny app
#'
#' GeoPressure Trainset is a shiny app designed to help you manually label pressure and
#' acceleration data for training machine learning models. This interactive app allows you to
#' visualize time series data, select data points or regions, and assign behavioral labels
#' (e.g., "flight", "discard", or custom elevation labels) to create training datasets.
#'
#' The Trainset app can be started based on a `tag` object or a `.Rdata` file containing
#' at least `tag` with pressure data. If acceleration data is available in the tag,
#' it will also be displayed and can be labeled.
#'
#' The app features:
#' - Interactive plotly visualization of pressure and acceleration time series
#' - Point and region selection for efficient labeling
#' - Support for stationary periods (STAP) navigation
#' - Custom label creation (elevation labels)
#' - Export functionality to save labeled data as CSV files
#'
#' Learn more about data labeling workflows in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/) or explore the
#' [GeoPressureR documentation](https://raphaelnussbaumer.com/GeoPressureR/).
#'
#'
#' @param x a GeoPressureR `tag` object, a `.Rdata` file or the
#' unique identifier `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true, the app runs in a background R session using the `callr` package. This
#' allows you to continue using your R session while the app is running.
#' @return Invisible process object if `run_bg = TRUE`, otherwise invisible NULL.
#' The labeled data can be exported directly from the app interface.
#'
#' @seealso [tag_label_read()], [tag_label_write()], [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/)
#' @export
trainset <- function(x, launch_browser = TRUE, run_bg = TRUE) {
  if (inherits(x, "tag")) {
    cli::cli_abort("{.arg x} is {.cls tag} object. {.fn trainset} requires a file or id.")
  }

  # Handle file path logic
  if (is.character(x) && length(x) == 1) {
    print(x)
    if (file.exists(x)) {
      # x is a path to a file that exists
      file <- x
      # Extract id: remove extension and keep only part before first '-'
      id <- sub("-.*$", "", sub("\\..*$", "", basename(file)))
    } else {
      # x is a character (id), try finding label files
      id <- x
      labeled_file <- glue::glue("./data/tag-label/{id}-labeled.csv")
      unlabeled_file <- glue::glue("./data/tag-label/{id}.csv")

      if (file.exists(labeled_file)) {
        file <- labeled_file
      } else if (file.exists(unlabeled_file)) {
        file <- unlabeled_file
      } else {
        cli::cli_abort(c(
          "Cannot find data files for id {.val {id}}",
          "i" = "Looked for:",
          "*" = "{.file {file.path(getwd(), labeled_file)}}",
          "*" = "{.file {file.path(getwd(), unlabeled_file)}}",
          "i" = "Please ensure the files exist or provide a valid file path"
        ))
      }
    }
  } else {
    cli::cli_abort("{.arg x} must be a single character string (file path or id)")
  }

  # Read trainset file
  csv <- GeoPressureR:::trainset_read_raw(file)

  tag <- GeoPressureR:::tag_create_dataframe(
    id,
    pressure_file = csv[csv$series == "pressure", ],
    acceleration_file = csv[csv$series == "acceleration", ],
    quiet = TRUE
  )

  tag <- tag_label_stap(tag, quiet = TRUE)

  if (run_bg) {
    p <- callr::r_bg(
      func = function(tag) {
        library(GeoPressureR)
        shiny::shinyOptions(tag = tag)
        shiny::runApp(system.file("trainset", package = "GeoPressureR"))
      },
      args = list(
        tag = tag
      )
    )

    port <- NA
    while (p$is_alive()) {
      p$poll_io(1000) # wait up to 1s for new output
      err <- p$read_error()
      out <- p$read_output()
      txt <- paste(err, out, sep = "\n")

      if (grepl("Listening on http://127\\.0\\.0\\.1:[0-9]+", txt)) {
        port <- sub(".*127\\.0\\.0\\.1:([0-9]+).*", "\\1", txt)
        url <- glue::glue("http://127.0.0.1:{port}")
        cli::cli_alert_success("Opening Trainset app at {.url {url}}")
        utils::browseURL(url)
        break
      }
    }
    return(invisible(p))
  } else {
    if (launch_browser) {
      launch_browser <- getOption("browser")
    } else {
      launch_browser <- getOption("shiny.launch.browser", interactive())
    }
    shiny::shinyOptions(tag = tag)

    # Start the app
    shiny::runApp(
      system.file("trainset", package = "GeoPressureR"),
      launch.browser = launch_browser
    )
  }
}
