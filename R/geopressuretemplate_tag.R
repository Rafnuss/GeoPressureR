#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_tag <- function(
    id,
    config = config::get(config = id),
    quiet = FALSE,
    file = glue::glue("./data/interim/{id}.RData"),
    assert_graph = FALSE,
    ...) {
  # Create the config file
  config <- geopressuretemplate_config(id, config = config, assert_graph = assert_graph, ...)

  # Check if folder exist
  dir_file <- dirname(file)
  if (!dir.exists(dir_file)) {
    cli::cli_bullets(c("!" = "The directory {.file {dir_file}} does not exists."))
    res <- utils::askYesNo("Do you want to create it?")
    if (res) {
      dir.create(dir_file, recursive = TRUE)
    } else {
      cli::cli_abort("Please create the directory and run the function again.")
    }
  }

  if (!quiet) {
    cli::cli_h2("Prepare tag")
  }
  # Create a tag object and initialize it with sensor data and other parameters
  tag <- do.call(tag_create, c(
    list(id = id, quiet = quiet),
    config$tag_create
  ))


  # Label the tag with additional metadata and annotations
  tag <- do.call(tag_label, c(
    list(tag = tag, quiet = quiet),
    config$tag_label
  ))

  # Set the geospatial map for the tag using provided parameters
  tag <- do.call(tag_set_map, c(
    list(tag = tag),
    config$tag_set_map
  ))


  # If light mapping is required, process it
  if ("map_light" %in% config$geopressuretemplate$likelihood) {
    if (!quiet) {
      cli::cli_h2("Build light likelihood map")
    }
    # Compute the twilight (light-based) map
    tag <- do.call(twilight_create, c(
      list(tag = tag),
      config$twilight_create
    ))

    tag <- do.call(twilight_label_read, c(
      list(tag = tag),
      config$twilight_label_read
    ))

    tag <- do.call(geolight_map, c(
      list(tag = tag, quiet = quiet),
      config$geolight_map
    ))
  }

  # If pressure mapping is required, process it
  if ("map_pressure" %in% config$geopressuretemplate$likelihood) {
    if (!quiet) {
      cli::cli_h2("Build pressure likelihood map")
    }
    tag <- do.call(geopressure_map, c(
      list(tag = tag, quiet = quiet, debug = FALSE),
      config$geopressure_map
    ))
  }

  param <- tag$param

  # Save the processed tag object to a file if requested
  save(
    tag,
    param,
    file = file
  )

  # Return the processed tag object invisibly
  invisible(tag)
}
