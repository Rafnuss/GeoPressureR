#' Workflow for GeoPressureR
#'
#' @description
#' The `geopressuretemplate` function manages the complete workflow for modeling bird trajectories
#' using geospatial pressure and light data. It includes the creation of tag data, likelihood map
#' computation (pressure and/or light), graph model construction, trajectory estimation, and
#' pressure path computation. The function follows a standard structure as defined by the GitHub
#' repository [geopressuretemplate](https://github.com/Rafnuss/GeoPressureTemplate), which
#' separates data and analysis for improved readability, sharability, and reproducibility.
#'
#' @param id A unique identifier for the tag being processed.
#' @param config A configuration object specifying workflow parameters, which is loaded by default
#' using `config::get(config = id)`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages during execution. The default
#' value is `FALSE`.
#' @param file A file path to save the intermediate results (e.g., tag, graph, and pressure paths).
#' Default is `./data/interim/{id}.RData`.
#' @param assert_graph Logical. If `TRUE`, check that the config is compatible for the creation of
#' a graph. The default value is `TRUE`. Set to `FALSE` only if you don't want to create a graph
#' model
#' @param ... Additional parameters to overwrite default or config values. Always prefer to modify
#' `config.yml` if possible.
#'
#' @details
#' The `geopressuretemplate` function is a high-level entry point that coordinates multiple
#' steps for processing geospatial animal movement data. It relies on underlying child
#' functions for each step:
#'
#' * **Tag Creation [`geopressuretemplate_tag()`]**: Initializes and labels the `tag` object. It
#' also generates light and pressure likelihood maps:
#'
#' 1. `tag_create()`: Initializes the tag object.
#' 2. `tag_label()`: Adds labels.
#' 3. `tag_set_map()`: Sets the spatial and temporal parameters.
#' 4. If `"map_pressure"` is in the `config$geopressuretemplate$likelihood`:
#'     - `geopressure_map()`: Computes the pressure likelihood.
#' 5. If `"map_light"` is in the `config$geopressuretemplate$likelihood`:
#'     - `twilight_create() |> twilight_read() |> geolight_map()`: Computes the light likelihood.
#'
#' * **Graph Creation [`geopressuretemplate_graph()`]**: Builds a movement model graph
#' based `tag` data, and can include wind effects if specified. Outputs such as
#' marginal distributions, most likely paths, and simulation paths can be computed.

#' 1. `graph_create()`: Creates the graph based on tag.
#' 2. If `config$graph_set_movement$type == "gs"` (i.e., no wind):
#'     - `graph_set_movement()`: Sets the movement model without wind.
#' 3. If `config$graph_set_movement$type == "as"` (i.e., with wind):
#'     - `graph_add_wind()`: Adds wind data to the graph.
#'     - `graph_set_movement()`: Sets the movement model with wind.
#' 4. If `"marginal"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_marginal()`: Computes the marginal distribution map.
#' 5. If `"most_likely"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_most_likely()`: Computes the most likely path based on the movement model.
#' 6. If `"simulation"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_simulation()`: Runs simulations to model multiple possible paths.
#' 7. `save()`: Saves the computed graph and associated objects in `data/interim/{id}.Rdata`
#'
#' * **Pressure Path Processing [`geopressuretemplate_pressurepath()`]**: Computes pressurepaths
#' (`pressurepath_create`) using the content of the `Rdata` file and appending the pressurepath
#' data.frame to the same file.
#'
#' 1. If `"most_likely"` is in `config$geopressuretemplate$pressurepath`, computes the pressure path
#' for `path_most_likely`.
#' 2. If `"geopressureviz"` is in `config$geopressuretemplate$pressurepath` computes the pressure
#' path for `path_geopressureviz`
#'
#' Each of these child functions can be called individually or automatically as part of
#' the `geopressuretemplate` workflow.
#'
#'
#' @return
#' The function returns nothing. Instead, it saves the processed outputs (tag, graph,
#' pressure paths, etc.) to the specified `file`.
#'
#' @examples
#' \dontrun{
#' # Run the complete geopressuretemplate workflow
#' geopressuretemplate("18LX", quiet = TRUE)
#' }
#'
#' @family geopressuretemplate
#' @export
geopressuretemplate <- function(
    id,
    config = config::get(config = id),
    quiet = FALSE,
    file = glue::glue("./data/interim/{id}.RData"),
    ...) {
  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate for {id}")
  }

  # Create the tag
  geopressuretemplate_tag(
    id = id,
    config = config,
    quiet = quiet,
    saveit = FALSE,
    file = file,
    ...
  )

  # Create and process the graph using the computed or loaded tag
  geopressuretemplate_graph(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  geopressuretemplate_pressurepath(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  invisible(file)
}
