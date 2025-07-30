#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_graph <- function(
    id,
    config = config::get(config = id),
    quiet = FALSE,
    file = glue::glue("./data/interim/{id}.RData"),
    ...) {
  save_list <- load(file)

  tag <- get("tag")

  if (tag$param$id != id) {
    cli::cli_abort(c(x = "{.var id}={id} is different from {.var tag$param$id}={tag$param$id}."))
  }

  tag_assert(tag)

  config <- geopressuretemplate_config(id,
    config = config,
    assert_graph = TRUE,
    ...
  )

  if (!all(config$geopressuretemplate$likelihood %in% names(tag))) {
    cli::cli_abort(c(
      x = "{.var geopressuretemplate$likelihood}={.val {config$geopressuretemplate$likelihood}}
      {?is/are} not present{?s} in {.var tag}."
    ))
  }

  # Create the geospatial graph using the provided or default parameters
  if (!quiet) {
    cli::cli_h2("Create Graph")
  }
  graph <- do.call(graph_create, c(
    list(
      tag = tag,
      quiet = quiet
    ),
    config$graph_create
  ))

  # Set the movement model based on the configuration
  tryCatch(
    {
      if (eval(config$graph_set_movement$type) == "gs") {
        # Without wind speed
        graph <- do.call(graph_set_movement, c(
          list(graph = graph),
          config$graph_set_movement
        ))
      } else {
        if (!quiet) {
          cli::cli_h2("Add wind to graph")
        }
        # With wind speed
        graph <- do.call(graph_add_wind, c(
          list(
            graph = graph,
            pressure = tag$pressure,
            quiet = quiet
          ),
          config$graph_add_wind
        ))

        bird <- do.call(bird_create, config$bird_create)

        graph <- do.call(graph_set_movement, c(
          list(
            graph = graph,
            bird = bird
          ),
          config$graph_set_movement
        ))
      }
    },
    error = function(e) {
      cli::cli_bullets(c(
        "x" = "{e$message}",
        "i" = "Error while defining the movement model.{.var graph} is return.",
        ">" = "Debug line by line by opening {.code edit(geopressuretemplate_graph)}"
      ))
      graph
    }
  )


  # Store the graph parameters
  param <- graph$param # nolint

  # Initialize a list to keep track of outputs to be saved
  save_list <- c("tag", "param")

  tryCatch(
    {
      # Compute the marginal distribution if requested
      if ("marginal" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute marginal map")
        }
        marginal <- graph_marginal(graph, quiet = quiet) # nolint
        save_list <- c(save_list, "marginal")
      }

      # Compute the most likely path if requested
      if ("most_likely" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute most likely path")
        }
        path_most_likely <- graph_most_likely(graph, quiet = quiet)
        edge_most_likely <- path2edge(path_most_likely, graph) # nolint
        save_list <- c(save_list, "path_most_likely", "edge_most_likely")
      }

      # Compute simulations if requested
      if ("simulation" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute simulation paths")
        }
        path_simulation <- graph_simulation(
          graph,
          nj = config$graph_simulation$nj,
          quiet = quiet
        )
        edge_simulation <- path2edge(path_simulation, graph) # nolint
        save_list <- c(save_list, "path_simulation", "edge_simulation")
      }
    },
    error = function(e) {
      cli::cli_bullets(c(
        "x" = "{e$message}",
        "x" = "Error while computing the outputs. {.var graph} is returned.",
        ">" = "Debug line by line by opening {.code edit(geopressuretemplate_graph)}"
      ))
      graph
    }
  )

  # Add path_geopressureviz if it exists as a csv
  file_path_geopressureviz <- glue::glue("./data/interim/path_geopressureviz_{id}.csv")
  if (file.exists(file_path_geopressureviz)) {
    path_geopressureviz <- utils::read.csv(file_path_geopressureviz) # nolint
    save_list <- c(save_list, "path_geopressureviz")
  }

  # Save the outputs to the specified file
  save(
    list = save_list,
    file = file
  )

  # Return the file path invisibly
  invisible(file)
}
