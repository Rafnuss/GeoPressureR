library(testthat)
library(GeoPressureR)

# Set working directory
setwd(system.file("extdata", package = "GeoPressureR"))

test_that("workflow | full", {
  geopressuretemplate("18LX", quiet = TRUE)

  file <- glue::glue("./data/interim/18LX.RData")
  save_list <- load(file)
  tag <- get("tag")
  path_most_likely <- get("path_most_likely")
  pressurepath <- get("pressurepath_most_likely")

  path <- tag2path(tag)
  expect_no_error(tag2path(tag, interp = 0.7))

  expect_no_error(print(tag))

  expect_no_error(plot(tag, type = "pressure"))
  expect_no_error(plot(tag, type = "pressure", plot_plotly = FALSE))
  expect_no_error(plot(tag, type = "light"))
  expect_no_error(plot(tag, type = "light", plot_plotly = FALSE))
  expect_no_error(plot(tag, type = "acceleration"))
  expect_no_error(plot(tag, type = "acceleration", variable = "pitch"))
  expect_no_error(plot(tag, type = "acceleration", plot_plotly = FALSE))
  expect_no_error(plot(
    tag,
    type = "acceleration",
    variable = "pitch",
    plot_plotly = FALSE
  ))
  expect_no_error(plot(tag, type = "twilight"))
  expect_no_error(plot(tag, type = "twilight", plot_plotly = FALSE))
  expect_no_error(plot(tag, type = "temperature"))
  expect_no_error(plot(tag, type = "temperature", variable = "external"))
  expect_no_error(plot(tag, type = "temperature", plot_plotly = FALSE))
  expect_no_error(plot(
    tag,
    type = "temperature",
    variable = "external",
    plot_plotly = FALSE
  ))
  expect_no_error(plot(tag, type = "actogram"))
  expect_no_error(plot(tag, type = "actogram", plot_plotly = FALSE))
  expect_no_error(plot(tag, type = "map_pressure"))
  expect_no_error(plot(tag, type = "map_light"))
  expect_no_error(plot(tag, type = "map"))
  plot(tag, type = "map", plot_leaflet = FALSE)

  expect_no_error(plot_pressurepath(pressurepath, type = "timeseries"))
  expect_no_error(plot_pressurepath(pressurepath, type = "altitude"))
  expect_no_error(plot_pressurepath(pressurepath, type = "histogram"))

  expect_no_error(plot_path(path_most_likely))
  expect_no_error(plot_path(path_most_likely, plot_leaflet = FALSE))
})

test_that("workflow | Missing pressure value", {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
  tag$pressure <- subset(tag$pressure, stap_id == 3 | stap_id == 4)

  tag <- tag_set_map(tag, extent = c(-16, 23, 0, 50), scale = 1)

  expect_warning(
    {
      expect_warning(
        {
          tag <- geopressure_map(tag, quiet = TRUE)
        },
        "*have less than 3 datapoints to be used*"
      )
    },
    "Pressure data is not on a regular interval"
  )

  expect_equal(
    sapply(tag$map_pressure$data, is.null),
    c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  expect_no_error(tag2path(tag))

  expect_error({
    graph <- graph_create(tag, quiet = TRUE)
  })

  tag$stap$include <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
  expect_no_error({
    graph <- graph_create(tag, quiet = TRUE)
  })
})


test_that("workflow | with elev_", {
  tag <- tag_create("18LX", quiet = TRUE)
  tag <- tag_label(
    tag,
    file = "./data/tag-label/18LX-labeled-elev.csv",
    quiet = TRUE
  )
  tag <- tag_set_map(tag, extent = c(-16, 23, 0, 50), scale = 1)
  tag <- geopressure_map(tag, quiet = TRUE)
  expect_equal(length(tag$map_pressure), nrow(tag$stap))
})


test_that("workflow | modelled fewer", {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
  tag <- tag_set_map(
    tag,
    extent = c(-16, 23, 0, 50),
    scale = 1,
    include_stap_id = c(2, 4, 5)
  )
  tag <- geopressure_map(tag, quiet = TRUE)

  path <- tag2path(tag)

  graph <- graph_create(tag, quiet = TRUE)
  graph <- graph_set_movement(graph)

  marginal <- graph_marginal(graph, quiet = TRUE)
  path <- graph_most_likely(graph, quiet = TRUE)
  sim <- graph_simulation(graph, quiet = TRUE)
  edge <- path2edge(path, graph)
  edge_sim <- path2edge(sim, graph)

  # gts <- pressurepath_create(tag)
})
