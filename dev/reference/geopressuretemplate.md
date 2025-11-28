# Workflow for GeoPressureR

The `geopressuretemplate()` function manages the complete workflow for
modelling bird trajectories using geolocator data. It includes the
creation `tag` object, the construction of the likelihood maps (pressure
and/or light), the creation of the graph model construction, and,
finally, the estimation of the trajectory products and the pressure path
computation. Read the [Workflow chapter in the
GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html)
for more information.

## Usage

``` r
geopressuretemplate(
  id,
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
)

geopressuretemplate_config(
  id,
  config = config::get(config = id),
  assert_tag = TRUE,
  assert_graph = FALSE,
  ...
)

geopressuretemplate_graph(
  id,
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
)

geopressuretemplate_pressurepath(
  id,
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
)

geopressuretemplate_tag(
  id,
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
)
```

## Arguments

- id:

  unique identifier of a tag.

- config:

  configuration object specifying workflow parameters, which is loaded
  by default using `config::get(config = id)`.

- quiet:

  Logical. If `TRUE`, suppresses informational messages during
  execution. The default value is `FALSE`.

- file:

  A file path to save the intermediate results (e.g., tag, graph, and
  pressure paths). Default is `./data/interim/{id}.RData`.

- ...:

  Additional parameters to overwrite default or config values. Always
  prefer to modify `config.yml` if possible.

- assert_tag:

  Logical. If `TRUE`, check that the config is compatible for the
  creation of a tag. The default value is `TRUE`.

- assert_graph:

  Logical. If `TRUE`, check that the config is compatible for the
  creation of a graph. The default value is `TRUE`. Set to `FALSE` only
  if you don't want to create a graph model

## Value

The function returns nothing. Instead, it saves the processed outputs
(tag, graph, pressure paths, etc.) to the specified `file`.

## Details

The `geopressuretemplate` function is a high-level entry point that
coordinates multiple steps for processing the geolocator data and
produce trajectories. It relies on underlying child functions for each
step:

- **Tag Creation `geopressuretemplate_tag()`**: Initializes and labels
  the `tag` object. It also generates light and pressure likelihood
  maps:

1.  [`tag_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_create.md):
    Initializes the tag object.

2.  [`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label.md):
    Adds labels.

3.  [`tag_set_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_set_map.md):
    Sets the spatial and temporal parameters.

4.  If `"map_pressure"` is in the
    `config$geopressuretemplate$likelihood`:

    - [`geopressure_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md):
      Computes the pressure likelihood.

5.  If `"map_light"` is in the `config$geopressuretemplate$likelihood`:

    - `twilight_create() |> twilight_read() |> geolight_map()`: Computes
      the light likelihood.

- **Graph Creation `geopressuretemplate_graph()`**: Builds a movement
  model graph based `tag` data, and can include wind effects if
  specified. Outputs such as marginal distributions, most likely paths,
  and simulation paths can be computed.

1.  [`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md):
    Creates the graph based on tag.

2.  If `config$graph_set_movement$type == "gs"` (i.e., no wind):

    - [`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md):
      Sets the movement model without wind.

3.  If `config$graph_set_movement$type == "as"` (i.e., with wind):

    - [`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md):
      Adds wind data to the graph.

    - [`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md):
      Sets the movement model with wind.

4.  If `"marginal"` is in `config$geopressuretemplate$outputs`:

    - [`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md):
      Computes the marginal distribution map.

5.  If `"most_likely"` is in `config$geopressuretemplate$outputs`:

    - [`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md):
      Computes the most likely path based on the movement model.

6.  If `"simulation"` is in `config$geopressuretemplate$outputs`:

    - [`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md):
      Runs simulations to model multiple possible paths.

7.  [`save()`](https://rdrr.io/r/base/save.html): Saves the computed
    graph and associated objects in `data/interim/{id}.Rdata`

- **Pressure Path Processing `geopressuretemplate_pressurepath()`**:
  Computes pressurepaths (`pressurepath_create`) using the content of
  the `Rdata` file and appending the pressurepath data.frame to the same
  file.

1.  If `"most_likely"` is in `config$geopressuretemplate$pressurepath`,
    computes the pressure path for `path_most_likely`.

2.  If `"geopressureviz"` is in
    `config$geopressuretemplate$pressurepath` computes the pressure path
    for `path_geopressureviz`

Each of these child functions can be called individually or
automatically as part of the `geopressuretemplate` workflow.

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run the complete geopressuretemplate workflow
geopressuretemplate("18LX")

# Or run step-by-step
# you can check that all the parameters are correctly set in the config file
geopressuretemplate_config(id)
# 1. creation of the tag
tag <- geopressuretemplate_tag("18LX")
# 2. creation of the graph
geopressuretemplate_graph("18LX")
# 3. Computation of the pressurepath
geopressuretemplate_pressurepath("18LX")
} # }
```
