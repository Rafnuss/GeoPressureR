# Simulate randomly multiple trajectories

This function randomly simulates multiple trajectories from a graph
using the forward filtering backward sampling algorithm. For more
details, see [section 2.3.3 of Nussbaumer et al.
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0013-title)
and the [GeoPressureManual](https://bit.ly/3YE83Wn).

## Usage

``` r
graph_simulation(graph, nj = 10, quiet = FALSE)
```

## Arguments

- graph:

  a graph object.

- nj:

  Number of simulations.

- quiet:

  logical to hide messages about the progress.

## Value

Path data.frame containing the columns -`stap_id` stationary period

- `j` unique ID for each simulation.

- `ind` indices of the coordinate in the 2D grid. Useful to retrieve map
  or graph information

- `lat` latitude,

- `lon` longitude

- `start` datetime of the start of the stationary period (same as in
  `stap`)

- `end` datetime of the end of the stationary period (same as in `stap`)

- `include` logical if stationary period was modelled (same as in
  `stap`)

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and
Daniel Sheldon. 2023. Reconstructing bird trajectories from pressure and
wind data using a highly optimized hidden Markov model. *Methods in
Ecology and Evolution*, 14, 1118–1129
[doi:10.1111/2041-210X.14082](https://doi.org/10.1111/2041-210X.14082) .

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory.html#product-3-simulated-paths)

Other graph:
[`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_add_wind.md),
[`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_create.md),
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_marginal.md),
[`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_most_likely.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_set_movement.md),
[`print.graph()`](https://raphaelnussbaumer.com/GeoPressureR/reference/print.graph.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read() |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
    ) |>
    geopressure_map(quiet = TRUE) |>
    geolight_map(quiet = TRUE)
})

# Create graph
graph <- graph_create(tag, quiet = TRUE)

# Define movement model
graph <- graph_set_movement(graph)

# Compute simulations
path_simulation <- graph_simulation(graph, quiet = TRUE)

plot_path(path_simulation, plot_leaflet = FALSE)

```
