# Compute the most likely trajectory

Compute the trajectory which maximizes the joint probability using the
[Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm) on
the graph structure. For more details, see [section 2.3.1 of Nussbaumer
et al.
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0011-title)
and the [GeoPressureManual](https://bit.ly/4578D12).

## Usage

``` r
graph_most_likely(graph, quiet = FALSE)
```

## Arguments

- graph:

  a graph object.

- quiet:

  logical to hide messages about the progress.

## Value

Path data.frame containing the columns -`stap_id` stationary period

- `j` unique ID for each path, here always 1 as there is a single path.

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

[GeoPressureManual](https://bit.ly/4578D12)

Other graph:
[`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md),
[`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md),
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md),
[`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md),
[`print.graph()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.graph.md)

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

# Compute most likely path
path_most_likely <- graph_most_likely(graph, quiet = TRUE)

plot_path(path_most_likely)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[17.05,17.15,14.35,15.85,13.15],"lat":[48.9,47.65000000000001,44.65000000000001,41.65000000000001,41.25]}]]],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.9,47.65000000000001,44.65000000000001,41.65000000000001,41.25],[17.05,17.15,14.35,15.85,13.15],[10.34131237750732,5.744562646538029,5.449631621480024,5.421612021659069,7.125933808241013],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,true,true,true,true],"fillColor":"grey","fillOpacity":0.8},null,null,null,null,["#1, 8.8 days","#2, 0.8 days","#3, 0.7 days","#4, 0.7 days","#5, 2 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[41.25,48.9],"lng":[13.15,17.15]}},"evals":[],"jsHooks":[]}
```
