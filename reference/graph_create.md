# Create a `graph` object

This function returns a trellis graph representing the trajectory of a
bird based on filtering and pruning the likelihood maps provided.

In the final graph, we only keep the likely nodes (i.e., position of the
bird at each stationary periods) defined as (1) those whose likelihood
value are within the threshold of percentile `thr_likelihood` of the
total likelihood map and (2) those which are connected to at least one
edge of the previous and next stationary periods requiring an average
ground speed lower than `thr_gs` (in km/h).

For more details and illustration, see [section 2.2 of Nussbaumer et al.
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0004-title)
and the [GeoPressureManual](https://bit.ly/3saLVqi)

## Usage

``` r
graph_create(
  tag,
  thr_likelihood = 0.99,
  thr_gs = 150,
  likelihood = NULL,
  quiet = FALSE,
  geosphere_dist = lifecycle::deprecated(),
  geosphere_bearing = lifecycle::deprecated(),
  workers = lifecycle::deprecated()
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- thr_likelihood:

  threshold of percentile (see details).

- thr_gs:

  threshold of groundspeed (km/h) (see details).

- likelihood:

  Field of the `tag` list containing the likelihood map (character).
  Possible value are `map_pressure`, `map_light`, `map_pressure_mse`,
  `map_pressure_mse`, `map_pressure_mse`, `mask_water`. Default `NA` is
  to take the product of `map_pressure` and `map_light`, or if not
  available, taking the first of the possible values.

- quiet:

  logical to hide messages about the progress.

- geosphere_dist:

  **\[deprecated\]** This argument is no longer used. Distance
  calculations now use a custom memory-efficient Haversine
  implementation.

- geosphere_bearing:

  **\[deprecated\]** This argument is no longer used. Bearing
  calculations now use a custom memory-efficient implementation.

- workers:

  **\[deprecated\]** This argument is no longer used. Parallel
  processing has been removed to avoid memory issues.

## Value

Graph as a list

- `s`: source node (index in the 3d grid lat-lon-stap)

- `t`: target node (index in the 3d grid lat-lon-stap)

- `gs`: average ground speed required to make that transition (km/h) as
  complex number representing the E-W as real and S-N as imaginary

- `obs`: observation model, corresponding to the normalized likelihood
  in a 3D matrix of size `sz`

- `sz`: size of the 3d grid lat-lon-stap

- `stap`: data.frame of all stationary periods (same as `tag$stap`)

- `equipment`: node(s) of the first stap (index in the 3d grid
  lat-lon-stap)

- `retrieval`: node(s) of the last stap (index in the 3d grid
  lat-lon-stap)

- `mask_water`: logical matrix of water-land

- `param`: list of parameters including `thr_likelihood` and `thr_gs`
  (same as `tag$param`)

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and
Daniel Sheldon. 2023. Reconstructing bird trajectories from pressure and
wind data using a highly optimized hidden Markov model. *Methods in
Ecology and Evolution*, 14, 1118–1129
<https://doi.org/10.1111/2041-210X.14082>.

## See also

[GeoPressureManual](https://bit.ly/3saLVqi)

Other graph:
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_marginal.md),
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
graph <- graph_create(tag, thr_likelihood = 0.95, thr_gs = 100, quiet = TRUE)

print(graph)
#> 
#> ── GeoPressureR `graph` object for 18LX ────────────────────────────────────────
#> Note: All green texts are fields of `graph` (i.e., `graph$field`).
#> 
#> ── Parameters param 
#> Run `graph$param` to display all parameters
#> 
#> ── Stationary periods stap 
#> stap_id | start               | end                 | known_lat | known_lon | include
#> 1       | 2017-07-27 00:00:00 | 2017-08-04 19:47:30 | 48.9      | 17.05     | TRUE   
#> 2       | 2017-08-04 23:17:30 | 2017-08-05 19:27:30 | NA        | NA        | TRUE   
#> ...
#> 5       | 2017-08-08 00:12:30 | 2017-08-09 23:57:30 | NA        | NA        | TRUE   
#> Run `tag$stap` to see full stap table
#> 
#> ── Map 
#> • Extent (W, E, S, N): -16°, 23°, 0°, 50°
#> • Dimensions (lat x lon): 500 x 390 (res. 0.1°)
#> 
#> ── Graph size 
#> • 1 equipement node
#> • 51 retrieval nodes
#> • 1,908 nodes
#> • 846,319 edges
#> 
#> ── Movement model 
#> ! Windspeed not computed. Use `graph_add_wind()`
#> ✖ No movement model defined. Use `graph_set_movement()`
```
