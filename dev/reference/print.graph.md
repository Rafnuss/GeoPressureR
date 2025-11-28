# Print a `graph` object

This function displays the information of a `graph` object.

## Usage

``` r
# S3 method for class 'graph'
print(x, ...)
```

## Arguments

- x:

  a GeoPressureR `graph` object.

- ...:

  arguments passed to other methods

## Value

`graph` is returned invisibly and unchanged

## See also

Other graph:
[`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md),
[`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md),
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md),
[`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md),
[`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md)

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

graph <- graph_create(tag, quiet = TRUE)

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
#> • 79 retrieval nodes
#> • 3,048 nodes
#> • 2,253,679 edges
#> 
#> ── Movement model 
#> ! Windspeed not computed. Use `graph_add_wind()`
#> ✖ No movement model defined. Use `graph_set_movement()`
```
