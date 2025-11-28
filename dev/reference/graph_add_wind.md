# Compute windspeed and airspeed on a `graph`

Reads the NetCDF files downloaded and interpolate the average windspeed
experienced by the bird on each possible edge, as well as the
corresponding airspeed.

In addition, the graph can be further pruned based on a threshold of
airspeed `thr_as`.

See section [2.2.4 in Nussbaumer
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0008-title)
for more technical details and the
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
for an illustration on how to use it.

## Usage

``` r
graph_add_wind(graph, thr_as = Inf, ...)
```

## Arguments

- graph:

  a GeoPressureR graph object.

- thr_as:

  threshold of airspeed (km/h).

- ...:

  Arguments passed on to
  [`edge_add_wind`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/edge_add_wind.md)

  `pressure`

  :   pressure measurement of the associated `tag` data used to estimate
      the pressure level (i.e., altitude) of the bird during the
      flights. This data.frame needs to contain `date` as POSIXt and
      `value` in hPa. If not provided, uses `graph$pressure`, assuming
      that argument `graph` is a GeoPressureR `tag` object.

  `rounding_interval`

  :   temporal resolution on which to query the variable (min). Default
      is to match ERA5 native resolution (1hr).

  `interp_spatial_linear`

  :   logical to interpolate the variable linearly over space, if
      `FALSE` takes the nearest neighbour. ERA5 native resolution is
      0.25°

  `quiet`

  :   logical to hide messages about the progress

  `file`

  :   absolute or relative path of the ERA5 wind data file to be
      downloaded. Function taking as arguments (1) the stationary period
      identifier and (2) the tag_id.

## Value

A `graph` object with windspeed and airspeed as `ws` and `as`
respectively.

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and
Daniel Sheldon. 2023. Reconstructing bird trajectories from pressure and
wind data using a highly optimized hidden Markov model. *Methods in
Ecology and Evolution*, 14, 1118–1129
[doi:10.1111/2041-210X.14082](https://doi.org/10.1111/2041-210X.14082) .

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)

Other graph:
[`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md),
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md),
[`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md),
[`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md),
[`print.graph()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.graph.md)
