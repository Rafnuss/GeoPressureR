# Configure the spatial and temporal parameters of the `map` of a `tag` object

This function adds to `tag` the parameters defining the 3D grid of the
map. The spatial parameters (`extent` and `scale`) define the
**geographical dimensions of the map**, and the **temporal dimension**
is defined based on the stationary periods built using the labels.
`include_stap_id` and `include_min_duration` can be used to limit which
stationary periods are computed and modelled in the analysis. By
default, all stationary periods are included.

In addition, `tag` offers the possibility to define `known` locations
(e.g., equipment or retrieval site). These can only be defined at the
level of a stationary period (i.e., assuming constant position during
the whole stationary period) but you can define as many known stationary
periods as you wish. Because the index of the last stationary period is
generally unknown, you can use negative indexing in `known`, i.e.,
`known$stap_id = -1` will be converted to `nrow(tag$stap)`.

By default, no likelihood map will be computed for these stationary
periods and the trajectory model will be more constrained, saving
significant computational time. You can change this using the
`compute_known` parameter in
[`geopressure_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md).

## Usage

``` r
tag_set_map(
  tag,
  extent,
  scale = 10,
  known = data.frame(stap_id = integer(), known_lat = double(), known_lon = double()),
  include_stap_id = NULL,
  include_min_duration = 0
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- extent:

  geographical extent of the map on which the likelihood and graph model
  will be computed. Vector of length 4 `c(xmin, xmax, ymin, ymax)` or
  `c(W, E, S, N)`.

- scale:

  number of pixels per 1° latitude-longitude. For instance, `scale = 10`
  for a resolution of 0.1° (~10km) and `scale=4` for a resolution of
  0.25° (~30km). To avoid interpolating the ERA5 data, the scale should
  be equal to or smaller than 10. Read more about scale on the [Google
  earth Engine
  documentation](https://developers.google.com/earth-engine/guides/scale)
  .

- known:

  data.frame containing the known positions of the bird (e.g., equipment
  or retrieval site) with columns `stap_id`, `known_lat` and
  `known_lon`. You can set position of the last stationary period using
  `stap_id = -1`. Also accept list which are converted as data.frame.

- include_stap_id:

  vector of `stap_id` defining which stationary period to model, that
  is, to compute in the likelihood map and use in the graph.

- include_min_duration:

  minimum duration threshold of stationary periods to include (in
  hours).

## Value

A GeoPressureR `tag` object with:

- `stap`: Data.frame of all stationary periods with three new columns:
  `known_lat` and `known_lon` define the known position during these
  stationary periods, and `include` defines whether the likelihood map
  of this stationary period should be computed and later used in the
  graph.

- `extent` same as input parameter `extent`

- `scale` same as input parameter `scale`

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/pressure-map.html#define-geographical-and-temporal-parameters-of-the-map)

Other tag:
[`print.tag()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.tag.md),
[`tag_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_create.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
})

# Default tag
tag_default <- tag_set_map(tag, c(-16, 23, 0, 50))

print(tag_default)
#> 
#> ── GeoPressureR `tag` object for 18LX ──────────────────────────────────────────
#> Note: All green texts are fields of `tag` (i.e., `tag$field`).
#> 
#> ── Parameter param 
#> Run `tag$param` to display all parameters
#> 
#> ── Sensors data 
#> Manufacturer: soi
#> Date range: 2017-06-20 to 2017-08-09 23:55:00
#> • pressure: 672 datapoints (30min)
#> • acceleration: 4,032 datapoints (5min)
#> • light: 4,032 datapoints (5min)
#> • temperature_external: 2,448 datapoints (30min)
#> 
#> ── Stationary periods stap 
#> stap_id | start               | end                 | known_lat | known_lon | include
#> 1       | 2017-07-27 00:00:00 | 2017-08-04 19:47:30 | NA        | NA        | TRUE   
#> 2       | 2017-08-04 23:17:30 | 2017-08-05 19:27:30 | NA        | NA        | TRUE   
#> ...
#> 5       | 2017-08-08 00:12:30 | 2017-08-09 23:57:30 | NA        | NA        | TRUE   
#> Run `tag$stap` to see full stap table
#> 
#> ── Map 
#> • Extent (W, E, S, N): -16°, 23°, 0°, 50°
#> • Dimensions (lat x lon): 500 x 390 (res. 0.1°)
#> ✖ No pressure likelihood computed yet. Use `geopressure_map()`.

# Customized tag, with coarse grid scale, known position for the first stationary
#  period and considering only the stationary periods lasting more than 20hours.
tag_custom <- tag_set_map(tag,
  extent = c(-16, 23, 0, 50),
  scale = 1,
  include_min_duration = 20,
  known = data.frame(
    stap_id = 1,
    known_lon = 17.05,
    known_lat = 48.9
  )
)

print(tag_custom)
#> 
#> ── GeoPressureR `tag` object for 18LX ──────────────────────────────────────────
#> Note: All green texts are fields of `tag` (i.e., `tag$field`).
#> 
#> ── Parameter param 
#> Run `tag$param` to display all parameters
#> 
#> ── Sensors data 
#> Manufacturer: soi
#> Date range: 2017-06-20 to 2017-08-09 23:55:00
#> • pressure: 672 datapoints (30min)
#> • acceleration: 4,032 datapoints (5min)
#> • light: 4,032 datapoints (5min)
#> • temperature_external: 2,448 datapoints (30min)
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
#> • Dimensions (lat x lon): 50 x 39 (res. 1°)
#> ✖ No pressure likelihood computed yet. Use `geopressure_map()`.
```
