# Create a `param` list

Create the list of parameter for GeoPressureR `tag` and `graph` objects.

`param` list are mostly used to archived the actual value of parameters
used to create a `tag` and/or a `graph`, thus allowing for examination
of parameters post-creation. This function should therefore not be used
to set/define parameters ahead of computation. In reality, there are
very few external case of use for this function.

## Usage

``` r
param_create(id, default = FALSE, ...)
```

## Arguments

- id:

  Unique identifier of a tag.

- default:

  logical to initiate param with default value of the package.

- ...:

  arguments passed to other methods.

## Value

A GeoPressureR `param` list

## See also

Other param:
[`print.param()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.param.md)

## Examples

``` r
param <- param_create("18LX", default = TRUE)
print(param)
#> 
#> ── GeoPressureR `param` object for id: 18LX ────────────────────────────────────
#> Note: All green texts are fields of `param` (i.e., `param$field`).
#> • GeoPressureR_version: 3.4.5.9000
#> 
#> ── Sensors data `tag_create()` 
#> • manufacturer:
#> • crop_start:
#> • crop_end:
#> • directory: `glue::glue("./data/raw-tag/{id}")`
#> • pressure_file:
#> • light_file:
#> • acceleration_file:
#> • temperature_external_file:
#> • temperature_internal_file:
#> • magnetic_file:
#> 
#> ── Tag label `tag_label()` 
#> • file: `glue::glue("./data/tag-label/{tag$param$id}-labeled.csv")`
#> 
#> ── Stationary period definition `tag_set_map()` 
#> • extent: [W:, E:, S:, N:]
#> • scale: 10
#> • known: `data.frame(stap_id = integer(), known_lat = double(), known_lon =
#>   double())`
#> • include_stap_id:
#> • include_min_duration: 0
#> 
#> ── Geopressure `geopressure_map()` 
#> • max_sample: 250
#> • margin: 30
#> • sd: 1
#> • thr_mask: 0.9
#> • log_linear_pooling_weight: `function(n) log(n)/n`
#> 
#> ── Twilight & Geolight `twilight_create()` `geolight_map()` 
#> • twl_thr:
#> • twl_offset:
#> • twilight_file:
#> • twl_calib_adjust: 1.4
#> • twl_llp: `function(n) log(n)/n`
#> 
#> ── Graph `graph_create()` 
#> • thr_likelihood: 0.99
#> • thr_gs: 150
#> 
#> ── Movement model & wind `graph_add_wind()` `graph_movement()` 
#> • thr_as: Inf
#> • file: `function(stap_id, tag_id) {
#>   glue::glue("./data/wind/{tag_id}/{tag_id}_{stap_id}.nc")}`
#> • type: `ifelse("ws" %in% names(graph), "as", "gs")`
#> • method: `ifelse("ws" %in% names(graph), "power", "gamma")`
#> • shape: 7
#> • scale: 7
#> • location: 40
#> • bird_create:
#>   scientific_name:
#>   mass:
#>   wing_span:
#>   wing_aspect:
#>   wing_area:
#>   body_frontal_area:
#> • power2prob: `function(power) (1/power)^3`
#> • low_speed_fix: 15
#> • zero_speed_ratio: 1
#> 
#> ── Outputs `graph_simulation()` `pressurepath_create()` 
#> • nj: 10
#> • variable: `c("altitude", "surface_pressure")`
#> • solar_dep: 0
#> • era5_dataset: "both"
```
