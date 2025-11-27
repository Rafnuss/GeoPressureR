# Construct a SpatRaster from a `map`

This function convert a GeoPressureR `map` object into a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with the data of each stationary periods stored in a different layer.

## Usage

``` r
rast.map(x, names = glue::glue("#{map$stap$stap_id}"), crs = "epsg:4326", ...)
```

## Arguments

- x:

  a GeoPressureR `map` object

- names:

  names of the SpatRaster layers created. See
  [`terra::names`](https://rspatial.github.io/terra/reference/names.html).

- crs:

  character. Description of the Coordinate Reference System (map
  projection) in `PROJ.4`, `WKT` or `authority:code` notation. See
  [`crs`](https://rspatial.github.io/terra/reference/crs.html). If this
  argument is missing, and the x coordinates are within -360 .. 360 and
  the y coordinates are within -90 .. 90, longitude/latitude is assigned

- ...:

  additional parameters for
  [`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)

## Value

A
[terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
object.

## See also

Other map:
[`map_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/map_create.md),
[`print.map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/print.map.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4
    ) |>
    geopressure_map(quiet = TRUE)
})

rast.map(tag$map_pressure)
#> class       : SpatRaster 
#> size        : 200, 156, 5  (nrow, ncol, nlyr)
#> resolution  : 0.25, 0.25  (x, y)
#> extent      : -16, 23, 0, 50  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :         #1,         #2,         #3,         #4,         #5 
#> min values  : 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000 
#> max values  : 0.00406242, 0.05795874, 0.08299387, 0.07016245, 0.02307327 
#> time        : 2017-07-27 to 2017-08-08 00:12:30 UTC (5 steps) 
```
