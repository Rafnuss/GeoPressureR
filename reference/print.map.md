# Print a `map` object

This function displays the information of a `map` object.

## Usage

``` r
# S3 method for class 'map'
print(x, ...)
```

## Arguments

- x:

  a GeoPressureR `map` object

- ...:

  arguments passed to other methods

## Value

`map` is returned invisibly and unchanged

## See also

Other map:
[`map_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/map_create.md),
[`rast.map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/rast.map.md)

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

print(tag$map_pressure)
#> 
#> ── GeoPressureR `map` object  of pressure for 18LX ─────────────────────────────
#> 
#> ── Map 
#> • Extent (W, E, S, N): -16°, 23°, 0°, 50°
#> • Dimensions (lat x lon): 200 x 156 (res. 0.25°)
#> 
#> ── Stationary periods stap (n=5) 
#> Run `map$stap` to display full table
```
