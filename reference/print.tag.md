# Print a `tag` object

This function displays the information of a `tag` object.

## Usage

``` r
# S3 method for class 'tag'
print(x, ...)
```

## Arguments

- x:

  a GeoPressureR `tag` object

- ...:

  further arguments passed to or from other methods.

## Value

`tag` is returned invisibly and unchanged

## See also

Other tag:
[`tag_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_create.md),
[`tag_set_map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_set_map.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)
})

print(tag)
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
#> ✖ No stationary periods defined yet. Use `tag_label()`
```
