# Print a `bird` object

This function displays the information of a `bird` object.

## Usage

``` r
# S3 method for class 'bird'
print(x, ...)
```

## Arguments

- x:

  a GeoPressureR `bird` object.

- ...:

  arguments passed to other methods

## Value

`bird` is returned invisibly and unchanged

## See also

Other bird:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/bird_create.md)

## Examples

``` r
# Using AVONET dataset
bird_create("Acrocephalus arundinaceus")
#> 
#> ── GeoPressureR `bird` object ──────────────────────────────────────────────────
#> • Scientific name: Acrocephalus arundinaceus
#> • Mass: 0.03 (kg).
#> • Body frontal area: 0 (m^2).
#> • Wing span: 0.2 (m).
#> • Wing aspect: 7.1 (-).
```
