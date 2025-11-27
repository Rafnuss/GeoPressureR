# Extract a likelihood `map` from a `tag`

Extract a likelihood `map` from a `tag`

## Usage

``` r
tag2map(tag, likelihood = NULL)
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- likelihood:

  Field of the `tag` list containing the likelihood map (character).
  Possible value are `map_pressure`, `map_light`, `map_pressure_mse`,
  `map_pressure_mse`, `map_pressure_mse`, `mask_water`. Default `NA` is
  to take the product of `map_pressure` and `map_light`, or if not
  available, taking the first of the possible values.

## Value

Likelihood map
