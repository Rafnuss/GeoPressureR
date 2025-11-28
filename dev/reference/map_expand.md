# Construct grid from `extent` and `scale`

Take the two parameters defining the spatial grid of a map in
GeoPressureR (`extent` and `scale`) and constructs various possible
spatial variable of interest for the spatial grid (e.g., grid dimension,
latitude and longitude...)

This function is used in multiple functions of GeoPressureR to allow to
only store `extent` and `scale` in `tag` and `graph` while having access
to all spatial variables anywhere anytime.

## Usage

``` r
map_expand(extent, scale)
```

## Arguments

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

## Value

A list containing:

- `extent` same as input

- `scale` same as input

- `lat` vector of latitude

- `lon` vector of longitude

- `dim` vector of length 2 of the dimension of the map (number of pixel
  in lat and lon)

## Examples

``` r
str(map_expand(extent = c(0, 10, 0, 5), scale = 1))
#> List of 5
#>  $ extent: num [1:4] 0 10 0 5
#>  $ scale : num 1
#>  $ lat   : num [1:5] 4.5 3.5 2.5 1.5 0.5
#>  $ lon   : num [1:10] 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5
#>  $ dim   : num [1:2] 5 10

str(map_expand(extent = c(-16, 23, 0, 50), scale = 10))
#> List of 5
#>  $ extent: num [1:4] -16 23 0 50
#>  $ scale : num 10
#>  $ lat   : num [1:500] 50 49.8 49.8 49.7 49.5 ...
#>  $ lon   : num [1:390] -15.9 -15.9 -15.8 -15.6 -15.6 ...
#>  $ dim   : num [1:2] 500 390
```
