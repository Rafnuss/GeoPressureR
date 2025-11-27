# Compute exact astronomical twilights from a `path` (positions and dates)

This function computes the theoretical twilight (i.e., datetime of
sunrise and sunset) at given locations and for specific date.

We use the [suntools](https://github.com/adokter/suntools) package for
this.

By default (`solar_dep=0`), the computation returns sunrise and sunset.
But, it is also possible to compute different twilights by setting
depression angle value greater than 0 (6° for civil, 12° for nautical
and 18° for astronomical).

## Usage

``` r
path2twilight(path, date = NULL, solar_dep = 0, return_long = TRUE)
```

## Arguments

- path:

  a GeoPressureR `path` or `pressurepath` data.frame

- date:

  a vector of POSIXt datetime for which sunrise and sunset are computed.
  Be default, uses the range of `path$date` provided.

- solar_dep:

  a numerical value representing the solar depression angle used to
  compute sunrise and sunset.

- return_long:

  logical defining the format of the data.frame returned. If `TRUE`,
  returns the long format identical to
  [`twilight_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/twilight_create.md).
  If `FALSE`, return the sunrise and sunset as different column, making
  the data.frame the same size as `date`.

## Value

if `return_long == TRUE`, a `twilight` data.frame (same as
[`twilight_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/twilight_create.md))
with columns:

- `date` same as input `date`

- `twilight` date-time of twilight

- `rise` logical indicating sunrise (`TRUE`) or sunset (`FALSE`).

- `stap_id` same as `path$stap_id`

- `lat` same as `path$lat`

- `lon` same as `path$lon`

if `return_long == FALSE`, a data.frame with the same size of `date`
with columns:

- `date` same as input `date`

- `sunrise` date-time of sunrise

- `sunset` date-time of sunset

- `stap_id` same as `path$stap_id`

- `lat` same as `path$lat`

- `lon` same as `path$lon`

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html)
, [suntools](https://github.com/adokter/suntools)

## Examples

``` r
path <- data.frame(
  stap_id = c(1, 2, 3, 4, 5),
  j = c(1L, 1L, 1L, 1L, 1L),
  ind = c(1652L, 1603L, 1505L, 1609L, 1463L),
  lat = c(48.9, 47.5, 45.5, 41.5, 37.5),
  lon = c(17.05, 16.5, 14.5, 16.5, 13.5),
  start = as.POSIXct(
    c(1501113450, 1501888650, 1501987950, 1502075550, 1502151150),
    tzone = "UTC"
  ),
  end = as.POSIXct(c(1501876050, 1501961250, 1502046750, 1502133150, 1502323050), tzone = "UTC"),
  include = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  known = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  interp = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

twl <- path2twilight(path)
```
