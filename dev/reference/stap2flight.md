# Compute flights from stationary periods

Convert a stationary period data.frame `stap` into a flight data.frame
or list. Flight are computed as the difference between the end of a
stationary period to the start of the next one. Because the
pressure/acceleration is labelled for "in flight", the bird was already
in flight before the first label and after the last label. We account
for this by adding to all flights duration half the temporal resolution
of the sensor.

You can compute the flight between specific stationary periods using
`include_stap_id`. In this case, the flight duration is computed as the
sum of individual flights in between.

You can return the flight as a data.frame or as a list if you want to
retrieve the information of all individual flight between the
`include_stap_id`.

## Usage

``` r
stap2flight(
  stap,
  include_stap_id = NULL,
  format = "df",
  units = "hours",
  return_numeric = TRUE
)
```

## Arguments

- stap:

  a stationary period data.frame (see
  [`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md)).

- include_stap_id:

  vector of the stationary period `stap_id` to consider in the flight.
  Default is to use `stap$stap_id[stap$include]` or `stap$stap_id` if
  `include` is not available in `stap`.

- format:

  character to return a list `"list"` or a data.frame `"df"` (see
  description)

- units:

  character string. Units in which the results are desired. Can be
  abbreviated. See [`difftime()`](https://rdrr.io/r/base/difftime.html)

- return_numeric:

  logical to return the duration as a numeric rather than with a
  duration format.

## Value

A list or a data.frame (see description) containing

- `start`: Start time of the (first) flight

- `end`: End time of the (last) flight

- `stap_id_s`: Source stap_id (i.e, start)

- `stap_id_t`: Target stap_id (i.e, end)

- `duration`: (Sum of the) duration of flight(s)

- (`n`: Number of flights) The value in brackets are only for the
  data.frame

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
})

# By default, return a data.frame of all individual flights
knitr::kable(stap2flight(tag$stap))
#> 
#> 
#> |start               |end                 | stap_s| stap_t| duration|  n|
#> |:-------------------|:-------------------|------:|------:|--------:|--:|
#> |2017-08-04 19:47:30 |2017-08-04 23:17:30 |      1|      2| 3.500000|  1|
#> |2017-08-05 19:27:30 |2017-08-06 02:52:30 |      2|      3| 7.416667|  1|
#> |2017-08-06 19:12:30 |2017-08-07 03:12:30 |      3|      4| 8.000000|  1|
#> |2017-08-07 19:12:30 |2017-08-08 00:12:30 |      4|      5| 5.000000|  1|

# Compute the total flight between stap 1,3 and 5. Sum flight duration in between.
knitr::kable(stap2flight(tag$stap, include_stap_id = c(1, 3, 5)))
#> 
#> 
#> |   |start               |end                 | stap_s| stap_t| duration|  n|
#> |:--|:-------------------|:-------------------|------:|------:|--------:|--:|
#> |1  |2017-08-04 19:47:30 |2017-08-06 02:52:30 |      1|      3| 10.91667|  2|
#> |3  |2017-08-06 19:12:30 |2017-08-08 00:12:30 |      3|      5| 13.00000|  2|

# Can also return as a list of data.frame to access individual flights information.
knitr::kable(stap2flight(tag$stap,
  include_stap_id = c(1, 3, 5), format = "list",
  units = "secs"
))
#> 
#> 
#> |start               |end                 | stap_s| stap_t| duration|
#> |:-------------------|:-------------------|------:|------:|--------:|
#> |2017-08-04 19:47:30 |2017-08-04 23:17:30 |      1|      2|    12600|
#> |2017-08-05 19:27:30 |2017-08-06 02:52:30 |      2|      3|    26700|
#> 
#> |   |start               |end                 | stap_s| stap_t| duration|
#> |:--|:-------------------|:-------------------|------:|------:|--------:|
#> |3  |2017-08-06 19:12:30 |2017-08-07 03:12:30 |      3|      4|    28800|
#> |4  |2017-08-07 19:12:30 |2017-08-08 00:12:30 |      4|      5|    18000|
```
