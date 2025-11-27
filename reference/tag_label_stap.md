# Create stationary periods from a tag label

This function computes the stationary periods from the pressure and/or
acceleration label data. In most case, this function should be run
directly after
[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_read.md)
in order to update `tag` to correspond to the new label file.

A stationary period is a period during which the bird is considered
static relative to the spatial resolution of interest (~10-50km). They
are defined by being separated by a flight of any duration (label
`"flight"`). The `stap_id` is an integer value for stationary periods
and decimal value for flight. The `stap_id` is added as a new column to
each sensor data.

If an acceleration data.frame is present and contains a column `label`,
the stationary period will be computed from it, otherwise, it uses the
pressure data.frame.

## Usage

``` r
tag_label_stap(
  tag,
  quiet = FALSE,
  warning_flight_duration = 2,
  warning_stap_duration = 6
)
```

## Arguments

- tag:

  a GeoPressure `tag` object.

- quiet:

  logical to display warning message.

- warning_flight_duration:

  Threshold of flight duration to display warning for (hours)

- warning_stap_duration:

  Threshold of stationary period duration to display warning for (hours)

## Value

`tag` is return with (1) a new data.frame of stationary periods
`tag$stap` and (2) a new column `stap_id` for each sensor data.

## See also

[GeoPressureManual](https://bit.ly/45gwcVu)

Other tag_label:
[`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label.md),
[`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_auto.md),
[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_read.md),
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_write.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label_read()

  tag <- tag_label_stap(tag)

  str(tag)

  str(tag$stap)
})
#> ── Short stationary periods (<6hr): ────────────────────────────────────────────
#> ✔ All 5 stationary periods duration are above 6 hours.
#> ── Short flights (<2hr): ───────────────────────────────────────────────────────
#> ✔ All 4 flights duration are above 2 hours.
#> List of 6
#>  $ param               :List of 4
#>   ..$ id                  : chr "18LX"
#>   ..$ GeoPressureR_version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:3] 3 4 5
#>   ..$ tag_create          :List of 6
#>   .. ..$ pressure_file            : chr "./data/raw-tag/18LX/18LX_20180725.pressure"
#>   .. ..$ light_file               : chr "./data/raw-tag/18LX/18LX_20180725.glf"
#>   .. ..$ acceleration_file        : chr "./data/raw-tag/18LX/18LX_20180725.acceleration"
#>   .. ..$ temperature_external_file: chr "./data/raw-tag/18LX/18LX_20180725.temperature"
#>   .. ..$ manufacturer             : chr "soi"
#>   .. ..$ directory                : 'glue' chr "./data/raw-tag/18LX"
#>   ..$ tag_label           :List of 3
#>   .. ..$ file                   : 'glue' chr "./data/tag-label/18LX-labeled.csv"
#>   .. ..$ warning_flight_duration: num 2
#>   .. ..$ warning_stap_duration  : num 6
#>   ..- attr(*, "class")= chr "param"
#>  $ pressure            :'data.frame':    672 obs. of  4 variables:
#>   ..$ date   : POSIXct[1:672], format: "2017-07-27 00:00:00" "2017-07-27 00:30:00" ...
#>   ..$ value  : num [1:672] 989 989 990 990 989 989 990 990 991 990 ...
#>   ..$ label  : chr [1:672] "" "" "" "" ...
#>   ..$ stap_id: num [1:672] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ light               :'data.frame':    4032 obs. of  3 variables:
#>   ..$ date   : POSIXct[1:4032], format: "2017-07-27 00:00:00" "2017-07-27 00:05:00" ...
#>   ..$ value  : num [1:4032] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ stap_id: num [1:4032] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ acceleration        :'data.frame':    4032 obs. of  5 variables:
#>   ..$ date   : POSIXct[1:4032], format: "2017-07-27 00:00:00" "2017-07-27 00:05:00" ...
#>   ..$ value  : num [1:4032] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ pitch  : num [1:4032] 26 27 27 28 28 28 28 27 28 27 ...
#>   ..$ label  : chr [1:4032] "" "" "" "" ...
#>   ..$ stap_id: num [1:4032] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ temperature_external:'data.frame':    2448 obs. of  2 variables:
#>   ..$ date : POSIXct[1:2448], format: "2017-06-20 00:00:00" "2017-06-20 00:30:00" ...
#>   ..$ value: num [1:2448] 32 32 32 32 32 33 33 32 33 32 ...
#>  $ stap                :'data.frame':    5 obs. of  3 variables:
#>   ..$ stap_id: num [1:5] 1 2 3 4 5
#>   ..$ start  : POSIXct[1:5], format: "2017-07-27 00:00:00" "2017-08-04 23:17:30" ...
#>   ..$ end    : POSIXct[1:5], format: "2017-08-04 19:47:30" "2017-08-05 19:27:30" ...
#>  - attr(*, "class")= chr "tag"
#> 'data.frame':    5 obs. of  3 variables:
#>  $ stap_id: num  1 2 3 4 5
#>  $ start  : POSIXct, format: "2017-07-27 00:00:00" "2017-08-04 23:17:30" ...
#>  $ end    : POSIXct, format: "2017-08-04 19:47:30" "2017-08-05 19:27:30" ...
```
