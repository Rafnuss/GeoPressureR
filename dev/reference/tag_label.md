# Label a `tag` object

This function performs the following operations:

1.  Read label file with
    [`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_read.md)
    and assign the label to a new column in each sensor data.frame

2.  Compute the stationary period `tag$stap` from the label and assign
    the corresponding `stap_id` on all sensors data.frame with
    [`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md)

If the label file does not exist, the function will suggest to create it
with
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_write.md)
and use
[`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_auto.md)
if acceleration data exists.

## Usage

``` r
tag_label(
  tag,
  file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv"),
  quiet = FALSE,
  ...
)
```

## Arguments

- tag:

  a GeoPressure `tag` object.

- file:

  Absolute or relative path of the label file.

- quiet:

  logical to display message.

- ...:

  Arguments passed on to
  [`tag_label_stap`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md)

  `warning_flight_duration`

  :   Threshold of flight duration to display warning for (hours)

  `warning_stap_duration`

  :   Threshold of stationary period duration to display warning for
      (hours)

## Value

Same `tag` list with

\(1\) a `stap` data.frame describing the STAtionary Period:

- `stap_id` unique identifier in increasing order 1,...,n

- `start` start date of each stationary period

- `end` end date of each stationary period

\(2\) an additional `label` and `stap_id` column on the sensor
data.frame:

- `date` datetime of measurement as POSIXt

- `value` sensor measurement

- `label` indicates the observation to be discarded (`"discard"` and
  `"flight"`) as well as grouped by elevation layer (`elev_*`)

- `stap_id` stationary period of the measurement matching the
  `tag$stap`.

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/labelling-tracks.html)

Other tag_label:
[`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_auto.md),
[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_read.md),
[`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md),
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_write.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)

  print(tag)

  tag <- tag_label(tag)

  print(tag)

  # The labelled `tag` contains additional column on the sensor data.frame
  str(tag)
})
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
#> ── Short stationary periods (<6hr): ────────────────────────────────────────────
#> ✔ All 5 stationary periods duration are above 6 hours.
#> ── Short flights (<2hr): ───────────────────────────────────────────────────────
#> ✔ All 4 flights duration are above 2 hours.
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
#> stap_id | start               | end                
#> 1       | 2017-07-27 00:00:00 | 2017-08-04 19:47:30
#> 2       | 2017-08-04 23:17:30 | 2017-08-05 19:27:30
#> ...
#> 5       | 2017-08-08 00:12:30 | 2017-08-09 23:57:30
#> Run `tag$stap` to see full stap table
#> 
#> ── Map 
#> ✖ No geographical parameters defined yet. Use `tag_set_map()`
#> List of 6
#>  $ param               :List of 4
#>   ..$ id                  : chr "18LX"
#>   ..$ GeoPressureR_version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:4] 3 4 5 9000
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
```
