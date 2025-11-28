# Read a tag label file

This function reads an exported csv file from
[TRAINSET](https://trainset.geocene.com/) and updates the data logger
dataset `tag`.

## Usage

``` r
tag_label_read(
  tag,
  file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv")
)
```

## Arguments

- tag:

  a GeoPressure `tag` object.

- file:

  Absolute or relative path of the label file.

## Value

Same data logger list as input, updated with the labels
`tag$pressure$label` and optionally `tag$acceleration$label`.

## See also

[GeoPressureManual](https://bit.ly/45v79gV)

Other tag_label:
[`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label.md),
[`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_auto.md),
[`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md),
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_write.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)

  tag <- tag_label_read(tag)

  str(tag)
})
#> List of 5
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
#>   ..$ tag_label           :List of 1
#>   .. ..$ file: 'glue' chr "./data/tag-label/18LX-labeled.csv"
#>   ..- attr(*, "class")= chr "param"
#>  $ pressure            :'data.frame':    672 obs. of  3 variables:
#>   ..$ date : POSIXct[1:672], format: "2017-07-27 00:00:00" "2017-07-27 00:30:00" ...
#>   ..$ value: num [1:672] 989 989 990 990 989 989 990 990 991 990 ...
#>   ..$ label: chr [1:672] "" "" "" "" ...
#>  $ light               :'data.frame':    4032 obs. of  2 variables:
#>   ..$ date : POSIXct[1:4032], format: "2017-07-27 00:00:00" "2017-07-27 00:05:00" ...
#>   ..$ value: num [1:4032] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ acceleration        :'data.frame':    4032 obs. of  4 variables:
#>   ..$ date : POSIXct[1:4032], format: "2017-07-27 00:00:00" "2017-07-27 00:05:00" ...
#>   ..$ value: num [1:4032] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ pitch: num [1:4032] 26 27 27 28 28 28 28 27 28 27 ...
#>   ..$ label: chr [1:4032] "" "" "" "" ...
#>  $ temperature_external:'data.frame':    2448 obs. of  2 variables:
#>   ..$ date : POSIXct[1:2448], format: "2017-06-20 00:00:00" "2017-06-20 00:30:00" ...
#>   ..$ value: num [1:2448] 32 32 32 32 32 33 33 32 33 32 ...
#>  - attr(*, "class")= chr "tag"
```
