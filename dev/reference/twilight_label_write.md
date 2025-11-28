# Write a twilight label file

This function writes the csv file of the labelled twilight which can be
read with TRAINSET <https://trainset.raphaelnussbaumer.com/>.

## Usage

``` r
twilight_label_write(
  tag,
  file = glue::glue("./data/twilight-label/{tag$param$id}.csv"),
  quiet = FALSE
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object

- file:

  Name of the twilight label file to be saved.

- quiet:

  logical to hide messages

## Value

None

## See also

Other geolight:
[`geolight_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geolight_map.md),
[`twilight_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/twilight_create.md),
[`twilight_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/twilight_label_read.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create()

  label_file <- twilight_label_write(tag)

  str(utils::read.csv(label_file))
})
#> ✔ ./data/twilight-label/18LX.csv written successfully.
#> 'data.frame':    28 obs. of  4 variables:
#>  $ series   : chr  "Rise" "Set" "Rise" "Set" ...
#>  $ timestamp: chr  "2017-07-27T03:05:00Z" "2017-07-27T18:30:00Z" "2017-07-28T03:20:00Z" "2017-07-28T18:45:00Z" ...
#>  $ value    : int  240 1165 255 1180 235 1190 240 1180 245 1185 ...
#>  $ label    : int  1 1 1 1 1 1 1 1 1 1 ...
```
