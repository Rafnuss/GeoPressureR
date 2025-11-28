# Write a tag label file

This function writes the csv file of labelled activity and pressure
which can be read with [TRAINSET](https://trainset.geocene.com/). If no
label data exist, it will first initialize the label data.

Optionally, it can also export a reference dataset for pressure
`tag$pressure$ref` as another series to be visualized on TRAINSET, but
without impacting the labelling process.

## Usage

``` r
tag_label_write(
  tag,
  file = glue::glue("./data/tag-label/{tag$param$id}.csv"),
  quiet = FALSE
)
```

## Arguments

- tag:

  a GeoPressure `tag` object.

- file:

  Absolute or relative path of the label file to be saved.

- quiet:

  logical to display message.

## Value

The file pathname is return invisibly

## See also

[GeoPressureManual](https://bit.ly/3QC7IBt)

Other tag_label:
[`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label.md),
[`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_auto.md),
[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_read.md),
[`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)

  # Writing unlabelled tag will initialize the labelling for trainset
  tag_label_write(tag)

  # Writing unlabelled tag will initialize the labelling for trainset
  tag <- tag_label_auto(tag)
  tag_label_write(tag)

  # Writing labelled tag will use the existing labels
  tag <- tag_label(tag)
  tag_label_write(tag)
})
#> ℹ No label data.
#> → Initialize automatically label using `tag_label_auto()`
#> ✔ ./data/tag-label/18LX.csv written successfully.
#> ✔ ./data/tag-label/18LX.csv written successfully.
#> ── Short stationary periods (<6hr): ────────────────────────────────────────────
#> ✔ All 5 stationary periods duration are above 6 hours.
#> ── Short flights (<2hr): ───────────────────────────────────────────────────────
#> ✔ All 4 flights duration are above 2 hours.
#> ✔ ./data/tag-label/18LX.csv written successfully.
```
