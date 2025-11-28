# Automatic labelling acceleration data of a `tag`

This function uses acceleration data to classify migratory flights. The
function uses a `k=2` mean clustering
([`kmeans()`](https://rdrr.io/r/stats/kmeans.html)) to identify high
activity periods. Periods of high activity lasting more than
`min_duration` are then considered to be migratory flight.

Additionally, we perform a post-processing step to improve the
classification of low-activity period happening during a migration
flight (e.g. gliding phase):

1.  Mid-activity surrounded by flights. The kmeans classification
    classify high-activity all values above 50% of the distance between
    the two clusters. Here, we classify as mid-activity non-flight
    activity data which value are greater than `thr_reclassify` of the
    distance between the two kmeans clusters. Any periods of
    mid-activity surrounded by flight are also considered as flights.

2.  To avoid stap lasting only a few datapoints, we also classify any
    datapoint which is surrounded by a flight before and after, within a
    window of +/- `post_proc_window` points. As a result, no stap will
    be shorter than `(2 * post_proc_window + 1) * dt`.

This function is inspired by the function `classify_flap` from the
[PAMlr package](https://github.com/KiranLDA/PAMlr).

## Usage

``` r
tag_label_auto(
  tag,
  min_duration = 30,
  thr_reclassify = 0.1,
  post_proc_window = 2
)
```

## Arguments

- tag:

  a GeoPressure `tag` object.

- min_duration:

  Minimal duration (in minutes) to consider a high activity as migratory
  flight.

- thr_reclassify:

  Post-processing threshold of activity considered for
  re-classification. Typically between `0` and `0.5` (no effect). See
  post-processing for details.

- post_proc_window:

  Post-processing windows considered for re-classification. Typically
  `0` (no effect) to `5` in unit of the temporal resolution. See
  post-processing for details.

## Value

Same data logger list than input `tag`, but with the column `label`
filled with `"flight"` in the acceleration data.frame when a sustained
high-activity period is detected.

## See also

[GeoPressureManual](https://bit.ly/45bthNt)

Other tag_label:
[`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label.md),
[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_read.md),
[`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md),
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_write.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)

  tag <- tag_label_auto(tag, min_duration = 15)
  str(tag$acceleration)
})
#> 'data.frame':    4032 obs. of  4 variables:
#>  $ date : POSIXct, format: "2017-07-27 00:00:00" "2017-07-27 00:05:00" ...
#>  $ value: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ pitch: num  26 27 27 28 28 28 28 27 28 27 ...
#>  $ label: chr  "" "" "" "" ...
```
