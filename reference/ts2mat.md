# Format timeserie data.frame into a matrix

Format timeserie data.frame into a matrix

## Usage

``` r
ts2mat(
  ts,
  twl_offset = 0,
  value = "value",
  twl_time_tolerance = formals(twilight_create)$twl_time_tolerance
)
```

## Arguments

- ts:

  data.frame of a `tag`, containing at least `date` and `value`.

- twl_offset:

  Shift of the middle of the night compared to 00:00 UTC (in hours). If
  not provided, it uses the middle of all nights.

- value:

  column name to extract

- twl_time_tolerance:

  Maximum allowed time difference in seconds between observations and
  the regular grid. Observations beyond this threshold will be set to
  NA. Default is 180 seconds (3 minutes).

## Value

A data.frame with columns `date` and `value`.
