# Compute duration of stationary periods

This function returns the duration between `stap$start` and `stap$end`.

The function can be used with any data.frame containing `start` and
`end` as POSIXct (e.g., `flight` or `path`).

## Usage

``` r
stap2duration(stap_path, units = "days", return_numeric = TRUE)
```

## Arguments

- stap_path:

  A `stap` data.frame.

- units:

  character string. Units in which the results are desired. Can be
  abbreviated. See [`difftime()`](https://rdrr.io/r/base/difftime.html)

- return_numeric:

  logical to return the duration as a numeric rather than with a
  duration format.

## Value

Vector of duration

## Examples

``` r
# Create fake stap
stap <- data.frame(
  start = seq(as.POSIXct("1990-01-01"), as.POSIXct("1991-01-01"), length.out = 13)
)
stap$end <- stap$start + 60 * 60 * 24 * 30
stap$stap_id <- seq_len(nrow(stap))

stap2duration(stap)
#>  [1] 30 30 30 30 30 30 30 30 30 30 30 30 30

stap2duration(stap, units = "mins", return_numeric = FALSE)
#> Time differences in mins
#>  [1] 43200 43200 43200 43200 43200 43200 43200 43200 43200 43200 43200 43200
#> [13] 43200
```
