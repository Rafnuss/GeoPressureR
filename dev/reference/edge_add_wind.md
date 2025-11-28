# Retrieve ERA5 variable along edge

Reads the NetCDF files and extracts the variable requested along each
flight defined by the edges.

- Time: linear interpolation using the resolution requested with
  `rounding_interval`

- Space: nearest neighbour interpolation by default, or bi-linear with
  [`pracma::interp2`](https://rdrr.io/pkg/pracma/man/interp2.html) if
  `interp_spatial_linear=TRUE`. Note: spatial interpolation is limited
  to 0.1° for computational reasons.

- Pressure/altitude: linear interpolation using the exact `pressure`
  values

## Usage

``` r
edge_add_wind(
  graph,
  edge_s,
  edge_t,
  pressure = NULL,
  variable = c("u", "v"),
  rounding_interval = 60,
  interp_spatial_linear = FALSE,
  return_averaged_variable = FALSE,
  file = function(stap_id, tag_id) {
    
    glue::glue("./data/wind/{tag_id}/{tag_id}_{stap_id}.nc")
 },
  quiet = FALSE
)
```

## Arguments

- graph:

  either a `tag` or a `graph` GeoPressureR object.

- edge_s:

  a index of the source node of the edge. Either a vector with 3D index
  or a matrix of 3 columns, one for each dimension.

- edge_t:

  a index of the target node of the edge. Either a vector with 3D index
  or a matrix of 3 columns, one for each dimension.

- pressure:

  pressure measurement of the associated `tag` data used to estimate the
  pressure level (i.e., altitude) of the bird during the flights. This
  data.frame needs to contain `date` as POSIXt and `value` in hPa. If
  not provided, uses `graph$pressure`, assuming that argument `graph` is
  a GeoPressureR `tag` object.

- variable:

  list of the variables to extract from [the ERA5 pressure
  level](https://bit.ly/3BrwLBM) using the `shortName` notation: `"u"`,
  `"v"`, `"t"`, `"cc"`, `"r"`, `"w"`, `"ciwc"`, `"clwc"`, `"q"`,
  `"cswc"`, `"d"`, `"z"`, `"o3"`, `"pv"`, `"vo"`.

- rounding_interval:

  temporal resolution on which to query the variable (min). Default is
  to match ERA5 native resolution (1hr).

- interp_spatial_linear:

  logical to interpolate the variable linearly over space, if `FALSE`
  takes the nearest neighbour. ERA5 native resolution is 0.25°

- return_averaged_variable:

  logical to return the variable for each timestep or average for the
  entire flight.

- file:

  absolute or relative path of the ERA5 wind data file to be downloaded.
  Function taking as arguments (1) the stationary period identifier
  and (2) the tag_id.

- quiet:

  logical to hide messages about the progress

## Value

If `return_averaged_variable = TRUE`, returns a data.frame with one row
per edge and columns:

- `stap_s` id of the source/origin stationary period

- `stap_t` id of the target/destination stationary period

- `s` node id of the source (same as/similar to `edge_s`)

- `t` node id of the target (same as/similar to `edge_t`)

- `lat_s` latitude of the source

- `lat_t` latitude of the target

- `lon_s` longitude of the source

- `lon_t` longitude of the target

- `start` start datetime of the flight

- `end` end datetime of the flight

- `duration` flight duration

- `n` number of flight

- `distance` distance of the flight

- `bearing` bearing of the flight

- `gs` groundspeed

- `ws` windspeed (if `graph` provided)

If `return_averaged_variable = FALSE`, returns a data.frame with one row
per time step and edge, and columns:

- `edge_id` edge index

- `val` value of the variable at each time step

- `pressure` pressure at each time step

- `date` datetime of each time step

- `w` weight for averaging

- `var` variable name

- `lat` latitude at each time step

- `lon` longitude at each time step

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html)
