# Request and download pressure time series at a given location

This function returns the surface atmospheric pressure time series from
ERA5 at any requested location.

If the location queried is over water, the location will be moved to the
closest onshore location.

The ERA5 pressure time series of the response \\P\_{ERA}\\ will be
provided on a hourly basis between `start_time` and `end_time` or the
same as `pressure$date` if `pressure` is supplied.

If you supply the `pressure` of the geolocator \\P\_{gl}\\, the function
will additionally return the altitude of the geolocator above sea level
\\z\_{gl}\\ using the barometric equation, \$\$
z\_{{gl}}(x)=z\_{ERA5}(x) + \frac{T\_{ERA5}(x)}{L_b} \left(
\frac{P\_{gl}}{P\_{ERA5}(x)} \right)^{\frac{RL_b}{g M}-1},\$\$ where
\\z\_{ERA}\\, \\T\_{ERA}\\, and \\P\_{ERA}\\ respectively correspond to
the ground level elevation, temperature at 2m, and ground level pressure
of ERA5, \\L_b\\ is the standard temperature lapse rate, \\R\\ is the
universal gas constant, \\g\\ is the gravity constant and \\M\\ is the
molar mass of air. See more information at [the GeoPressureAPI
documentation](https://github.com/Rafnuss/GeoPressureAPI).

To be able to compare the temporal variation of the retrieved pressure
of ERA5 \\P\_{ERA}\\ to the geolocator pressure \\P\_{gl}\\, the
function also returns the ERA pressure normalized with the geolocator
mean pressure measurement as `surface_pressure_norm`. \$\$
P\_{ERA5,0}(\boldsymbol{x})\[t\] = \left(
P\_{ERA5}(\boldsymbol{x})\[t\]-P\_{gl}\[t\]\right) - \left(
\frac{1}{n}\sum\_{i=1}^{n} P\_{ERA5}(\boldsymbol{x})\[i\]-P\_{gl}\[i\]
\right).\$\$

## Usage

``` r
geopressure_timeseries(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE
)
```

## Arguments

- lat:

  Latitude to query (0° to 90°).

- lon:

  Longitude to query (-180° to 180°).

- pressure:

  A data.frame of pressure time series, containing at least a `"date"`
  and `"value"` column.

- start_time:

  If `pressure` is not provided, `start_time` defines the start time of
  the time series as POSIXlt.

- end_time:

  If `pressure` is not provided, `end_time` defines the end time of the
  time series as POSIXlt.

- quiet:

  logical to hide messages about the progress

- debug:

  logical to display additional information to debug a request

## Value

A data.frame containing

- `date` POSIXct date time

- `surface_pressure` pressure (hPa)

- `lon` same as input `lon` except if over water

- `lat` same as input `lat` except if over water.

- `surface_pressure_norm` only if `pressure` is provided as input

- `altitude` only if `pressure` is provided as input

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti.
2023. Global Positioning with Animal‐borne Pressure Sensors. *Methods in
Ecology and Evolution*, 14, 1118–1129
[doi:10.1111/2041-210X.14043](https://doi.org/10.1111/2041-210X.14043) .

## See also

Other pressurepath:
[`plot_pressurepath()`](https://raphaelnussbaumer.com/GeoPressureR/reference/plot_pressurepath.md),
[`pressurepath_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/pressurepath_create.md)

## Examples

``` r
# Request pressure at a given location
pressurepath <- geopressure_timeseries(
  lat = 46, lon = 6,
  start_time = "2017-01-01 00:00",
  end_time = "2017-01-02 00:00",
  quiet = TRUE
)

str(pressurepath)
#> 'data.frame':    25 obs. of  4 variables:
#>  $ date            : POSIXct, format: "2017-01-01 00:00:00" "2017-01-01 01:00:00" ...
#>  $ surface_pressure: num  969 969 968 968 967 ...
#>  $ lat             : num  46 46 46 46 46 46 46 46 46 46 ...
#>  $ lon             : num  6 6 6 6 6 6 6 6 6 6 ...

plot(pressurepath$date, pressurepath$surface_pressure,
  type = "b", ylab = "Pressure (hPa)", xlab = "Datetime"
)


# Retrieve the altitude of a bird being at this location adding random noise on the sensor.
pressurepath <- geopressure_timeseries(
  lat = 46, lon = 6,
  pressure = data.frame(
    data.frame(
      date = pressurepath$date,
      value = pressurepath$surface_pressure + rnorm(nrow(pressurepath))
    )
  ),
  quiet = TRUE
)

str(pressurepath)
#> 'data.frame':    25 obs. of  7 variables:
#>  $ date                 : POSIXct, format: "2017-01-01 00:00:00" "2017-01-01 01:00:00" ...
#>  $ pressure_tag         : num  969 967 970 968 966 ...
#>  $ surface_pressure     : num  969 969 968 968 967 ...
#>  $ altitude             : num  491 497 468 481 491 ...
#>  $ lat                  : num  46 46 46 46 46 46 46 46 46 46 ...
#>  $ lon                  : num  6 6 6 6 6 6 6 6 6 6 ...
#>  $ surface_pressure_norm: num  970 969 968 968 967 ...

plot(pressurepath$date, pressurepath$altitude,
  type = "b", ylab = "Altitude (m)", xlab = "Datetime"
)
```
