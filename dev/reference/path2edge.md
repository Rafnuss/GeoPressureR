# Extract the edges of a `path` from a `graph`

Retrieve the edges in a `graph` corresponding to the flight transition
defined by a `path`. These edges can be useful to extract flight
information specific to a path.

## Usage

``` r
path2edge(path, tag_graph)
```

## Arguments

- path:

  a GeoPressureR `path` data.frame

- tag_graph:

  either a `tag` or a `graph` GeoPressureR object.

## Value

Data.frame of the edge containing:

- `s`: index in 3D (lat-lon-stap) of the origin (source).

- `t`: index in 3D (lat-lon-stap) of the destination (target).

- `lat_s`: latitude of the origin (source).

- `lat_t`: latitude of the destination (target).

- `lon_s`: longitude of the origin (source).

- `lon_t`: longitude of the destination (target).

- `stap_s`: stationary period of the origin (source).

- `stap_t`: stationary period of the destination (target).

- `distance`: Distance (in km) of the flight.

- `start`: end of the flight.

- `end`: start of the flight.

- `duration`: duration of the flight.

- `n`: number of flight.

- `bearing`: Angle of the flight in degree (0° = North, 90° = East),
  computed with
  [`geosphere::bearing()`](https://rdrr.io/pkg/geosphere/man/bearing.html).

- `gs`: groundspeed vector expressed as a complex number. You can
  compute the groundspeed value (km/h) with `abs(gs)`, the W-E and S-N
  component of the flight with `Re(gs)` and `Im(gs)`, and the
  angle/direction with `Arg(gs)`. If graph provided.

- `ws`: if computed with
  [`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md),
  same value as `gs`. Airspeed is computed with `as = gs - ws` in
  complex number to keep the vectorial additive properties. If graph
  provided.

## See also

[GeoPressureManual](https://bit.ly/47MhQxN)

Other path:
[`ind2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/ind2path.md),
[`path2elevation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2elevation.md),
[`path2twilight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2twilight.md),
[`plot_path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_path.md),
[`tag2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag2path.md)
