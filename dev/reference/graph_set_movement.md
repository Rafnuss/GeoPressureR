# Define the movement model of a `graph`

Configure the movement model of a `graph` by defining the value of the
parameters needed to build the transition
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_transition.md)
through
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/speed2prob.md).

Three methods are currently implemented with two parametric function
`"gamma"` and `"logis"` suitable when wind data is not available and are
thus defining the probability of a groundspeed.

If wind data is available, it is recommended to use the `"power"` method
which rely on the power curve equation (energy vs airspeed) to estimate
the probability of an airspeed. Read more about this approach in
[section 2.2.5. of Nussbaumer et al.
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0009-title)

## Usage

``` r
graph_set_movement(
  graph,
  type = ifelse("ws" %in% names(graph), "as", "gs"),
  method = ifelse("ws" %in% names(graph), "power", "gamma"),
  shape = 7,
  scale = 7,
  location = 40,
  bird = NULL,
  power2prob = function(power) (1/power)^3,
  low_speed_fix = 15,
  zero_speed_ratio = 1
)
```

## Arguments

- graph:

  a GeoPressureR `graph` object.

- type:

  Ground speed `"gs"` or airspeed `"as"`

- method:

  method used to convert the speed to probability ("gamma", "logis" or
  "power")

- shape:

  parameter of the gamma distribution (km/h)

- scale:

  parameter of the gamma and logistic distribution (km/h)

- location:

  parameter for the logistic distribution (km/h)

- bird:

  A GeoPressureR `bird` object containing the basic morphological traits
  necessary: mass, wing span, wing aspect ratio, and body frontal area.
  See
  [`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/bird_create.md).

- power2prob:

  function taking power as a single argument and returning a probability

- low_speed_fix:

  speed below which the probability remains the same, i.e. we assign the
  same probability at `low_speed_fix` for any lower speed. This
  parameter is used to allow short flights covering small distances.
  (unit of km/h)

- zero_speed_ratio:

  multiplicative ratio of the probability for speed zero. This ratio
  apply only when the bird is staying at the same location (fly and come
  back or stay within pixel size). This parameter (when greater than 1)
  is used to favour a bird to stay at the same location rather than
  perform short fly.

## Value

Graph list with a new list `graph$movement` storing all the parameters
needed to compute the transition probability

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and
Daniel Sheldon. 2023. Reconstructing bird trajectories from pressure and
wind data using a highly optimized hidden Markov model. *Methods in
Ecology and Evolution*, 14, 1118–1129
[doi:10.1111/2041-210X.14082](https://doi.org/10.1111/2041-210X.14082) .

## See also

Other graph:
[`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md),
[`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md),
[`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md),
[`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md),
[`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md),
[`print.graph()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.graph.md)

Other movement:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/bird_create.md),
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_transition.md),
[`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_graph_movement.md),
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/speed2prob.md),
[`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_download_wind.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read() |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
    ) |>
    geopressure_map(quiet = TRUE) |>
    geolight_map(quiet = TRUE)
})

graph <- graph_create(tag, quiet = TRUE)

graph <- graph_set_movement(graph,
  method = "gamma",
  shape = 4,
  scale = 6,
  low_speed_fix = 10
)
plot_graph_movement(graph)


graph <- graph_set_movement(graph,
  method = "logis",
  shape = 4,
  location = 60,
  low_speed_fix = 10
)
plot_graph_movement(graph)

```
