# Plot movement model of a `graph`

This function display a plot of pressure time series recorded by a tag

## Usage

``` r
plot_graph_movement(graph, speed = seq(0, 120), plot_plotly = FALSE)
```

## Arguments

- graph:

  a GeoPressureR `graph` object.

- speed:

  Vector of speed value (km/h) used on the x-axis.

- plot_plotly:

  logical to use `plotly`

## Value

a plot or ggplotly object.

## See also

Other movement:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/bird_create.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_set_movement.md),
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_transition.md),
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/reference/speed2prob.md),
[`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_download_wind.md)
