# Compute transition probabilities of a `graph`

Use the movement model (see
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md))
to convert ground speed `gs` (or airspeed `as` if available) into the
transition probability of the edges of the graph.

## Usage

``` r
graph_transition(graph)
```

## Arguments

- graph:

  Graph constructed with
  [`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md)
  and with a movement (see
  [`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md)).

## Value

Vector of transition probability for each edge.

## Details

The vector return correspond to the elements of the transition matrices
\\T_k \forall k \in \[1,n\]\\ extracted for all edges considered in the
graph. Each of these values thus corresponds to the probability \\P(X_k
\mid X\_{k-1})\\, where \\X_k\\ is the random variable of the position
of the bird at time \\k\\.

To create a generic function, we define `speed2prob` which converts the
speed of an edge into the transition probability.

## See also

Other movement:
[`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/bird_create.md),
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md),
[`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_graph_movement.md),
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/speed2prob.md),
[`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_download_wind.md)
