# Assert the status of a `graph`

This function check the condition of a `graph` object.

## Usage

``` r
graph_assert(graph, condition = "graph")
```

## Arguments

- graph:

  a GeoPressureR `graph` object

- condition:

  condition to assert `graph` for. One of `"graph"` (default),
  `"movement"`, or `"full"`.

## Value

logical indicating whether the `graph` object fulfil the condition.
