# Start the GeoPressureViz shiny app

GeoPressureViz is a shiny app designed to help you visualize the overall
trajectory of the bird as well as each step-by-step move. This app is
particularly useful to check the correspondence between pressure map,
light map and flight distance. You can edit the path and query pressure
time series for individual stationary period to test manual what seems
the optimal path.

## Usage

``` r
geopressureviz(
  x,
  path = NULL,
  marginal = NULL,
  launch_browser = TRUE,
  run_bg = TRUE
)
```

## Arguments

- x:

  a GeoPressureR `tag` object, a `.Rdata` file or the unique identifier
  `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.

- path:

  a GeoPressureR `path` or `pressurepath` data.frame.

- marginal:

  map of the marginal probability computed with
  [`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md).
  Overwrite the `path` or `pressurepath` contained in the `.Rdata` file.

- launch_browser:

  If true (by default), the app runs in your browser, otherwise it runs
  on Rstudio.

- run_bg:

  If true, the app runs in a background R session using the `callr`
  package. This allows you to continue using your R session while the
  app is running.

## Value

When `run_bg = FALSE`: The updated path visualized in the app. Can also
be retrieved with `shiny::getShinyOption("path_geopressureviz")` after
the app completes. When `run_bg = TRUE`: Returns the background process
object.

## Details

GeoPressureViz can be started based on a `.Rdata` file containing at
least `tag`, but also optionally `marginal` and/or `path`
(`path_most_likely` is also accepted).

You can retrieve the edited path from the return value of this function
(when `run_bg = FALSE`) or with
`shiny::getShinyOption("path_geopressureviz")` after the app completes.

Learn more about GeoPressureViz in the
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
or with this [demo of the Great Reed Warbler
(18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).

## See also

[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
