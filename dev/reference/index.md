# Package index

## Tag

Core functions related to a `tag` object.

- [`tag_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_create.md)
  :

  Create a `tag` object

- [`tag_set_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_set_map.md)
  :

  Configure the spatial and temporal parameters of the `map` of a `tag`
  object

### Visualize tag

Print and plot a `tag` object.

- [`print(`*`<tag>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.tag.md)
  :

  Print a `tag` object

- [`plot(`*`<tag>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot.tag.md)
  :

  Plot a `tag` object

- [`plot_tag_pressure()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_pressure.md)
  :

  Plot pressure data of a `tag`

- [`plot_tag_light()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_light.md)
  :

  Plot light data of a `tag`

- [`plot_tag_twilight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_twilight.md)
  :

  Plot twilight data of a `tag`

- [`plot_tag_acceleration()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_acceleration.md)
  :

  Plot acceleration data of a `tag`

- [`plot_tag_temperature()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_temperature.md)
  :

  Plot temperature data of a `tag`

- [`plot_tag_actogram()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_tag_actogram.md)
  :

  Plot Actogram data of a `tag`

### Label tag

Core functions related to labelling a `tag` object.

- [`tag_label()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label.md)
  :

  Label a `tag` object

- [`tag_label_auto()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_auto.md)
  :

  Automatic labelling acceleration data of a `tag`

- [`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_write.md)
  : Write a tag label file

- [`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_read.md)
  : Read a tag label file

- [`tag_label_stap()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_label_stap.md)
  : Create stationary periods from a tag label

### Tag utilities

Various utility functions related to a `tag` object.

- [`tag_assert()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_assert.md)
  :

  Assert the status of a `tag`

- [`tag2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag2path.md)
  :

  Build a `path` from the likelihood maps of a `tag`

- [`tag2map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag2map.md)
  :

  Extract a likelihood `map` from a `tag`

## GeoPressure

Core functions of the
[GeoPressureAPI](https://github.com/Rafnuss/GeoPressureAPI).

### GeoPressure map

Geopositioning based on pressure data.

- [`geopressure_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md)
  [`geopressure_map_likelihood()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md)
  [`geopressure_map_mismatch()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md)
  : Compute likelihood map from pressure data

- [`geopressure_map_preprocess()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map_preprocess.md)
  :

  Prepare pressure data for
  [`geopressure_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_map.md)

### GeoPressure time series

Retrieving time series pressure at a given location.

- [`geopressure_timeseries()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressure_timeseries.md)
  : Request and download pressure time series at a given location

## GeoLight

Geopositioning based on light data.

- [`twilight_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/twilight_create.md)
  : Estimate twilights from light data
- [`twilight_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/twilight_label_write.md)
  : Write a twilight label file
- [`twilight_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/twilight_label_read.md)
  : Read a twilight label file
- [`geolight_map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geolight_map.md)
  : Compute likelihood map from twilight
- [`ts2mat()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/ts2mat.md)
  : Format time series data frame into a matrix

## Graph

Constructing the Hidden-Markov model with a `graph` and computing
trajectory products.

- [`graph_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_create.md)
  :

  Create a `graph` object

### Graph movement

Defining the movement model, optionally using wind data and bird
morphology.

- [`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_set_movement.md)
  :

  Define the movement model of a `graph`

- [`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_transition.md)
  :

  Compute transition probabilities of a `graph`

- [`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag_download_wind.md)
  : Download flight data

- [`graph_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_add_wind.md)
  :

  Compute windspeed and airspeed on a `graph`

- [`bird_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/bird_create.md)
  : Create bird flight traits

- [`print(`*`<bird>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.bird.md)
  :

  Print a `bird` object

- [`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/speed2prob.md)
  : Compute probability of a bird speed

- [`edge_add_wind()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/edge_add_wind.md)
  : Retrieve ERA5 variable along edge

- [`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_graph_movement.md)
  :

  Plot movement model of a `graph`

### Graph products

Compute the three main products of the Hidden-Markov model.

- [`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md)
  : Compute the most likely trajectory
- [`graph_marginal()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_marginal.md)
  : Compute marginal probability map
- [`graph_simulation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_simulation.md)
  : Simulate randomly multiple trajectories

### Graph utilities

Utility functions for a `graph` object

- [`graph_assert()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_assert.md)
  :

  Assert the status of a `graph`

- [`print(`*`<graph>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.graph.md)
  :

  Print a `graph` object

## Map

Container for spatio-temporal (stationary periods) data.

- [`map_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/map_create.md)
  :

  Create a `map` object

- [`print(`*`<map>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.map.md)
  :

  Print a `map` object

- [`plot(`*`<map>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot.map.md)
  :

  Plot a `map` object

- [`rast.map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/rast.map.md)
  :

  Construct a SpatRaster from a `map`

- [`map_expand()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/map_expand.md)
  :

  Construct grid from `extent` and `scale`

## Path

Data.frame of positions defining a bird trajectory.

- [`ind2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/ind2path.md)
  :

  Create a `path` from indices of coordinates

- [`path2edge()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2edge.md)
  :

  Extract the edges of a `path` from a `graph`

- [`plot_path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_path.md)
  :

  Plot a `path`

## Pressurepath

Data.frame of pressure time series along a path.

- [`pressurepath_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/pressurepath_create.md)
  :

  Create a `pressurepath`

- [`plot_pressurepath()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_pressurepath.md)
  :

  Plot a `pressurepath`

- [`path2elevation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2elevation.md)
  : Download ground elevation along a path

## GeoPressureTemplate

Wrapper functions for the full GeoPressureR worflow

- [`geopressuretemplate()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressuretemplate.md)
  [`geopressuretemplate_config()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressuretemplate.md)
  [`geopressuretemplate_graph()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressuretemplate.md)
  [`geopressuretemplate_pressurepath()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressuretemplate.md)
  [`geopressuretemplate_tag()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressuretemplate.md)
  : Workflow for GeoPressureR
- [`load_interim()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/load_interim.md)
  : Load an interim RData object

## Param

List of parameters used in `tag` and `graph`.

- [`param_create()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/param_create.md)
  :

  Create a `param` list

- [`print(`*`<param>`*`)`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/print.param.md)
  :

  Print a `param` list

## Utilities

General utility functions of the package.

- [`stap2flight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/stap2flight.md)
  : Compute flights from stationary periods

- [`stap2duration()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/stap2duration.md)
  : Compute duration of stationary periods

- [`speed2bearing()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/speed2bearing.md)
  : Compute the bearing of a speed vector

- [`windsupport()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/windsupport.md)
  : Compute wind support and drift from a wind and ground speed vectors

- [`path2twilight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2twilight.md)
  :

  Compute exact astronomical twilights from a `path` (positions and
  dates)

- [`geopressureviz()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/geopressureviz.md)
  : Start the GeoPressureViz shiny app

- [`GeoPressureR`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/GeoPressureR.md)
  : GeoPressureR: Global positioning by atmospheric pressure.
