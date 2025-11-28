# Plot a `path`

This function plots a `path` data.frame. This function is used in
[`plot.map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot.map.md).

## Usage

``` r
plot_path(
  path,
  plot_leaflet = TRUE,
  provider = "Esri.WorldTopoMap",
  provider_options = leaflet::providerTileOptions(),
  pad = 3,
  ...
)
```

## Arguments

- path:

  a GeoPressureR `path` data.frame.

- plot_leaflet:

  logical defining if the plot is an interactive `leaflet` map or a
  static basic plot.

- provider:

  the name of the provider (see
  <https://leaflet-extras.github.io/leaflet-providers/preview/> and
  <https://github.com/leaflet-extras/leaflet-providers>)

- provider_options:

  tile options. See leaflet::addProviderTiles() and
  leaflet::providerTileOptions()

- pad:

  padding of the map in degree lat-lon (only for
  `plot_leaflet = FALSE`).

- ...:

  additional parameters for `plot_path_leaflet()`

## Value

modified map object

## See also

[`plot.map()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot.map.md)

Other path:
[`ind2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/ind2path.md),
[`path2edge()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2edge.md),
[`path2elevation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2elevation.md),
[`path2twilight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2twilight.md),
[`tag2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/tag2path.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(c(-16, 23, 0, 50), scale = 1)
})
path <- ind2path(c(1652, 1603, 1755, 1708, 1607), tag)

plot_path(path)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[17.5,16.5,19.5,18.5,16.5],"lat":[48.5,47.5,45.5,42.5,43.5]}]]],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.5,47.5,45.5,42.5,43.5],[17.5,16.5,19.5,18.5,16.5],[10.34131237750732,5.744562646538029,5.449631621480024,5.421612021659069,7.125933808241013],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,true,true,true,true],"fillColor":"grey","fillOpacity":0.8},null,null,null,null,["#1, 8.8 days","#2, 0.8 days","#3, 0.7 days","#4, 0.7 days","#5, 2 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[42.5,48.5],"lng":[16.5,19.5]}},"evals":[],"jsHooks":[]}
```
