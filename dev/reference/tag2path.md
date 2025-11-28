# Build a `path` from the likelihood maps of a `tag`

Find the position of the highest value in a map, typically most probable
value in a likelihood map.

Note that this path is the most likely, considering only the observation
model and ignoring the movement model. Prefer to use
[`graph_most_likely()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/graph_most_likely.md)
for the most realistic path accounting for flight duration.

`interp` can be used to interpolate unrealistic position from short
stationary periods based on the position of the longer ones. In order to
preserve a reference to the grid, the interpolation is only performed at
the centre of the grid cell (defined by the `likelihood` map). Note also
that it is not possible to interpolate the first and last stationary
period.

## Usage

``` r
tag2path(tag, likelihood = NULL, interp = FALSE, use_known = TRUE)
```

## Arguments

- tag:

  a GeoPressureR `tag` object

- likelihood:

  Field of the `tag` list containing the likelihood map (character).
  Possible value are `map_pressure`, `map_light`, `map_pressure_mse`,
  `map_pressure_mse`, `map_pressure_mse`, `mask_water`. Default `NA` is
  to take the product of `map_pressure` and `map_light`, or if not
  available, taking the first of the possible values.

- interp:

  the position of the stationary period shorter than `interp` will be
  replace by a linear average from other position accounting for flight
  duration (in days).

- use_known:

  If true, enforce the known position in the path created. The known
  positions are approximated to the map resolution in order to
  corresponds to integer index.

## Value

A path data.frame

- `stap_id` stationary period

- `ind` indices of the coordinate in the 2D grid. Useful to retrieve map
  or graph information.

- `lat` Latitude,

- `lon` longitude

## See also

Other path:
[`ind2path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/ind2path.md),
[`path2edge()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2edge.md),
[`path2elevation()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2elevation.md),
[`path2twilight()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/path2twilight.md),
[`plot_path()`](https://raphaelnussbaumer.com/GeoPressureR/dev/reference/plot_path.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 2
    ) |>
    geopressure_map(quiet = TRUE)
})

# Extract a path from pressure map
path <- tag2path(tag)
plot_path(path)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[16.75,-0.25,14.25,21.25,12.25],"lat":[48.75,35.25,31.25,49.75,41.75]}]]],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.75,35.25,31.25,49.75,41.75],[16.75,-0.25,14.25,21.25,12.25],[10.34131237750732,5.744562646538029,5.449631621480024,5.421612021659069,7.125933808241013],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,true,true,true,true],"fillColor":"grey","fillOpacity":0.8},null,null,null,null,["#1, 8.8 days","#2, 0.8 days","#3, 0.7 days","#4, 0.7 days","#5, 2 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[31.25,49.75],"lng":[-0.25,21.25]}},"evals":[],"jsHooks":[]}
# Short stationary periods (e.g. 1 day) can be unreliably
# estimated, so interpolating them is often better
path <- tag2path(tag, interp = 1)
plot_path(path)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[16.75,16.25,14.75,13.25,12.25],"lat":[48.75,47.75,45.75,43.25,41.75]}]]],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.75,47.75,45.75,43.25,41.75],[16.75,16.25,14.75,13.25,12.25],[10.34131237750732,5.744562646538029,5.449631621480024,5.421612021659069,7.125933808241013],null,[1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,false,false,false,true],"fillColor":"grey","fillOpacity":0.8},null,null,null,null,["#1, 8.8 days","#2, 0.8 days","#3, 0.7 days","#4, 0.7 days","#5, 2 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[41.75,48.75],"lng":[12.25,16.75]}},"evals":[],"jsHooks":[]}
```
