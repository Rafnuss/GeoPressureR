# Download ground elevation along a path

This function is a wrapper of the GeoPressureAPI elevationPath entry
point. It queries the ground elevation from SRTM
([SRTM90_V4](https://developers.google.com/earth-engine/datasets/catalog/CGIAR_SRTM90_V4))
along a polyline specify by `path`.

Because the position are often defined on a relative coarse scale (e.g.,
0.25° ~ 29km), you can request the elevation at a specify resolution
defined by `scale` and return pre-defined `percentile` of the elevation
at this resolution.

The returned data.frame provide the ground elevation along the path with
a resolution defined by `sampling_scale`.

## Usage

``` r
path2elevation(
  path,
  scale = 4,
  sampling_scale = scale,
  percentile = c(10, 50, 90),
  debug = FALSE
)
```

## Arguments

- path:

  a GeoPressureR `path` data.frame

- scale:

  spatial resolution of the SRTM to use to query the elevation. `scale`
  is defined as the number of pixels per 1° latitude-longitude (see
  [`tag_set_map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_set_map.md)
  for details). Native resolution of SRTM is 30m.

- sampling_scale:

  spatial resolution of the point along the polyline on which the SRTM
  is estimated. Same unit as `scale`.

- percentile:

  percentile of the ground elevation distribution found within each grid
  cell of the SRTM at the resolution defined by `scale`. `50`
  corresponds to the median.

- debug:

  logical to display additional information to debug a request

## Value

A data.frame containing

- `stap_id` numeric value corresponding to the ratio of distance between
  position of known stap

- `lon`

- `lat`

- `distance` distance in km along the path starting at the first stap_id

## See also

Other path:
[`ind2path()`](https://raphaelnussbaumer.com/GeoPressureR/reference/ind2path.md),
[`plot_path()`](https://raphaelnussbaumer.com/GeoPressureR/reference/plot_path.md),
[`tag2path()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag2path.md)

## Examples

``` r
# Create a path
path <- data.frame(
  lon = c(8.47, 9.41, 9.01, -0.91, 14.24, 27.30, 34.39, 30.00),
  lat = c(48.89, 44.78, 40.07, 37.68, 17.33, 7.32, 8.09, -23.13),
  start = as.POSIXct(
    c(
      "2017-05-01 00:42", "2017-05-03 01:22", "2017-05-03 22:47", "2017-05-06 03:32",
      "2017-05-07 01:12", "2017-05-07 22:32", "2017-05-09 21:52", "2017-05-10 21:12"
    ),
    tz = "UTC"
  ),
  end = as.POSIXct(
    c(
      "2017-05-02 22:12", "2017-05-03 20:12", "2017-05-06 02:47", "2017-05-06 23:22",
      "2017-05-07 17:42", "2017-05-09 20:27", "2017-05-10 19:57", "2017-05-11 21:17"
    ),
    tz = "UTC"
  ),
  stap_id = seq(1, 8)
)


plot_path(path)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[8.470000000000001,9.41,9.01,-0.91,14.24,27.3,34.39,30],"lat":[48.89,44.78,40.07,37.68,17.33,7.32,8.09,-23.13]}]]],null,[1,1,1,1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.89,44.78,40.07,37.68,17.33,7.32,8.09,-23.13],[8.470000000000001,9.41,9.01,-0.91,14.24,27.3,34.39,30],[7.040464112938501,5.647161892331753,7.27946187557618,5.720676213867304,5.463480860513616,7.056527334184535,5.876443933167747,6.005201565352651],null,[1,1,1,1,1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,true,true,true,true,true,true,true],"fillColor":"grey","fillOpacity":0.8},null,null,null,null,["#1, 1.9 days","#2, 0.8 days","#3, 2.2 days","#4, 0.8 days","#5, 0.7 days","#6, 1.9 days","#7, 0.9 days","#8, 1 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-23.13,48.89],"lng":[-0.91,34.39]}},"evals":[],"jsHooks":[]}
elevation <- path2elevation(path)
#> Error in `$<-.data.frame`(`*tmp*`, "stap_id", value = integer(0)): replacement has 0 rows, data has 389

plot(elevation$distance, elevation$X50,
  type = "l",
  ylab = "Elevation (m)", xlab = "Distance from start (km)"
)
#> Error: object 'elevation' not found
lines(elevation$distance, elevation$X10, lty = 5)
#> Error: object 'elevation' not found
lines(elevation$distance, elevation$X90, lty = 5)
#> Error: object 'elevation' not found
id <- elevation$stap_id %% 1 == 0
#> Error: object 'elevation' not found
points(elevation$distance[id], elevation$X90[id], col = "red")
#> Error: object 'elevation' not found
```
