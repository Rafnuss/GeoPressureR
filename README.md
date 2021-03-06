
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoPressureR <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Rafnuss/GeoPressureR/workflows/R-CMD-check/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions)
[![pkgdown](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Rafnuss/GeoPressureR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Rafnuss/GeoPressureR?branch=master)
[![lint](https://github.com/Rafnuss/GeoPressureR/actions/workflows/lint.yaml/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions/workflows/lint.yaml)

<!-- badges: end -->

`GeoPressureR` seeks to help researchers analyse data retrieved from
multi-sensor geolocators to accurately geoposition birds along their
migratory journey. We focus in particular on geopositiong using
atomspheric pressure measurements.

Part 1 provides an R wrapper around the
[GeoPressureAPI](https://github.com/Rafnuss/GeoPressureAPI) to generate
probability maps of the bird’s position based on a match between the
pressure timeseries measured by the geolocator and the reanalysed
pressure data from
[ERA5](https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5).
See the publication
[10.21203/rs.3.rs-1381915/v2](https://doi.org/10.21203/rs.3.rs-1381915/v2)
for more details.

Part 2 uses a graphical model to compute the full trajectory of the bird
building on light data, pressure measurements, flight duration and wind
data (optional). See the publication
[10.21203/rs.3.rs-1693751/v1](https://doi.org/10.21203/rs.3.rs-1693751/v1)
for more details.

**`GeoPressureR` is currently still in active development phase.**

## Where to start?

### Learn about `GeoPressureR` with 18LX

Using the example of a Great Reed Warbler (18LX), we developed a series
of [vignettes](https://r-pkgs.org/vignettes.html) to guide you through
each step in detail.

The [Pressure map](/articles/pressure-map.html) vignette is a good place
to start to understand the basic workflow used to compute a probability
map from pressure measurements (Part 1). It guides you through each
step, from loading the data, to labeling the timeseries, querying the
ERA5 pressure map and finally computing the probability map.

Once the probability maps are computed, the [Basic
graph](/articles/basic-graph.html) vignette helps you create the graph
and compute three main outputs: (1) the most likely trajectory, (2) the
(posterior) probability map of each stationary period and (3) a
simulation of possible paths (Part 2).

Finally, the [Wind graph](/articles/wind-graph.html) vignette extends
the capability of the basic graph to include wind data and refine
possible flight distances. It also allows to estimate wind support,
airspeed, and energy expenditure for each flight.

[![](man/figures/geopressureviz-demo.png "GeoPressureViz Demo")](https://rafnuss.shinyapps.io/GeoPressureViz/)
*Screenshot of
[GeoPressureViz](https://rafnuss.shinyapps.io/GeoPressureViz/) showing
the estimated position of the Great Reed Warbler 18LX at one stationary
period , based on (1) the pressure and light match (map colorscale and
timeseries) and (2) potential flight distances from previous and next
stationary periods (circles).*

### Start your own analysis

To help you start using `GeoPressureR` for your own study, we suggest
you use the R template
[GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate).
Using this standardized code structure and output will allow for code
sharing and troubleshooting, data archiving, work reproducibility, and
ease for reviewer to check the setting.

## Related resources

### Publication

> Raphaël Nussbaumer, Mathieu Gravey, Martins Briedis, Felix Liechti.
> Global positioning with animal-borne pressure sensors, 14 June 2022,
> PREPRINT (Version 2) available at Research Square
> \[<https://doi.org/10.21203/rs.3.rs-1381915/v2>\]

> Raphaël Nussbaumer, Mathieu Gravey, Martins Briedis, Felix Liechti.
> Inferring bird’s trajectory from multi-sensor geolocators and remote
> sensing with a graphical model, 25 May 2022, PREPRINT (Version 1)
> available at Research Square
> \[<https://doi.org/10.21203/rs.3.rs-1693751/v1>\]

### Presentation

> Raphaël Nussbaumer, Mathieu Gravey, Felix Liechti. Improving the
> spatial accuracy of multi-sensor geolocators’ position using
> atmospheric surface pressure. October 2021. *7th International
> Bio-logging Science Symposium*. PRESENTATION available at
> [Youtube](https://www.youtube.com/watch?v=0JsYU_xfKN8).

### Other resources

-   [GeoPressureMAT](https://github.com/Rafnuss/GeoPressureMAT) The
    development of the method was done on MATLAB. Here is the repo with
    all the codes.
-   [GeoPressure API](https://github.com/Rafnuss/GeoPressureServer) This
    is where the core computation with Google Earth Engine is done. You
    can have a look here to see how this is done.
-   [GeoLight](https://github.com/slisovski/GeoLight/tree/Update_2.01) R
    package to analyse light data. We are borrowing some of their
    function (see [`geolight.R`](./reference/index.html#geolight))
-   [PAMLr](https://github.com/KiranLDA/PAMLr): Extensive toolbox to
    analyse multi-sensor geolocator. Several function from this package
    are inspired from this package (see
    [`pam.R`](./reference/index.html#pam-data)).

## Projects using `GeoPressureR`

List of all research projects using `GeoPressureR` with links to the
code used to analyse and create figures. This might be helpful to get
idea of what and how to analyse your data and borrow some code sections
for your own project. Feel free to contact me if you’ll like to appear
on this list.

| Species             | GeoPressureTemplate/code                                                    | Publication/status                                                     |
|---------------------|-----------------------------------------------------------------------------|------------------------------------------------------------------------|
| Mongolian Nightjar  | [Rafnuss/MongolianNightjar](https://github.com/Rafnuss/MongolianNightjar)   | [Lathouwers et al. (2022)](https://doi.org/10.1007/s10336-022-02000-4) |
| Woodland Kingfisher | [Rafnuss/WoodlandKingfisher](https://github.com/Rafnuss/WoodlandKingfisher) | Osinubi et al. (in prep.)                                              |
| Northern Wheatear   | [Rafnuss/Val-Piora-Wheatear](https://github.com/Rafnuss/Val-Piora-Wheatear) | Rime et al. (in prep.)                                                 |
