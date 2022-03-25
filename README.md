
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoPressureR <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/Rafnuss/GeoPressureR/workflows/R-CMD-check/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions)
[![pkgdown](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The goal of GeoPressureR is to help researcher to analyse pressure
measurement from geolocator. Firstly, it provides a R wrapper around the
[GeoPressure API](https://github.com/Rafnuss/GeoPressureServer) to query
the probability map using ERA5 pressure. Secondly, using a mathematical
graph, it allows you to model the trajectory producing the exact
probability map at stationary period and simulating paths.

## Installation

You can install the development version of GeoPressureR from
[GitHub](https://github.com/Rafnuss/GeoPressureR) with:

``` r
# install.packages("devtools")
devtools::install_github("Rafnuss/GeoPressureR")
```

## Where to start?

Using the example of a Great Reed Warbler (18LX), the vignettes were
designed to guide you through all the steps with great details.

The vignette [Pressure map](./articles/pressure-map.html) is a good
place to understand the basic workflow used to compute a proability map
from pressure measurement. It will guide you through all the steps.

Once the probability maps are computed, [the vignette on
graph](./articles/basic-graph.html) will help you create the graph and
compute the three outputs: the most likely trajectory, the (posteriori)
probability map of each stationary period and simulation of possible
path.

## Related ressources

-   [GeoPressureMAT](https://github.com/Rafnuss/GeoPressureMAT)
    Developement of the method was done on MATLAB. Here is the repo with
    all the codes.
-   [GeoPressure API](https://github.com/Rafnuss/GeoPressureServer) This
    is where the hard core computation with Google Earth Engine is done.
    You can have a look here to see how this is done.
-   [GeoLight](https://github.com/slisovski/GeoLight/tree/Update_2.01) R
    package to analyse light data.
-   [PAMLr](https://github.com/KiranLDA/PAMLr): Extensive toolbox to
    analyse multi-sensor geolocator. Several function from this package
    are inspired from this package.

## How to cite?

*manuscript in preparation*

## Want to contribute?

The code is still in active development. Feel free to [submit an issue
on Github](https://github.com/Rafnuss/GeoPressureR/issues) with
suggetions or bugs
