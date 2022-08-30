
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoPressureR <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Rafnuss/GeoPressureR/workflows/R-CMD-check/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions)
[![pkgdown](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Rafnuss/GeoPressureR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Rafnuss/GeoPressureR?branch=master)
[![lint](https://github.com/Rafnuss/GeoPressureR/actions/workflows/lint.yaml/badge.svg)](https://github.com/Rafnuss/GeoPressureR/actions/workflows/lint.yaml)
<!-- badges: end -->

GeoPressureR is a R package which help researchers determine the
position of a bird based on the data retrieved from multi-sensor
geolocators.

The main novelty of this package is to match atmospheric pressure
measurements to weather reanalysis database (ERA-5) and produce a
probability maps of position.

In addition, we use a graphical model to combine pressure, light,
acceleration and windspeed data into a trajectory model of the bird.

## Learn how to use GeoPressureR

The
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/) is
a great place to start learning about how you can determine the
trajectory of your bird.

Using the example of a Great Reed Warbler (18LX), this user guide takes
you through each step of the analysis in detail.

<p align="center">
<a href="https://raphaelnussbaumer.com/GeoPressureManual/"><img src="https://github.com/Rafnuss/GeoPressureManual/raw/main/assets/cover.png" style="margin: 0 1rem 0 1rem;box-shadow: 0 .5rem 1rem rgba(0,0,0,.15);" align="center" width="250" height="328"></a>
</p>

## Start your own analysis

Once you are familiar with the method and want to start your own study,
we suggest you use
[GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate), a
[github template
repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository)
which provides a standard code structure for your analysis.

Using this standardized code structure and output will allow for code
sharing and troubleshooting, data archiving, work reproducibility, and
easy check by reviewers.

## Getting help

The best way to get help is to use the [github
discussions](https://github.com/Rafnuss/GeoPressureR/discussions).

Contributions to the code should follow the [Contributor Code of
Conduct](https://raphaelnussbaumer.com/GeoPressureR/CONTRIBUTING.html).
