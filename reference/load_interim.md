# Load an interim RData object

Loads an object from the `./data/interim/` directory created during the
GeoPressure analysis. This is a convenience function to quickly restore
saved objects (e.g., `tag`, `graph`, `path_most_likely`) for a given tag
`id`.

## Usage

``` r
load_interim(id, envir = parent.frame(), verbose = FALSE)
```

## Arguments

- id:

  A character string of length 1 corresponding to the tag identifier
  used in the GeoPressureTemplate (e.g., `"18LX"`). The function will
  look for a file named `./data/interim/{id}.RData`.

- envir:

  the environment where the data should be loaded.

- verbose:

  should item names be printed during loading?

## Value

Invisibly returns the names of the objects loaded (as in
[`base::load()`](https://rdrr.io/r/base/load.html)).
