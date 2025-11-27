# Create bird flight traits

This function returns a list with the four morphological information
necessary to construct the power curve: mass, wing span, wing aspect
ratio, and body frontal area.

When any of these variables are missing, we query the AVONET database
([doi:10.6084/m9.figshare.16586228.v5](https://doi.org/10.6084/m9.figshare.16586228.v5)
) using the scientific name from [the Clements
Checklist](https://www.birds.cornell.edu/clementschecklist/).

## Usage

``` r
bird_create(
  scientific_name,
  mass = NULL,
  wing_span = NULL,
  wing_aspect = NULL,
  wing_area = NULL,
  body_frontal_area = NULL,
  species_name = lifecycle::deprecated()
)
```

## Arguments

- scientific_name:

  Scientific name of the species

- mass:

  Mass of the bird in kilograms

- wing_span:

  Wing span in meters

- wing_aspect:

  Wing aspect ratio (no unit)

- wing_area:

  Wing area in meter square

- body_frontal_area:

  Body frontal area in meter square

- species_name:

  **\[deprecated\]** Use `scientific_name` instead

## Value

List containing mass, wing span, wing aspect ratio, and body frontal
area.

## See also

Other bird:
[`print.bird()`](https://raphaelnussbaumer.com/GeoPressureR/reference/print.bird.md)

Other movement:
[`graph_set_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_set_movement.md),
[`graph_transition()`](https://raphaelnussbaumer.com/GeoPressureR/reference/graph_transition.md),
[`plot_graph_movement()`](https://raphaelnussbaumer.com/GeoPressureR/reference/plot_graph_movement.md),
[`speed2prob()`](https://raphaelnussbaumer.com/GeoPressureR/reference/speed2prob.md),
[`tag_download_wind()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_download_wind.md)

## Examples

``` r
# Using AVONET dataset
bird_create("Acrocephalus arundinaceus")
#> 
#> ── GeoPressureR `bird` object ──────────────────────────────────────────────────
#> • Scientific name: Acrocephalus arundinaceus
#> • Mass: 0.03 (kg).
#> • Body frontal area: 0 (m^2).
#> • Wing span: 0.2 (m).
#> • Wing aspect: 7.1 (-).

# Using AVONET dataset + custom values
bird_create("Acrocephalus arundinaceus", wing_aspect = 8)
#> 
#> ── GeoPressureR `bird` object ──────────────────────────────────────────────────
#> • Scientific name: Acrocephalus arundinaceus
#> • Mass: 0.03 (kg).
#> • Body frontal area: 0 (m^2).
#> • Wing span: 0.2 (m).
#> • Wing aspect: 8 (-).

# Import your own bird. You will need mass, and at least two of wing_span,
# wing_aspect or wing_area.
bird_create("Madynuss nutshell", mass = 8, wing_span = 0.2, wing_aspect = 4)
#> 
#> ── GeoPressureR `bird` object ──────────────────────────────────────────────────
#> • Scientific name: Madynuss nutshell
#> • Mass: 8 (kg).
#> • Body frontal area: 0 (m^2).
#> • Wing span: 0.2 (m).
#> • Wing aspect: 4 (-).
```
