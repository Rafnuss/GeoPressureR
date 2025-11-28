# Update a `pressurepath`

**\[deprecated\]**

This function was deprecated because (1) few people use it, (2) hard to
maintain and (3) not so slow to use the alternative of creating of new
`pressurepath` altogether.

When updating the labelling file of a tag, often, only a few stationary
periods are changing. To avoid recomputing the entire workflow, this
function figure out which stationary period have been changing on only
update those in `tag$map_pressure` and `pressurepath`.

## Usage

``` r
pressurepath_update(pressurepath, tag, path = tag2path(tag), quiet = FALSE)
```

## Arguments

- pressurepath:

  a GeoPressureR `pressurepath` data.frame

- tag:

  a GeoPressureR `tag` object.

- path:

  a GeoPressureR `path` data.frame.

- quiet:

  logical to hide messages about the progress

## Value

a list containing the new `pressurepath`.
