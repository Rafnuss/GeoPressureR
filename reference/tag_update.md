# Update a `tag` object

**\[deprecated\]**

This function was deprecated because (1) few people use it, (2) hard to
maintain and (3) not so slow to use the alternative of creating of new
`tag` altogether.

When updating the labelling file of a `tag`, we often change only a few
stationary periods. To avoid recomputing the entire workflow, this
function identifies which stationary periods have been modified and
updates only these ones in `tag$map_pressure` and `tag$map_light`.

## Usage

``` r
tag_update(
  tag,
  file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv"),
  known = NULL,
  include_stap_id = NULL,
  quiet = FALSE
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- file:

  absolute or relative path of the label file.

- known:

  data.frame containing the known positions of the bird (e.g., equipment
  or retrieval site). The default is to use `tag$stap`, which assumes
  that the `stap_id` has not changed for the known stationary periods.

- include_stap_id:

  vector of `stap_id` defining which stationary period to model, that
  is, to compute in the likelihood map and use in the graph. The default
  is to use the same original value.

- quiet:

  logical to hide messages about the progress

## Value

The updated `tag` object
