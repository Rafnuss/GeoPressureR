#' Update a `tag` object
#'
#' When updating the labelling file of a `tag`, we often change only a few stationary periods. To
#' avoid recomputing the entire workflow, this function identifies which stationary periods have
#' been modified and updates only these ones in `tag$map_pressure` and `pressurepath`.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param file absolute or relative path of the label file.
#' @param known data.frame containing the known positions of the bird (e.g., equipment or retrieval
#' site). The default is to use `tag$stap`, which assumes that the `stap_id` has not changed for the
#' known stationary periods.
#' @param include_stap_id vector of `stap_id` defining which stationary period to model, that is,
#' to compute in the likelihood map and use in the graph. The default is to use the same original
#' value.
#' @param quiet logical to hide messages about the progress
#'
#' @return The updated `tag` object
#' @examples
#' setwd(system.file("extdata", package = "GeoPressureR"))
#' tag <- tag_create("18LX", quiet = TRUE) |>
#'   tag_label(quiet = TRUE) |>
#'   tag_set_map(extent = c(-16, 23, 0, 50), scale = 1) |>
#'   geopressure_map(quiet = TRUE)
#'
#' print(tag)
#'
#' tag <- tag_update(tag,
#'   file = "./data/tag-label/18LX-labeled-updated.csv",
#'   quiet = TRUE
#' )
#'
#' print(tag)
#' @family tag
#' @export
tag_update <- function(tag,
                       file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv"),
                       known = NULL,
                       include_stap_id = NULL,
                       quiet = FALSE) {
  # Only work if the tag has already been labelled.
  tag_assert(tag, "map_pressure")

  # Re-create the original tag before label
  tag_new <- tag_create(
    id = tag$param$id,
    manufacturer = NULL,
    directory = glue::glue("./data/raw-tag/{tag$param$id}/"),
    crop_start = tag$param$crop_start,
    crop_end = tag$param$crop_end,
    pressure_file = tag$param$pressure_file,
    light_file = tag$param$light_file,
    acceleration_file = tag$param$acceleration_file,
    quiet = TRUE
  )

  # Read the new file and compute the stationary period
  tag_new <- tag_label_read(tag_new, file = file)
  tag_new <- tag_label_stap(tag_new, quiet = TRUE)

  # check length of sd
  if (length(tag$param$sd) != 1 && length(tag$param$sd) != nrow(tag_new$stap)) {
    cli::cli_abort(c(
      "x" = "{.var tag$param$sd} is of length {.val {length(tag$param$sd)}}.",
      ">" = "{.var tag$param$sd} needs to be of length {.val {1}} or {.val {nrow(tag_new$stap)}} \\
      ({.code nrow(tag_new$stap)})."
    ))
  }

  # Find which stap have change or not
  # 1. find the stationary period based on start and end date
  # create a copy to modify the column name and allow a merge
  tmp <- tag$stap[names(tag$stap) %in% c("start", "end", "stap_id")]
  names(tmp)[names(tmp) == "stap_id"] <- "old_stap_id"
  stap_new <- merge(tag_new$stap, tmp, by = c("start", "end"), all.x = TRUE)

  # If not specify, build the original known
  if (is.null(known)) {
    # Use the same original known
    # We will only processed by assuming that the known stap_id have not changed
    # (discard of pressure is ok to have change)
    known <- tag$param$known
  }

  # Check that the the known stap_id have not changed. Only check for stap_id more than 1 to avoid
  # comparing negative indexing and 1.
  tmp <- known$stap_id[known$stap_id > 1]
  if (any(stap_new$stap_id[which(stap_new$old_stap_id == tmp)] != tmp)) {
    cli::cli_warn(c(
      "x" = "Known position were defined at stationary period{?s} \\
      {.val {as.character(tmp)}}, yet th{?is/ese} stationary period{?s} ha{?s/ve} \\
      changed",
      ">" = "Provides {.var known} argument to {.fun tag_upate} or start again from the raw data"
    ))
  }

  # Deal with include
  if (is.null(include_stap_id)) {
    if (is.null(tag$param$include_stap_id)) {
      # If no include_sta_id were specify on creation of tag, tag$param$include_stap_id was set as
      # NULL. In this case, we use the default value of including all stap_id.
      include_stap_id <- tag_new$stap$stap_id
    } else {
      # If include_stap_id was specific, then use the same one
      include_stap_id <- tag$param$include_stap_id
    }
  }

  # Check that include_stap_id exists
  tmp <- include_stap_id %in% tag_new$stap$stap_id
  if (any(!tmp)) {
    cli::cli_abort(c(
      "x" = "{.field include_stap_id} was specified for {.val {include_stap_id}}, but \\
        {.val {include_stap_id[!tmp]}} is not available with the new label data.",
      ">" = "Provides {.var include_stap_id} argument to {.fun tag_upate} or start again from the \\
      raw data"
    ))
  }

  # Build the new tag_new as it would look like without using update
  tag_new <- tag_set_map(tag_new,
    extent = tag$param$extent,
    scale = tag$param$scale,
    known = known,
    include_min_duration = tag$param$include_min_duration,
    include_stap_id = include_stap_id
  )

  # Build the tag to compute only the stap_id which need to be recompute. We will modify the include
  # column of stpa, but we don't want this stap to be returned. The return stap should be the same
  # as if it didn't go through tag_update()
  tag_new_include <- tag_new

  # 2. Check which stap_id have a discard pressure label which has changed
  # we don't care about flight label change because they have already impacted the merge of the
  # new stap, only pressure outlier and stapelev are important at this stage
  discard_label_chg <- (tag_new$pressure$label != tag$pressure$label) &
    (tag_new$pressure$label != "flight" | tag$pressure$label != "flight")

  # Overwrite include to false to all stap_id which have not changed (ie. no discard_label_chg)
  # and which have not a matching old_stap_id
  tag_new_include$stap$include[
    !(tag_new_include$stap$stap_id %in% unique(tag_new$pressure$stap_id[discard_label_chg])) &
      !is.na(stap_new$old_stap_id)
  ] <- FALSE

  # Check if nothing had changed
  if (all(!tag_new_include$stap$include)) {
    cli::cli_warn(c(
      "!" = "There are no changes with the new label file",
      ">" = "the original {.var tag} will be returned.\f"
    ))
    return(tag)
  }

  # Build the new map
  tag_new_include <- geopressure_map(tag_new_include,
    max_sample = tag$param$max_sample,
    margin = tag$param$margin,
    sd = tag$param$sd,
    thr_mask = tag$param$thr_mask,
    log_linear_pooling_weight = tag$param$log_linear_pooling_weight,
    keep_mask = "map_pressure_mask" %in% names(tag),
    keep_mse = "map_pressure_mse" %in% names(tag),
    quiet = quiet
  )

  # Retrieve sd
  tag_new$param$max_sample <- tag_new_include$param$max_sample
  tag_new$param$margin <- tag_new_include$param$margin
  tag_new$param$sd <- tag_new_include$param$sd
  tag_new$param$thr_mask <- tag_new_include$param$thr_mask
  tag_new$param$log_linear_pooling_weight <- tag_new_include$param$log_linear_pooling_weight

  if ("nb_sample" %in% names(tag$stap)) {
    tag_new$stap$nb_sample <- tag_new_include$stap$nb_sample
    tag_new$stap$nb_sample[stap_new$stap_id[!tag_new_include$stap$include]] <-
      tag$stap$nb_sample[stap_new$old_stap_id[!tag_new_include$stap$include]]
  }

  # Add the likelihood which have not changed
  tag_new$map_pressure <- tag_new_include$map_pressure
  tag_new$map_pressure$data[stap_new$stap_id[!tag_new_include$stap$include]] <-
    tag$map_pressure$data[stap_new$old_stap_id[!tag_new_include$stap$include]]
  tag_new$map_pressure$stap <- tag_new$stap

  if ("map_pressure_mse" %in% names(tag)) {
    tag_new$map_pressure_mse <- tag_new_include$map_pressure_mse
    tag_new$map_pressure_mse$data[stap_new$stap_id[!tag_new_include$stap$include]] <-
      tag$map_pressure_mse$data[stap_new$old_stap_id[!tag_new_include$stap$include]]
    tag_new$map_pressure_mse$stap <- tag_new$stap
  }

  if ("map_pressure_thr" %in% names(tag)) {
    tag_new$map_pressure_thr <- tag_new_include$map_pressure_thr
    tag_new$map_pressure_thr$data[stap_new$stap_id[!tag_new_include$stap$include]] <-
      tag$map_pressure_thr$data[stap_new$old_stap_id[!tag_new_include$stap$include]]
    tag_new$map_pressure_thr$stap <- tag_new$stap
  }

  if ("map_light" %in% names(tag)) {
    if (any(is.na(stap_new$old_stap_id)) || any(stap_new$stap_id != stap_new$old_stap_id)) {
      cli::cli_warn("Light map {.code tag$map_light} was deleted as some stationary periods have \\
                    changed.\f")
      tag_new$map_light <- NULL
      tag_new$twilight <- NULL
    } else {
      # No stap have changed, we can keep the original light map
      tag_new$twilight <- tag$twilight
      tag_new$map_light <- tag$map_light

      tag_new$param$twl_thr <- tag_new_include$param$twl_thr
      tag_new$param$twl_offset <- tag_new_include$param$twl_offset
      tag_new$param$twilight_file <- tag_new_include$param$twilight_file
      tag_new$param$twl_calib_adjust <- tag_new_include$param$twl_calib_adjust
      tag_new$param$twl_llp <- tag_new_include$param$twl_llp
    }
  }

  return(tag_new)
}
