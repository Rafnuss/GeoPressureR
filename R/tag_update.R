#' Update tag label and recompute tag
#'
#' When updating the labelling file of a tag, often, only a few stationary periods are changing. To
#' avoid recomputing the entire workflow, this function figure out which stationary period have
#' been changing on only update those in `tag$map_pressure` and `pressurepath`.
#'
#'
#' @inheritParams tag_label
#' @return a list containing the new `tag`
#' @examples
#' tag <- tag_create("18LX") |>
#'   tag_label() |>
#'   tag_geostap(extent = c(-16, 23, 0, 50), scale = 1) |>
#'   geopressure_map(tag$pressure)
#'
#' tag
#'
#' tag <- tag_update(tag,
#'   file = "././data/tag-label/18LX-labeled-modif.csv"
#' )
#' tag
#' @export
tag_update <- function(tag,
                       file = glue::glue("./data/tag-label/{tag$param$id}-labeled.csv"),
                       known = NULL) {
  # Only work if the tag has already been labeled.
  tag_assert(tag, "map_pressure")

  # Re-create the original tag before label
  tag_new <- tag_create(id = tag$param$id,
                        pressure_file = tag$param$sensor_paths[1],
                        light_file = tag$param$sensor_paths[2],
                        acceleration_file = tag$param$sensor_paths[3],
                        crop_start = tag$param$create_crop_start,
                        crop_end = tag$param$create_crop_end,
                        quiet = TRUE)

  # Read the new file and compute the stationary period
  tag_new <- tag_label_read(tag_new, file = file)
  tag_new <- tag_label_stap(tag_new, quiet = TRUE)

  # Find stap which have change, and those tha have not
  # 1. find the stationary period match based on start and end date
  tag_stap_copy <- tag$stap[names(tag$stap) %in% c("start", "end", "stap_id")] # create a copy to modify the column name and allow a merge
  names(tag_stap_copy)[names(tag_stap_copy) == "stap_id"] <- "old_stap_id"
  stap_new <- merge(tag_new$stap, tag_stap_copy, by = c("start", "end"), all.x = TRUE)

  # Deal with known
  # As known are defined with stap_id, we will only processed by assuming that the known stap_id have not changed (discard of pressure is ok though)
  # Build the original known
  if (is.null(known)) {
    # Use the exact same known as provided
    known <- tag$stap[!is.na(tag$stap$known_lat), names(tag$stap) %in% c("stap_id", "known_lat", "known_lon")]
    # Check that the old_stap_id is the same as the new ones for the known stap_id
    if (!stap_new$stap_id[stap_new$old_stap_id == known$stap_id] == known$stap_id) {
      cli::cli_abort(c(
        "x" = "Known position were defined at stationary period{?s} {.val {known$stap_id}}, yet these stationary period have changed because you changed {.val flight} label before or after them.",
        ">" = "In such case, you need start again from the raw data or provide an updated {.var known} arguement."
      ))
    }
  }

  # Find the stap_include which were excluded and keep the same one if found in the matching of the new stap
  old_stap_include_exclude <- tag$stap$stap_id[!tag$stap$include]
  if (!all(old_stap_include_exclude %in% stap_new$old_stap_id)) {
    tmp <- old_stap_include_exclude[!(old_stap_include_exclude %in% stap_new$old_stap_id)]
    cli::cli_warn(c(
      "!" = "Stationary period{?s} {.val tmp} were excluded ({.code include = FALSE}) from the \\
        original {.var tag} but are not present in the new {.var tag}",
      ">" = "We will assume that the same stap_id excluded should again be excluded."
    ))
  }
  # Build the include to be used in the modeling, but not for geopressure and geolight
  stap_new$include <- !(stap_new$old_stap_id %in% old_stap_include_exclude)

  # Build the geostapof stap_id to recompute and the one included
  tag_new <- tag_geostap(tag_new,
                         extent = tag$extent,
                         scale = tag$scale,
                         known = known,
                         include_stap_id = stap_new$stap_id[stap_new$include]
  )


  # 2. Check which stap_id have a discard pressure label which has changed
  # we don't care about flight label change because they have already impacted the merge of the new stap, only pressure outliar are important at this stage
  discard_label_chg <- (tag_new$pressure$label == "discard" | tag$pressure$label == "discard") &
    (tag_new$pressure$label != tag$pressure$label)
  stap_new$recompute <- F
  stap_new$recompute[tag_new$pressure$stap_id[discard_label_chg]] <- TRUE

  # Acutally not needed
  # if ("acceleration" %in% names(tag))
  #  stap_new$old_stap_id[unique(tag_new$acceleration$stap_id[tag_new$acceleration$label != tag$acceleration$label])] <- NA

  # Check if known have change pressure label, in which case we don't really care.
  # if (any(known$stap_id %in% stap_new$stap_id[stap_new$recompute])) {
  #  cli::cli_warn(c(
  #    "!" = "The labeling of pressure during the known stationary period {.val {known$stap_id}} has changed.",
  #    ">" = "We will keep the same {.var tag$stap$known} and the likelihood map is kept the same."
  #  ))
  # }

  tag_new$stap$include <- stap_new$include & stap_new$recompute

  # Check if nothing had changed
  if (all(!tag_new$stap$include)){
    cli::cli_warn(c(
      "!" = "There are no changes with the new label file",
      ">" = "the original {.var tag} will be returned."
    ))
    return(tag)
  }

  # Build the new map
  tag_new <- geopressure_map(tag_new,
                             max_sample = tag$param$max_sample,
                             margin = tag$param$margin,
                             sd = tag$param$sd,
                             thr_mask = tag$param$thr_mask,
                             log_linear_pooling_weight = tag$param$log_linear_pooling_weight,
                             keep_mse_mask = "map_pressure_mse" %in% names(tag)
  )

  tag_new_arch <- tag_new

  # Add the likelihood which have not changed
  tag_new$map_pressure[stap_new$stap_id[!stap_new$recompute]] <- tag$map_pressure[stap_new$old_stap_id[!stap_new$recompute]]
  if ("map_pressure_mse" %in% names(tag)) {
    tag_new$map_pressure_mse[stap_new$stap_id[!stap_new$recompute]] <- tag$map_pressure_mse[stap_new$old_stap_id[!stap_new$recompute]]
    tag_new$map_pressure_thr[stap_new$stap_id[!stap_new$recompute]] <- tag$map_pressure_thr[stap_new$old_stap_id[!stap_new$recompute]]
  }

  # Overwrite the model and keep the initial stap_id excluded
  tag_new$stap$include <- stap_new$include


  if ("map_light" %in% names(tag)) {
    if (all(stap_new$stap_id == stap_new$old_stap_id)) {
      # No stap have changed, we can keep the original light map
      tag_new$twilight <- tag$twilight
      tag_new$map_light <- tag$map_light
    } else {
      cli::cli_warn("Light maps needs to be recomputed as some stationay periods have changed.")
      tag_new$map_light <- NULL
      tag_new$twilight <- NULL
    }
  }

  return(tag_new)
}
