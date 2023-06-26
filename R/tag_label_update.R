#' Update tag label and recompute tag
#'
#' When updating the labelling file of a tag, often, only a few stationary periods are changing. To
#' avoid recomputing the entire workflow, this function figure out which stationary period have
#' been changing on only update those in `tag$map_pressure` and `path_pres`.
#'
#'
#' @inheritParams tag_label
#' @inheritParams geopressure_map
#' @param path_pres geopressure_timeseries
#' @return a list containing the new `tag`, `tag` and `path_pres`.
#' @examples
#' tag <- tag_label(tag)
#'
#' tag <- tag |>
#'   tag_create(extent = c(-16, 23, 0, 50), scale = 1) |>
#'   geopressure_map(tag$pressure)
#'
#' path_pres <- tag |>
#'   map2path() |>
#'   geopressure_timeseries(tag$pressure)
#'
#' update <- tag_label_update(
#'   file = "data/1-tag_label/18LX-labeled-modif.csv",
#'   tag,
#'   tag,
#'   path_pres
#' )
#'
#' list2env(update, env = .GlobalEnv)
#' @export
tag_label_update <- function(tag,
                             file = glue::glue("data/1-tag_label/{tag$id}-labeled.csv"),
                             path_pres = NA) {
  # check input

  # read the new file
  tag_new <- tag_label(tag, file)

  # Find stap which have change, and those tha have not
  # 1. find the stationary period which matchs in terms of duration
  names(tag$stap)[names(tag$stap) == "stap_id"] <- "old_stap_id"
  stap_new <- merge(tag_new$stap, tag$stap, by = c("start", "end"), all.x = TRUE)

  # 2. Check that the pressure discard has not change in one of them Find the matching old stap_id
  stap_new$old_stap_id[unique(tag_new$pressure$stap_id[tag_new$pressure$label != tag$pressure$label])] <- NA

  # Only update tag if provided
  if (!all(is.na(tag))) {
    stap_id_recompute <- which(is.na(stap_new$old_stap_id))

    # Transfer known by keeping the same info as defined in the inital one
    init_stap_id_known <- !is.na(tag$stap$known_lat)
    known <- tag$stap[init_stap_id_known, names(tag$stap) %in% c("stap_id", "known_lat", "known_lon")]
    if (any(known$stap_id %in% stap_id_recompute)) {
      cli::cli_warn(c(
        "!" = "The known stationary period {.val {known$stap_id}} seems to have been changing.",
        ">" = "We will keep the same {.var stap_id} for {.field known} in {.var tag}"
      ))
    }

    # Find the stap_include which were excluded and keep the same one if found in the matching of the new stap
    init_stap_include_exclude <- which(!tag$stap$include)
    if (length(init_stap_include_exclude) > 0) {
      cli::cli_warn(c(
        "!" = "There are stationary period not included to be modeled on the inital tag. The matching with the new labelling can quite tricky in those case",
        ">" = "We will assume that the same stap_id excluded should again be excluded."
      ))
    }
    new_stap_include_exclude <- stap_new$stap_id[stap_new$old_stap_id %in% init_stap_include_exclude]

    # Build the new stap_id by taking the one that have change (i.e. NA) and remove the initial exclude
    stap_include <- stap_id_recompute[!(stap_id_recompute %in% new_stap_include_exclude)]

    stap_include <- stap_id_recompute[!(stap_id_recompute %in% new_stap_include_exclude)]
    tag_new <- tag_geostap(tag_new,
      extent = tag$extent,
      scale = tag$scale,
      known = known,
      stap_include = stap_include
    )

    tag_new <- geopressure_map(tag_new,
      tag_new$pressure,
      max_sample = tag$param$max_sample,
      margin = tag$param$margin,
      sd = tag$param$sd,
      thr_mask = tag$param$thr_mask,
      log_linear_pooling_weight = tag$param$log_linear_pooling_weight
    )

    # Add the likelihood which have not changed
    tag_new$likelihood[which(!is.na(stap_new$old_stap_id))] <- tag$map_pressure[stap_new$old_stap_id[!is.na(stap_new$old_stap_id)]]

    # Overwrite the model and keep the initial stap_id excluded
    tag_new$stap$include <- !(tag_new$stap$stap_id %in% new_stap_include_exclude)
  }


  # Only update path_pres if provided
  if (!all(is.na(path_pres))) {
    # Compute the new bext path
    path <- tag_new |>
      map2path()

    # Filter the path to only keep the stap_id to be recomputed
    path <- path[path$stap_id %in% stap_id_recompute, ]

    pressure_timeseries_chg <- geopressure_timeseries(path, tag_new$pressure)

    path_pres_new <- rbind(
      path_pres[!(path_pres$stap_ref %in% stap_id_recompute), ],
      pressure_timeseries_chg
    )

    path_pres_new <- path_pres_new[order(path_pres_new$date), ]
  }

  # list2env(list(tag=tag_new, tag = tag_new, pressure_timeseries = path_pres_new), env=.GlobalEnv)

  return(list(tag = tag_new, tag = tag_new, path_pre = path_pres_new))
}
