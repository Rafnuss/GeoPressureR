#' Update a `pressurepath`
#'
#' When updating the labelling file of a tag, often, only a few stationary periods are changing. To
#' avoid recomputing the entire workflow, this function figure out which stationary period have
#' been changing on only update those in `tag$map_pressure` and `pressurepath`.
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame
#' @inheritParams pressurepath_create
#' @return a list containing the new `pressurepath`.
#' @family pressurepath
#' @export
pressurepath_update <- function(pressurepath,
                                tag,
                                path = tag2path(tag)) {
  # Check pressurepath
  assertthat::assert_that(is.data.frame(pressurepath))
  assertthat::assert_that(assertthat::has_name(
    pressurepath, c(
      "date", "pressure_tag", "label", "stap_id", "pressure_era5", "altitude", "lat",
      "lon", "pressure_era5_norm", "stap_ref"
    )
  ))

  # Check tag and pressure
  tag_assert(tag, "setmap")
  if (assertthat::has_attr(pressurepath, "preprocess")) {
    preprocess <- attr(pressurepath, "preprocess")
  } else {
    preprocess <- FALSE
  }
  if (preprocess) {
    pressure <- geopressure_map_preprocess(tag)
  } else {
    pressure <- tag$pressure
  }

  # Assert path
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("{.var path} is empty.")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some {.field stap_id} of {.var path} are not present in {.var tag$pressure}.\f")
  }


  # Find the new stap_id for which the corresponding pressure has a different old stap_id
  tmp <- merge(pressure,
    data.frame(
      date = pressurepath$date,
      stap_id_old = pressurepath$stap_id
    ),
    by = "date"
  )
  stap_id_recompute_pres <- unique(tmp[tmp$stap_id != tmp$stap_id_old, ]$stap_id)

  # Find the new stap_id for which the corresponding path which has a different old stap_id
  pp_path <- unique(pressurepath[, names(pressurepath) %in% names(path)])
  names(pp_path)[names(pp_path) == "lat"] <- "lat_old"
  names(pp_path)[names(pp_path) == "lon"] <- "lon_old"
  tmp <- merge(path, pp_path, by = "stap_id")
  stap_id_recompute_path <- tmp$stap_id[tmp$lat_old != tmp$lat | tmp$lon_old != tmp$lon |
    is.na(tmp$lon_old)]

  stap_id_recompute <- union(stap_id_recompute_path, stap_id_recompute_pres)

  if (length(stap_id_recompute) > 0) {
    if (assertthat::has_attr(pressurepath, "include_flight")) {
      include_flight <- attr(pressurepath, "include_flight")
    } else {
      include_flight <- any(pressurepath$stap_id == 0)
    }

    pressurepath_diff <- pressurepath_create(tag,
      path = path[path$stap_id %in% stap_id_recompute, ],
      include_flight = include_flight,
      preprocess = preprocess
    )

    pressurepath_new <- rbind(
      pressurepath[!(pressurepath$stap_ref %in% stap_id_recompute), ],
      pressurepath_diff
    )
  } else {
    pressurepath_new <- pressurepath
  }

  # Update label which are not affecting pressurepath
  pressurepath_new$label <- NULL
  pressurepath_new <- merge(pressurepath_new,
    pressure[, names(pressurepath) %in% c("date", "label")],
    by = "date"
  )


  # Sort by date
  pressurepath_new <- pressurepath_new[order(pressurepath_new$date), ]

  return(pressurepath_new)
}
