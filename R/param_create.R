#' Create a `param` list
#'
#' @description
#' Create the list of parameter for GeoPressureR `tag` and `graph` objects.
#'
#' `param` list are mostly used to archived the actual value of parameters used to create a `tag`
#' and/or a `graph`, thus allowing for examination of parameters post-creation. This function should
#' therefore not be used to set/define parameters ahead of computation. In reality, there are very
#' few external case of use for this function.
#'
#' @param id Unique identifier of a tag.
#' @param default logical to initiate param with default value of the package.
#' @param ... arguments passed to other methods.
#'
#' @return A GeoPressureR `param` list
#'
#' @examples
#' param <- param_create("18LX", default = TRUE)
#' print(param)
#'
#' @family param
#' @export
param_create <- function(id, default = FALSE, ...) {
  assertthat::assert_that(is.character(id))

  if (default) {
    param <- list(
      id = id,
      tag_create = list(
        manufacturer = formals(tag_create)$manufacturer,
        crop_start = formals(tag_create)$crop_start,
        crop_end = formals(tag_create)$crop_end,
        directory = formals(tag_create)$directory,
        pressure_file = formals(tag_create)$pressure_file,
        light_file = formals(tag_create)$light_file,
        acceleration_file = formals(tag_create)$acceleration_file,
        temperature_external_file = formals(tag_create)$temperature_external_file,
        temperature_internal_file = formals(tag_create)$temperature_internal_file,
        magnetic_file = formals(tag_create)$magnetic_file,
        assert_pressure = formals(tag_create)$assert_pressure
      ),
      tag_label = list(
        file = formals(tag_label)$file
      ),
      tag_set_map = list(
        extent = NULL,
        scale = formals(tag_set_map)$scale,
        known = formals(tag_set_map)$known,
        include_stap_id = formals(tag_set_map)$include_stap_id,
        include_min_duration = formals(tag_set_map)$include_min_duration
      ),
      geopressure_map = list(
        max_sample = formals(geopressure_map)$max_sample,
        margin = formals(geopressure_map)$margin,
        sd = formals(geopressure_map)$sd,
        thr_mask = formals(geopressure_map)$thr_mask,
        log_linear_pooling_weight = formals(geopressure_map)$log_linear_pooling_weight,
        compute_known = formals(geopressure_map)$compute_known
      ),
      twilight_create = list(
        twl_thr = formals(twilight_create)$twl_thr,
        twl_offset = formals(twilight_create)$twl_offset,
        transform_light = formals(twilight_create)$transform_light
      ),
      twilight_label_read = list(
        file = formals(twilight_label_read)$file
      ),
      geolight_map = list(
        twl_calib_adjust = formals(geolight_map)$twl_calib_adjust,
        twl_llp = formals(geolight_map)$twl_llp,
        compute_known = formals(geolight_map)$compute_known
      ),
      graph_create = list(
        thr_likelihood = formals(graph_create)$thr_likelihood,
        thr_gs = formals(graph_create)$thr_gs,
        likelihood = formals(graph_create)$likelihood
      ),
      graph_set_movement = list(
        type = formals(graph_set_movement)$type,
        method = formals(graph_set_movement)$method,
        shape = formals(graph_set_movement)$shape,
        scale = formals(graph_set_movement)$scale,
        location = formals(graph_set_movement)$location,
        power2prob = formals(graph_set_movement)$power2prob,
        low_speed_fix = formals(graph_set_movement)$low_speed_fix,
        zero_speed_ratio = formals(graph_set_movement)$zero_speed_ratio
      ),
      bird = list(
        mass = formals(bird_create)$mass,
        wing_span = formals(bird_create)$wing_span,
        wing_aspect = formals(bird_create)$wing_aspect,
        wing_area = formals(bird_create)$wing_area,
        body_frontal_area = formals(bird_create)$body_frontal_area
      ),
      graph_add_wind = list(
        rounding_interval = formals(graph_add_wind)$rounding_interval,
        interp_spatial_linear = formals(graph_add_wind)$interp_spatial_linear,
        thr_as = formals(graph_add_wind)$thr_as,
        file = formals(graph_add_wind)$file
      ),
      graph_simulation = list(
        nj = formals(graph_simulation)$nj
      ),
      pressurepath_create = list(
        variable = formals(pressurepath_create)$variable,
        solar_dep = formals(pressurepath_create)$solar_dep
      ),
      GeoPressureR_version = utils::packageVersion("GeoPressureR")
    )

    # Overwrite default value with input value
    param <- merge_params(param, list(...))
  } else {
    param <- list(
      id = id,
      GeoPressureR_version = utils::packageVersion("GeoPressureR"),
      ...
    )
  }

  if (is.list(param$tag_set_map$known)) {
    param$tag_set_map$known <- do.call(rbind, lapply(param$tag_set_map$known, as.data.frame))
  }

  return(structure(param, class = "param"))
}

#' Merge two parameters list.
#'
#' Strongly inspired by [config::merge()] and rmarkdown
#' [https://github.com/rstudio/rmarkdown/blob/main/R/util.R#L231]
#'
#' @param base_param Parameter list to merge values into
#' @param overlay_param Parameter list to merge values from
#' @param only_in_base Logical to only merge `overlay_param` if present in `base_param`
#'
#' @return Configuration which includes the values from
#'  `merge_config` merged into `base_config`.
#'
#' @seealso [config::merge()]
#'
#' @noRd
merge_params <- function(base_param, overlay_param, only_in_base = FALSE) {
  if (length(base_param) == 0) {
    overlay_param
  } else if (length(overlay_param) == 0) {
    base_param
  } else {
    merged_param <- base_param
    for (name in names(overlay_param)) {
      # Only merge if name already exist in base_param with only_in_base is TRUE
      if (!only_in_base || (name %in% base_param)) {
        base <- base_param[[name]]
        overlay <- overlay_param[[name]]
        if (!is.null(base) && is.list(base)) {
          merged_param[[name]] <- merge_params(base, overlay)
        } else {
          merged_param[[name]] <- NULL
          merged_param <- append(
            merged_param,
            overlay_param[which(names(overlay_param) %in% name)]
          )
        }
      }
    }
    class(merged_param) <- class(base_param)
    return(merged_param)
  }
}
