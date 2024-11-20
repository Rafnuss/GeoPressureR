#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_pressurepath <- function(
    id,
    config = config::get(config = id),
    quiet = FALSE,
    file = glue::glue("./data/interim/{id}.RData"),
    ...) {
  config <- geopressuretemplate_config(id,
    config = config,
    ...
  )


  if ("pressurepath" %in% names(config$geopressuretemplate)) {
    if (!quiet) {
      cli::cli_h2("Compute pressurepath")
    }

    save_list <- load(file)

    tag <- get("tag")

    tag_assert(tag)

    if (tag$param$id != id) {
      cli::cli_abort(c(x = "{.var id}={id} is different from {.var tag$param$id}={tag$param$id}."))
    }

    if ("most_likely" %in% config$geopressuretemplate$pressurepath &&
      "path_most_likely" %in% save_list) {
      path_most_likely <- get("path_most_likely")
      pressurepath_most_likely <- pressurepath_create( # nolint
        tag,
        path = path_most_likely,
        variable = config$pressurepath_create$variable,
        solar_dep = config$pressurepath_create$solar_dep,
        quiet = quiet
      )
      save_list <- c(save_list, "pressurepath_most_likely")
    }

    if ("geopressureviz" %in% config$geopressuretemplate$pressurepath &&
      "path_geopressureviz" %in% save_list) {
      path_geopressureviz <- get("path_geopressureviz")
      pressurepath_geopressureviz <- pressurepath_create( # nolint
        tag,
        path = path_geopressureviz,
        variable = config$pressurepath_create$variable,
        solar_dep = config$pressurepath_create$solar_dep,
        quiet = quiet
      )
      save_list <- c(save_list, "pressurepath_geopressureviz")
    }

    if ("pressurepath_tag" %in% config$geopressuretemplate$pressurepath &&
      "path_tag" %in% save_list) {
      path_tag <- get("path_tag")
      pressurepath_geopressureviz <- pressurepath_create( # nolint
        tag,
        path = path_tag,
        variable = config$pressurepath_create$variable,
        solar_dep = config$pressurepath_create$solar_dep,
        quiet = quiet
      )
      save_list <- c(save_list, "pressurepath_tag")
    }

    if ("pressurepath_simulation" %in% config$geopressuretemplate$pressurepath &&
      "path_simulation" %in% save_list) {
      path_simulation <- get("path_tag")
      pressurepath_geopressureviz <- pressurepath_create( # nolint
        tag,
        path = path_simulation,
        variable = config$pressurepath_create$variable,
        solar_dep = config$pressurepath_create$solar_dep,
        quiet = quiet
      )
      save_list <- c(save_list, "pressurepath_simulation")
    }

    # Save the outputs to the specified file
    save(
      list = save_list,
      file = file
    )
  }

  # Return the file path invisibly
  invisible(file)
}
