#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_config <- function(
  id,
  config = config::get(config = id),
  assert_tag = TRUE,
  assert_graph = FALSE,
  ...
) {
  # 1. Create the config from the default value
  c <- param_create(id, default = TRUE)

  # 2. Overwrite with config file provided as input of the function
  if (!is.null(config)) {
    c <- merge_params(c, config)
  }

  # 3. Overwrite with any parameter provided as input directly
  c <- merge_params(c, list(...))

  # Perform check
  if (assert_tag) {
    if (length(c$geopressuretemplate$likelihood) <= 0) {
      cli::cli_abort(c(
        x = "{.var geopressuretemplate$likelihood} needs to be provided.",
        ">" = "Make sure {.var geopressuretemplate$likelihood} is a list in {.file config.yml} (e.g.
        {.code ['map_pressure', 'map_light']}."
      ))
    }
  }

  # Additional check to perform only if the workflow needs to run the graph model. This allows to
  # pick up on error in config from the begining of the workflow.
  if (assert_graph) {
    # Validate that the likelihood is properly configured to include either
    # pressure or light mapping, or both
    if (
      !("map_pressure" %in%
        c$geopressuretemplate$likelihood ||
        "map_light" %in% c$geopressuretemplate$likelihood)
    ) {
      cli::cli_abort(c(
        x = "{.var geopressuretemplate$likelihood} needs to contain either  {.field map_pressure} or
        {.field map_light} or both",
        ">" = "Make sure {.var geopressuretemplate$likelihood} is set in {.file config.yml}."
      ))
    }

    # Validate that the outputs include at least one of the possible output types
    possible_outputs <- c("marginal", "most_likely", "simulation")
    if (!any(possible_outputs %in% config$geopressuretemplate$outputs)) {
      cli::cli_abort(c(
        x = "{.var outputs} needs to be provived and contains at least one of
        {.val {possible_outputs}}",
        ">" = "Make sure {.var outputs} is set in {.file config.yml}."
      ))
    }

    # Validate that `nj` is specified if simulation is requested in outputs
    if (
      "simulation" %in%
        config$geopressuretemplate$outputs &&
        config$graph_simulation$nj <= 0
    ) {
      cli::cli_abort(c(
        x = "{.var nj} is required with {.val simulation} in {.var outputs}.",
        ">" = "Make sure {.var geopressuretemplate$nj} is set in {.file config.yml}."
      ))
    }
  }

  return(c)
}
