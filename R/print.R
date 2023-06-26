#' Print data logger
#'
#' This function display the basic information on a tag list
#
#' @param tag A tag list
#'
#' @return `tag` is returned invisibly and unchanged
#' @family tag
#' @export
print.tag <- function(tag){
  cli::cli_text("Data logger of {.field {tag$id}}")
  cli::cli_text("Date range: {tag$pressure$date[1]} to {tail(tag$pressure$date,1)}")
  cli::cli_text("Sensor data.frame:")
  cli::cli_ul()
  cli::cli_li("{.field pressure}: {nrow(tag$pressure)} datapoints")
  if ("acceleration" %in% names(tag)){
    cli::cli_li("{.field acceleration}: {nrow(tag$acceleration)} datapoints")
  }
  if ("light" %in% names(tag)){
    cli::cli_li("{.field light}: {nrow(tag$light)} datapoints")
  }
  cli::cli_text("Status:")
  if ("stap" %in% names(tag)){
    cli::cli_alert_success("{nrow(tag$stap)} stationary periods computed!")
  } else {
    cli::cli_alert_danger("Not yet labeled. Use {.fun tag_write} and/or {.fun tag_read}")
    invisible(tag)
  }
  if ("stap" %in% names(tag)){
    cli::cli_alert_success("{nrow(tag$stap)} stationary periods computed!")
  } else {
    cli::cli_alert_danger("Not yet labeled. Use {.fun tag_write} and/or {.fun tag_read}")
    invisible(tag)
  }
}

#' Print geostap
#'
#' This function display the basic information on a `geostap` list.
#
#' @param geostap A geostap list
#'
#' @return `geostap` is returned invisibly and unchanged
#' @seealso geostap_create
#' @export
print.geostap <- function(geostap){
  cli::cli_text("Definition of the geographical and stationary periods of {.field {geostap$id}}")
  cli::cli_h3("Stationary periods {.field stap}")
  cli::cli_text("{.val {nrow(geostap$stap)}} stationary periods")
  print(head(geostap$stap))

  cli::cli_h3("Geographical {.field geo}")
  geo <- geo_expand(geostap$extent, geostap$scale)
  cli::cli_text("Extent W-E: {.val {geostap$extent[1]}}° to {.val {geostap$extent[2]}}°")
  cli::cli_text("Extent S-N: {.val {geostap$extent[3]}}° to {.val {geostap$extent[4]}}°")
  cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}°")
  cli::cli_text("Resolution lat-lon: {.val {1/geostap$scale}}°")

  cli::cli_h3("Likelihood")
  if ("map_pressure" %in% names(geostap)){
    cli::cli_alert_success("Pressure likelihood computed!")
  } else {
    if ("mse" %in% names(geostap)){
      cli::cli_alert_warning("Pressure mismatched computed, but not likelihood. Use {.fun geopressure_map_likelihood}.")
    } else {
      cli::cli_alert_danger("No pressure likelihood computed. Use {.fun geopressure_map}.")
    }
  }
  if ("map_light" %in% names(geostap)){
    cli::cli_alert_success("Light likelihood computed!")
  }
  invisible(geostap)
}


#' Print graph
#'
#' This function display the basic information on a `graph` list.
#
#' @param graph A graph list
#'
#' @return `graph` is returned invisibly and unchanged
#' @seealso graph_create
#' @export
print.graph <- function(graph){
  cli::cli_text("Graph of {.field {graph$id}}")
  cli::cli_h3("Stationary periods {.field stap}")
  cli::cli_text("{.val {nrow(geostap$stap)}} stationary periods")
  print(head(graph$stap))

  cli::cli_h3("Geographical {.field geo}")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_text("Extent W-E: {.val {graph$extent[1]}}° to {.val {graph$extent[2]}}°")
  cli::cli_text("Extent S-N: {.val {graph$extent[3]}}° to {.val {graph$extent[4]}}°")
  cli::cli_text("Dimension lat-lon: {.val {geo$dim[1]}} x {.val {geo$dim[2]}}°")
  cli::cli_text("Resolution lat-lon: {.val {1/graph$scale}}°")

  cli::cli_h3("Graph built")
  geo <- geo_expand(graph$extent, graph$scale)
  cli::cli_li("{.val {length(graph$s)}} edges")
  cli::cli_li("{.val {length(graph$equipment)}} equipements nodes")
  cli::cli_li("{.val {length(graph$retrieval)}} retrieval nodes")

  if ("ws" %in% names(graph)){
    cli::cli_alert_success("Windspeed computed!")
  } else {
    cli::cli_alert_warning("Windspeed not computed. Use {.fun graph_add_wind}")
  }

  if ("movement" %in% names(graph)){
    cli::cli_alert_success("Movement model defined for {.field {graph$movement$type}}")
  } else {
    cli::cli_alert_danger("No movement model defined. Use {.fun graph_add_movement}")
  }

  invisible(graph)
}
