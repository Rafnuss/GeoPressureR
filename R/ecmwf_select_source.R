ecmwf_select_source <- function(source, quiet) {
  source <- match.arg(source, c("auto", "arco", "api"))
  if (source != "auto") {
    return(source)
  }

  source <- if (ecmwf_key_available()) "arco" else "api"
  if (!quiet) {
    if (source == "arco") {
      cli::cli_alert_info("Using the ECMWF ARCO backend.")
    } else {
      cli::cli_alert_info("No readable ECMWF API key was found; using GeoPressureAPI.")
    }
  }
  source
}
