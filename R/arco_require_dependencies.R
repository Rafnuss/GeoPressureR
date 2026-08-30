arco_require_dependencies <- function() {
  if (!requireNamespace("Rarr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg Rarr} is required to use the ARCO backend.",
      "i" = "Install it with {.run BiocManager::install('Rarr')}.",
      ">" = "Alternatively, use {.code source = \"api\"}."
    ))
  }
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ecmwfr} is required to use the ARCO backend.",
      "i" = "Install it with {.run install.packages('ecmwfr')}.",
      ">" = "Alternatively, use {.code source = \"api\"}."
    ))
  }
}
