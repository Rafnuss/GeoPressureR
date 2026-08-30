ecmwf_key_available <- function() {
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    return(FALSE)
  }
  key <- suppressMessages(try(ecmwfr::wf_get_key(), silent = TRUE))
  !inherits(key, "try-error") && nzchar(key)
}
