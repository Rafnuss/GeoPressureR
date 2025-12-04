# Test Setup: Copy extdata to temporary location
# ================================================
# This setup file runs once before all tests. It copies the inst/extdata directory
# to a temporary location to avoid modifying the installed package files during tests.

# Copy extdata to temp directory
extdata_src <- system.file("extdata", package = "GeoPressureR")
extdata_tmp <- file.path(tempdir(), "GeoPressureR-extdata")

# Clean up any previous test runs
if (dir.exists(extdata_tmp)) {
  unlink(extdata_tmp, recursive = TRUE, force = TRUE)
}

# Create fresh temp directory and copy all test data
dir.create(extdata_tmp, recursive = TRUE, showWarnings = FALSE)
copied <- file.copy(
  from = list.files(extdata_src, full.names = TRUE),
  to = extdata_tmp,
  recursive = TRUE
)
stopifnot(all(copied))

# Set option so helper functions know where to find test data
options(geopressurer.extdata_path = extdata_tmp)


# Helper Functions
# ================

#' Get the path to test data
#'
#' Returns the path to the extdata directory used for tests.
#' By default, this is a temporary copy of inst/extdata, but can be
#' overridden by setting options(geopressurer.extdata_path = "path/to/data").
#'
#' @return Character string with absolute path to test data directory
test_path_extdata <- function() {
  path <- getOption("geopressurer.extdata_path")
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    path <- system.file("extdata", package = "GeoPressureR")
  }
  path
}

#' Temporarily change working directory to test data location
#'
#' Many GeoPressureR functions expect a specific directory structure
#' (e.g., ./data/raw-tag/). This helper temporarily sets the working
#' directory to the test data location using withr::local_dir().
#'
#' @param env Environment for the temporary directory change (default: parent frame)
test_with_extdata <- function(env = parent.frame()) {
  withr::local_dir(test_path_extdata(), .local_envir = env)
}
