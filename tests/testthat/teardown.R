# Test Teardown: Clean up temporary test data
# =============================================
# This teardown file runs once after all tests complete.
# It removes the temporary copy of test data created in setup.R

unlink(getOption("geopressurer.extdata_path"), recursive = TRUE, force = TRUE)
options(geopressurer.extdata_path = NULL)
