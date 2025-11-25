tag_label_stap <- function(tag, path = tag2path(tag), quiet = FALSE) {
  # if path is not pressurepath, create one
  if (path) {
    pp <- pressurepath_create(
      tag,
      path = path,
      variable = c("surface_pressure"),
      era5_dataset = "land",
      workers = "auto",
      quiet = quiet
    )
  } else {
    pp <- path
  }

  for (stap_id in unique(pp$stap_id)) {
    pp_stap <- pp[pp$stap_id == stap_id, ]
  }
}
