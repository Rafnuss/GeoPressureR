tag_label_period <- function(tag, twl = NULL, night = FALSE) {
  # Check that tag has a 'label' column
  GeoPressureR::tag_assert(tag, "label")

  if (is.null(twl)) {
    # If no twilight data provided, compute from tag's light data
    tag_twl <- twilight_create(tag)
    twl <- tag_twl
  }

  # Ensure twilight times are POSIXct
  twl$twilight <- as.POSIXct(twl$twilight)

  # Find nearest twilight index for each pressure measurement
  idx <- findInterval(tag$pressure$date, twl$twilight)

  # Replace idx == 0 (before first twilight) with NA to avoid invalid indexing
  idx[idx == 0] <- NA

  # Determine which measurements to discard
  discard_mask <- !is.na(idx) &
    (twl$rise[idx] != night) &
    tag$pressure$label == ""

  # Assign 'discard' label
  tag$pressure$label[discard_mask] <- "discard"

  return(tag)
}
