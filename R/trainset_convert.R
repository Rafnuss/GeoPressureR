trainset_convert <- function(tag,
                             directory = "./data/tag-label/",
                             file = glue::glue(tag$id, "_act_pres-labeled.csv"),
                             keep_acc = FALSE) {
  tag_assert(tag)
  assertthat::assert_that(is.character(directory))
  assertthat::assert_that(is.character(file))
  assertthat::assert_that(assertthat::is.dir(directory))
  full_path <- glue::glue(directory, "/", file)
  assertthat::assert_that(file.exists(full_path))

  csv <- utils::read.csv(full_path)

  # Make a copy of the old file
  csv$label[is.na(csv$label)] <- ""
  utils::write.csv(
    csv,
    glue::glue(directory, "/", gsub(".csv", "-geopressurerv2.csv", file)),
    row.names = FALSE
  )
  csv$label[csv$label == ""] <- NA

  csv$timestamp <- strptime(csv$timestamp, "%FT%T", tz = "UTC")
  series <- NULL
  csv_acc <- subset(csv, series == "acceleration")
  csv_pres <- subset(csv, series == "pressure")

  # retrieve the labelling of flihgt from acceleration data
  csv_pres_label <- stats::approx(as.numeric(csv_acc$timestamp), !is.na(csv_acc$label),
    as.numeric(csv_pres$timestamp),
    method = "constant"
  )$y
  csv_pres_label <- ifelse(csv_pres_label, "flight", "")

  # Add outlier/discard from pressure
  csv_pres_label[!is.na(csv_pres$label) & csv_pres_label != "flight"] <- "discard"

  # Overwrite existing value
  csv_pres$label <- csv_pres_label

  # Remove acceleration if useless
  if (keep_acc) {
    csv_acc$label <- ifelse(is.na(csv_acc$label), "", "flight")
    csv <- rbind(csv_pres, csv_acc)
  } else {
    csv <- csv_pres
  }

  # Convert date to string for trainset
  csv$timestamp <- strftime(csv$timestamp,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  # Overwrite the file
  utils::write.csv(
    csv,
    glue::glue(directory, "/", file),
    row.names = FALSE
  )
}
