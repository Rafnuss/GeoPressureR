# Read Migrate Technology tag files
#' @noRd
tag_create_migratetech <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  deg_file = NULL,
  light_file = NULL,
  quiet = FALSE
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read Pressure
  if (is.null(deg_file)) {
    deg_file <- "*.deg"
  }
  deg_path <- tag_create_detect(deg_file, directory, quiet = quiet)
  if (is.null(deg_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {deg_file}}",
      "!" = "{.var deg_file} is required!"
    ))
  }
  assertthat::assert_that(grepl(
    "Migrate Technology",
    readLines(deg_path, n = 1)
  ))
  line2 <- readLines(deg_path, n = 2)[[2]]
  v <- regmatches(line2, regexpr("Type: \\K\\d+", line2, perl = TRUE))
  if (v < 13) {
    cli::cli_abort(
      "The deg file {.file {deg_path}} is not compatible. Line 2 should \\
               contains {.val Type:x}, with x>=13."
    )
  }
  # Retrieve full model number
  tag$param$migratec_model <- regmatches(
    line2,
    regexpr("Type: \\K[\\d.]+", line2, perl = TRUE)
  )
  # Check for drift
  line16 <- readLines(deg_path, n = 16)[[16]]
  drift <- abs(
    as.numeric(regmatches(line16, regexpr("-?\\d+\\.\\d*", line16))) / 60
  )
  if (drift > 30) {
    cli::cli_warn(c(
      "!" = "The deg file {.file {deg_path}} is recording a drift of {format_minutes(drift)} \\
      (line 16) which seems suspicious.",
      ">" = "Check for error (e.g. timezone)"
    ))
  }
  # Find column index with pressure
  hdr <- utils::read.delim(
    deg_path,
    skip = 19,
    nrow = 1,
    header = FALSE,
    sep = ""
  )
  col <- which(hdr == "P(Pa)")
  if (length(col) > 0) {
    # Read file
    tag$pressure <- tag_create_dto(
      deg_path,
      skip = 20,
      col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    # convert Pa in hPa
    tag$pressure$value <- tag$pressure$value / 100
  }

  # Read acceleration
  col <- which(hdr == "Zact")
  if (length(col) != 0) {
    # Read file
    tag$acceleration <- tag_create_dto(
      deg_path,
      skip = 20,
      col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    tag$param$tag_create$acceleration_file <- deg_path
  }

  # Read temperature
  col <- which(hdr == "T('C)")
  if (length(col) != 0) {
    tag$temperature <- tag_create_dto(
      deg_path,
      skip = 20,
      col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
    tag$param$tag_create$temperature_external_file <- deg_path
  }

  # Read light
  if (is.null(light_file)) {
    light_path <- tag_create_detect("*.lux", directory, quiet = TRUE)
  } else {
    light_path <- tag_create_detect(light_file, directory, quiet = quiet)
  }
  if (!is.null(light_path)) {
    line16 <- readLines(light_path, n = 16)[[16]]
    drift <- abs(
      as.numeric(regmatches(line16, regexpr("-?\\d+\\.\\d*", line16))) / 60
    )
    if (drift > 30) {
      cli::cli_warn(c(
        "!" = "The light file {.file {light_path}} is recording a drift of \\
      {format_minutes(drift)}  (line 16) which seems suspicious.",
        ">" = "Check for error (e.g. timezone)"
      ))
    }
    # find column index with light
    hdr <- utils::read.delim(
      light_path,
      skip = 19,
      nrow = 1,
      header = FALSE,
      sep = ""
    )
    col <- which(hdr == "light(lux)")
    if (length(col) == 0) {
      cli::cli_abort(
        "The light file {.file {light_path}} is not compatible. Line 20 \\
              should contains {.val light(lux)}"
      )
    }
    # Read file
    tag$light <- tag_create_dto(
      light_path,
      skip = 20,
      col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )

    if (drift > 5) {
      cli::cli_warn(c(
        "!" = "The light file {.file {light_path}} is recording a drift of \\
      {format_minutes(drift)} (line 16) which is higher than the resolution.",
        "i" = "Check for error (e.g. timezone)"
      ))
    }
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- deg_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$manufacturer <- "migratetech"
  tag$param$tag_create$directory <- directory

  return(tag)
}
