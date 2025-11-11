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
  if (!is.null(deg_path)) {
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
  }

  # Read light
  if (is.null(light_file)) {
    light_path <- tag_create_detect("*.lux", directory, quiet = TRUE)
  } else {
    light_path <- tag_create_detect(light_file, directory, quiet = quiet)
  }
  if (!is.null(light_path)) {
    # Read first 30 lines for both drift check and header detection
    first_lines <- readLines(light_path, n = 30)

    # Find the line containing "light(lux)"
    header_line <- which(grepl("light\\(lux\\)", first_lines))

    if (length(header_line) == 0) {
      cli::cli_abort(
        "The light file {.file {light_path}} is not compatible. \\
              Could not find {.val light(lux)} in the first 30 lines"
      )
    }

    header_line <- header_line[1] # Take first occurrence

    # Check for drift (line 16) if available
    if (length(first_lines) >= 16) {
      line16 <- first_lines[16]
      drift_match <- regmatches(line16, regexpr("-?\\d+\\.\\d*", line16))
      if (length(drift_match) > 0) {
        drift <- abs(as.numeric(drift_match) / 60)
        if (drift > 5) {
          cli::cli_warn(c(
            "!" = "The light file {.file {light_path}} is recording a drift of \\
          {format_minutes(drift)} (line 16) which is higher than the resolution.",
            "i" = "Check for error (e.g. timezone)"
          ))
        }
      }
    }

    # Extract column index directly from the header line
    hdr_parts <- strsplit(first_lines[header_line], "\\s+")[[1]]
    col <- which(hdr_parts == "light(lux)")

    if (length(col) == 0) {
      cli::cli_abort(
        "The light file {.file {light_path}} header does not contain \\
              {.val light(lux)} column"
      )
    }

    # Read file
    tag$light <- tag_create_dto(
      light_path,
      skip = header_line,
      col = col,
      date_format = "%d/%m/%Y %H:%M:%S",
      quiet = quiet
    )
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- deg_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$manufacturer <- "migratetech"
  tag$param$tag_create$directory <- directory

  return(tag)
}
