#' Extract dimension structure from Eurostat dataset
#'
#' Helper function that downloads a Eurostat dataset and extracts all dimensions
#' with their unique levels and labels, plus the frequency/interval and unit
#' mapping for each dimension combination. Dimensions with only one unique value
#' are excluded as they are constants rather than true dimensions.
#'
#' @param code the original Eurostat code (e.g. agr_r_animal)
#' @param use_cache logical, set to true for testing, false in production
#'
#' @return a list with three elements:
#'   - `dimensions`: a named list where each element is a dataframe with columns
#'     `level_value` and `level_text` for that dimension. Only dimensions with
#'     2+ unique values are included.
#'   - `interval`: a single character string with the interval code (e.g., "M", "A", "Q")
#'   - `unit_mapping`: a dataframe with dimension level columns plus a `unit` column,
#'     showing which unit applies to each dimension combination
#' @export
extract_dimension_structure <- function(code, use_cache = FALSE) {
  # Download data
  data <- eurostat::get_eurostat(code, cache = use_cache)

  # Check freq column exists
  if (!"freq" %in% names(data)) {
    stop(sprintf("Dataset '%s' does not have a 'freq' column", code))
  }

  freq_values <- unique(data$freq)

  # Check if single or multiple frequencies
  if (length(freq_values) == 1) {
    interval <- as.character(freq_values[1])
    freq_is_constant <- TRUE
  } else {
    # Multiple frequencies - will be in unit_mapping
    interval <- NULL
    freq_is_constant <- FALSE
  }

  # Identify unit-like dimensions
  unit_dims <- c("unit", "indic", "indic_et", "indic_de", "indic_bt", "indic_nrg")

  # Find which unit dimensions exist
  available_unit_cols <- intersect(names(data), unit_dims)


  # Separate into those with variation and those that are constants
  unit_cols_with_variation <- purrr::keep(available_unit_cols, function(col) {
    length(unique(data[[col]])) > 1
  })

  unit_cols_constant <- purrr::keep(available_unit_cols, function(col) {
    length(unique(data[[col]])) == 1
  })

  # Pick the unit column (prefer one with variation, otherwise use constant)
  if (length(unit_cols_with_variation) > 0) {
    unit_col <- unit_cols_with_variation[1]
  } else if (length(unit_cols_constant) > 0) {
    unit_col <- unit_cols_constant[1]
  } else {
    unit_col <- character(0)
  }

  if (length(unit_cols_with_variation) > 1) {
    stop(sprintf("Dataset '%s' has multiple unit-like dimensions with variation: %s",
                 code, paste(unit_cols_with_variation, collapse = ", ")))
  }

  # Identify and exclude non-dimension columns
  exclude_cols <- c("TIME_PERIOD", "values")
  if (freq_is_constant) {
    exclude_cols <- c(exclude_cols, "freq")
  }

  # Get dimension columns
  dim_cols <- setdiff(names(data), exclude_cols)

  # Extract unique levels for each dimension with their labels
  dim_structure <- purrr::map(dim_cols, function(dim) {
    codes <- unique(data[[dim]]) |> sort()
    dic <- eurostat::get_eurostat_dic(dim)
    labels <- dic$full_name[match(codes, dic[[1]])]
    tibble::tibble(level_value = codes, level_text = labels)
  })
  names(dim_structure) <- dim_cols

  # Remove dimensions with only one unique value
  dim_structure <- purrr::keep(dim_structure, ~ nrow(.x) > 1)

  # Create unit mapping
  mapping_cols <- names(dim_structure)
  if (!freq_is_constant) {
    mapping_cols <- c(mapping_cols, "freq")
  }

  if (length(unit_col) == 1) {
    unit_mapping <- data |>
      dplyr::select(dplyr::all_of(c(mapping_cols, unit_col))) |>
      dplyr::distinct() |>
      dplyr::mutate(unit = .data[[unit_col]])
  } else if (!freq_is_constant) {
    # No unit column but multiple freqs - need freq in mapping
    unit_mapping <- data |>
      dplyr::select(dplyr::all_of(mapping_cols)) |>
      dplyr::distinct() |>
      dplyr::mutate(unit = NA_character_)  # Will need manual input
  } else {
    unit_mapping <- NULL
  }

  # Add interval to mapping if not constant
  if (!freq_is_constant && !is.null(unit_mapping)) {
    unit_mapping <- unit_mapping |>
      dplyr::rename(interval = freq)
  }

  list(
    dimensions = dim_structure,
    interval = interval,  # NULL if multiple frequencies
    unit_mapping = unit_mapping  # Now includes interval column if freq varies
  )
}
#' Get UMAR unit ID from Eurostat unit code
#'
#' Maps Eurostat's granular unit codes to UMAR's simplified unit taxonomy and
#' retrieves the corresponding unit ID from the database. Eurostat uses hundreds
#' of specific unit codes (e.g., different index base years, chain-linked volumes),
#' while UMAR maintains a curated set of high-level units (index, percentage, etc.).
#'
#' The mapping is maintained in the package data `eurostat_unit_map`. When an
#' unmapped unit is encountered, the function stops with an error message
#' suggesting how to add the mapping.
#'
#' @param eurostat_unit Character, the Eurostat unit code (e.g., "I15", "PC_GDP")
#' @param con Database connection object
#' @param schema Character, database schema name, default is "platform"
#'
#' @return Integer, the unit ID from the database
#'
#' @export
get_umar_unit_id <- function(eurostat_unit, con, schema = "platform") {
  mapped <- eurostat_unit_map$umar_unit[eurostat_unit_map$eurostat_unit == eurostat_unit]

  if (length(mapped) > 0) {
    # Found in mapping table
    umar_unit <- mapped[1]
  } else {
    # Try pattern matching
    if (grepl("^PC_", eurostat_unit) || grepl("^PCH_", eurostat_unit)) {
      umar_unit <- "%"
      message(sprintf("Auto-mapped '%s' - > 'percentage' (PC_/PCH_ pattern)", eurostat_unit))
    } else if (grepl("^I\\d{2}$", eurostat_unit)) {
      umar_unit <- "indeks"
      message(sprintf("Auto-mapped '%s' - > 'index' (I## pattern)", eurostat_unit))
    } else {
      stop(sprintf(
        "Eurostat unit '%s' not mapped.\n\nAdd to data-raw/eurostat_unit_map.R and run:\n source('data-raw/eurostat_unit_map.R')\nThen push package update.",
        eurostat_unit
      ))
    }
  }
  UMARaccessR::sql_get_unit_id_from_unit_name(umar_unit, con, schema)
}


#' Expanding from levels to series codes and titles
#'
#' These two helper functions take a set of non-time levels for a single table
#' and expand the grid to get all of their combinations and then either return
#' a dataframe with columns for each level code, or one where the level texts
#' have been concatenated into the series titles.
#' @param code_no code e.g. 0300230S
#' @return dataframe with expanded levels, one column per non-time dimension plus
#' unit_id for the level codes and sinle column with series titles for the other one.
#' @rdname expanding
#' @keywords internal
expand_to_level_codes <- function (tbl_id, con, schema = "platform") {
  levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema )
  do.call(expand.grid,
          c(setNames(split(levels$level_value, levels$tab_dim_id),
                     paste0("Var", seq_along(unique(levels$tab_dim_id)))),
            stringsAsFactors = FALSE))
}

#' @rdname expanding
#' @keywords internal
expand_to_series_titles <- function (tbl_id, con, schema = "platform") {
  levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema )
  do.call(expand.grid,
          c(setNames(split(levels$level_text, levels$tab_dim_id),
                     paste0("Var", seq_along(unique(levels$tab_dim_id)))),
            stringsAsFactors = FALSE)) |>
    tidyr::unite("name_long", dplyr::everything(), sep = " -- ")
}


#' Helper function to format period IDs
#'
#' converts date to period id. (originally from DESEZONIRANJEfetchR)
#'
#' @param dates date
#' @param interval M or Q or A
#'
#' @keywords internal
format_period_id <- function(dates, interval) {
  dates <- as.Date(dates)
  year <- format(dates, "%Y")

  # Handle empty input
  if (length(dates) == 0) {
    return(character(0))
  }
  if (interval == "A") {
    # Annual:
    year
  } else if (interval == "M") {
    # Monthly: 2025M08
    month <- format(dates, "%m")
    paste0(year, "M", month)
  } else if (interval == "Q") {
    # Quarterly: 2025Q2
    quarter <- as.integer(format(dates, "%m"))
    quarter <- ceiling(quarter / 3)
    paste0(year, "Q", quarter)
  } else {
    stop("Unsupported interval: ", interval)
  }
}

#' Get Eurostat Release Calendar
#'
#' Retrieves the Eurostat release calendar for a specified date range using
#' an undocumented internal API endpoint. This function accesses the same data
#' source that powers the Eurostat web calendar interface.
#'
#' @param start_date Date or character string in YYYY-MM-DD format. Start date
#'   for the calendar period.
#' @param end_date Date or character string in YYYY-MM-DD format. End date
#'   for the calendar period.
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{datasetCodes}{Character. Comma-separated list of Eurostat dataset
#'       codes to be released (e.g., "prc_hpi_a,prc_hpi_inw,prc_hpi_q"). May be
#'       empty string for non-dataset releases.}
#'     \item{title}{Character. Title/description of the release.}
#'     \item{start}{Character. Release date and time in ISO 8601 format with
#'       timezone (e.g., "2026-01-09T11:00Z").}
#'     \item{period}{Character. Reference period for the data release
#'       (e.g., "Q3/2025", "June 2026"). May be empty string.}
#'   }
#'   Returns NULL if the API request fails.
#'
#' @details
#' \strong{Important}: This function uses an undocumented Eurostat internal API
#' endpoint that is not officially supported for public use. While functional,
#' this endpoint:
#' \itemize{
#'   \item Has no official documentation or versioning
#'   \item May change or break without notice
#'   \item Comes with no guarantees from Eurostat regarding availability or
#'     data accuracy
#'   \item Should be used with appropriate error handling in production code
#' }
#'
#' The function excludes ECB (European Central Bank) and ECFIN (Economic and
#' Financial Affairs) releases by default to focus on core Eurostat datasets.
#'
#' @examples
#' \dontrun{
#' # Get next 12 months of releases
#' calendar <- get_eurostat_calendar(
#'   start_date = Sys.Date(),
#'   end_date = Sys.Date() + 365
#' )
#'
#' # Get releases for specific date range
#' calendar <- get_eurostat_calendar(
#'   start_date = "2025-12-01",
#'   end_date = "2026-01-31"
#' )
#'
#' # Extract releases for specific datasets
#' my_datasets <- c("prc_hpi_a", "sts_inpi_m", "ext_st_eu27_2020bec")
#' calendar |>
#'   dplyr::filter(grepl(paste(my_datasets, collapse = "|"), datasetCodes))
#' }
#'
#' @export
get_eurostat_calendar <- function(start_date, end_date) {

  tryCatch({
    base_url <- "https://ec.europa.eu/eurostat/o/calendars/eventsJson"

    # Format dates as ISO 8601 with timezone
    start_str <- format(as.POSIXct(start_date), "%Y-%m-%dT00:00:00.000Z", tz = "UTC")
    end_str <- format(as.POSIXct(end_date), "%Y-%m-%dT00:00:00.000Z", tz = "UTC")

    response <- httr::GET(
      base_url,
      query = list(
        theme = 0,
        category = 0,
        keywords = "",
        isEuroindicator = "",
        authorInclude = "",
        authorExclude = "ecb,ecfin",
        start = start_str,
        end = end_str,
        timeZone = "Europe/Luxembourg"
      )
    )

    httr::stop_for_status(response)

    content <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content, flatten = TRUE)

    return(data)

  }, error = function(e) {
    warning("Eurostat calendar API request failed: ", e$message,
            "\nThis undocumented endpoint may have changed or be unavailable.")
    return(NULL)
  })
}
