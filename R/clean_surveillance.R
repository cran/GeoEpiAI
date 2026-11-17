#' Validate and clean disease surveillance data
#'
#' Cleans disease surveillance records by standardising location, date,
#' and case-count variables. Optionally validates population data,
#' aggregates duplicate location-date records, and calculates monthly
#' incidence per 100,000 population.
#'
#' @param data A data.frame containing surveillance records.
#' @param location Column containing geographic unit.
#' @param date Column containing date/month.
#' @param cases Column containing disease cases.
#' @param population Optional population column.
#' @param aggregate If TRUE, aggregate duplicate location-date records.
#'
#' @return A cleaned data.frame containing location, date, cases, month,
#'   and incidence. If population is supplied, the population column is
#'   also returned.
#'
#' @examples
#' dat <- data.frame(
#'   location = c("A", "A", "B", "B"),
#'   date = as.Date(c(
#'     "2025-01-01",
#'     "2025-02-01",
#'     "2025-01-01",
#'     "2025-02-01"
#'   )),
#'   cases = c(10, 15, 5, 8),
#'   population = c(10000, 10000, 8000, 8000)
#' )
#'
#' clean_surveillance(
#'   dat,
#'   location = "location",
#'   date = "date",
#'   cases = "cases",
#'   population = "population"
#' )
#'
#' @export
clean_surveillance <- function(
    data,
    location,
    date,
    cases,
    population = NULL,
    aggregate = TRUE) {
  
  # Check required columns
  check_cols(
    data,
    c(location, date, cases)
  )
  
  # Convert to data.frame
  x <- as.data.frame(data)
  
  # Standardise internal variables
  x$.location <- as.character(
    x[[location]]
  )
  
  x$.date <- as_date_safe(
    x[[date]]
  )
  
  x$.cases <- suppressWarnings(
    as.numeric(x[[cases]])
  )
  
  # Validate cases
  if (anyNA(x$.cases)) {
    stop(
      "Cases contain non-numeric or missing values.",
      call. = FALSE
    )
  }
  
  if (any(x$.cases < 0)) {
    stop(
      "Cases cannot be negative.",
      call. = FALSE
    )
  }
  
  # Process population if supplied
  if (!is.null(population)) {
    
    check_cols(
      x,
      population
    )
    
    x$.population <- suppressWarnings(
      as.numeric(x[[population]])
    )
    
    if (anyNA(x$.population)) {
      stop(
        "Population contains non-numeric or missing values.",
        call. = FALSE
      )
    }
    
    if (any(x$.population <= 0)) {
      stop(
        "Population must be > 0.",
        call. = FALSE
      )
    }
  }
  
  # Aggregate duplicate location-date records
  if (aggregate) {
    
    by <- c(
      ".location",
      ".date"
    )
    
    if (!is.null(population)) {
      
      x <- x |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(by)
          )
        ) |>
        dplyr::summarise(
          .cases = sum(
            .cases,
            na.rm = TRUE
          ),
          population = mode_value(
            .population
          ),
          .groups = "drop"
        )
      
    } else {
      
      x <- x |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(by)
          )
        ) |>
        dplyr::summarise(
          .cases = sum(
            .cases,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
    }
  }
  
  # Sort and rename standardised variables
  x <- x |>
    dplyr::arrange(
      .date,
      .location
    ) |>
    dplyr::rename(
      location = .location,
      date = .date,
      cases = .cases
    )
  
  # Create month variable
  x$month <- lubridate::floor_date(
    x$date,
    unit = "month"
  )
  
  # Calculate incidence per 100,000 population
  if (
    !is.null(population) &&
    "population" %in% names(x)
  ) {
    
    x$incidence <-
      1e5 *
      x$cases /
      x$population
    
  } else {
    
    x$incidence <- NA_real_
  }
  
  # Return cleaned data
  x
}


#' Validate surveillance data without modifying it
#'
#' Checks surveillance data for missing locations, missing dates,
#' non-numeric case counts, and negative case counts.
#'
#' @param data A data.frame.
#' @param location Location column.
#' @param date Date column.
#' @param cases Case-count column.
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' dat <- data.frame(
#'   location = c("A", "B"),
#'   date = as.Date(c(
#'     "2025-01-01",
#'     "2025-02-01"
#'   )),
#'   cases = c(10, 5)
#' )
#'
#' validate_surveillance(
#'   dat,
#'   location = "location",
#'   date = "date",
#'   cases = "cases"
#' )
#'
#' @export
validate_surveillance <- function(
    data,
    location,
    date,
    cases) {
  
  # Check required columns
  check_cols(
    data,
    c(
      location,
      date,
      cases
    )
  )
  
  # Check missing locations
  if (anyNA(data[[location]])) {
    warning(
      "Location contains missing values.",
      call. = FALSE
    )
  }
  
  # Check missing dates
  if (anyNA(data[[date]])) {
    warning(
      "Date contains missing values.",
      call. = FALSE
    )
  }
  
  # Check numeric case counts
  z <- suppressWarnings(
    as.numeric(
      data[[cases]]
    )
  )
  
  if (anyNA(z)) {
    warning(
      "Cases contain non-numeric values.",
      call. = FALSE
    )
  }
  
  # Check negative cases
  if (any(z < 0, na.rm = TRUE)) {
    warning(
      "Negative case counts detected.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

