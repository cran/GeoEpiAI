# Check whether required columns are present
check_cols <- function(data, cols) {
  
  miss <- setdiff(
    cols,
    names(data)
  )
  
  if (length(miss)) {
    
    stop(
      "Missing required columns: ",
      paste(
        miss,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
}


# Safely convert values to Date
as_date_safe <- function(x) {
  
  # Already a Date object
  if (inherits(x, "Date")) {
    return(x)
  }
  
  # POSIX date-time objects
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x))
  }
  
  # Try YYYY-MM-DD format
  y <- suppressWarnings(
    lubridate::ymd(
      x,
      quiet = TRUE
    )
  )
  
  # Try DD-MM-YYYY format for values
  # that could not be parsed
  bad <- is.na(y)
  
  if (any(bad)) {
    
    y[bad] <- suppressWarnings(
      lubridate::dmy(
        x[bad],
        quiet = TRUE
      )
    )
  }
  
  # Stop if any dates remain invalid
  if (anyNA(y)) {
    
    stop(
      "Some date values could not be parsed.",
      call. = FALSE
    )
  }
  
  y
}


# Return the most frequent non-missing value
mode_value <- function(x) {
  
  # Remove missing values
  x <- x[!is.na(x)]
  
  # Return NA if no values remain
  if (!length(x)) {
    return(NA)
  }
  
  # Identify unique values
  ux <- unique(x)
  
  # Return the most frequent value
  ux[
    which.max(
      tabulate(
        match(x, ux)
      )
    )
  ]
}
