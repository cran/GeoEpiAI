
#' Detect Early-Warning Signals in Surveillance Data
#'
#' Calculates rolling baselines and Z-scores to identify potential
#' increases in disease cases. When a location column is available,
#' calculations are performed independently for each location.
#' At least two previous observations are required to calculate
#' a Z-score.
#'
#' @param data A data frame containing surveillance observations.
#' @param date Character string specifying the date column.
#' @param cases Character string specifying the case-count column.
#' @param window Number of previous observations used for the
#' rolling baseline. Must be at least 2.
#' @param threshold Positive Z-score threshold for an alert.
#' @param location Optional character string specifying the
#' location column. If NULL, a column named "location" is
#' detected automatically when available.
#'
#' @return A data frame containing date, cases, baseline,
#' z_score, alert, and alert_level. A location column is
#' retained when location-wise analysis is performed.
#'
#' @details
#' The rolling baseline is calculated using previous observations
#' only. Calculations are performed independently for each location.
#' At least two previous observations are required to calculate
#' the sample standard deviation and Z-score.
#'
#' When the historical standard deviation is zero, an increase
#' above the baseline is assigned an infinite Z-score and flagged
#' as an alert. Otherwise, the Z-score is set to zero.
#'
#' @examples
#' surveillance <- data.frame(
#'   location = rep(c("A", "B"), each = 8),
#'   date = rep(as.Date("2026-01-01") + 0:7, 2),
#'   cases = c(
#'     10, 11, 10, 12, 11, 13, 30, 35,
#'     2, 2, 3, 2, 3, 2, 3, 3
#'   )
#' )
#'
#' early_warning(
#'   surveillance,
#'   date = "date",
#'   cases = "cases",
#'   window = 4,
#'   threshold = 2
#' )
#'
#' @export

early_warning <- function(
    data,
    date,
    cases,
    window = 4,
    threshold = 2,
    location = NULL
) {
  
  # ----------------------------------------------------------
  # 1. Basic validation
  # ----------------------------------------------------------
  
  if (!is.data.frame(data)) {
    stop(
      "data must be a data.frame.",
      call. = FALSE
    )
  }
  
  if (nrow(data) == 0L) {
    stop(
      "data must contain at least one observation.",
      call. = FALSE
    )
  }
  
  if (!is.character(date) ||
      length(date) != 1L ||
      is.na(date) ||
      !date %in% names(data)) {
    
    stop(
      "Date column not found in data.",
      call. = FALSE
    )
  }
  
  if (!is.character(cases) ||
      length(cases) != 1L ||
      is.na(cases) ||
      !cases %in% names(data)) {
    
    stop(
      "Cases column not found in data.",
      call. = FALSE
    )
  }
  
  if (length(window) != 1L ||
      !is.numeric(window) ||
      is.na(window) ||
      !is.finite(window) ||
      window < 2 ||
      window != as.integer(window)) {
    
    stop(
      "window must be a single number >= 2.",
      call. = FALSE
    )
  }
  
  if (length(threshold) != 1L ||
      !is.numeric(threshold) ||
      is.na(threshold) ||
      !is.finite(threshold) ||
      threshold <= 0) {
    
    stop(
      "threshold must be a single positive number.",
      call. = FALSE
    )
  }
  
  window <- as.integer(window)
  
  # ----------------------------------------------------------
  # 2. Detect and validate location column
  # ----------------------------------------------------------
  
  if (is.null(location) &&
      "location" %in% names(data)) {
    
    location <- "location"
  }
  
  if (!is.null(location)) {
    
    if (!is.character(location) ||
        length(location) != 1L ||
        is.na(location) ||
        !location %in% names(data)) {
      
      stop(
        "A valid location column must be specified.",
        call. = FALSE
      )
    }
    
    location_values <- as.character(data[[location]])
    
    if (anyNA(location_values) ||
        any(!nzchar(trimws(location_values)))) {
      
      stop(
        "Location contains missing or empty values.",
        call. = FALSE
      )
    }
    
  } else {
    
    location_values <- rep("All", nrow(data))
  }
  
  # ----------------------------------------------------------
  # 3. Standardise date and case-count variables
  # ----------------------------------------------------------
  
  converted_date <- tryCatch(
    as.Date(data[[date]]),
    error = function(e) {
      rep(as.Date(NA), nrow(data))
    }
  )
  
  converted_cases <- suppressWarnings(
    as.numeric(as.character(data[[cases]]))
  )
  
  out <- data.frame(
    date = converted_date,
    cases = converted_cases
  )
  
  if (anyNA(out$date)) {
    
    stop(
      "Date contains missing or invalid values.",
      call. = FALSE
    )
  }
  
  if (anyNA(out$cases) ||
      any(!is.finite(out$cases))) {
    
    stop(
      "Cases must contain finite numeric values.",
      call. = FALSE
    )
  }
  
  if (any(out$cases < 0)) {
    
    stop(
      "Cases cannot be negative.",
      call. = FALSE
    )
  }
  
  if (!is.null(location)) {
    
    out$location <- location_values
  }
  
  # ----------------------------------------------------------
  # 4. Preserve original row order
  # ----------------------------------------------------------
  
  out$.original_order <- seq_len(nrow(out))
  
  # Sort chronologically, separately by location
  if (!is.null(location)) {
    
    out <- out[
      order(
        out$location,
        out$date,
        out$.original_order
      ),
      ,
      drop = FALSE
    ]
    
  } else {
    
    out <- out[
      order(
        out$date,
        out$.original_order
      ),
      ,
      drop = FALSE
    ]
  }
  
  n <- nrow(out)
  
  # ----------------------------------------------------------
  # 5. Initialise output variables
  # ----------------------------------------------------------
  
  baseline <- rep(NA_real_, n)
  z_score <- rep(NA_real_, n)
  
  # ----------------------------------------------------------
  # 6. Define location-specific groups
  # ----------------------------------------------------------
  
  groups <- if (!is.null(location)) {
    
    split(
      seq_len(n),
      out$location
    )
    
  } else {
    
    list(All = seq_len(n))
  }
  
  # ----------------------------------------------------------
  # 7. Calculate rolling baseline and Z-scores
  # ----------------------------------------------------------
  
  for (idx in groups) {
    
    for (j in seq_along(idx)) {
      
      i <- idx[j]
      
      # No historical observations available
      if (j <= 1L) {
        next
      }
      
      # Select previous observations only
      previous_idx <- idx[
        seq.int(
          max(1L, j - window),
          j - 1L
        )
      ]
      
      previous_cases <- out$cases[previous_idx]
      
      # Calculate rolling baseline
      baseline[i] <- mean(previous_cases)
      
      # At least two historical observations required
      # for calculating a sample standard deviation.
      if (length(previous_cases) < 2L) {
        
        if (out$cases[i] == baseline[i]) {
          z_score[i] <- 0
        }
        
        next
      }
      # Calculate historical standard deviation
      sd_previous <- stats::sd(previous_cases)
      
      if (is.finite(sd_previous) &&
          sd_previous > 0) {
        
        z_score[i] <-
          (out$cases[i] - baseline[i]) /
          sd_previous
        
      } else if (
        out$cases[i] > baseline[i]
      ) {
        
        # Increase above a constant historical baseline
        z_score[i] <- Inf
        
      } else {
        
        z_score[i] <- 0
      }
    }
  }
  
  # ----------------------------------------------------------
  # 8. Classify early-warning signals
  # ----------------------------------------------------------
  
  alert <- !is.na(z_score) &
    z_score >= threshold
  
  alert_level <- rep("Normal", n)
  
  alert_level[alert] <- "Alert"
  
  high_alert <- !is.na(z_score) &
    z_score >= (2 * threshold)
  
  alert_level[high_alert] <- "High"
  
  # ----------------------------------------------------------
  # 9. Append calculated variables
  # ----------------------------------------------------------
  
  out$baseline <- baseline
  out$z_score <- z_score
  out$alert <- alert
  out$alert_level <- alert_level
  
  # ----------------------------------------------------------
  # 10. Restore original row order
  # ----------------------------------------------------------
  
  out <- out[
    order(out$.original_order),
    ,
    drop = FALSE
  ]
  
  out$.original_order <- NULL
  
  rownames(out) <- NULL
  
  return(out)
}

