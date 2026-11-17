#' Run the complete GeoEpiAI surveillance workflow
#'
#' Runs the main GeoEpiAI surveillance workflow, including surveillance-data
#' cleaning and early-warning analysis, with optional machine-learning
#' risk prediction and spatial risk mapping.
#'
#' @param surveillance Surveillance data.frame.
#' @param location Location column.
#' @param date Date column.
#' @param cases Case-count column.
#' @param population Optional population column.
#' @param risk_formula Optional binary risk formula.
#' @param sf_data Optional sf spatial data for mapping.
#'
#' @return A named list of cleaned data, warning results and optional
#'   model and mapping results.
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
#' result <- geoepi_pipeline(
#'   surveillance = dat,
#'   location = "location",
#'   date = "date",
#'   cases = "cases",
#'   population = "population"
#' )
#'
#' names(result)
#'
#' @export
geoepi_pipeline <- function(
    surveillance,
    location,
    date,
    cases,
    population = NULL,
    risk_formula = NULL,
    sf_data = NULL) {
  
  # ------------------------------------------------------------
  # Clean surveillance data
  # ------------------------------------------------------------
  
  clean <- clean_surveillance(
    surveillance,
    location = location,
    date = date,
    cases = cases,
    population = population
  )
  
  
  # ------------------------------------------------------------
  # Early-warning analysis
  # ------------------------------------------------------------
  
  warn <- early_warning(
    clean,
    "date",
    "cases"
  )
  
  
  # ------------------------------------------------------------
  # Initialise output
  # ------------------------------------------------------------
  
  out <- list(
    cleaned = clean,
    early_warning = warn
  )
  
  
  # ------------------------------------------------------------
  # Optional machine-learning analysis
  # ------------------------------------------------------------
  
  if (!is.null(risk_formula)) {
    
    # Required variables in the risk formula
    required_vars <- all.vars(
      risk_formula
    )
    
    # Check that all variables are available in
    # the original surveillance data.
    missing_vars <- setdiff(
      required_vars,
      names(surveillance)
    )
    
    if (length(missing_vars) > 0) {
      
      stop(
        "Variables required by risk_formula are missing from surveillance: ",
        paste(missing_vars, collapse = ", "),
        call. = FALSE
      )
    }
    
    # Preserve original ML variables
    ml_data <- surveillance
    
    # Convert the specified date variable to Date
    ml_data[[date]] <- as_date_safe(
      ml_data[[date]]
    )
    
    # Train Random Forest
    rf <- train_rf_risk(
      risk_formula,
      ml_data
    )
    
    # Train XGBoost
    xgb <- train_xgb_risk(
      risk_formula,
      ml_data
    )
    
    # Prediction data contains predictor variables only.
    predictor_vars <- all.vars(
      stats::delete.response(
        stats::terms(
          risk_formula
        )
      )
    )
    
    prediction_data <- ml_data[
      ,
      predictor_vars,
      drop = FALSE
    ]
    
    out$random_forest <- rf
    out$xgboost <- xgb
    
    out$rf_risk <- predict_risk(
      rf,
      prediction_data
    )
    
    out$xgb_risk <- predict_risk(
      xgb,
      prediction_data
    )
  }
  
  
  # ------------------------------------------------------------
  # Optional spatial risk map
  # ------------------------------------------------------------
  
  if (!is.null(sf_data)) {
    
    if (!inherits(sf_data, "sf")) {
      
      stop(
        "sf_data must be an sf object.",
        call. = FALSE
      )
    }
    
    out$map <- dynamic_risk_map(
      sf_data
    )
  }
  
  
  # ------------------------------------------------------------
  # Assign pipeline class
  # ------------------------------------------------------------
  
  class(out) <- c(
    "GeoEpiAI_pipeline",
    "list"
  )
  
  out
}

