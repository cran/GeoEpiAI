#' Train a Random Forest disease-risk model
#'
#' Trains a Random Forest classifier for binary disease-risk prediction.
#'
#' @param formula Model formula; outcome should be binary 0/1 for classification.
#' @param data Training data.
#' @param ntree Number of trees.
#' @param seed Random seed.
#'
#' @return A randomForest model.
#'
#' @examples
#' set.seed(123)
#'
#' dat <- data.frame(
#'   risk = factor(
#'     rep(c("Low", "High"), each = 10)
#'   ),
#'   precipitation = runif(20, 50, 200),
#'   temperature = runif(20, 20, 35)
#' )
#'
#' rf <- train_rf_risk(
#'   risk ~ precipitation + temperature,
#'   data = dat,
#'   ntree = 20
#' )
#'
#' rf
#'
#' @export
train_rf_risk <- function(
    formula,
    data,
    ntree = 500,
    seed = 123) {
  
  set.seed(seed)
  
  y <- stats::model.response(
    stats::model.frame(
      formula,
      data
    )
  )
  
  if (
    length(
      unique(
        stats::na.omit(y)
      )
    ) != 2
  ) {
    
    stop(
      "Random Forest risk classification requires exactly two outcome classes.",
      call. = FALSE
    )
  }
  
  if (is.numeric(y)) {
    
    data[[all.vars(formula)[1]]] <- factor(
      y,
      levels = c(0, 1)
    )
  }
  
  randomForest::randomForest(
    formula,
    data = data,
    ntree = ntree,
    importance = TRUE
  )
}


#' Train an XGBoost disease-risk model
#'
#' Trains an XGBoost binary classification model for disease-risk prediction.
#'
#' @param formula Binary classification formula.
#' @param data Training data.
#' @param nrounds Number of boosting rounds.
#' @param max_depth Maximum tree depth.
#' @param eta Learning rate.
#' @param seed Random seed.
#'
#' @return A list containing the xgboost model and preprocessing information.
#'
#' @examples
#' set.seed(123)
#'
#' dat <- data.frame(
#'   risk = rep(c(0, 1), each = 10),
#'   precipitation = runif(20, 50, 200),
#'   temperature = runif(20, 20, 35)
#' )
#'
#' xgb <- train_xgb_risk(
#'   risk ~ precipitation + temperature,
#'   data = dat,
#'   nrounds = 10
#' )
#'
#' xgb
#'
#' @export
train_xgb_risk <- function(
    formula,
    data,
    nrounds = 200,
    max_depth = 4,
    eta = 0.05,
    seed = 123) {
  
  # Construct model frame and remove incomplete observations
  mf <- stats::model.frame(
    formula,
    data = data,
    na.action = stats::na.omit
  )
  
  # Extract outcome
  y <- stats::model.response(mf)
  
  # Convert two-level factor outcome to 0/1
  if (is.factor(y)) {
    
    if (nlevels(y) != 2) {
      
      stop(
        "Outcome must have two classes.",
        call. = FALSE
      )
    }
    
    y <- as.integer(
      y == levels(y)[2]
    )
  }
  
  # Convert outcome to numeric
  y <- as.numeric(y)
  
  # Check binary coding
  if (!all(y %in% c(0, 1))) {
    
    stop(
      "XGBoost outcome must be coded 0/1.",
      call. = FALSE
    )
  }
  
  # Construct model matrix
  mm <- stats::model.matrix(
    attr(mf, "terms"),
    mf
  )
  
  # Remove intercept
  if ("(Intercept)" %in% colnames(mm)) {
    
    mm <- mm[
      ,
      colnames(mm) != "(Intercept)",
      drop = FALSE
    ]
  }
  
  # Train XGBoost
  set.seed(seed)
  
  fit <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = max_depth,
      eta = eta,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = xgboost::xgb.DMatrix(
      mm,
      label = y
    ),
    nrounds = nrounds,
    verbose = 0
  )
  
  # Store model and preprocessing information
  structure(
    list(
      model = fit,
      terms = attr(mf, "terms"),
      contrasts = attr(mm, "contrasts"),
      columns = colnames(mm)
    ),
    class = "GeoEpiAI_xgb"
  )
}


#' Predict disease risk from a GeoEpiAI model
#'
#' Generates predicted disease-risk probabilities from a Random Forest
#' or GeoEpiAI XGBoost model.
#'
#' @param model Random Forest, xgboost, or GeoEpiAI_xgb model.
#' @param newdata New observations containing predictor variables.
#'
#' @return Numeric predicted risk probabilities.
#'
#' @examples
#' set.seed(123)
#'
#' dat <- data.frame(
#'   risk = factor(
#'     rep(c("Low", "High"), each = 10)
#'   ),
#'   precipitation = runif(20, 50, 200),
#'   temperature = runif(20, 20, 35)
#' )
#'
#' rf <- train_rf_risk(
#'   risk ~ precipitation + temperature,
#'   data = dat,
#'   ntree = 20
#' )
#'
#' new_data <- data.frame(
#'   precipitation = c(100, 150),
#'   temperature = c(25, 30)
#' )
#'
#' predict_risk(
#'   rf,
#'   new_data
#' )
#'
#' @export
predict_risk <- function(
    model,
    newdata) {
  
  # ------------------------------------------------------------
  # Random Forest
  # ------------------------------------------------------------
  if (inherits(model, "randomForest")) {
    
    p <- predict(
      model,
      newdata,
      type = "prob"
    )
    
    return(
      as.numeric(
        p[, ncol(p)]
      )
    )
  }
  
  
  # ------------------------------------------------------------
  # GeoEpiAI XGBoost
  # ------------------------------------------------------------
  if (inherits(model, "GeoEpiAI_xgb")) {
    
    # Remove the response variable from the stored formula
    predictors <- stats::delete.response(
      model$terms
    )
    
    # Build model frame using predictor-only formula
    mf <- stats::model.frame(
      predictors,
      newdata,
      na.action = stats::na.omit
    )
    
    # Build model matrix
    mm <- stats::model.matrix(
      predictors,
      mf
    )
    
    # Remove intercept
    if ("(Intercept)" %in% colnames(mm)) {
      
      mm <- mm[
        ,
        colnames(mm) != "(Intercept)",
        drop = FALSE
      ]
    }
    
    # Identify columns expected by the trained model
    missing_cols <- setdiff(
      model$columns,
      colnames(mm)
    )
    
    # Add missing columns as zero
    for (cc in missing_cols) {
      
      mm <- cbind(
        mm,
        0
      )
      
      colnames(mm)[ncol(mm)] <- cc
    }
    
    # Ensure exactly the same columns and order
    # as used during model training
    mm <- mm[
      ,
      model$columns,
      drop = FALSE
    ]
    
    # Generate predicted probabilities
    return(
      as.numeric(
        predict(
          model$model,
          xgboost::xgb.DMatrix(mm)
        )
      )
    )
  }
  
  
  # ------------------------------------------------------------
  # Unsupported model
  # ------------------------------------------------------------
  stop(
    "Unsupported model class.",
    call. = FALSE
  )
}



