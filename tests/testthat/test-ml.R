test_that("train_rf_risk works with binary numeric outcome", {
  set.seed(123)
  
  dat <- data.frame(
    outcome = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    x1 = c(1, 2, 1, 3, 2, 4, 1, 5, 2, 6),
    x2 = c(5, 4, 6, 3, 5, 2, 7, 1, 6, 2)
  )
  
  fit <- train_rf_risk(
    outcome ~ x1 + x2,
    data = dat,
    ntree = 20,
    seed = 123
  )
  
  expect_s3_class(fit, "randomForest")
  expect_true("importance" %in% names(fit))
})


test_that("train_rf_risk rejects non-binary outcome", {
  dat <- data.frame(
    outcome = c(0, 1, 2, 0, 1, 2),
    x1 = 1:6
  )
  
  expect_error(
    train_rf_risk(
      outcome ~ x1,
      data = dat,
      ntree = 10
    ),
    "exactly two outcome classes"
  )
})


test_that("train_xgb_risk works with numeric binary outcome", {
  set.seed(123)
  
  dat <- data.frame(
    outcome = c(
      0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
      1, 0, 1, 0, 1, 0, 1, 0, 1, 0
    ),
    x1 = seq_len(20),
    x2 = 21:40
  )
  
  fit <- train_xgb_risk(
    outcome ~ x1 + x2,
    data = dat,
    nrounds = 10,
    max_depth = 2,
    eta = 0.1,
    seed = 123
  )
  
  expect_s3_class(fit, "GeoEpiAI_xgb")
  expect_true("model" %in% names(fit))
  expect_true("terms" %in% names(fit))
  expect_true("columns" %in% names(fit))
})


test_that("train_xgb_risk works with two-level factor outcome", {
  set.seed(123)
  
  dat <- data.frame(
    outcome = factor(
      c(
        "No", "Yes", "No", "Yes", "No",
        "Yes", "No", "Yes", "No", "Yes"
      )
    ),
    x1 = 1:10,
    x2 = 11:20
  )
  
  fit <- train_xgb_risk(
    outcome ~ x1 + x2,
    data = dat,
    nrounds = 10,
    max_depth = 2,
    eta = 0.1,
    seed = 123
  )
  
  expect_s3_class(fit, "GeoEpiAI_xgb")
})


test_that("train_xgb_risk rejects non-binary outcome", {
  dat <- data.frame(
    outcome = c(0, 1, 2, 0, 1, 2),
    x1 = 1:6
  )
  
  expect_error(
    train_xgb_risk(
      outcome ~ x1,
      data = dat,
      nrounds = 5
    ),
    "coded 0/1"
  )
})


test_that("predict_risk works with Random Forest", {
  set.seed(123)
  
  dat <- data.frame(
    outcome = c(
      0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 1, 0, 1, 0, 1, 0
    ),
    x1 = 1:16,
    x2 = 17:32
  )
  
  fit <- train_rf_risk(
    outcome ~ x1 + x2,
    data = dat,
    ntree = 20,
    seed = 123
  )
  
  predictions <- predict_risk(
    fit,
    dat[, c("x1", "x2")]
  )
  
  expect_type(predictions, "double")
  expect_length(predictions, nrow(dat))
  expect_true(all(predictions >= 0 & predictions <= 1))
})


test_that("predict_risk works with XGBoost", {
  set.seed(123)
  
  dat <- data.frame(
    outcome = c(
      0, 1, 0, 1, 0, 1, 0, 1,
      0, 1, 1, 0, 1, 0, 1, 0
    ),
    x1 = 1:16,
    x2 = 17:32
  )
  
  fit <- train_xgb_risk(
    outcome ~ x1 + x2,
    data = dat,
    nrounds = 10,
    max_depth = 2,
    eta = 0.1,
    seed = 123
  )
  
  predictions <- predict_risk(
    fit,
    dat[, c("x1", "x2")]
  )
  
  expect_type(predictions, "double")
  expect_length(predictions, nrow(dat))
  expect_true(all(predictions >= 0 & predictions <= 1))
})


test_that("predict_risk rejects unsupported model", {
  expect_error(
    predict_risk(
      list(foo = "bar"),
      data.frame(x1 = 1:3)
    ),
    "Unsupported model class"
  )
})


