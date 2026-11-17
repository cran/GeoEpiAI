library(testthat)
library(GeoEpiAI)


# ============================================================
# Basic pipeline
# ============================================================

test_that("geoepi_pipeline runs the basic surveillance workflow", {
  
  surveillance <- data.frame(
    location = c(
      "A", "A", "A", "A",
      "B", "B", "B", "B"
    ),
    date = as.Date(c(
      "2026-01-01",
      "2026-02-01",
      "2026-03-01",
      "2026-04-01",
      "2026-01-01",
      "2026-02-01",
      "2026-03-01",
      "2026-04-01"
    )),
    cases = c(
      2, 3, 4, 8,
      1, 2, 3, 7
    )
  )
  
  result <- geoepi_pipeline(
    surveillance = surveillance,
    location = "location",
    date = "date",
    cases = "cases"
  )
  
  expect_s3_class(
    result,
    "GeoEpiAI_pipeline"
  )
  
  expect_true(
    all(
      c(
        "cleaned",
        "early_warning"
      ) %in%
        names(result)
    )
  )
  
  expect_true(
    is.data.frame(result$cleaned)
  )
  
  expect_true(
    is.data.frame(result$early_warning)
  )
})


# ============================================================
# Pipeline with population
# ============================================================

test_that("geoepi_pipeline handles population data", {
  
  surveillance <- data.frame(
    location = c(
      "A", "A", "A",
      "B", "B", "B"
    ),
    date = as.Date(c(
      "2026-01-01",
      "2026-02-01",
      "2026-03-01",
      "2026-01-01",
      "2026-02-01",
      "2026-03-01"
    )),
    cases = c(
      2, 4, 6,
      1, 3, 5
    ),
    population = c(
      1000, 1000, 1000,
      2000, 2000, 2000
    )
  )
  
  result <- geoepi_pipeline(
    surveillance = surveillance,
    location = "location",
    date = "date",
    cases = "cases",
    population = "population"
  )
  
  expect_s3_class(
    result,
    "GeoEpiAI_pipeline"
  )
  
  expect_true(
    "population" %in%
      names(result$cleaned)
  )
  
  expect_true(
    "incidence" %in%
      names(result$cleaned)
  )
  
  expect_true(
    all(
      is.finite(
        result$cleaned$incidence
      )
    )
  )
})


# ============================================================
# Pipeline with Random Forest and XGBoost
# ============================================================

test_that("geoepi_pipeline runs machine-learning models", {
  
  set.seed(123)
  
  surveillance <- data.frame(
    location = rep(
      c("A", "B"),
      each = 20
    ),
    date = seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 40
    ),
    cases = sample(
      1:10,
      40,
      replace = TRUE
    ),
    risk = rep(
      c(0, 1),
      20
    ),
    x1 = rnorm(40),
    x2 = rnorm(40)
  )
  
  result <- geoepi_pipeline(
    surveillance = surveillance,
    location = "location",
    date = "date",
    cases = "cases",
    risk_formula = risk ~ x1 + x2
  )
  
  expect_s3_class(
    result,
    "GeoEpiAI_pipeline"
  )
  
  expect_true(
    "random_forest" %in%
      names(result)
  )
  
  expect_true(
    "xgboost" %in%
      names(result)
  )
  
  expect_true(
    "rf_risk" %in%
      names(result)
  )
  
  expect_true(
    "xgb_risk" %in%
      names(result)
  )
  
  expect_true(
    is.numeric(result$rf_risk)
  )
  
  expect_true(
    is.numeric(result$xgb_risk)
  )
  
  expect_equal(
    length(result$rf_risk),
    nrow(result$cleaned)
  )
  
  expect_equal(
    length(result$xgb_risk),
    nrow(result$cleaned)
  )
})


# ============================================================
# Pipeline with spatial data
# ============================================================

test_that("geoepi_pipeline creates a risk map with sf data", {
  
  surveillance <- data.frame(
    location = c(
      "A", "B", "C", "D"
    ),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01",
      "2026-02-01",
      "2026-02-01"
    )),
    cases = c(
      2, 4, 6, 8
    )
  )
  
  sf_data <- sf::st_as_sf(
    data.frame(
      location = c(
        "A", "B", "C", "D"
      ),
      date = as.Date(c(
        "2026-01-01",
        "2026-01-01",
        "2026-02-01",
        "2026-02-01"
      )),
      risk = c(
        0.10, 0.30, 0.60, 0.90
      ),
      x = c(
        0, 1, 0, 1
      ),
      y = c(
        0, 0, 1, 1
      )
    ),
    coords = c(
      "x",
      "y"
    ),
    crs = 4326
  )
  
  result <- geoepi_pipeline(
    surveillance = surveillance,
    location = "location",
    date = "date",
    cases = "cases",
    sf_data = sf_data
  )
  
  expect_s3_class(
    result,
    "GeoEpiAI_pipeline"
  )
  
  expect_true(
    "map" %in%
      names(result)
  )
  
  expect_s3_class(
    result$map,
    "ggplot"
  )
})


# ============================================================
# Pipeline with both ML and spatial data
# ============================================================

test_that(
  "geoepi_pipeline handles machine learning and spatial data together",
  {
    
    surveillance <- data.frame(
      location = rep(
        c("A", "B"),
        each = 20
      ),
      date = seq.Date(
        as.Date("2024-01-01"),
        by = "month",
        length.out = 40
      ),
      cases = sample(
        1:10,
        40,
        replace = TRUE
      ),
      risk = rep(
        c(0, 1),
        20
      ),
      x1 = rnorm(40),
      x2 = rnorm(40)
    )
    
    sf_data <- sf::st_as_sf(
      data.frame(
        location = c(
          "A", "B"
        ),
        date = as.Date(c(
          "2026-01-01",
          "2026-02-01"
        )),
        risk = c(
          0.25,
          0.75
        ),
        x = c(
          0,
          1
        ),
        y = c(
          0,
          1
        )
      ),
      coords = c(
        "x",
        "y"
      ),
      crs = 4326
    )
    
    result <- geoepi_pipeline(
      surveillance = surveillance,
      location = "location",
      date = "date",
      cases = "cases",
      risk_formula = risk ~ x1 + x2,
      sf_data = sf_data
    )
    
    expect_s3_class(
      result,
      "GeoEpiAI_pipeline"
    )
    
    expect_true(
      all(
        c(
          "cleaned",
          "early_warning",
          "random_forest",
          "xgboost",
          "rf_risk",
          "xgb_risk",
          "map"
        ) %in%
          names(result)
      )
    )
    
    expect_s3_class(
      result$map,
      "ggplot"
    )
  }
)


# ============================================================
# Invalid sf_data
# ============================================================

test_that(
  "geoepi_pipeline rejects non-sf spatial data",
  {
    
    surveillance <- data.frame(
      location = c(
        "A", "B"
      ),
      date = as.Date(c(
        "2026-01-01",
        "2026-02-01"
      )),
      cases = c(
        2, 4
      )
    )
    
    invalid_sf <- data.frame(
      location = c(
        "A", "B"
      ),
      risk = c(
        0.2,
        0.8
      )
    )
    
    expect_error(
      geoepi_pipeline(
        surveillance = surveillance,
        location = "location",
        date = "date",
        cases = "cases",
        sf_data = invalid_sf
      ),
      "sf_data must be an sf object"
    )
  }
)


# ============================================================
# Pipeline returns expected cleaned structure
# ============================================================

test_that(
  "geoepi_pipeline returns expected cleaned fields",
  {
    
    surveillance <- data.frame(
      location = c(
        "A", "A",
        "B", "B"
      ),
      date = as.Date(c(
        "2026-01-01",
        "2026-02-01",
        "2026-01-01",
        "2026-02-01"
      )),
      cases = c(
        5, 8,
        3, 6
      )
    )
    
    result <- geoepi_pipeline(
      surveillance = surveillance,
      location = "location",
      date = "date",
      cases = "cases"
    )
    
    expect_true(
      all(
        c(
          "location",
          "date",
          "cases",
          "month",
          "incidence"
        ) %in%
          names(result$cleaned)
      )
    )
    
    expect_true(
      all(
        c(
          "date",
          "cases",
          "baseline",
          "z_score",
          "alert",
          "alert_level"
        ) %in%
          names(result$early_warning)
      )
    )
  }
)
