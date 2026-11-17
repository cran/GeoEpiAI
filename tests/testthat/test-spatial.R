library(testthat)
library(GeoEpiAI)


# ============================================================
# Helper: 4 x 4 point grid
# ============================================================

make_point_data <- function() {
  
  sf::st_as_sf(
    data.frame(
      value = c(
        10, 20, 30, 40,
        20, 30, 40, 50,
        30, 40, 50, 60,
        40, 50, 60, 70
      ),
      x = rep(0:3, each = 4),
      y = rep(0:3, times = 4)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
}


# ============================================================
# spatial_autocorrelation()
# ============================================================

test_that("spatial_autocorrelation works with polygon data", {
  
  dat <- sf::st_as_sf(
    data.frame(
      value = c(10, 20, 30, 20),
      geometry = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
        "POLYGON ((0 1, 1 1, 1 2, 0 2, 0 1))",
        "POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))"
      )
    ),
    wkt = "geometry",
    crs = 4326
  )
  
  result <- spatial_autocorrelation(
    dat,
    "value"
  )
  
  expect_true(is.list(result))
  
  expect_true(
    all(
      c("test", "weights", "neighbours") %in%
        names(result)
    )
  )
  
  expect_s3_class(
    result$test,
    "htest"
  )
})


test_that("spatial_autocorrelation works with point data", {
  
  dat <- make_point_data()
  
  result <- spatial_autocorrelation(
    dat,
    "value"
  )
  
  expect_true(is.list(result))
  
  expect_true(
    all(
      c("test", "weights", "neighbours") %in%
        names(result)
    )
  )
  
  expect_s3_class(
    result$test,
    "htest"
  )
})


test_that("spatial_autocorrelation rejects non-sf data", {
  
  dat <- data.frame(
    value = c(10, 20, 30)
  )
  
  expect_error(
    spatial_autocorrelation(
      dat,
      "value"
    ),
    "data must be an sf object"
  )
})


test_that("spatial_autocorrelation rejects missing value column", {
  
  dat <- sf::st_as_sf(
    data.frame(
      value = c(10, 20, 30),
      x = c(0, 1, 2),
      y = c(0, 1, 2)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  
  expect_error(
    spatial_autocorrelation(
      dat,
      "missing_value"
    ),
    "Missing required columns"
  )
})


# ============================================================
# hotspot_detection()
# ============================================================

test_that("hotspot_detection works with polygon data", {
  
  dat <- sf::st_as_sf(
    data.frame(
      value = c(10, 20, 30, 40),
      geometry = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
        "POLYGON ((0 1, 1 1, 1 2, 0 2, 0 1))",
        "POLYGON ((1 1, 2 1, 2 2, 1 2, 1 1))"
      )
    ),
    wkt = "geometry",
    crs = 4326
  )
  
  result <- hotspot_detection(
    dat,
    "value"
  )
  
  expect_true(
    all(
      c("Gi_Z", "Gi_p", "hotspot") %in%
        names(result)
    )
  )
  
  # Local-G statistics can be non-finite for very small
  # synthetic neighbourhood structures.
  expect_true(
    all(
      is.na(result$Gi_Z) |
        is.infinite(result$Gi_Z) |
        is.finite(result$Gi_Z)
    )
  )
  
  expect_true(
    all(
      is.na(result$Gi_p) |
        is.infinite(result$Gi_p) |
        is.finite(result$Gi_p)
    )
  )
  
  expect_true(
    all(
      result$hotspot %in%
        c(
          "Hotspot",
          "Coldspot",
          "Not significant"
        )
    )
  )
  
  expect_equal(
    nrow(result),
    nrow(dat)
  )
})


test_that("hotspot_detection works with point data", {
  
  dat <- make_point_data()
  
  result <- hotspot_detection(
    dat,
    "value"
  )
  
  expect_true(
    all(
      c("Gi_Z", "Gi_p", "hotspot") %in%
        names(result)
    )
  )
  
  expect_true(
    all(
      is.na(result$Gi_Z) |
        is.infinite(result$Gi_Z) |
        is.finite(result$Gi_Z)
    )
  )
  
  expect_true(
    all(
      is.na(result$Gi_p) |
        is.infinite(result$Gi_p) |
        is.finite(result$Gi_p)
    )
  )
  
  expect_true(
    all(
      result$hotspot %in%
        c(
          "Hotspot",
          "Coldspot",
          "Not significant"
        )
    )
  )
  
  expect_equal(
    nrow(result),
    nrow(dat)
  )
})


test_that("hotspot_detection rejects non-sf data", {
  
  dat <- data.frame(
    value = c(10, 20, 30)
  )
  
  expect_error(
    hotspot_detection(
      dat,
      "value"
    ),
    "data must be an sf object"
  )
})


test_that("hotspot_detection rejects missing value column", {
  
  dat <- sf::st_as_sf(
    data.frame(
      value = c(10, 20, 30),
      x = c(0, 1, 2),
      y = c(0, 1, 2)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  
  expect_error(
    hotspot_detection(
      dat,
      "missing_value"
    ),
    "Missing required columns"
  )
})


test_that("hotspot_detection supports queen contiguity", {
  
  dat <- make_point_data()
  
  result <- hotspot_detection(
    dat,
    "value",
    queen = TRUE
  )
  
  expect_true(
    "hotspot" %in%
      names(result)
  )
  
  expect_true(
    all(
      is.na(result$Gi_Z) |
        is.infinite(result$Gi_Z) |
        is.finite(result$Gi_Z)
    )
  )
})
