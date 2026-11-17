test_that("dynamic_risk_map creates a ggplot map", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  
  p1 <- sf::st_polygon(list(matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  p2 <- sf::st_polygon(list(matrix(
    c(
      1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  dat <- sf::st_sf(
    risk = c(0.2, 0.8),
    date = as.Date(c("2026-01-01", "2026-01-01")),
    geometry = sf::st_sfc(p1, p2, crs = 4326)
  )
  
  result <- dynamic_risk_map(
    dat,
    risk = "risk",
    time = "date"
  )
  
  expect_s3_class(result, "ggplot")
})


test_that("dynamic_risk_map works with multiple time periods", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  
  p1 <- sf::st_polygon(list(matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  p2 <- sf::st_polygon(list(matrix(
    c(
      1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  dat <- sf::st_sf(
    risk = c(0.2, 0.8, 0.4, 0.6),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01",
      "2026-02-01",
      "2026-02-01"
    )),
    geometry = sf::st_sfc(
      p1, p2, p1, p2,
      crs = 4326
    )
  )
  
  result <- dynamic_risk_map(
    dat,
    risk = "risk",
    time = "date"
  )
  
  expect_s3_class(result, "ggplot")
  expect_true(
    any(vapply(
      result$layers,
      function(x) inherits(x$geom, "GeomSf"),
      logical(1)
    ))
  )
})


test_that("dynamic_risk_map rejects non-sf data", {
  dat <- data.frame(
    risk = c(0.2, 0.8),
    date = as.Date(c("2026-01-01", "2026-02-01"))
  )
  
  expect_error(
    dynamic_risk_map(dat),
    "data must be an sf object"
  )
})


test_that("dynamic_risk_map detects missing risk column", {
  skip_if_not_installed("sf")
  
  p <- sf::st_polygon(list(matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  dat <- sf::st_sf(
    date = as.Date("2026-01-01"),
    geometry = sf::st_sfc(p, crs = 4326)
  )
  
  expect_error(
    dynamic_risk_map(
      dat,
      risk = "missing_risk",
      time = "date"
    ),
    "Missing required columns"
  )
})


test_that("dynamic_risk_map detects missing time column", {
  skip_if_not_installed("sf")
  
  p <- sf::st_polygon(list(matrix(
    c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ),
    ncol = 2,
    byrow = TRUE
  )))
  
  dat <- sf::st_sf(
    risk = 0.5,
    geometry = sf::st_sfc(p, crs = 4326)
  )
  
  expect_error(
    dynamic_risk_map(
      dat,
      risk = "risk",
      time = "missing_time"
    ),
    "Missing required columns"
  )
})

