test_that("integrate_environment left join works", {
  
  surveillance <- data.frame(
    location = c("A", "B", "C"),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01",
      "2026-01-02"
    )),
    cases = c(2, 4, 3)
  )
  
  environment <- data.frame(
    location = c("A", "B", "C"),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01",
      "2026-01-02"
    )),
    rainfall = c(10, 20, 15)
  )
  
  result <- integrate_environment(
    surveillance,
    environment,
    by = c("location", "date"),
    method = "left"
  )
  
  expect_equal(nrow(result), 3)
  expect_true("rainfall" %in% names(result))
  expect_equal(result$rainfall, c(10, 20, 15))
})


test_that("integrate_environment inner join works", {
  
  surveillance <- data.frame(
    location = c("A", "B", "C"),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01",
      "2026-01-02"
    )),
    cases = c(2, 4, 3)
  )
  
  environment <- data.frame(
    location = c("A", "B"),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-01"
    )),
    rainfall = c(10, 20)
  )
  
  result <- integrate_environment(
    surveillance,
    environment,
    by = c("location", "date"),
    method = "inner"
  )
  
  expect_equal(nrow(result), 2)
  expect_true("rainfall" %in% names(result))
  expect_equal(result$rainfall, c(10, 20))
})


test_that("integrate_environment detects missing join columns", {
  
  surveillance <- data.frame(
    location = c("A", "B"),
    date = as.Date(c(
      "2026-01-01",
      "2026-01-02"
    )),
    cases = c(2, 4)
  )
  
  environment <- data.frame(
    location = c("A", "B"),
    rainfall = c(10, 20)
  )
  
  expect_error(
    integrate_environment(
      surveillance,
      environment,
      by = c("location", "date"),
      method = "left"
    )
  )
})


test_that("integrate_environment rejects invalid spatial input", {
  
  surveillance <- data.frame(
    location = c("A", "B"),
    cases = c(2, 4)
  )
  
  environment <- data.frame(
    location = c("A", "B"),
    rainfall = c(10, 20)
  )
  
  expect_error(
    integrate_environment(
      surveillance,
      environment,
      method = "spatial_nearest"
    ),
    "spatial_nearest requires both inputs to be sf objects"
  )
})


test_that("integrate_environment spatial nearest works", {
  
  skip_if_not_installed("sf")
  
  surveillance <- sf::st_as_sf(
    data.frame(
      location = c("A", "B"),
      cases = c(2, 4),
      x = c(0, 10),
      y = c(0, 0)
    ),
    coords = c("x", "y"),
    crs = 3857
  )
  
  environment <- sf::st_as_sf(
    data.frame(
      station = c("E1", "E2"),
      rainfall = c(10, 20),
      x = c(1, 11),
      y = c(0, 0)
    ),
    coords = c("x", "y"),
    crs = 3857
  )
  
  result <- integrate_environment(
    surveillance,
    environment,
    method = "spatial_nearest"
  )
  
  expect_s3_class(result, "sf")
  expect_true("rainfall" %in% names(result))
  expect_true(".distance" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$rainfall, c(10, 20))
})


test_that("integrate_environment respects maximum spatial distance", {
  
  skip_if_not_installed("sf")
  
  surveillance <- sf::st_as_sf(
    data.frame(
      location = "A",
      cases = 2,
      x = 0,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 3857
  )
  
  environment <- sf::st_as_sf(
    data.frame(
      station = "E1",
      rainfall = 10,
      x = 100,
      y = 0
    ),
    coords = c("x", "y"),
    crs = 3857
  )
  
  result <- integrate_environment(
    surveillance,
    environment,
    method = "spatial_nearest",
    max_distance = 10
  )
  
  expect_equal(nrow(result), 0)
})

