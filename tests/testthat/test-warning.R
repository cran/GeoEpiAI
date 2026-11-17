test_that("early warning returns required fields", {
  d <- data.frame(
    date = seq.Date(
      as.Date("2026-01-01"),
      by = "month",
      length.out = 10
    ),
    cases = c(1, 1, 2, 1, 2, 1, 2, 2, 3, 10)
  )
  
  z <- GeoEpiAI::early_warning(
    d,
    "date",
    "cases",
    window = 4
  )
  
  expect_true(
    all(
      c(
        "baseline",
        "z_score",
        "alert",
        "alert_level"
      ) %in% names(z)
    )
  )
  
  expect_true(
    any(z$alert, na.rm = TRUE)
  )
})

test_that("early warning rejects invalid window", {
  d <- data.frame(
    date = seq.Date(
      as.Date("2026-01-01"),
      by = "month",
      length.out = 5
    ),
    cases = c(1, 2, 1, 2, 3)
  )
  
  expect_error(
    early_warning(
      d,
      "date",
      "cases",
      window = 1
    ),
    "window must be a single number >= 2"
  )
})


test_that("early warning rejects invalid threshold", {
  d <- data.frame(
    date = seq.Date(
      as.Date("2026-01-01"),
      by = "month",
      length.out = 5
    ),
    cases = c(1, 2, 1, 2, 3)
  )
  
  expect_error(
    early_warning(
      d,
      "date",
      "cases",
      threshold = 0
    ),
    "threshold must be a single positive number"
  )
})


test_that("early warning rejects missing date column", {
  d <- data.frame(
    cases = c(1, 2, 3, 4, 5)
  )
  
  expect_error(
    early_warning(
      d,
      "date",
      "cases"
    ),
    "Date column not found"
  )
})


test_that("early warning rejects missing cases column", {
  d <- data.frame(
    date = seq.Date(
      as.Date("2026-01-01"),
      by = "month",
      length.out = 5
    )
  )
  
  expect_error(
    early_warning(
      d,
      "date",
      "cases"
    ),
    "Cases column not found"
  )
})


test_that("early warning rejects negative cases", {
  d <- data.frame(
    date = seq.Date(
      as.Date("2026-01-01"),
      by = "month",
      length.out = 5
    ),
    cases = c(1, 2, -1, 2, 3)
  )
  
  expect_error(
    early_warning(
      d,
      "date",
      "cases"
    ),
    "Cases cannot be negative"
  )
})

test_that("early_warning calculates baselines independently by location", {
  
  test_data <- data.frame(
    location = rep(c("A", "B"), each = 8),
    date = rep(as.Date("2026-01-01") + 0:7, 2),
    cases = c(
      10, 11, 10, 12, 11, 13, 30, 35,
      2, 2, 3, 2, 3, 2, 3, 3
    )
  )
  
  result <- early_warning(
    test_data,
    date = "date",
    cases = "cases",
    window = 4,
    threshold = 2
  )
  
  expect_true("location" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
  
  # Location A should trigger an alert on January 7
  a_result <- result[
    result$location == "A" &
      result$date == as.Date("2026-01-07"),
  ]
  
  expect_true(a_result$alert)
  
  # Location B should not be affected by Location A
  b_result <- result[
    result$location == "B" &
      result$date == as.Date("2026-01-07"),
  ]
  
  expect_false(b_result$alert)
  
  expect_equal(b_result$baseline, 2.5)
})

test_that("early_warning handles all-zero case counts", {
  
  zero_data <- data.frame(
    location = "A",
    date = as.Date("2026-01-01") + 0:9,
    cases = rep(0, 10)
  )
  
  result <- early_warning(
    zero_data,
    date = "date",
    cases = "cases"
  )
  
  expect_false(any(result$alert))
  expect_true(all(result$alert_level == "Normal"))
  expect_equal(
    result$z_score[-1],
    rep(0, 9)
  )
})
