test_that("validate_surveillance works", {
  dat <- data.frame(
    location = c("A", "B"),
    date = as.Date(c("2026-01-01", "2026-01-02")),
    cases = c(2, 3)
  )
  
  expect_true(
    validate_surveillance(
      dat,
      "location",
      "date",
      "cases"
    )
  )
})
test_that("validate_surveillance warns about missing location", {
  dat <- data.frame(
    location = c("A", NA),
    date = as.Date(c("2026-01-01", "2026-01-02")),
    cases = c(2, 3)
  )
  
  expect_warning(
    validate_surveillance(
      dat,
      "location",
      "date",
      "cases"
    ),
    "Location contains missing values"
  )
})


test_that("validate_surveillance warns about missing dates", {
  dat <- data.frame(
    location = c("A", "B"),
    date = as.Date(c("2026-01-01", NA)),
    cases = c(2, 3)
  )
  
  expect_warning(
    validate_surveillance(
      dat,
      "location",
      "date",
      "cases"
    ),
    "Date contains missing values"
  )
})


test_that("validate_surveillance warns about non-numeric cases", {
  dat <- data.frame(
    location = c("A", "B"),
    date = as.Date(c("2026-01-01", "2026-01-02")),
    cases = c("2", "invalid")
  )
  
  expect_warning(
    validate_surveillance(
      dat,
      "location",
      "date",
      "cases"
    ),
    "Cases contain non-numeric values"
  )
})


test_that("validate_surveillance warns about negative cases", {
  dat <- data.frame(
    location = c("A", "B"),
    date = as.Date(c("2026-01-01", "2026-01-02")),
    cases = c(2, -3)
  )
  
  expect_warning(
    validate_surveillance(
      dat,
      "location",
      "date",
      "cases"
    ),
    "Negative case counts detected"
  )
})
