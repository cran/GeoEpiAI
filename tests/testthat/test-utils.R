test_that("check_cols passes when required columns exist", {
  dat <- data.frame(
    location = c("A", "B"),
    cases = c(1, 2)
  )
  
  expect_silent(
    check_cols(dat, c("location", "cases"))
  )
})


test_that("check_cols detects missing columns", {
  dat <- data.frame(
    location = c("A", "B")
  )
  
  expect_error(
    check_cols(dat, c("location", "cases")),
    "Missing required columns: cases"
  )
})


test_that("as_date_safe handles Date input", {
  x <- as.Date(c("2026-01-01", "2026-01-02"))
  
  result <- as_date_safe(x)
  
  expect_s3_class(result, "Date")
  expect_equal(result, x)
})


test_that("as_date_safe handles POSIXct input", {
  x <- as.POSIXct(
    c("2026-01-01", "2026-01-02"),
    tz = "UTC"
  )
  
  result <- as_date_safe(x)
  
  expect_s3_class(result, "Date")
  expect_equal(
    result,
    as.Date(x)
  )
})


test_that("as_date_safe parses YYYY-MM-DD dates", {
  x <- c("2026-01-01", "2026-02-15")
  
  result <- as_date_safe(x)
  
  expect_s3_class(result, "Date")
  expect_equal(
    result,
    as.Date(x)
  )
})


test_that("as_date_safe parses DD-MM-YYYY dates", {
  x <- c("01-03-2026", "15-04-2026")
  
  result <- as_date_safe(x)
  
  expect_s3_class(result, "Date")
  expect_equal(
    result,
    as.Date(c("2026-03-01", "2026-04-15"))
  )
})


test_that("as_date_safe rejects invalid dates", {
  x <- c("2026-01-01", "not-a-date")
  
  expect_error(
    as_date_safe(x),
    "Some date values could not be parsed"
  )
})


test_that("mode_value returns the most frequent value", {
  x <- c(1, 2, 2, 3, 2, 1)
  
  expect_equal(
    mode_value(x),
    2
  )
})


test_that("mode_value ignores missing values", {
  x <- c(NA, 2, 2, NA, 1)
  
  expect_equal(
    mode_value(x),
    2
  )
})


test_that("mode_value returns NA when all values are missing", {
  x <- c(NA, NA, NA)
  
  expect_true(
    is.na(mode_value(x))
  )
})


test_that("mode_value works with character values", {
  x <- c("A", "B", "B", "C", "B")
  
  expect_equal(
    mode_value(x),
    "B"
  )
})
