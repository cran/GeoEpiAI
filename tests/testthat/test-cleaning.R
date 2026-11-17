testthat::test_that("clean_surveillance works", {
  d <- data.frame(
    location = c("A","A","B"),
    date = c("2026-01-01","2026-01-01","2026-02-01"),
    cases = c(2,3,4)
  )
  z <- GeoEpiAI::clean_surveillance(d, "location", "date", "cases")
  testthat::expect_equal(nrow(z), 2)
  testthat::expect_equal(sum(z$cases), 9)
})
