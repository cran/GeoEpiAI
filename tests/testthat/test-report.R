test_that("epi_report generates an HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("ggplot2")
  
  surveillance <- data.frame(
    location = c("A", "A", "B", "B"),
    date = as.Date(c(
      "2026-01-01",
      "2026-02-01",
      "2026-01-01",
      "2026-02-01"
    )),
    cases = c(5, 8, 3, 6)
  )
  
  output_file <- file.path(
    tempdir(),
    "GeoEpiAI_test_report.html"
  )
  
  result <- epi_report(
    surveillance = surveillance,
    output_file = output_file,
    title = "Test Epidemiological Report"
  )
  
  expect_true(file.exists(result))
  
  expect_true(
    grepl(
      "GeoEpiAI_test_report\\.html$",
      result
    )
  )
})


test_that("epi_report accepts risk map and warning table", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("ggplot2")
  
  surveillance <- data.frame(
    location = c("A", "A", "B", "B"),
    date = as.Date(c(
      "2026-01-01",
      "2026-02-01",
      "2026-01-01",
      "2026-02-01"
    )),
    cases = c(5, 8, 3, 6)
  )
  
  risk_map <- ggplot2::ggplot(
    surveillance,
    ggplot2::aes(
      x = date,
      y = cases
    )
  ) +
    ggplot2::geom_line()
  
  warning_table <- data.frame(
    date = surveillance$date,
    cases = surveillance$cases,
    alert = c(FALSE, TRUE, FALSE, TRUE)
  )
  
  output_file <- file.path(
    tempdir(),
    "GeoEpiAI_test_report_options.html"
  )
  
  result <- epi_report(
    surveillance = surveillance,
    output_file = output_file,
    title = "Test Report",
    risk_map = risk_map,
    warning_table = warning_table
  )
  
  expect_true(file.exists(result))
})


test_that("epi_report creates a missing output directory", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not_installed("ggplot2")
  
  surveillance <- data.frame(
    location = c("A", "B"),
    date = as.Date(c(
      "2026-01-01",
      "2026-02-01"
    )),
    cases = c(2, 4)
  )
  
  output_dir <- file.path(
    tempdir(),
    paste0(
      "GeoEpiAI_test_",
      as.integer(Sys.time())
    )
  )
  
  output_file <- file.path(
    output_dir,
    "report.html"
  )
  
  result <- epi_report(
    surveillance = surveillance,
    output_file = output_file
  )
  
  expect_true(dir.exists(output_dir))
  expect_true(file.exists(result))
})

