
#' Generate an automated epidemiological report
#'
#' Generates an HTML epidemiological report containing a surveillance
#' summary, disease trend, optional risk map, and optional
#' early-warning results.
#'
#' @param surveillance Clean surveillance data containing at least
#'   `location`, `date`, and `cases` columns.
#' @param output_file Explicit path and filename for the generated HTML
#'   report. No default output path is used.
#' @param title Report title.
#' @param risk_map Optional ggplot risk map.
#' @param warning_table Optional early-warning table.
#'
#' @return Path to the generated report.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("rmarkdown", quietly = TRUE)) {
#'   surveillance <- data.frame(
#'     location = c("A", "A", "B", "B"),
#'     date = as.Date(c(
#'       "2025-01-01",
#'       "2025-02-01",
#'       "2025-01-01",
#'       "2025-02-01"
#'     )),
#'     cases = c(10, 15, 5, 8)
#'   )
#'
#'   output_file <- file.path(
#'     tempdir(),
#'     "GeoEpiAI_example_report.html"
#'   )
#'
#'   epi_report(
#'     surveillance = surveillance,
#'     output_file = output_file,
#'     title = "Example GeoEpiAI Report"
#'   )
#' }
#' }
#'
#' @export
epi_report <- function(
    surveillance,
    output_file,
    title = "GeoEpiAI Epidemiological Report",
    risk_map = NULL,
    warning_table = NULL
) {
  
  # Validate output path
  if (
    missing(output_file) ||
    !is.character(output_file) ||
    length(output_file) != 1L ||
    is.na(output_file) ||
    !nzchar(trimws(output_file))
  ) {
    stop(
      "Please provide an explicit, non-empty output_file path.",
      call. = FALSE
    )
  }
  
  # Validate surveillance data
  if (!is.data.frame(surveillance)) {
    stop(
      "surveillance must be a data.frame.",
      call. = FALSE
    )
  }
  
  required_columns <- c(
    "location",
    "date",
    "cases"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(surveillance)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required surveillance columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Check rmarkdown availability
  if (!requireNamespace(
    "rmarkdown",
    quietly = TRUE
  )) {
    stop(
      "Install the 'rmarkdown' package to generate reports.",
      call. = FALSE
    )
  }
  
  # Resolve the explicitly supplied output directory
  output_file <- path.expand(output_file)
  
  out_dir <- dirname(output_file)
  
  if (!dir.exists(out_dir)) {
    created <- dir.create(
      out_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (!created && !dir.exists(out_dir)) {
      stop(
        "Could not create the output directory.",
        call. = FALSE
      )
    }
  }
  
  out_dir <- normalizePath(
    out_dir,
    winslash = "/",
    mustWork = TRUE
  )
  
  output_name <- basename(output_file)
  
  # Create temporary report template directory
  report_dir <- file.path(
    tempdir(),
    "GeoEpiAI_report_template"
  )
  
  dir.create(
    report_dir,
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  template <- file.path(
    report_dir,
    "report.Rmd"
  )
  
  # Define R Markdown template
  rmd <- c(
    "---",
    "title: \"`r params$title`\"",
    "output: html_document",
    "params:",
    "  title: \"GeoEpiAI Epidemiological Report\"",
    "  summary: NULL",
    "  surveillance: NULL",
    "  risk_map: NULL",
    "  warning_table: NULL",
    "---",
    "",
    "## Surveillance summary",
    "",
    "```{r}",
    "knitr::kable(params$summary)",
    "```",
    "",
    "## Disease trend",
    "",
    "```{r}",
    "p <- ggplot2::ggplot(params$surveillance,",
    "                     ggplot2::aes(date, cases)) +",
    "  ggplot2::geom_line() +",
    "  ggplot2::labs(x = NULL, y = 'Cases',",
    "                title = 'Reported disease cases') +",
    "  ggplot2::theme_minimal()",
    "p",
    "```",
    "",
    "## Risk map",
    "",
    "```{r}",
    "if (!is.null(params$risk_map)) print(params$risk_map)",
    "```",
    "",
    "## Early-warning signals",
    "",
    "```{r}",
    "if (!is.null(params$warning_table))",
    "  knitr::kable(params$warning_table)",
    "```"
  )
  
  # Write temporary R Markdown template
  writeLines(
    rmd,
    template
  )
  
  # Generate surveillance summary
  summary <- data.frame(
    Records = nrow(surveillance),
    Locations = dplyr::n_distinct(
      surveillance$location
    ),
    Total_cases = sum(
      surveillance$cases,
      na.rm = TRUE
    ),
    Start_date = min(
      surveillance$date,
      na.rm = TRUE
    ),
    End_date = max(
      surveillance$date,
      na.rm = TRUE
    )
  )
  
  # Render report to the explicitly requested location
  rmarkdown::render(
    input = template,
    output_file = output_name,
    output_dir = out_dir,
    quiet = TRUE,
    params = list(
      title = title,
      summary = summary,
      surveillance = surveillance,
      risk_map = risk_map,
      warning_table = warning_table
    ),
    envir = new.env(
      parent = globalenv()
    )
  )
  
  # Return generated report path
  normalizePath(
    file.path(
      out_dir,
      output_name
    ),
    winslash = "/",
    mustWork = TRUE
  )
}
