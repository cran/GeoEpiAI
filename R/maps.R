#' Create a dynamic risk map
#'
#' Creates a ggplot-based spatial risk map with separate facets
#' for each time period.
#'
#' @param data sf object containing predicted risk and time.
#' @param risk Column containing predicted risk.
#' @param time Column containing time period.
#'
#' @return A ggplot object faceted by time.
#'
#' @examples
#' map_data <- sf::st_as_sf(
#'   data.frame(
#'     risk = c(0.2, 0.5, 0.7, 0.9),
#'     date = as.Date(c(
#'       "2025-01-01",
#'       "2025-01-01",
#'       "2025-02-01",
#'       "2025-02-01"
#'     )),
#'     x = c(0, 1, 0, 1),
#'     y = c(0, 0, 1, 1)
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' dynamic_risk_map(
#'   map_data,
#'   risk = "risk",
#'   time = "date"
#' )
#'
#' @export
dynamic_risk_map <- function(
    data,
    risk = "risk",
    time = "date") {
  
  if (!inherits(data, "sf")) {
    stop(
      "data must be an sf object.",
      call. = FALSE
    )
  }
  
  check_cols(
    data,
    c(risk, time)
  )
  
  ggplot2::ggplot(data) +
    ggplot2::geom_sf(
      ggplot2::aes(
        fill = .data[[risk]]
      ),
      colour = "grey40",
      linewidth = 0.15
    ) +
    ggplot2::facet_wrap(
      stats::as.formula(
        paste("~", time)
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      na.value = "grey90"
    ) +
    ggplot2::labs(
      fill = "Predicted risk",
      x = NULL,
      y = NULL,
      title = "Dynamic disease-risk map"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold"
      )
    )
}
