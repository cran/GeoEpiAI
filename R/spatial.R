#' Global Moran's I spatial autocorrelation
#'
#' Calculates Global Moran's I to assess spatial autocorrelation
#' in an sf point or polygon dataset.
#'
#' @param data sf polygon/point object.
#' @param value Variable to analyse.
#' @param queen Use queen contiguity for polygons.
#'
#' @return A list containing the Moran test and spatial weights.
#'
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     value = c(
#'       10, 20, 30, 40,
#'       20, 30, 40, 50,
#'       30, 40, 50, 60,
#'       40, 50, 60, 70
#'     ),
#'     x = rep(0:3, each = 4),
#'     y = rep(0:3, times = 4)
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' moran_result <- spatial_autocorrelation(
#'   pts,
#'   value = "value"
#' )
#'
#' moran_result$test
#'
#' @export
spatial_autocorrelation <- function(
    data,
    value,
    queen = TRUE) {
  
  if (!inherits(data, "sf")) {
    stop(
      "data must be an sf object.",
      call. = FALSE
    )
  }
  
  check_cols(
    data,
    value
  )
  
  g <- sf::st_geometry_type(
    data
  )
  
  if (
    all(
      g %in% c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    )
  ) {
    
    nb <- spdep::poly2nb(
      data,
      queen = queen
    )
    
  } else {
    
    xy <- sf::st_coordinates(
      sf::st_centroid(data)
    )
    
    nb <- spdep::knn2nb(
      spdep::knearneigh(
        xy,
        k = min(
          4,
          nrow(xy) - 1
        )
      )
    )
  }
  
  lw <- spdep::nb2listw(
    nb,
    style = "W",
    zero.policy = TRUE
  )
  
  z <- as.numeric(
    data[[value]]
  )
  
  test <- spdep::moran.test(
    z,
    lw,
    zero.policy = TRUE
  )
  
  list(
    test = test,
    weights = lw,
    neighbours = nb
  )
}


#' Local Getis-Ord Gi* hotspot detection
#'
#' Identifies statistically high and low spatial concentrations
#' using the Local Getis-Ord Gi* statistic.
#'
#' @param data sf object.
#' @param value Variable to analyse.
#' @param queen Use queen contiguity for polygons.
#'
#' @return An sf object containing the Gi statistic, p-value,
#'   and hotspot classification.
#'
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     value = c(
#'       10, 20, 30, 40,
#'       20, 30, 40, 50,
#'       30, 40, 50, 60,
#'       40, 50, 60, 70
#'     ),
#'     x = rep(0:3, each = 4),
#'     y = rep(0:3, times = 4)
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' hotspot_result <- hotspot_detection(
#'   pts,
#'   value = "value"
#' )
#'
#' hotspot_result
#'
#' @export
hotspot_detection <- function(
    data,
    value,
    queen = TRUE) {
  
  if (!inherits(data, "sf")) {
    stop(
      "data must be an sf object.",
      call. = FALSE
    )
  }
  
  check_cols(
    data,
    value
  )
  
  g <- sf::st_geometry_type(
    data
  )
  
  if (
    all(
      g %in% c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    )
  ) {
    
    nb <- spdep::poly2nb(
      data,
      queen = queen
    )
    
  } else {
    
    xy <- sf::st_coordinates(
      sf::st_centroid(data)
    )
    
    nb <- spdep::knn2nb(
      spdep::knearneigh(
        xy,
        k = min(
          4,
          nrow(xy) - 1
        )
      )
    )
  }
  
  lw <- spdep::nb2listw(
    nb,
    style = "W",
    zero.policy = TRUE
  )
  
  gi <- spdep::localG(
    as.numeric(
      data[[value]]
    ),
    lw,
    zero.policy = TRUE
  )
  
  out <- data
  
  out$Gi_Z <- as.numeric(
    gi
  )
  
  out$Gi_p <- 2 *
    stats::pnorm(
      -abs(out$Gi_Z)
    )
  
  out$hotspot <- dplyr::case_when(
    out$Gi_Z >= 1.96 ~ "Hotspot",
    out$Gi_Z <= -1.96 ~ "Coldspot",
    TRUE ~ "Not significant"
  )
  
  out
}
