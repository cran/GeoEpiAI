#' Integrate environmental predictors with surveillance locations
#'
#' Supports tabular location/date joins and sf spatial joins.
#'
#' @param surveillance Surveillance data.frame.
#' @param environment Environmental data.frame or sf object.
#' @param by Character vector of shared keys, e.g. c("location", "date").
#' @param method Join method: "left", "inner", or "spatial_nearest".
#' @param max_distance Maximum distance for nearest spatial join, in CRS units.
#'
#' @return Joined data.frame or sf object.
#'
#' @examples
#' surveillance <- data.frame(
#'   location = c("A", "B"),
#'   date = as.Date(c("2025-01-01", "2025-01-01")),
#'   cases = c(10, 5)
#' )
#'
#' environment <- data.frame(
#'   location = c("A", "B"),
#'   date = as.Date(c("2025-01-01", "2025-01-01")),
#'   precipitation = c(120, 80),
#'   temperature = c(25, 23)
#' )
#'
#' integrate_environment(
#'   surveillance,
#'   environment,
#'   by = c("location", "date"),
#'   method = "left"
#' )
#'
#' @export
integrate_environment <- function(
    surveillance,
    environment,
    by = c("location", "date"),
    method = c("left", "inner", "spatial_nearest"),
    max_distance = Inf) {
  
  method <- match.arg(method)
  
  # Tabular joins
  if (method %in% c("left", "inner")) {
    
    check_cols(
      surveillance,
      by
    )
    
    check_cols(
      environment,
      by
    )
    
    if (method == "left") {
      
      return(
        dplyr::left_join(
          surveillance,
          environment,
          by = by,
          suffix = c("", "_env")
        )
      )
    }
    
    return(
      dplyr::inner_join(
        surveillance,
        environment,
        by = by,
        suffix = c("", "_env")
      )
    )
  }
  
  # Spatial nearest-neighbour join
  if (
    !inherits(surveillance, "sf") ||
    !inherits(environment, "sf")
  ) {
    
    stop(
      "spatial_nearest requires both inputs to be sf objects.",
      call. = FALSE
    )
  }
  
  # Check CRS
  if (
    is.na(sf::st_crs(surveillance)) ||
    is.na(sf::st_crs(environment))
  ) {
    
    stop(
      "Both sf objects need a CRS.",
      call. = FALSE
    )
  }
  
  # Transform environmental data if CRS differs
  if (
    sf::st_crs(surveillance) !=
    sf::st_crs(environment)
  ) {
    
    environment <- sf::st_transform(
      environment,
      sf::st_crs(surveillance)
    )
  }
  
  # Find nearest environmental feature
  idx <- sf::st_nearest_feature(
    surveillance,
    environment
  )
  
  # Calculate distances
  d <- sf::st_distance(
    surveillance,
    environment[idx, ],
    by_element = TRUE
  )
  
  # Combine data
  out <- dplyr::bind_cols(
    surveillance,
    sf::st_drop_geometry(
      environment[idx, ]
    )
  )
  
  # Add distance
  out$.distance <- as.numeric(d)
  
  # Apply maximum distance
  out <- out[
    out$.distance <= max_distance,
    ,
    drop = FALSE
  ]
  
  out
}
