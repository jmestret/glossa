#' Remove duplicated points from a dataframe
#'
#' This function removes duplicated points from a dataframe object based on specified coordinate column names.
#'
#' @param x A dataframe object.
#' @param coords Names of the coordinate columns used for identifying duplicate points.
#' @return A dataframe without duplicated points.
#' @export
remove_duplicate_points <- function(x, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if x is a dataframe
  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a data frame object.")
  }

  # Check if coordinate columns are specified
  if (is.null(coords) || !is.character(coords) || length(coords) == 0) {
    stop("Argument 'coords' must be a non-empty character vector specifying coordinate column names.")
  }

  # Ensure specified coordinate columns exist in the dataframe
  missing_coords <- setdiff(coords, colnames(x))
  if (length(missing_coords) > 0) {
    stop(paste("Coordinate column(s) not found in dataframe: ", paste(missing_coords, collapse = ", ")))
  }

  # Remove duplicated points
  unique_data <- x[!duplicated(x[, coords]), ]

  return(unique_data)
}


#' Remove points from a spatial dataframe that fall within or outside a polygon.
#'
#' This function removes points from a spatial dataframe that either fall within or outside the specified polygon,
#' depending on the 'overlapping' parameter.
#'
#' @param x A dataframe object with points.
#' @param sf_poly Polygon object (sf object) to define the region for point removal.
#' @param overlapping Logical indicating whether to remove points within (TRUE) or outside (FALSE) the polygon.
#' @param coords Character vector specifying the column names for longitude and latitude.
#'
#' @return A data.frame containing the filtered points.
#'
#' @export
remove_points_poly <- function(x, sf_poly, overlapping = TRUE, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if x is a dataframe
  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a data.frame object.")
  }

  # Convert dataframe to sf object and set CRS
  x <- sf::st_as_sf(x, coords = coords)
  x <- sf::st_set_crs(x, sf::st_crs(sf_poly))

  # Ensure consistent handling of spatial attributes
  sf::st_agr(x) <- "constant"

  # Filter points based on overlapping parameter
  if (overlapping) {
    filtered_points <- x[!sapply(sf::st_intersects(x, sf_poly), any), ]
  } else {
    filtered_points <- x[sapply(sf::st_intersects(x, sf_poly), any), ]
  }

  # Convert back to data.frame
  filtered_points <- cbind(sf::st_coordinates(filtered_points)[, "X"],
                           sf::st_coordinates(filtered_points)[, "Y"],
                           as.data.frame(filtered_points))
  colnames(filtered_points)[1:2] <- coords
  filtered_points <- filtered_points[, -ncol(filtered_points)]

  return(filtered_points)
}

#' Clean Coordinates of Presence/Absence Data
#'
#' This function cleans coordinates of presence/absence data by removing NA coordinates, rounding coordinates if specified, removing duplicated points, and removing points outside specified spatial polygon boundaries.
#'
#' @param data A data frame containing presence/absence data with columns "decimalLongitude" and "decimalLatitude".
#' @param sf_poly A spatial polygon representing the boundaries within which coordinates should be kept.
#' @param overlapping Logical indicating whether points overlapping the polygon should be kept (TRUE) or removed (FALSE).
#' @param decimal_digits Number of digits to round the coordinates to, if it is not NULL.
#' @param coords Character vector specifying the column names for longitude and latitude.
#'
#' @return A cleaned data frame containing presence/absence data with valid coordinates.
#' @details This function takes a data frame containing presence/absence data with longitude and latitude coordinates, a spatial polygon representing boundaries within which to keep points, and parameters for rounding coordinates and handling duplicated points. It returns a cleaned data frame with valid coordinates within the specified boundaries.
#'
#' @export
clean_coordinates <- function(data, sf_poly, overlapping = TRUE, decimal_digits = NULL, coords = c("decimalLongitude", "decimalLatitude")) {
  # Assumptions:
  # - 'land_mask' is a spatial polygon representing the land boundaries
  # - Coordinates are in WGS84 (EPSG:4326) coordinate system

  # Remove NA coordinates
  data <- data[complete.cases(data[, coords]), ]

  # Round coordinates
  if (!is.null(decimal_digits)) {
    data[, coords[1]] <- round(data[, coords[1]], decimal_digits)
    data[, coords[2]] <- round(data[, coords[2]], decimal_digits)
  }

  # Remove duplicated points
  data <- remove_duplicate_points(data, coords = coords)

  # Remove points outside the ocean boundaries
  data <- remove_points_poly(data,
                             sf_poly = sf_poly,
                             overlapping = overlapping,
                             coords = coords)

  return(data)
}
