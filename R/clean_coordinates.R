#' Remove duplicated points from a dataframe
#'
#' This function removes duplicated points from a dataframe object based on specified coordinate column names.
#'
#' @param x A dataframe object with each row representing one point.
#' @param coords Names of the coordinate columns used for identifying duplicate points.
#'
#' @return A dataframe without duplicated points.
#'
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
#' @param x A dataframe object with rows representing points.
#' @param study_area Polygon object (sf object) to define the region for point removal.
#' @param overlapping Logical indicating whether to remove points within (TRUE) or outside (FALSE) the polygon.
#' @param coords Character vector specifying the column names for longitude and latitude.
#'
#' @return A data.frame containing the filtered points.
#'
#' @export
remove_points_poly <- function(x, study_area, overlapping = FALSE, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if x is a dataframe
  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a data.frame object.")
  }

  # Convert dataframe to sf object and set CRS
  x <- sf::st_as_sf(x, coords = coords)
  x <- sf::st_set_crs(x, sf::st_crs(study_area))

  # Ensure consistent handling of spatial attributes
  sf::st_agr(x) <- "constant"

  # Filter points based on overlapping parameter
  if (overlapping) {
    filtered_points <- x[!sapply(sf::st_intersects(x, study_area), any), ]
  } else {
    filtered_points <- x[sapply(sf::st_intersects(x, study_area), any), ]
  }

  # Convert back to data.frame
  filtered_points <- tryCatch({
    filtered_points <- cbind(sf::st_coordinates(filtered_points)[, "X"],
                             sf::st_coordinates(filtered_points)[, "Y"],
                             as.data.frame(filtered_points))
    colnames(filtered_points)[1:2] <- coords
    filtered_points <- filtered_points[, -ncol(filtered_points)]
    filtered_points
  }, error = function(e){
    filtered_points <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(filtered_points) <- c(coords, "timestamp", "pa")
    filtered_points
  })


  return(filtered_points)
}

#' Remove Close Points
#'
#' Filters out data points that are closer together than a specified distance.
#'
#' @param data A data frame containing the spatial data to be thinned. This data frame must include columns for longitude and latitude.
#' @param sp_thin_dist A numeric value specifying the minimum distance (in kilometers) that must separate the data points.
#' @param coords A character vector of length 2, specifying the column names in `data` that contain the longitude and latitude values, respectively. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#'
#' @return A data frame that contains the subset of the original data, filtered to ensure that no two points are closer together than the specified `sp_thin_dist`.
#'
#' @export
remove_close_points <- function(data, sp_thin_dist, coords = c("decimalLongitude", "decimalLatitude")){
  data$SPEC <- "SPEC"
  thinned_occ <- spThin::thin(
    loc.data = data, long.col = coords[1], lat.col = coords[2], spec.col = "SPEC",
    thin.par = sp_thin_dist, reps = 100,
    locs.thinned.list.return = TRUE, write.files = FALSE,
    write.log.file = FALSE, verbose = FALSE
    )

  max_occ <- max(sapply(thinned_occ, nrow))
  n_occ <- sapply(thinned_occ, nrow)
  max_records <- thinned_occ[[which(n_occ == max_occ)[1]]]

  data <- data[as.numeric(rownames(max_records)),]
  data$SPEC <- NULL

  return(data)
}

#' Clean Coordinates of Presence/Absence Data
#'
#' This function cleans coordinates of presence/absence data by removing NA coordinates, rounding coordinates if specified, removing duplicated points, and removing points outside specified spatial polygon boundaries.
#'
#' @param data  A dataframe object with rows representing points. Coordinates are in WGS84 (EPSG:4326) coordinate system.
#' @param study_area A spatial polygon in WGS84 (EPSG:4326) representing the boundaries within which coordinates should be kept.
#' @param overlapping Logical indicating whether points overlapping the polygon should be kept (TRUE) or removed (FALSE).
#' @param sp_thin_dist Distance in kilometers that you want the occurrences to be separated by.
#' @param coords Character vector specifying the column names for longitude and latitude.
#' @param by_timestamp If TRUE it will be clean coordinates taking into account different time periods defined in the column `timestamp`.
#' @param seed Optional; an integer seed for reproducibility of results.
#'
#' @return A cleaned data frame containing presence/absence data with valid coordinates.
#'
#' @details This function takes a data frame containing presence/absence data with longitude and latitude coordinates, a spatial polygon representing boundaries within which to keep points, and parameters for rounding coordinates and handling duplicated points. It returns a cleaned data frame with valid coordinates within the specified boundaries.
#'
#' @export
clean_coordinates <- function(data, study_area, overlapping = FALSE, sp_thin_dist = NULL, coords = c("decimalLongitude", "decimalLatitude"), by_timestamp = TRUE, seed = NULL) {
  # Assumptions:
  # - Coordinates are in WGS84 (EPSG:4326) coordinate system

  # Remove NA coordinates
  if (by_timestamp){
    data <- data[complete.cases(data[, c(coords, "timestamp")]), ]
  } else {
    data <- data[complete.cases(data[, coords]), ]
  }

  # Remove duplicated points
  if (by_timestamp){
    data <- remove_duplicate_points(data, coords = c(coords, "timestamp"))
  } else {
    data <- remove_duplicate_points(data, coords = coords)
  }

  # Remove closer points
  if (!is.null(sp_thin_dist)){
    set.seed(seed)
    data <- remove_close_points(data, sp_thin_dist, coords)
  }

  # Remove points outside the study area
  if (!is.null(study_area)){
    data <- remove_points_poly(data, study_area = study_area,
                               overlapping = overlapping, coords = coords)
  }

  return(data)
}
