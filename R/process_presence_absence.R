#' Remove Duplicated Points from a Dataframe
#'
#' This function removes duplicated points from a dataframe based on specified coordinate columns.
#'
#' @param df A dataframe object with each row representing one point.
#' @param coords A character vector specifying the names of the coordinate columns used for identifying duplicate points. Default is c("decimalLongitude", "decimalLatitude").
#'
#' @return A dataframe without duplicated points.
#'
#' @export
remove_duplicate_points <- function(df, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if df is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Check if coordinate columns are specified correctly
  if (!is.character(coords) || length(coords) == 0) {
    stop("Coordinate columns must be specified as a non-empty character vector.")
  }

  # Ensure specified coordinate columns exist in the dataframe
  missing_coords <- setdiff(coords, colnames(df))
  if (length(missing_coords) > 0) {
    stop(paste("Coordinate column(s) not found in dataframe:", paste(missing_coords, collapse = ", ")))
  }

  # Remove duplicated points
  unique_data <- df[!duplicated(df[, coords]), ]

  return(unique_data)
}


#' Remove Points Inside or Outside a Polygon
#'
#' This function removes points from a dataframe based on their location relative to a specified polygon.
#'
#' @param df A dataframe object with rows representing points.
#' @param polygon An sf polygon object defining the region for point removal.
#' @param overlapping Logical indicating whether points overlapping the polygon should be removed (TRUE) or kept (FALSE).
#' @param coords Character vector specifying the column names for longitude and latitude. Default is c("decimalLongitude", "decimalLatitude").
#'
#' @return A dataframe containing the filtered points.
#'
#' @export
remove_points_polygon <- function(df, polygon, overlapping = FALSE, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if df is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Check if coords is a character vector with exactly two elements
  if (!is.character(coords) || length(coords) != 2) {
    stop("Coordinate columns must be specified as a character vector with exactly two elements.")
  }

  # Ensure specified coordinate columns exist in the dataframe
  missing_coords <- setdiff(coords, colnames(df))
  if (length(missing_coords) > 0) {
    stop(paste("Coordinate column(s) not found in dataframe:", paste(missing_coords, collapse = ", ")))
  }

  # Convert dataframe to sf object and set CRS
  points_sf <- sf::st_as_sf(df, coords = coords, crs = sf::st_crs(polygon))

  # Ensure consistent handling of spatial attributes
  sf::st_agr(points_sf) <- "constant"

  # Filter points based on overlapping parameter
  if (overlapping) {
    filtered_points <- points_sf[!sf::st_intersects(points_sf, polygon, sparse = FALSE), ]
  } else {
    filtered_points <- points_sf[sf::st_intersects(points_sf, polygon, sparse = FALSE), ]
  }

  # Convert back to dataframe
  filtered_points_df <- cbind(sf::st_coordinates(filtered_points), as.data.frame(filtered_points))
  colnames(filtered_points_df)[1:2] <- coords
  filtered_points_df <- filtered_points_df[, !colnames(filtered_points_df) %in% c("geometry")]

  return(filtered_points_df)
}


#' Remove Close Points
#'
#' Filters out data points that are closer together than a specified distance.
#'
#' @param df A data frame containing the spatial data to be thinned. This data frame must include columns for longitude and latitude.
#' @param sp_thin_dist A numeric value specifying the minimum distance (in kilometers) that must separate the data points.
#' @param coords A character vector of length 2, specifying the column names in `df` that contain the longitude and latitude values, respectively. Defaults to `c("decimalLongitude", "decimalLatitude")`.
#'
#' @return A data frame that contains the subset of the original data, filtered to ensure that no two points are closer together than the specified `sp_thin_dist`.
#'
#' @export
remove_close_points <- function(df, sp_thin_dist, coords = c("decimalLongitude", "decimalLatitude")) {
  # Check if data is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # Check if sp_thin_dist is a numeric value
  if (!is.numeric(sp_thin_dist) || length(sp_thin_dist) != 1) {
    stop("The thinning distance must be a single numeric value.")
  }

  # Check if coords is a character vector with exactly two elements
  if (!is.character(coords) || length(coords) != 2) {
    stop("Coordinate columns must be specified as a character vector with exactly two elements.")
  }

  # Ensure specified coordinate columns exist in the dataframe
  missing_coords <- setdiff(coords, colnames(df))
  if (length(missing_coords) > 0) {
    stop(paste("Coordinate column(s) not found in dataframe:", paste(missing_coords, collapse = ", ")))
  }

  # Add a temporary species column for the thinning process
  df$SPEC <- "SPEC"

  # Perform spatial thinning
  thinned_occ <- spThin::thin(
    loc.data = df,
    long.col = coords[1],
    lat.col = coords[2],
    spec.col = "SPEC",
    thin.par = sp_thin_dist,
    reps = 100,
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    write.log.file = FALSE,
    verbose = FALSE
  )

  # Determine the maximum number of records after thinning
  max_occ <- max(sapply(thinned_occ, nrow))
  n_occ <- sapply(thinned_occ, nrow)
  max_records <- thinned_occ[[which(n_occ == max_occ)[1]]]

  # Filter the original data to keep only the thinned records
  filtered_data <- df[as.numeric(rownames(max_records)),]
  filtered_data$SPEC <- NULL

  return(filtered_data)
}


#' Clean Coordinates of Presence/Absence Data
#'
#' This function cleans coordinates of presence/absence data by removing NA coordinates, rounding coordinates if specified, removing duplicated points, and removing points outside specified spatial polygon boundaries.
#'
#' @param df A dataframe object with rows representing points. Coordinates are in WGS84 (EPSG:4326) coordinate system.
#' @param study_area A spatial polygon in WGS84 (EPSG:4326) representing the boundaries within which coordinates should be kept.
#' @param overlapping Logical indicating whether points overlapping the polygon should be removed (TRUE) or kept (FALSE).
#' @param sp_thin_dist Distance in kilometers that you want the occurrences to be separated by.
#' @param coords Character vector specifying the column names for longitude and latitude.
#' @param by_timestamp If TRUE, clean coordinates taking into account different time periods defined in the column `timestamp`.
#' @param seed Optional; an integer seed for reproducibility of results.
#'
#' @return A cleaned data frame containing presence/absence data with valid coordinates.
#'
#' @details This function takes a data frame containing presence/absence data with longitude and latitude coordinates, a spatial polygon representing boundaries within which to keep points, and parameters for rounding coordinates and handling duplicated points. It returns a cleaned data frame with valid coordinates within the specified boundaries.
#'
#' @export
clean_coordinates <- function(df, study_area, overlapping = FALSE, sp_thin_dist = NULL, coords = c("decimalLongitude", "decimalLatitude"), by_timestamp = TRUE, seed = NULL) {
  # Remove NA coordinates
  if (by_timestamp) {
    df <- df[complete.cases(df[, c(coords, "timestamp")]), ]
  } else {
    df <- df[complete.cases(df[, coords]), ]
  }

  # Remove duplicated points
  if (by_timestamp) {
    df <- remove_duplicate_points(df, coords = c(coords, "timestamp"))
  } else {
    df <- remove_duplicate_points(df, coords = coords)
  }

  # Remove closer points
  if (!is.null(sp_thin_dist)) {
    if (!is.null(seed)) set.seed(seed)
    df <- remove_close_points(df, sp_thin_dist, coords)
  }

  # Remove points outside the study area
  if (!is.null(study_area)) {
    df <- remove_points_polygon(df, study_area, overlapping, coords)
  }

  return(df)
}
