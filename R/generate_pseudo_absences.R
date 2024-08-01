#' Generate pseudo-absence points based on presence points, covariates, and study area polygon
#'
#' This function generates pseudo-absence points within the study area.
#'
#' @param presences Data frame containing presence points.
#' @param study_area Spatial polygon defining the study area ('sf').
#' @param raster_stack SpatRaster containing covariate data.
#' @param predictor_variables Name of the predictor variables selected for this species.
#' @param coords Character vector specifying the column names for latitude and longitude.
#' @param sp_thin_dist Distance in kilometers that you want the occurrences to be separated by.
#' @param attempts Number of attempts to generate exact pseudo-absences.
#'
#' @return Data frame containing both presence and pseudo-absence points.
#'
#' @export
generate_pseudo_absences <- function(presences, study_area, raster_stack, predictor_variables, coords = c("decimalLongitude", "decimalLatitude"), sp_thin_dist = NULL, attempts = 100) {

  # Check inputs
  if (!is.null(study_area)) {
    stopifnot(inherits(study_area, "sf") || inherits(study_area, "sfc"))
  }
  stopifnot(inherits(raster_stack[[1]], "SpatRaster"))

  # Initialize variables
  n_presences <- nrow(presences)
  timestamp_values <- presences$timestamp
  col_names <- c(coords, "timestamp", predictor_variables)
  absences <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(absences) <- col_names
  if(is.null(study_area)){
    crs <- terra::crs(raster_stack[[1]])
    bounding_box <- sf::st_bbox(as.vector(terra::ext(raster_stack[[1]]))[c("xmin", "ymin", "xmax", "ymax")])
  } else {
    crs <- sf::st_crs(study_area)
    bounding_box <- sf::st_bbox(study_area)
  }
  bbox_hull_poly <- sf::st_as_sfc(bounding_box) %>%
    sf::st_set_crs(crs)

  sf::sf_use_s2(FALSE)

  # Loop for attempts to generate pseudo-absences
  curr_attempt <- 0
  while (nrow(absences) < n_presences & curr_attempt < attempts) {
    # Sample points from the bounding box
    new_abs <- sf::st_sample(bbox_hull_poly, size = n_presences - nrow(absences), type = "random", exact = TRUE, oriented=TRUE)

    # Remove points outside the study area polygon
    if (!is.null(study_area)){
      # Set CRS to NA to avoid sf annoying messages
      sf::st_crs(study_area) <- NA
      sf::st_crs(new_abs) <- NA
      absences_inside_pol <- sapply(sf::st_intersects(new_abs, study_area), any)
      new_abs <- new_abs[absences_inside_pol]
    }

    # Convert to data frame and round coordinates
    new_abs <- as.data.frame(sf::st_coordinates(new_abs))
    colnames(new_abs) <- coords
    if (!is.null(sp_thin_dist) & nrow(new_abs) > 0){
      new_abs <- remove_close_points(new_abs, sp_thin_dist, coords) # remove close points
    }

    # Sample timestamp values
    timestamp_sampled <- sample(timestamp_values, nrow(new_abs))
    new_abs$timestamp <- timestamp_sampled

    # Extract covariate values and remove points with missing covariate values
    if (nrow(new_abs) > 0){
      new_abs <- extract_noNA_cov_values(new_abs, raster_stack, predictor_variables)
    }

    if (nrow(new_abs) > 0){
      timestamp_keeped <- new_abs$timestamp

      # Update remaining timestamp values
      timestamp_values <- timestamp_values[-match(timestamp_keeped, timestamp_values)]
    }

    # Bind with already sampled absences
    absences <- rbind(absences, new_abs)

    # Remove duplicate points
    absences <- remove_duplicate_points(absences, coords = c(coords, "timestamp"))

    # Remove points already present in the occurrence data
    absences <- dplyr::anti_join(absences, presences, by = c(coords, "timestamp"))

    # Increment attempt counter
    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      warning("Could not generate balanced pseudo-absences. Try increasing the number of attempts.")
    }
  }

  # Add species information to absences
  # absences$species <- presences$species

  # Set presence/absence indicator
  presences$pa <- 1
  absences$pa <- 0

  # Combine presences and absences
  return(rbind(presences, absences))
}
