#' Generate pseudo-absence points based on presence points, covariates, and study area polygon
#'
#' This function generates pseudo-absence points within the study area.
#'
#' @param presences Data frame containing presence points.
#' @param study_area Spatial polygon defining the study area.
#' @param raster_stack SpatRaster containing covariate data.
#' @param coords Character vector specifying the column names for latitude and longitude.
#' @param digits Number of decimal digits to round for the coordinates.
#' @param attempts Number of attempts to generate exact pseudo-absences.
#'
#' @return Data frame containing both presence and pseudo-absence points.
#'
#' @export
generate_pseudo_absences <- function(presences, study_area, raster_stack, coords = c("decimalLongitude", "decimalLatitude"), digits = NULL, attempts = 50) {

  # Check inputs
  if (!is.null(study_area)) {
    stopifnot(inherits(study_area, "sf") || inherits(study_area, "sfc"))
  }
  stopifnot(inherits(raster_stack, "SpatRaster"))

  # Initialize variables
  n_presences <- nrow(presences)
  absences <- data.frame(X = numeric(), Y = numeric())
  colnames(absences) <- coords
  if(is.null(study_area)){
    crs <- terra::crs(raster_stack)
    bounding_box <- sf::st_bbox(as.vector(terra::ext(raster_stack))[c("xmin", "ymin", "xmax", "ymax")])
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
    if (!is.null(digits)){
      new_abs <- round(new_abs, digits) # Custom round precision
    }

    # Remove points with missing covariate values
    design_matrix <- terra::extract(raster_stack, new_abs)
    new_abs <- new_abs[complete.cases(design_matrix), ]

    # Bind with already sampled absences
    absences <- rbind(absences, new_abs)

    # Remove duplicate points
    absences <- remove_duplicate_points(absences, coords = coords)

    # Remove points already present in the occurrence data
    absences <- dplyr::anti_join(absences, presences, by = coords)

    # Increment attempt counter
    curr_attempt <- curr_attempt + 1

    # Check for maximum attempts
    if (curr_attempt >= attempts) {
      stop("Could not generate pseudo-absences. Try increasing the number of attempts.")
    }
  }

  # Add year to absences
  if ("year" %in% colnames(presences)){
    absences$year <- presences$year
  }

  # Add species information to absences
  absences$species <- presences$species

  # Extract covariate values for absences
  #absences <- extract_noNA_cov_values(absences, raster_stack) %>%
  #  dplyr::select(!ID)

  # Set presence/absence indicator
  presences$pa <- 1
  absences$pa <- 0

  # Combine presences and absences
  return(rbind(presences, absences))
}
