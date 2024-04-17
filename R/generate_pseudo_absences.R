#' Generate pseudo-absence points based on presence points, covariates and study area polygon
#'
#' This function generates pseudo-absence points within the study area.
#'
#' @param presences Data frame containing presence points
#' @param sf_poly Spatial polygon defining the study area
#' @param raster_stack  SpatRaster containing covariate data
#' @param coords Character vector specifying the column names for latitude and longitude
#' @param attempts Number of attempts to generate exact pseudo-absences
#'
#' @return Data frame containing both presence and pseudo-absence points
#'
#' @export
generate_pseudo_absences <- function(presences, sf_poly, raster_stack, coords = c("decimalLongitude", "decimalLatitude"), digits = 4, attempts = 50) {

  # Check inputs
  stopifnot(inherits(sf_poly, "sf") || inherits(sf_poly, "sfc"))
  stopifnot(inherits(raster_stack, "SpatRaster"))

  # Initialize variables
  n_presences <- nrow(presences)
  absences <- data.frame(X = numeric(), Y = numeric())
  crs <- sf::st_crs(sf_poly)
  bounding_box <- sf::st_bbox(sf_poly)
  bounding_box_poly <- sf::st_polygon(list(
    matrix(c(bounding_box$xmin, bounding_box$ymin, bounding_box$xmax, bounding_box$ymin,
             bounding_box$xmax, bounding_box$ymax, bounding_box$xmin, bounding_box$ymax,
             bounding_box$xmin, bounding_box$ymin),
           ncol = 2, byrow = TRUE))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(crs)

  sf::sf_use_s2(FALSE)

  # Loop for attempts to generate pseudo-absences
  curr_attempt <- 0
  while (nrow(absences) < n_presences & curr_attempt < attempts) {
    # Sample points from the bounding box
    sf::st_crs(sf_poly) <-crs # Set crs for sampling
    new_abs <- sf::st_sample(bounding_box_poly, size = n_presences - nrow(absences), type = "random", exact = TRUE, oriented=TRUE)

    # Remove points outside the study area polygon
    # Set CRS to NA to avoid sf annoying messages
    sf::st_crs(sf_poly) <- NA
    sf::st_crs(new_abs) <- NA
    absences_inside_pol <- sapply(sf::st_intersects(new_abs, sf_poly), any)
    new_abs <- new_abs[absences_inside_pol]

    # Convert to data frame and round coordinates
    new_abs <- as.data.frame(sf::st_coordinates(new_abs))
    colnames(new_abs) <- coords
    new_abs <- round(new_abs, digits) # Custom round precision

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
  #design_matrix <- terra::extract(raster_stack, absences[, coords])
  #absences <- cbind(absences, design_matrix)

  # Set presence/absence indicator
  presences$pa <- 1
  absences$pa <- 0

  # Combine presences and absences
  return(rbind(presences, absences))
}
