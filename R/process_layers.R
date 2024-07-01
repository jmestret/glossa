#' Apply Polygon Mask to Raster Layers
#'
#' This function crops and extends raster layers to a study area extent (bbox) defined by longitude
#' and latitude then applies a mask based on a provided spatial polygon to remove areas outside the polygon.
#'
#' @param layers A  stack of raster layers (`SpatRaster` object) to be processed.
#' @param study_area A spatial polygon (`sf` object) used to mask the raster layers.
#'
#' @return A `SpatRaster` object representing the masked raster layers.
#'
#' @export
layer_mask <- function(layers, study_area) {
  # Crop to study_area extent
  cropped_layer <- terra::crop(layers, terra::ext(terra::vect(study_area)))

  # Extend to study_area extent
  extended_layer <- terra::extend(cropped_layer, terra::ext(terra::vect(study_area)))

  # Remove polygon values
  processed_layers <- terra::mask(extended_layer, terra::vect(study_area))

  return(processed_layers)
}
