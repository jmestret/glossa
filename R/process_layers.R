#' Apply Global Mask to Raster Layers
#'
#' This function crops and extends raster layers to a global extent defined by longitude
#' from -180 to 180 and latitude from -90 to 90, then applies a mask based on a provided
#' spatial polygon to remove areas outside the polygon.
#'
#' @param layers A SpatRaster object or stack of raster layers to be processed.
#' @param sf_poly A spatial polygon (`sf` or `sp` object) used to mask the raster layers.
#'
#' @return A `Raster*` object representing the masked raster layers.
#'
#' @export
global_mask <- function(layers, sf_poly) {
  # Crop to globe extent
  cropped_layer <- terra::crop(layers, terra::ext(-180, 180, -90, 90))

  # Extend to globe extent
  extended_layer <- terra::extend(cropped_layer, terra::ext(-180, 180, -90, 90))

  # Remove polygon values
  processed_layers <- terra::mask(extended_layer, terra::vect(sf_poly))

  return(processed_layers)
}
