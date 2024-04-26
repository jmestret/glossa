#' Apply Polygon Mask to Raster Layers
#'
#' This function crops and extends raster layers to a sf_poly extent (bbox) defined by longitude
#' and latitude then applies a mask based on a provided spatial polygon to remove areas outside the polygon.
#'
#' @param layers A SpatRaster object or stack of raster layers to be processed.
#' @param sf_poly A spatial polygon (`sf` or `sp` object) used to mask the raster layers.
#'
#' @return A `Raster*` object representing the masked raster layers.
#'
#' @export
layer_mask <- function(layers, sf_poly) {
  # Crop to sf_poly extent
  cropped_layer <- terra::crop(layers, terra::ext(terra::vect(sf_poly)))

  # Extend to sf_poly extent
  extended_layer <- terra::extend(cropped_layer, terra::ext(terra::vect(sf_poly)))

  # Remove polygon values
  processed_layers <- terra::mask(extended_layer, terra::vect(sf_poly))

  return(processed_layers)
}
