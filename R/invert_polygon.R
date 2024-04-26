#' Invert a polygon
#
#' This function inverts a polygon.
#
#' @param sf_poly An sf polygon to be inverted.
#' @param bbox Optionally provide yourself the resulting bounding box.
#' @return An sf polygon representing the inverted area.
#'
#' @export
invert_polygon <- function(sf_poly, bbox = NULL) {
  # Create bbox polygon
  if (is.null(bbox)){
    bbox <- sf::st_bbox(sf_poly)
    bbox_poly <- sf::st_as_sfc(bbox)
  } else {
    bbox <- sf::st_bbox(bbox)
    bbox_poly <- sf::st_as_sfc(bbox)
    sf::st_crs(bbox_poly) <- sf::st_crs(sf_poly)
  }


  # When it's projected sf has issues computing the difference
  # An alternative is to change sf_use_s2() to FALSE
  #crs <- sf::st_crs(sf_poly)
  #sf::st_crs(sf_poly) <- NA
  #sf::st_crs(bbox_poly) <- NA

  # Invert the polygon
  inverted_poly <- sf::st_difference(bbox_poly, sf_poly)
  #sf::st_crs(inverted_poly) <- crs

  return(inverted_poly)
}
