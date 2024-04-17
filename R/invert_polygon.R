#' Invert a polygon with respect to the world.
#
#' This function inverts a polygon with respect to the entire world.
#
#' @param sf_poly An sf polygon to be inverted.
#' @return An sf polygon representing the inverted area.
#'
#' @export
invert_polygon <- function(sf_poly) {
  # Create a world polygon
  world_poly <- sf::st_polygon(
    list(matrix(c(-180, -90, -180, 90, 180, 90, 180, -90, -180, -90),
                ncol = 2, byrow = TRUE))) %>%
    sf::st_sfc()

  # When it's projected sf has issues computing the difference
  # An alternative is to change sf_use_s2() to FALSE
  crs <- sf::st_crs(sf_poly)
  sf::st_crs(sf_poly) <- NA

  # Invert the polygon with respect to the world
  inverted_poly <- sf::st_difference(world_poly, sf_poly)
  sf::st_crs(inverted_poly) <- crs

  return(inverted_poly)
}
