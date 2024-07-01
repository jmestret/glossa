# Process environmental layers

test_that("Mask envrionmental layers with study area", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")
  file2 <- system.file("extdata", "world.gpkg", package="glossa")
  layers <- suppressWarnings(glossa::read_fit_layers_zip(file_path = file1))
  study_area <- glossa::read_extent_poly(file_path = file2)

  expect_s4_class(glossa::layer_mask(layers, study_area), "SpatRaster")
})
