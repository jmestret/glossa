# Invert polygon

test_that("Invert polygon", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "world.gpkg", package="glossa")
  study_area <- glossa::read_extent_poly(file_path = file1)

  expect_s3_class(glossa::invert_polygon(study_area), "sfc")

  expect_s3_class(glossa::invert_polygon(study_area, bbox = c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)), "sfc")
})
