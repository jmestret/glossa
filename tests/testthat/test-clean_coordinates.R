# Clean and processing of presence/absence points

test_that("Read presence/absence files", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "sp1.txt", package="glossa")
  file2 <- system.file("extdata", "world.gpkg", package="glossa")
  pa_data <- glossa::read_presences_absences_csv(file_path = file1)
  study_area <- glossa::read_extent_poly(file_path = file2)

  expect_s3_class(glossa::clean_coordinates(pa_data, study_area, decimal_digits = 4), "data.frame")
})
