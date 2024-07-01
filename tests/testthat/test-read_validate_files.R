# Read input data from user

test_that("Read presence/absence files", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "sp1.txt", package="glossa")
  file2 <- system.file("extdata", "sp2.csv", package="glossa")

  expect_s3_class(glossa::read_presences_absences_csv(file_path = file1), "data.frame")

  expect_s3_class(glossa::read_presences_absences_csv(file_path = file2, file_name = "sp2" ), "data.frame")
})

test_that("Read fit_layers file", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")

  suppressWarnings(
    expect_s4_class(glossa::read_fit_layers_zip(file_path = file1), "SpatRaster")
  )
})

test_that("Read projection layers", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "project_layers_1.zip", package="glossa")

  suppressWarnings(
    expect_type(glossa::read_projections_layers(file_path = file1), "list")
  )
})

test_that("Validate projection layers", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")
  file2 <- system.file("extdata", "project_layers_1.zip", package="glossa")

  expect_type(glossa::validate_projection_layers_zip(file_path = file1), "character")
  expect_equal(glossa::validate_fit_proj_layers(file1, file2), TRUE)
})

test_that("Read extent polygon", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "world.gpkg", package="glossa")

  suppressWarnings(
    expect_s3_class(glossa::read_extent_poly(file_path = file1), "sf")
  )
})
