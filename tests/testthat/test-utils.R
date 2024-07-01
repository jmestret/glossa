
test_that("Get covariate names from zip", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "project_layers_1.zip", package="glossa")


  expect_type(glossa::get_covariate_names(file1), "character")
})


