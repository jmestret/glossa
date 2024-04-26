#' Function to load presence/absence data from CSV files and perform validation
#'
#' @param file_path Path to the CSV file containing presence/absence data.
#' @return A data frame containing the loaded presence/absence data.
#' @details This function loads presence/absence data from a CSV file, performs validation to ensure required columns are present, and adds a default value for the presence/absence column if not present.
#' @export
read_glossa_pa <- function(file_path) {
  # Assumptions:
  # - CSV files contain columns: "decimalLongitude", "decimalLatitude", "year", "species"
  # - Files may or may not have a "pa" column; if not present, a default value of 1 is added for all records
  
  if (!file.exists(file_path)) {
    stop(paste("Error: File", file_path, "not found. Please verify the file path."))
  }
  
  # Load data
  data <- read.csv2(file_path, header = TRUE, sep = "\t", dec = ".")
  
  # Ensure required columns are present
  required_columns <- c("decimalLongitude", "decimalLatitude", "year", "species")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("Missing columns in the CSV file:", paste(missing_columns, collapse = ", "))
  }
  
  # Add "pa" column if not present
  if (!("pa" %in% colnames(data))) {
    data$pa <- 1
  }
  
  return(data[, c(required_columns, "pa")])
}


#' Function to load covariate layers from ZIP files and perform validation
#'
#' @param file_path Path to the ZIP file containing covariate layers.
#' @return A list containing raster layers for each covariate.
#' @details This function loads covariate layers from a ZIP file, verifies their spatial characteristics, and returns them as a list of raster layers.
#' @export
read_glossa_layers <- function(file_path) {
  # Assumptions:
  # - ZIP files contain subdirectories for each covariate, with raster files stored within these directories
  # - All covariates have the same number of files and the same spatial characteristics (e.g., CRS, resolution)
  
  tmpdir <- tempdir()
  
  # Extract contents of the zip file
  zip_contents <- unzip(file_path, unzip = getOption("unzip"), exdir = tmpdir)
  
  # Get unique covariate directories
  covariates <- unique(dirname(zip_contents))
  
  # Verify if each covariate has the same number of files
  n_files <- sapply(covariates, function(x) length(list.files(x)))
  if (length(unique(n_files)) != 1) {
    stop("Error: The layers uploaded differ in number between the different covariates.")
  }
  
  # Load the first layer of each covariate to check CRS and resolution
  layers <- lapply(covariates, function(x) {
    terra::rast(list.files(x, full.names = TRUE)[1])
  })
  
  # Check if all layers have the same CRS
  crs_list <- sapply(layers, function(x) {
    terra::crs(x, proj = TRUE)
  })
  if (length(unique(crs_list)) != 1) {
    stop("Error: The layers uploaded have different CRS. They must be in WGS84.")
  }
  
  # Check if all layers have the same resolution
  res_list <- sapply(layers, function(x) {
    terra::res(x)
  })
  if (length(unique(paste(res_list[,1], res_list[,2]))) != 1) {
    stop("Error: The layers uploaded have different resolution.")
  }
  
  layers <- list()
  for (cov_dir in covariates){
    cov_name <- basename(cov_dir)
    # Load layers
    layers[[cov_name]] <- terra::rast(list.files(cov_dir, full.names = TRUE))
  }
  
  #unlink(tmpdir, recursive = TRUE)
  
  return(layers)
}