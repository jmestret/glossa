#=========================================================#
# Read and Validate inputs  ----
#=========================================================#

#' Read and validate presences/absences CSV file
#'
#' This function reads and validates a CSV file containing presences and absences data for species occurrences. It checks if the file has the expected columns and formats.
#'
#' @param file_path The file path to the CSV file.
#' @param file_name Optional. The name of the file. If not provided, the base name of the file path is used.
#' @param show_modal Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.
#' @param coords Optional. Character vector of length 2 specifying the names of the columns containing the longitude and latitude coordinates. Default is c("decimalLongitude", "decimalLatitude").
#' @param sep Optional. The field separator character. Default is tab-separated.
#' @param dec Optional. The decimal point character. Default is ".".
#' @return A data frame with the validated data if the file has the expected columns and formats, NULL otherwise.
#' @keywords internal
#' @export
read_presences_absences_csv <- function(file_path, file_name = NULL, show_modal = FALSE, coords = c("decimalLongitude", "decimalLatitude"), sep = "\t", dec = ".") {
  if (is.null(file_name)){
    file_name <- basename(file_path)
  }

  # Load the CSV file
  data <- tryCatch(
    read.csv2(file_path, sep = sep, dec = dec, header = TRUE, stringsAsFactors = FALSE),
    error = function(e) "error"
  )
  if (inherits(data, "character")){
    msg <- paste("Check", file_name, "file format, delimiters or separators.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Check if the data has the expected columns
  if (all(coords %in% colnames(data))) {
    # Check if "pa" column is present
    if (!("pa" %in% colnames(data))) {
      data[, "pa"] <- 1
    }
    data <- data[, c(coords, "pa")]
  } else {
    msg <- paste("The", file_name, "file does not have the correct column names.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Remove NA coordinates
  data <- data[complete.cases(data[, coords]), ]
  if (nrow(data) == 0){
    msg <- paste("No records with valid coordinates in", file_name, ".")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Check column formats
  for (i in coords){
    if (!all(sapply(data[, i], is.numeric))) {
      msg <- paste("Column", i, "is not numeric in", file_name, "file .")
      warning(msg)
      if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
      return(NULL)
    }
  }
  if ("pa" %in% colnames(data)) {
    if (!(all(data[, "pa"] %in% c(0, 1)))){
      msg <- paste("column 'pa' has values other than 0 and 1 in", file_name, "file .")
      warning(msg)
      if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
      return(NULL)
    }

  }

  return(data)
}

#' Read and validate fit layers zip
#'
#' This function validates a zip file containing layers. It checks if the layers have the same number of files, CRS (Coordinate Reference System), and resolution.
#'
#' @param file_path Path to the zip file containing layers to fit the model.
#' @param show_modal Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.
#' @return A raster stack of layers if the layers pass validation criteria, NULL otherwise.
#' @keywords internal
#' @export
read_fit_layers_zip <- function(file_path, show_modal = FALSE) {
  # Extract contents of the zip file
  tmpdir <- tempdir()
  zip_contents <- utils::unzip(file_path, unzip = getOption("unzip"), exdir = tmpdir)

  # Load the first layer of each covariate to check CRS and resolution
  layers <- lapply(zip_contents, function(x) {
    tryCatch(terra::rast(x), error = function(e) NULL)
  })
  names(layers) <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(zip_contents))
  if (any(sapply(layers, is.null))){
    msg <- paste("Error: check format from files.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Check if all layers have the same CRS
  crs_list <- sapply(layers, function(x) {
    terra::crs(x, proj = TRUE)
  })
  if (any(sapply(crs_list, is.na) == TRUE)){
    msg <- paste("There are rasters with undefined coordinate reference system (CRS).")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }
  if (length(unique(crs_list)) != 1) {
    msg <- paste("There are layers with different coordinate reference system (CRS).")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "warning")
  }

  # Project all layers to +proj=longlat +datum=WGS84
  layers <- lapply(layers, function(x){
    terra::project(x, "epsg:4326")
  })

  # Check if all layers have the same resolution
  res_list <- sapply(layers, function(x) {
    paste(terra::res(x), collapse = "")
  })
  if (length(unique(res_list)) != 1) {
    msg <- paste("The layers uploaded have different resolution.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Check if all layers have the same extent
  ext_list <- lapply(layers, function(x){
    as.vector(terra::ext(x))
  })
  if (length(unique(ext_list)) != 1) {
    msg <- paste("There are layers with different extent. We will transform layers to biggest extent.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "warning")

    ext_df <- do.call(rbind, ext_list)
    largest_ext <- terra::ext(c(min(ext_df[, 1]), max(ext_df[, 2]), min(ext_df[, 3]), max(ext_df[, 4]))) # xmin xmax ymin ymax
    layers <- lapply(layers, function(x){
      terra::extend(x, largest_ext)
    })
  }

  layers <- terra::rast(layers)

  # If all checks passed, return layers
  return(layers)
}

#' Validate Projection Layers Zip
#'
#' This function validates a zip file containing projection layers. It checks if the layers have the same number of files, CRS (Coordinate Reference System), and resolution.
#'
#' @param file_path Path to the zip file containing projection layers.
#' @param show_modal Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.
#' @return TRUE if the layers pass validation criteria, FALSE otherwise.
#' @keywords internal
validate_projection_layers_zip <- function(file_path, show_modal = FALSE) {
  # Extract contents of the zip file
  tmpdir <- tempdir()
  zip_contents <- utils::unzip(file_path, unzip = getOption("unzip"), exdir = tmpdir)

  # Get unique covariate directories
  covariates <- unique(dirname(zip_contents))
  # Check if each covariate has the same number of files
  n_files <- sapply(covariates, function(x) length(list.files(x)))
  if (length(unique(n_files)) != 1) {
    msg <- paste("The layers uploaded differ in number between the different covariates.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Load the first layer of each covariate to check CRS and resolution
  layers <- lapply(covariates, function(x) {
    tryCatch(terra::rast(list.files(x, full.names = TRUE)[1]), error = function(e) NULL)
  })
  if (any(sapply(layers, is.null))){
    msg <- paste("Error: check format from files.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # Check if all layers have the same CRS
  crs_list <- sapply(layers, function(x) {
    terra::crs(x, proj = TRUE)
  })
  if (any(sapply(crs_list, is.na) == TRUE)){
    msg <- paste("There are rasters with undefined coordinate reference system (CRS).")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }
  if (length(unique(crs_list)) != 1) {
    msg <- paste("There are layers with different coordinate reference system (CRS).")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
  }

  # Check if all layers have the same resolution
  res_list <- sapply(layers, function(x) {
    paste(terra::res(x), collapse = "")
  })
  if (length(unique(res_list)) != 1) {
    msg <- paste("The layers uploaded have different resolution.")
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  # If all checks passed, return TRUE
  return(file_path)
}

#' Validate Fit and Projection Layers
#'
#' This function validates fit and projection layers by checking their CRS, resolution, and covariates.
#'
#' @param fit_layers_path Path to the ZIP file containing fit layers.
#' @param proj_path Path to the ZIP file containing projection layers.
#' @param show_modal Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.
#' @return TRUE if the layers pass validation criteria, FALSE otherwise.
#'
#' @keywords internal
validate_fit_proj_layers <- function(fit_layers_path, proj_path, show_modal = FALSE) {
  # Extract contents of the zip file
  tmpdir_fit_layers <- tempdir()
  tmpdir_proj <- tempdir()
  fit_layers_content <- utils::unzip(fit_layers_path, unzip = getOption("unzip"), exdir = tmpdir_fit_layers)
  proj_contents <- utils::unzip(proj_path, unzip = getOption("unzip"), exdir = tmpdir_proj)

  # Get unique covariate directories
  fit_layers_covariates <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(fit_layers_content))
  proj_covariates <- basename(unique(dirname(proj_contents)))

  # Check they have same covariates
  if (!all(paste(sort(fit_layers_covariates), collapse = "") == paste(sort(proj_covariates), collapse = ""))){
    msg <- "The projection layers uploaded have different covariates from the fit layers."
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(FALSE)
  }

  # If all checks passed, return TRUE
  return(TRUE)
}

#' Read and Validate Extent Polygon
#'
#' This function validates a polygon file containing the extent. It checks if the file has the correct format.
#'
#' @param file_path Path to the polygon file containing the extent.
#' @param show_modal Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.
#' @return A spatial object representing the extent if the file is valid, NULL otherwise.
#'
#' @keywords internal
read_extent_poly <- function(file_path, show_modal = FALSE){
  extent_poly <- tryCatch(
    sf::st_read(file_path, drivers = c("GPKG", "ESRI Shapefile")),
    error = function(e) "error"
  )
  if (inherits(extent_poly, "character")){
    msg <- "Check file format."
    warning(msg)
    if (show_modal) showNotification(msg, duration = 5, closeButton = TRUE, type = "error")
    return(NULL)
  }

  return(extent_poly)
}


#' Load Covariate Layers from ZIP Files
#'
#' This function loads covariate layers from a ZIP file, verifies their spatial characteristics, and returns them as a list of raster layers.
#'
#' @param file_path Path to the ZIP file containing covariate layers.
#' @return A list containing raster layers for each covariate.
#' @keywords internal
#' @export
read_projections_layers <- function(file_path) {
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

  layers <- lapply(1:terra::nlyr(layers[[1]]), function(i){
    layers_i <- lapply(layers, function(x){
      terra::project(x[[i]], "epsg:4326")
    })
    ext_list <- lapply(layers_i, function(x){
      as.vector(terra::ext(x))
    })
    if (length(unique(ext_list)) != 1) {
      msg <- paste("There are layers with different extent. We will transform layers to biggest extent.")
      warning(msg)

      ext_df <- do.call(rbind, ext_list)
      largest_ext <- terra::ext(c(min(ext_df[, 1]), max(ext_df[, 2]), min(ext_df[, 3]), max(ext_df[, 4]))) # xmin xmax ymin ymax
      layers_i <- lapply(layers_i, function(x){
        terra::extend(x, largest_ext)
      })
    }

    layers_i <- terra::rast(layers_i)
  })

  #unlink(tmpdir, recursive = TRUE)

  return(layers)
}
