#=========================================================#
# glossa utils  ----
#=========================================================#

#' Validate presences/absences CSV file
#'
#' This function validates a CSV file containing presences and absences data for species occurrences. It checks if the file has the expected columns and formats.
#'
#' @param file_path The file path to the CSV file.
#' @return TRUE if the file has the expected columns and formats, FALSE otherwise.
#' @details This function validates the format of a CSV file containing presence/absence data. It checks if the file has the expected columns and formats. If the "pa" column is missing, it assumes the presence/absence column and adds it with default values.
#' @keywords internal
validate_presences_absences_csv <- function(file_path) {
  # Define expected columns and formats
  expected_columns <- c("decimalLongitude", "decimalLatitude", "year", "species")
  expected_formats <- c("numeric", "numeric", "numeric", "character")

  # Load the CSV file
  data <- read.csv2(file_path, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = FALSE)

  # Check if the data has the expected columns
  if (all(expected_columns %in% colnames(data))) {
    # Check if "pa" column is present
    if ("pa" %in% colnames(data)) {
      expected_columns <- c(expected_columns, "pa")
    }
    data <- data[, expected_columns]
  } else { # If columns are not named correctly, check if positional order matches
    if (ncol(data) == 4) { # Assuming it doesn't have the "pa" column
      colnames(data) <- expected_columns
    } else if (ncol(data) >= 5) { # Assuming it has the "pa" column
      data <- data[, 1:5]
      colnames(data) <- c(expected_columns, "pa")
    } else {
      warning("The uploaded CSV files do not have the correct format.")
      return(FALSE)
    }
  }

  # Check column formats
  for (i in seq_along(expected_formats)) {
    if (expected_formats[i] == "numeric" && !all(sapply(data[[i]], is.numeric))) {
      warning(paste("Column", i, "is not numeric."))
      return(FALSE)
    } else if (expected_formats[i] == "character" && !all(sapply(data[[i]], is.character))) {
      warning(paste("Column", i, "is not character."))
      return(FALSE)
    }
  }

  if ("pa" %in% colnames(data)) {
    if (!(all(data[, "pa"]) %in% c(0, 1)))
      return(FALSE)
  }

  return(TRUE)
}


#' Validate Layers Zip
#'
#' This function validates a zip file containing layers. It checks if the layers have the same number of files, CRS (Coordinate Reference System), and resolution.
#'
#' @param file_path Path to the zip file containing layers.
#' @return TRUE if the layers pass validation criteria, FALSE otherwise.
#' @details This function expects that each subfolder within the zip file represents a covariate, and each covariate contains one or more raster files. It checks if the layers within each covariate have the same CRS and resolution.
#'
#' @importFrom terra rast crs res
#' @keywords internal
validate_layers_zip <- function(file_path) {
  # Extract contents of the zip file
  tmpdir <- tempdir()
  zip_contents <- unzip(file_path, unzip = getOption("unzip"), exdir = tmpdir)

  # Get unique covariate directories
  covariates <- unique(dirname(zip_contents))
  # Check if each covariate has the same number of files
  n_files <- sapply(covariates, function(x) length(list.files(x)))
  if (length(unique(n_files)) != 1) {
    warning("The layers uploaded differ in number between the different covariates.")
    return(FALSE)
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
    warning("The layers uploaded have different CRS. They must be in WGS84.")
    return(FALSE)
  }

  # Check if all layers have the same resolution
  res_list <- sapply(layers, function(x) {
    paste(terra::res(x), collapse = "")
  })
  if (length(unique(res_list)) != 1) {
    warning("The layers uploaded have different resolution.")
    return(FALSE)
  }

  # If all checks passed, return TRUE
  return(TRUE)
}


#' Validate Historical and Future Layers
#'
#' This function validates historical and future layers by checking their CRS, resolution, and covariates.
#'
#' @param hist_path Path to the ZIP file containing historical layers.
#' @param fut_path Path to the ZIP file containing future layers.
#' @return TRUE if the layers pass validation criteria, FALSE otherwise.
#' @details This function compares historical and future layers to ensure they have the same CRS, resolution, and covariates.
#'
#' @keywords internal
validate_hist_fut_layers <- function(hist_path, fut_path) {
  # Extract contents of the zip file
  tmpdir_hist <- tempdir()
  tmpdir_fut <- tempdir()
  hist_contents <- unzip(hist_path, unzip = getOption("unzip"), exdir = tmpdir_hist)
  fut_contents <- unzip(fut_path, unzip = getOption("unzip"), exdir = tmpdir_fut)

  # Get unique covariate directories
  hist_covariates <- unique(dirname(hist_contents))
  fut_covariates <- unique(dirname(fut_contents))

  # Load one layer to check CRS and resolution
  hist_layer <- terra::rast(list.files(hist_covariates[1], full.names = TRUE)[1])
  fut_layer <- terra::rast(list.files(fut_covariates[1], full.names = TRUE)[1])

  # Check if they have the same CRS
  if (!(terra::crs(hist_layer, proj = TRUE) == terra::crs(fut_layer, proj = TRUE))) {
    warning("The future layers have different CRS from historical layers. They must be in WGS84.")
    return(FALSE)
  }

  # Check if they have the same resolution
  if (!(paste(terra::res(hist_layer), collapse = "") == paste(terra::res(fut_layer), collapse = ""))) {
    warning("The future layers uploaded have different resolution from the historical layers.")
    return(FALSE)
  }

  # Check they have same covariates
  if (!all(paste(sort(basename(hist_covariates)), collapse = "") == paste(sort(basename(fut_covariates)), collapse = ""))){
    warning("The future layers uploaded have different covariates from the historical layers.")
    return(FALSE)
  }

  # If all checks passed, return TRUE
  return(TRUE)
}

get_covariate_names <- function(file_path){
  # Extract contents of the zip file
  tmpdir <- tempdir()
  zip_contents <- unzip(file_path, unzip = getOption("unzip"), exdir = tmpdir)

  # Get unique covariate directories
  covariates <- basename(unique(dirname(zip_contents)))

  return(covariates)
}


#' Extract Covariate Values without NA
#'
#' This function extracts covariate values for species occurrences, excluding NA values.
#'
#' @param data A data frame containing species occurrence data with columns "decimalLongitude" and "decimalLatitude".
#' @param covariate_layers A list of raster layers representing covariates.
#' @return A data frame containing species occurrence data with covariate values, excluding NA values.
#' @details This function extracts covariate values for each species occurrence location from the provided covariate layers. It returns a data frame containing species occurrence data with covariate values, excluding any NA values.
#'
#' @export
extract_noNA_cov_values <- function(data, covariate_layers){
  covariate_values <- terra::extract(
    covariate_layers,
    data[, c("decimalLongitude", "decimalLatitude")]
  )

  covariate_values <- cbind(data, covariate_values) %>%
    tidyr::drop_na()

  return(covariate_values)
}

#' Create Geographic Coordinate Layers
#'
#' Generates raster layers for longitude and latitude from given raster data,
#' applies optional scaling, and restricts the output to a specified spatial mask.
#'
#' @param layers Raster or stack of raster layers to derive geographic extent and resolution.
#' @param sf_poly Spatial object for masking output layers.
#' @param scale_layers Logical indicating if scaling is applied. Default is FALSE.
#'
#' @return Raster stack with layers 'decimalLongitude' and 'decimalLatitude'.
#'
#' @export
create_coords_layer <- function(layers, sf_poly, scale_layers = FALSE){
  # Create layer with longitude and latitude values
  coords_layer <- terra::rast(terra::ext(layers), resolution = terra::res(layers))
  terra::crs(coords_layer) <- terra::crs(layers)
  df_longlat <- terra::crds(coords_layer, df = TRUE)

  # Create longitude raster
  raster_long <- terra::rast(cbind(df_longlat, df_longlat$x))
  raster_long <- terra::extend(raster_long, coords_layer)
  terra::crs(raster_long) <- terra::crs(coords_layer)

  # Create latitude raster
  raster_lat <- terra::rast(cbind(df_longlat, df_longlat$y))
  raster_lat <- terra::extend(raster_lat, coords_layer)
  terra::crs(raster_lat) <- terra::crs(coords_layer)

  # Optionally scale the longitude and latitude rasters
  if (scale_layers) {
    raster_long <- terra::scale(raster_long)
    raster_lat <- terra::scale(raster_lat)
  }

  # Apply a mask to combined layers
  coords_layer <- c(raster_long, raster_lat)
  coords_layer <- terra::mask(coords_layer, terra::vect(sf_poly))
  names(coords_layer) <- c("decimalLongitude", "decimalLatitude")

  return(coords_layer)
}

#' Export Glossa Model Results
#'
#' This function exports various types of Glossa model results, including native range predictions, suitable habitat predictions, model data, variable importance, functional response results, and presence/absence probability cutoffs. It generates raster files for prediction results, CSV files for model data and variable importance, and CSV files for functional response results. Additionally, it creates a CSV file for presence/absence probability cutoffs if provided.
#'
#' @keywords internal
glossa_export <- function(species = NULL, mods = NULL, time = NULL, fields = NULL,
                          model_data = FALSE, fr = FALSE, prob_cut = FALSE,
                          varimp = FALSE, layer_format = "tif",
                          prediction_results = NULL, presence_absence_list = NULL,
                          other_results = NULL, pa_cutoff = NULL) {
  # Initialize an empty vector to store file paths of exported files
  export_files <- c()

  for (sp in species){
    tmp_sp <- file.path(tempdir(), sp)
    if (file.exists(tmp_sp)){
      unlink(tmp_sp, recursive = TRUE)
    }
    dir.create(tmp_sp)

    if ("native_range" %in% mods){
      # Create a temporary directory to store native range files
      tmp_nr <- file.path(tmp_sp, "native_range")
      dir.create(tmp_nr)

      # Iterate over each time period
      for (t in time){
        if (t == "historical"){
          dir.create(file.path(tmp_nr, t))
          # Iterate over each field and export raster files
          for (value in fields) {
            dir.create(file.path(tmp_nr, t, value))
            terra::writeRaster(
              prediction_results[[t]][["native_range"]][[sp]][[value]],
              filename = file.path(file.path(tmp_nr, t, value, paste(gsub(" ", "_", sp), "_native_range_", t, "_", value, ".", layer_format, sep = ""))),
              overwrite = TRUE
            )
          }
        } else if (t == "past"){
          dir.create(file.path(tmp_nr, t))
          # Iterate over each year and each field to export raster files
          for (value in fields) {
            dir.create(file.path(tmp_nr, t, value))
            for (year in seq_along(prediction_results[[t]][["native_range"]][[sp]])){
              terra::writeRaster(
                prediction_results[[t]][["native_range"]][[sp]][[year]][[value]],
                filename = file.path(file.path(tmp_nr, t, value, paste(gsub(" ", "_", sp), "_native_range_", t, "_", year,  "_", value, ".", layer_format, sep = ""))),
                overwrite = TRUE
              )
            }
          }
        } else if (t == "future"){
          dir.create(file.path(tmp_nr, t))
          # Iterate over each scenario, year, and field to export raster files
          for (scenario in names(prediction_results[[t]][["native_range"]][[sp]])){
            dir.create(file.path(tmp_nr, t, scenario))
            for (value in fields) {
              dir.create(file.path(tmp_nr, t, scenario, value))
              for (year in seq_along(prediction_results[[t]][["native_range"]][[sp]][[scenario]])){
                terra::writeRaster(
                  prediction_results[[t]][["native_range"]][[sp]][[scenario]][[year]][[value]],
                  filename = file.path(file.path(tmp_nr, t, scenario, value, paste(gsub(" ", "_", sp), "_native_range_", t, "_", year, "_", value, ".", layer_format, sep = ""))),
                  overwrite = TRUE
                )
              }
            }
          }
        }
      }
    }


    if ("suitable_habitat" %in% mods){
      # Create a temporary directory to store suitable habitat files
      tmp_sh <- file.path(tmp_sp, "suitable_habitat")
      dir.create(tmp_sh)

      for (t in time){
        if (t == "historical"){
          dir.create(file.path(tmp_sh, t))
          # Iterate over each field and export raster files
          for (value in fields) {
            dir.create(file.path(tmp_sh, t, value))
            terra::writeRaster(
              prediction_results[[t]][["suitable_habitat"]][[sp]][[value]],
              filename = file.path(file.path(tmp_sh, t, value, paste(gsub(" ", "_", sp), "_suitable_habitat_", t, "_", value, ".", layer_format, sep = ""))),
              overwrite = TRUE
            )
          }
        } else if (t == "past"){
          dir.create(file.path(tmp_sh, t))
          # Iterate over each year and each field to export raster files
          for (value in fields) {
            dir.create(file.path(tmp_sh, t, value))
            for (year in seq_along(prediction_results[[t]][["suitable_habitat"]][[sp]])){
              terra::writeRaster(
                prediction_results[[t]][["suitable_habitat"]][[sp]][[year]][[value]],
                filename = file.path(file.path(tmp_sh, t, value, paste(gsub(" ", "_", sp), "_suitable_habitat_", t, "_", year,  "_", value, ".", layer_format, sep = ""))),
                overwrite = TRUE
              )
            }
          }
        } else if (t == "future"){
          dir.create(file.path(tmp_sh, t))
          # Iterate over each scenario, year, and field to export raster files
          for (scenario in names(prediction_results[[t]][["suitable_habitat"]][[sp]])){
            dir.create(file.path(tmp_sh, t, scenario))
            for (value in fields) {
              dir.create(file.path(tmp_sh, t, scenario, value))
              for (year in seq_along(prediction_results[[t]][["suitable_habitat"]][[sp]][[scenario]])){
                terra::writeRaster(
                  prediction_results[[t]][["suitable_habitat"]][[sp]][[scenario]][[year]][[value]],
                  filename = file.path(file.path(tmp_sh, t, scenario, value, paste(gsub(" ", "_", sp), "_suitable_habitat_", t, "_", year, "_", value, ".", layer_format, sep = ""))),
                  overwrite = TRUE
                )
              }
            }
          }
        }
      }
    }

    # Export model data if requested
    if (model_data){
      if (!is.null(presence_absence_list[["model_pa"]])){
        tmp_model_data <- file.path(tmp_sp, paste(gsub(" ", "_", sp), "_model_data.csv", sep = ""))
        df <- presence_absence_list[["model_pa"]][[sp]]
        write.table(df, tmp_model_data, quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
      } else {
        warning("Unable to download model data as it has not been computed.")
      }

    }

    # Export variable importance if requested
    if (varimp){
      if (!is.null(other_results[["variable_importance"]])){
        tmp_var_imp <- file.path(tmp_sp, "variable_importance")
        dir.create(tmp_var_imp)
        for (mod in names(other_results[["variable_importance"]])){
          sp_values <- other_results[["variable_importance"]][[mod]][[sp]]
          df <- data.frame(covariates = names(sp_values), variable_importance = sp_values, row.names = NULL)
          write.table(df, file = file.path(tmp_var_imp, paste(gsub(" ", "_", sp), "_variable_importance_", mod, ".csv", sep = "")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
        }
      } else {
        warning("Unable to download variable importance as it has not been computed.")
      }

    }

    # Export functional responses if requested
    if (fr){
      if (!is.null(other_results[["response_curve"]])){
        tmp_fr <- file.path(tmp_sp, "functional_responses")
        dir.create(tmp_fr)
        for (cov in names(other_results[["response_curve"]][[sp]])){
          write.table(other_results[["response_curve"]][[sp]][[cov]], file = file.path(tmp_fr, paste(gsub(" ", "_", sp), "_functional_response_", cov, ".csv", sep = "")), quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
        }
      } else {
        warning("Unable to download functional response results as they have not been computed.")
      }

    }

    # Export presence/absence probability cutoffs if requested
    if (prob_cut){
      if (!all(sapply(pa_cutoff, is.null))){
        tmp_cutoff <- file.path(tmp_sp, paste(gsub(" ", "_", sp), "_presence_probability_cutoff.csv", sep = ""))
        sp_values <- lapply(pa_cutoff, function(x) x[[sp]])
        sp_values <- sp_values[!sapply(sp_values,is.null)]
        df <- data.frame(model = names(sp_values), prob_cutoff = unlist(sp_values), row.names = NULL)
        write.table(df, tmp_cutoff, quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
      } else {
        warning("Unable to download P/A probability cutoff as they have not been computed.")
      }
    }

    # Add the temporary directory to the list of exported files
    export_files <- c(export_files, tmp_sp)
  }

  if (is.null(export_files)) {
    tmp_empty <- file.path(tempdir(), "glossa_empty")
    dir.create(tmp_empty)
    export_files <- tmp_empty
  }

  return(export_files)
}

# cutoff functions----
# Functions obtained from https://github.com/selva86/InformationValue

#' Compute specificity and sensitivity
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
getFprTpr<- function(actuals, predictedScores, threshold=0.5){
  return(list(1-specificity(actuals=actuals, predictedScores=predictedScores, threshold=threshold),
              sensitivity(actuals=actuals, predictedScores=predictedScores, threshold=threshold)))
}

#' Calculate the specificity for a given logit model
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
specificity <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  no_without_and_predicted_to_not_have_event <- sum(actual_dir != 1 & predicted_dir != 1, na.rm=T)
  no_without_event <- sum(actual_dir != 1, na.rm=T)
  return(no_without_and_predicted_to_not_have_event/no_without_event)
}

#' Calculate the sensitivity for a given logit model
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
sensitivity <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  no_with_and_predicted_to_have_event <- sum(actual_dir == 1 & predicted_dir == 1, na.rm=T)
  no_with_event <- sum(actual_dir == 1, na.rm=T)
  return(no_with_and_predicted_to_have_event/no_with_event)
}

#' Calculate Youden's index
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
youdensIndex <- function(actuals, predictedScores, threshold=0.5){
  Sensitivity <- sensitivity(actuals, predictedScores, threshold = threshold)
  Specificity <- specificity(actuals, predictedScores, threshold = threshold)
  return(Sensitivity + Specificity - 1)
}

#' Misclassification Error
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
misClassError <- function(actuals, predictedScores, threshold=0.5){
  predicted_dir <- ifelse(predictedScores < threshold, 0, 1)
  actual_dir <- actuals
  return(round(sum(predicted_dir != actual_dir, na.rm=T)/length(actual_dir), 4))
}

#' Compute the optimal probability cutoff score
#'
#' @details This function was obtained from the InformationValue R package (https://github.com/selva86/InformationValue).
#' @keywords internal
optimalCutoff <- function(actuals, predictedScores, optimiseFor="misclasserror", returnDiagnostics=FALSE){
  # initialise the diagnostics dataframe to study the effect of various cutoff values.
  sequence <- seq(max(predictedScores), min(predictedScores), -0.01)
  sensMat <- data.frame(CUTOFF=sequence, FPR= numeric(length(sequence)),TPR= numeric(length(sequence)), YOUDENSINDEX=numeric(length(sequence)))
  sensMat[, c(2:3)] <- as.data.frame(t(mapply(getFprTpr, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))))
  sensMat$YOUDENSINDEX <- mapply(youdensIndex, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))
  sensMat$SPECIFICITY <- (1 - as.numeric(sensMat$FPR))
  sensMat$MISCLASSERROR <- mapply(misClassError, threshold=sequence, MoreArgs=list(actuals=actuals, predictedScores=predictedScores))

  # Select the cutoff
  if(optimiseFor=="Both"){
    rowIndex <- which(sensMat$YOUDENSINDEX == max(as.numeric(sensMat$YOUDENSINDEX)))[1]  # choose the maximum cutoff
  }else if(optimiseFor=="Ones"){
    rowIndex <- which(sensMat$TPR == max(as.numeric(sensMat$TPR)))[1]  # choose the maximum cutoff
  }else if(optimiseFor=="Zeros"){
    rowIndex <- tail(which(sensMat$SPECIFICITY == max(as.numeric(sensMat$SPECIFICITY))), 1)  # choose the minimum cutoff
  }else if(optimiseFor=="misclasserror"){
    rowIndex <- tail(which(sensMat$MISCLASSERROR == min(as.numeric(sensMat$MISCLASSERROR))), 1)  # choose the minimum cutoff
  }

  # what should the function return
  if(!returnDiagnostics){
    return(sensMat$CUTOFF[rowIndex])
  } else {
    output <- vector(length=6, mode="list")  # initialise diagnostics output
    names(output) <- c("optimalCutoff", "sensitivityTable", "misclassificationError", "TPR", "FPR", "Specificity")  # give names
    output$optimalCutoff <- sensMat$CUTOFF[rowIndex]
    output$sensitivityTable <- sensMat
    output$misclassificationError <- misClassError(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])
    output$TPR <- getFprTpr(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])[[2]]
    output$FPR <- getFprTpr(actuals, predictedScores, threshold=sensMat$CUTOFF[rowIndex])[[1]]
    output$Specificity <- sensMat$SPECIFICITY[rowIndex]
    return(output)
  }
}
# End cutoff functions----

#=========================================================#
# UI utils ----
#=========================================================#

#' Create a Sparkline Value Box
#'
#' This function creates a custom value box with a sparkline plot embedded in it.
#'
#' @param title The title or heading of the value box.
#' @param sparkline_data The data used to generate the sparkline plot.
#' @param description A short description or additional information displayed below the value box.
#' @param type The type of sparkline plot to generate. Default is "line".
#' @param box_color The background color of the value box.
#' @param width The width of the value box. Default is 4.
#' @param elevation The elevation of the value box. Default is 0.
#' @param ... Additional parameters to be passed to the sparkline function.
#'
#' @return Returns a custom value box with the specified parameters.
#'
#' @keywords internal
sparkvalueBox <- function(title, sparkline_data, description, type = "line", box_color = "white", width = 4, elevation = 0, ...) {

  # Generate the sparkline plot
  if (type == "line"){
    # Calculate percentage increase and format description accordingly
    value <- sparkline_data[length(sparkline_data)]
    value <- ifelse(nchar(value) > 6, format(value, scientific = TRUE, digits = 2), value)
    perc_inc <- round(((sparkline_data[length(sparkline_data)] - sparkline_data[1]) / sparkline_data[1]) * 100, 1)
    perc_inc <- ifelse(is.nan(perc_inc), 0, perc_inc)
    description <- paste0(ifelse(perc_inc >= 0, "+", ""), perc_inc, description)
    # Determine icon and colors based on percentage increase
    icon_name <- ifelse(perc_inc >= 0, "arrow-trend-up", "arrow-trend-down")
    icon_color <- ifelse(perc_inc >= 0, "green", "red")
    lineColor <- ifelse(perc_inc >= 0, "#4e8eed", "#E38CC0")
    fillColor <- ifelse(perc_inc >= 0, "#bcd3f5", "#F7D5EB")
    # Generate sparkline plot
    sparkline_plot <- sparkline(sparkline_data, type = "line", width = "100%", height = "50px", lineColor = lineColor, fillColor = fillColor, ...)
  } else if (type == "bar") {
    # Calculate ratio and format description accordingly
    value <- paste0(sparkline_data[1], "/", sparkline_data[2])
    ratio <- round(sparkline_data[1]/sparkline_data[2], 1)
    ratio <- ifelse(is.nan(ratio), 1, ratio)
    description <- paste0(ratio, description)
    # Determine icon color based on ratio
    icon_name <- "scale-balanced"
    if (ratio > 1){
      icon_name <- "scale-unbalanced"
    }
    if (ratio < 1) {
      icon_name <- "scale-unbalanced-flip"
    }
    icon_color <-  "#007bff"
    # No sparkline plot for bar type
    #sparkline_plot <- sparkline(sparkline_data, type = "bar", barWidth = "100%", height = "50px", chartRangeMin = 0, ...)
    sparkline_plot <- NULL
  }


  # CSS styling for the value box
  valueBox_css <- "
    .spark-value-and-sparkline {
      display: flex; /* or use display: grid; */
      align-items: center; /* if using flexbox */
    }

    .spark-box-number,
    .spark-box-sparkline {
      width: 50%; /* Adjust as needed */
      flex-grow: 1; /* Ensure equal width distribution */
    }

    .spark-box-number {
      font-size: 2em; /* Relative font size */
      font-weight: bold; /* Make the value bold */
    }

    /* Adjust the alignment or size of the sparkline_plot as needed */
    .spark-box-sparkline {
      /* Your styles for sparkline_plot */
    }

    .spark-box-description {
      text-align: right; /* Right-align the description */
      padding-right: 1em; /* Adjust as needed */
    }
  "

  # Create the custom value box div
  valueBoxCl <- paste0("small-box bg-", box_color)
  valueBoxCl <- paste0(valueBoxCl, " elevation-", elevation) # Add elevation class
  custom_valueBox <- div(
    class = valueBoxCl,
    div(
      class = "inner",
      div(
        class = "info-box-text",
        title
      ),
      div(
        class = "spark-value-and-sparkline", # Parent div for value and sparkline_plot
        div(
          class = "spark-box-number",
          value
        ),
        div(
          class = "spark-box-sparkline",
          sparkline_plot
        )
      )
    ),
    div(
      class = "spark-box-description",
      icon(icon_name, style = paste0("color:", icon_color, ";")), # Add an icon (you can choose a different one)
      description
    )
  )

  # Return the value box div
  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    tags$style(HTML(valueBox_css)), # Include CSS styling
    custom_valueBox
  )
}

customFileInput <- function(inputId, multiple = FALSE, accept = NULL, width = NULL,
                            buttonLabel = "Browse...", capture = NULL)
{
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, class = "shiny-input-file",
                         name = inputId, type = "file",
                         style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
                         `data-restore` = restoredValue)
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }
  tags$label(class = "btn btn-file",
             style = "background-color: white; border: 1px solid #007bff; padding: 5px 10px; font-size: 12px; border-radius: 5px !important;",
             buttonLabel, inputTag)
}



#' Create Download Action Button
#'
#' This function generates a download action button that triggers the download of a file when clicked.
#'
#' @param outputId The output ID for the button.
#' @param label The label text displayed on the button. Default is "Download".
#' @param icon The icon to be displayed on the button. Default is NULL.
#' @param width The width of the button. Default is NULL.
#' @param status The status of the button. Default is NULL.
#' @param outline Logical indicating whether to use outline style for the button. Default is FALSE.
#' @param ... Additional parameters to be passed to the actionButton function.
#'
#' @return Returns a download action button with the specified parameters.
#'
#' @keywords internal
downloadActionButton <- function(outputId, label = "Download", icon = NULL,
                                 width = NULL, status = NULL, outline = FALSE, ...){
  # Generate the action button using actionButton from the bs4Dash package
  bttn <- bs4Dash::actionButton(
    inputId = paste0(outputId, "_bttn"),
    label = tagList(tags$a(id = outputId,
                           class = "btn shiny-download-link", href = "", target = "_blank",
                           download = NA), label),
    icon = icon,
    width = width,
    status = status,
    outline = outline,
    ...
  )

  # Append onclick attribute to the button to trigger download when clicked
  htmltools::tagAppendAttributes(bttn, onclick = sprintf("getElementById('%s').click()",
                                                         outputId))
}

#=========================================================#
# Plots ----
#=========================================================#
#' Generate Prediction Plot
#'
#' This function generates a plot based on prediction raster layers and presence/absence points.
#'
#' @param prediction_layer Raster prediction layer.
#' @param pa_points Presence/absence points.
#' @param legend_label Label for the legend.
#' @param non_study_area_mask Spatial polygon representing the non study areas.
#'
#' @return Returns a ggplot object representing the world prediction plot.
#'
#' @keywords internal
generate_prediction_plot <- function(prediction_layer, pa_points, legend_label, non_study_area_mask) {
  p <- ggplot2::ggplot()

  # Add prediction layer if available
  if (!is.null(prediction_layer)) {
    if (legend_label == "potential_presences"){
      p <- p +
        tidyterra::geom_spatraster(data = terra::as.factor(prediction_layer)) +
        ggplot2::scale_fill_manual(values  = c("#65c4d8", "#f67d33"), name = legend_label)
    } else {
      lim <- switch(
        legend_label,
        "mean" = c(0, 1),
        "median" = c(0, 1),
        "sd" = NULL,
        "q0.025" = c(0, 1),
        "q0.975" = c(0, 1),
        "diff" = c(0, 1)
      )
      p <- p +
        tidyterra::geom_spatraster(data = prediction_layer) +
        ggplot2::scale_fill_gradientn(colours = c("#9fe5d7", "#65c4d8", "#39a6d5", "#2b8fc7", "#f67d33", "#f44934", "#ca3a43", "#9e0142"),
                                      limits = lim, name = legend_label)
    }
  }

  # Add presence/absence points if available
  if (!is.null(pa_points)) {
    p <- p +
      ggplot2::geom_point(data = pa_points, aes(x = decimalLongitude, y = decimalLatitude, color = as.factor(pa)), alpha = 1) +
      ggplot2::scale_color_manual(values = c("0" = "black","1" = "green"), labels = c("Absences", "Presences"), name = NULL)
  }

  # Add non study area mask
  p <- p +
    geom_sf(data = non_study_area_mask, color = "#353839", fill = "antiquewhite") +
    theme(
      panel.grid.major = element_line(
        color = gray(.5),
        linetype = "dashed",
        linewidth = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      legend.position = "bottom"
    )

  return(p)
}
