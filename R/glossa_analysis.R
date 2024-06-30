#' Main Analysis Function for Glossa Package
#'
#' This function wraps all the analysis that the Glossa package performs. It processes presence-absence data,
#' environmental covariates, and performs species distribution modeling and projections under past and future scenarios.
#'
#' @param pa_data A list of data frames containing presence-absence data.
#' @param fit_layers A SpatRaster stack containing model fitting environmental layers.
#' @param proj_files A list of file paths containing environmental layers for projection scenarios.
#' @param study_area_poly A spatial polygon defining the study area.
#' @param predictor_variables A list of predictor variables to be used in the analysis.
#' @param decimal_digits Integer; number of digits to round coordinates to if not NULL.
#' @param scale_layers Logical; if TRUE, covariate layers will be scaled based on fit layers.
#' @param native_range A vector of scenarios ('fit_layers', 'projections') where native range modeling should be performed.
#' @param suitable_habitat A vector of scenarios ('fit_layers', 'projections') where habitat suitability modeling should be performed.
#' @param other_analysis A vector of additional analyses to perform (e.g., 'variable_importance', 'response_curve', 'cross_validation').
#' @param seed Optional; an integer seed for reproducibility of results.
#' @param waiter Optional; a waiter instance to update progress in a Shiny application.
#'
#' @return A list containing structured outputs from each major section of the analysis, including model data, projections,
#' variable importance scores, and habitat suitability assessments.
#'
#' @export
glossa_analysis <- function(
    pa_data = NULL, fit_layers = NULL, proj_files = NULL,
    study_area_poly = NULL, predictor_variables = NULL, decimal_digits = NULL, scale_layers = FALSE,
    native_range = NULL, suitable_habitat = NULL, other_analysis = NULL,
    seed = NA, waiter = NULL) {

  start_time <- Sys.time()
  print(paste("Start time:", start_time))

  #=========================================================#
  # 0. Check inputs and load necessary data ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Initializing objects")))}
  print("Initializing objects...")

  # Set seed
  if (is.na(seed)){
    seed <- NULL
  }

  # Check format of study area mask
  sf::sf_use_s2(FALSE)
  if (!is.null(study_area_poly)){
    stopifnot(inherits(study_area_poly, "sf") || inherits(study_area_poly, "sfc"))
    non_study_area_poly <- invert_polygon(study_area_poly)
  }

  # Initialize empty output
  presence_absence_list <- list(raw_pa = NULL, clean_pa = NULL, model_pa = NULL)
  covariate_list <- list(fit_layers = NULL, projections = NULL)
  projections_results <- list(fit_layers = NULL, projections = NULL)
  other_results <- list(variable_importance = NULL, response_curve = NULL, cross_validation = NULL)
  pa_cutoff <- list(native_range = NULL, suitable_habitat = NULL)
  habitat_suitability <- list(fit_layers = NULL, projections = NULL)
  design_matrix <- list()

  #=========================================================#
  # 1. Load presence(/absence) data and environmental layers ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Loading input data")))}
  print("Loading input data...")

  # * Load presence(/absence) data ----
  presence_absence_list$raw_pa <- pa_data
  sp_names <- names(presence_absence_list$raw_pa)
  long_lat_cols <- colnames(presence_absence_list$raw_pa[[1]])[c(1,2)]

  # * Load covariate layers ----
  # Fit layers
  covariate_list$fit_layers <- fit_layers
  cov_names <- names(covariate_list$fit_layers)

  # projections layers
  if ("projections" %in% native_range | "projections" %in% suitable_habitat){
    if (length(proj_files) <= 0){
      stop("Error: No projections layers provided.")
    }
    covariate_list$projections <- lapply(proj_files, read_glossa_projections_layers)
    pred_scenario <- names(covariate_list$projections)


    # Check for same layers for fitting and projections
    same_fit_pred_layers <- all(sapply(covariate_list$projections, function(x){
      all(names(x) == names(covariate_list$fit_layers))
    }))
    if (!same_fit_pred_layers){
      stop("Error: projection layers differ in the covariate names from fit layers.")
    }
  }

  # * Select predictor variables ----
  if (is.null(predictor_variables)){
    predictor_variables <- lapply(seq_along(presence_absence_list$raw_pa), function(x) names(covariate_list$fit_layers))
  }
  names(predictor_variables) <- sp_names

  load_data_time <- Sys.time()
  print(paste("Load data execution time:", difftime(load_data_time, start_time, units = "secs"), "secs"))

  #=========================================================#
  # 2. Clean coordinates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Processing P/A coordinates")))}
  print("Processing P/A coordinates...")

  presence_absence_list$clean_pa <- lapply(presence_absence_list$raw_pa, function(x){
    clean_coordinates(
      data = x,
      study_area = study_area_poly,
      overlapping = FALSE,
      decimal_digits = decimal_digits,
      coords = long_lat_cols
    )
  })

  clean_coords_time <- Sys.time()
  print(paste("Clean coordinates execution time:", difftime(clean_coords_time, load_data_time, units = "secs"), "secs"))

  #=========================================================#
  # 3. Covariate layer processing ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Processing covariate layers")))}
  print("Processing covariate layers...")

  # * Process environmental layers for model fitting ----
  if (!is.null(study_area_poly)){
    covariate_list$fit_layers <- layer_mask(layers = covariate_list$fit_layers, study_area = study_area_poly)
  }

  if (scale_layers) {
    # If need to compute functional responses save not scaled layers
    if ("functional_responses" %in% other_analysis){
      no_scaled_layers <- covariate_list$fit_layers
    }
    # Compute mean and sd of the fit_layers model fitting layers for each environmental variable
    fit_layers_mean <- terra::global(covariate_list$fit_layers, "mean", na.rm = TRUE)
    fit_layers_sd <- terra::global(covariate_list$fit_layers, "sd", na.rm = TRUE)

    # Scale fit layers with fit_layers mean and sd
    covariate_list$fit_layers <- lapply(1:terra::nlyr(covariate_list$fit_layers), function(x, i){
      terra::scale(
        x[[i]],
        center = fit_layers_mean[i,],
        scale = fit_layers_sd[i, ]
      )
    }, x = covariate_list$fit_layers)
    covariate_list$fit_layers <- terra::rast(covariate_list$fit_layers)
  }

  # * Process projections environmental layers ----
  if ("projections" %in% native_range | "projections" %in% suitable_habitat) {
    # Mask polygon if provided
    if (!is.null(study_area_poly)){
      covariate_list$projections <- lapply(covariate_list$projections, function(scenario){
        scenario <- lapply(scenario, function(x){
          layer_mask(layers = x, study_area = study_area_poly)
        })
      })
    }

    # Scale layers with fit_layers mean and sd
    if (scale_layers){
      covariate_list$projections <- lapply(covariate_list$projections, function(scenario){
        scenario <- lapply(scenario, function(year){
          year <- lapply(1:terra::nlyr(year), function(x, i){
            terra::scale(
              x[[i]],
              center = fit_layers_mean[i,],
              scale = fit_layers_sd[i, ]
            )
          }, x = covariate_list$fit_layers)
          year <- terra::rast(year)
          return(year)
        })
      })
    }
  }

  process_layers_time <- Sys.time()
  print(paste("Layer processing execution time:", difftime(process_layers_time, clean_coords_time, units = "secs"), "secs"))

  layers <- covariate_list$fit_layers # layer to fit models


  #=========================================================#
  # 4. Remove presences/absences with NA values in covariates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Building model matrix")))}
  print("Building model matrix...")

  # Remove points with NA values in any environmental variable
  presence_absence_list$model_pa <- lapply(seq_along(presence_absence_list$clean_pa), function(i){
    x <- presence_absence_list$clean_pa[[i]][, c(long_lat_cols, "pa")]
    fit_points <- extract_noNA_cov_values(x, layers[[predictor_variables[[i]]]]) %>%
      dplyr::select(colnames(x))
    return(fit_points)
  })
  names(presence_absence_list$model_pa) <- names(presence_absence_list$clean_pa)

  #=========================================================#
  # 5. Randomly generate balanced pseudoabsences ----
  #=========================================================#

  # If only occurrences were provided generate balanced random pseudoabsences
  set.seed(seed)
  presence_absence_list$model_pa <- lapply(seq_along(presence_absence_list$model_pa), function(i) {
    x <- presence_absence_list$model_pa[[i]]
    if (all(x[, "pa"] == 1)){
      x <- generate_pseudo_absences(x, study_area_poly, layers[[predictor_variables[[i]]]], coords = long_lat_cols, digits = decimal_digits)
    }
    return(x)
  })
  names(presence_absence_list$model_pa) <- names(presence_absence_list$clean_pa)

  # * Create design matrix ----
  design_matrix <- lapply(seq_along(presence_absence_list$model_pa), function (i){
    terra::extract(
      layers[[predictor_variables[[i]]]],
      presence_absence_list$model_pa[[i]][, long_lat_cols]
    ) %>%
      dplyr::select(!"ID")
  })
  names(design_matrix) <- names(presence_absence_list$model_pa)

  model_matrix_time <- Sys.time()
  print(paste("Build model matrix execution time:", difftime(model_matrix_time, process_layers_time, units = "secs"), "secs"))

  #=========================================================#
  # 6. Native range  ----
  #=========================================================#

  # If Native Ranges include longitude and latitude for native ranges modeling
  if (!is.null(native_range)){
    start_nr_time <- Sys.time()

    # Create layer with longitude and latitude values
    coords_layer <- create_coords_layer(layers, study_area_poly, scale_layers = scale_layers)
    names(coords_layer) <- long_lat_cols

    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Fitting native range models")))}
    print("Fitting native range models...")

    models_native_range <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        presence_absence_list$model_pa[[i]],
        layers = c(layers[[predictor_variables[[i]]]], coords_layer),
        seed = seed
      )
    })
    names(models_native_range) <- names(presence_absence_list$model_pa)

    fit_nr_time <- Sys.time()
    print(paste("Fit native range model execution time:", difftime(fit_nr_time, start_nr_time, units = "mins"), "mins"))


    # * Variable importance ----
    other_results$variable_importance$native_range <- lapply(models_native_range, variable_importance)

    var_imp_nr_time <- Sys.time()
    print(paste("Variable importance execution time:", difftime(var_imp_nr_time, fit_nr_time, units = "mins"), "mins"))


    # * Optimal cutoff ----
    pa_cutoff$native_range <- lapply(names(models_native_range), function(sp) {
      pa_optimal_cutoff(
        presence_absence_list$model_pa[[sp]][, c(long_lat_cols, "pa")],
        c(layers[[predictor_variables[[sp]]]], coords_layer),
        models_native_range[[sp]]
      )
    })
    names(pa_cutoff$native_range) <- names(models_native_range)

    pa_cutoff_nr_time <- Sys.time()
    print(paste("P/A cutoff execution time:", difftime(pa_cutoff_nr_time, var_imp_nr_time, units = "mins"), "mins"))


    # * fit_layers projections ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting native range for fit layer")))}
    projections_results$fit_layers$native_range <- lapply(names(models_native_range), function(sp) {
      predict_bart(
        models_native_range[[sp]],
        c(layers[[predictor_variables[[sp]]]], coords_layer),
        pa_cutoff$native_range[[sp]]
      )
    })
    names(projections_results$fit_layers$native_range) <- names(models_native_range)


    hist_nr_time <- Sys.time()
    print(paste("projections on fit layers execution time:", difftime(hist_nr_time, pa_cutoff_nr_time, units = "mins"), "mins"))

    # * Spatial projections to new scenarios/times ----
    if ("projections" %in% native_range){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting other secenarios native range")))}
      projections_results$projections$native_range <- lapply(names(models_native_range), function(sp) {
        projections <- lapply(covariate_list$projections, function(scenario){
          projections_scenario <- lapply(scenario, function(pred_layers){
            #  Covariates by year
            pred_layers <- c(pred_layers[[predictor_variables[[sp]]]], coords_layer)

            predict_bart(models_native_range[[sp]], pred_layers, pa_cutoff$native_range[[sp]])
          })
          return(projections_scenario)
        })
      })
      names(projections_results$projections$native_range) <- names(models_native_range)
    }

    pred_nr_time <- Sys.time()
    print(paste("Native range projections execution time:", difftime(pred_nr_time, hist_nr_time, units = "mins"), "mins"))

    # Free memory by removing fitted models
    rm(models_native_range)

    end_nr_time <- Sys.time()
    print(paste("Native range execution time:", difftime(end_nr_time, start_nr_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 7. Suitable habitat  ----
  #=========================================================#

  if (!is.null(suitable_habitat)){
    start_sh_time <- Sys.time()

    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Fitting suitable habitat models")))}
    print("Fitting suitable habitat models...")

    models_suitable_habitat <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        presence_absence_list$model_pa[[i]],
        layers = layers[[predictor_variables[[i]]]],
        seed = seed
      )
    })
    names(models_suitable_habitat) <- names(presence_absence_list$model_pa)

    fit_sh_time <- Sys.time()
    print(paste("Fit suitable habitat model execution time:", difftime(fit_sh_time, start_sh_time, units = "mins"), "mins"))

    # * Variable importance ----
    other_results$variable_importance$suitable_habitat <- lapply(models_suitable_habitat, variable_importance)

    var_imp_sh_time <- Sys.time()
    print(paste("Variable importance execution time:", difftime(var_imp_sh_time, fit_sh_time, units = "mins"), "mins"))

    # * Optimal cutoff ----
    pa_cutoff$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
      pa_optimal_cutoff(
        presence_absence_list$model_pa[[sp]],
        layers[[predictor_variables[[sp]]]],
        models_suitable_habitat[[sp]]
      )
    })
    names(pa_cutoff$suitable_habitat) <- names(models_suitable_habitat)

    pa_cutoff_sh_time <- Sys.time()
    print(paste("P/A cutoff execution time:", difftime(pa_cutoff_sh_time, var_imp_sh_time, units = "mins"), "mins"))

    # * fit_layers projections ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting suitable habitat for fit layers")))}
    projections_results$fit_layers$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
      predict_bart(
        models_suitable_habitat[[sp]],
        layers[[predictor_variables[[sp]]]],
        pa_cutoff$suitable_habitat[[sp]]
      )
    })
    names(projections_results$fit_layers$suitable_habitat) <- names(models_suitable_habitat)

    hist_sh_time <- Sys.time()
    print(paste("projections on fit layers execution time:", difftime(hist_sh_time, pa_cutoff_sh_time, units = "mins"), "mins"))

    # * Spatial projections to new scenarios/times ----
    if ("projections" %in% suitable_habitat){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Predicting other scenarios suitable habitat")))}
      projections_results$projections$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        projections <- lapply(covariate_list$projections, function(scenario){
          projections_scenario <- lapply(scenario, function(pred_layers){
            pred_layers <- pred_layers[[predictor_variables[[sp]]]]
            predict_bart(models_suitable_habitat[[sp]], pred_layers, pa_cutoff$suitable_habitat[[sp]])
          })
          return(projections_scenario)
        })
      })
      names(projections_results$projections$suitable_habitat) <- names(models_suitable_habitat)
    }

    pred_sh_time <- Sys.time()
    print(paste("Suitable habitat projections execution time:", difftime(pred_sh_time, hist_sh_time, units = "mins"), "mins"))

    # * Habitat suitability change ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Computing habitat suitability change")))}
    # Covered area
    habitat_suitability$fit_layers$covered_area <- lapply(names(models_suitable_habitat), function(sp){
      layer <- projections_results$fit_layers$suitable_habitat[[sp]]["mean"]
      area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
      area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
      return(area)
    })
    names(habitat_suitability$fit_layers$covered_area) <- names(models_suitable_habitat)

    # Mean suitability probability
    habitat_suitability$fit_layers$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
      layer <- projections_results$fit_layers$suitable_habitat[[sp]]
      prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
      return(as.numeric(prob))
    })
    names(habitat_suitability$fit_layers$suit_prob) <- names(models_suitable_habitat)

    if ("projections" %in% suitable_habitat) {
      # Covered area
      habitat_suitability$projections$covered_area <- lapply(names(models_suitable_habitat), function(sp){
        covered_area_scenarios <- lapply(projections_results$projections$suitable_habitat[[sp]], function(scenario) {
          covered_area <- sapply(scenario, function(layer) {
            layer <- layer["mean"]
            area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
            area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
            return(area)
          })
        })
      })
      names(habitat_suitability$projections$covered_area) <- names(models_suitable_habitat)

      # Mean suitability probability
      habitat_suitability$projections$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
        suit_prob_scenarios <- lapply(projections_results$projections$suitable_habitat[[sp]], function(scenario) {
          suit_prob <- sapply(scenario, function(layer) {
            prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
            return(as.numeric(prob))
          })
        })
      })
      names(habitat_suitability$projections$suit_prob) <- names(models_suitable_habitat)
    }

    hab_sh_time <- Sys.time()
    print(paste("Habitat suitability execution time:", difftime(hab_sh_time, pred_sh_time, units = "mins"), "mins"))

    # Free memory by removing fitted models
    if (scale_layers | !"functional_responses" %in% other_analysis){
      rm(models_suitable_habitat)
    }

    end_sh_time <- Sys.time()
    print(paste("Suitable habitat execution time:", difftime(end_sh_time, start_sh_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 8. Functional responses ----
  #=========================================================#

  if ("functional_responses" %in% other_analysis){
    start_fr_time <- Sys.time()
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Computing functional responses")))}
    print("Computing functional responses...")

    if (scale_layers){
      fr_layers <- no_scaled_layers
    } else {
      fr_layers <- layers
    }

    other_results$response_curve <- lapply(names(presence_absence_list$model_pa), function(i){
      x <- presence_absence_list$model_pa[[i]][, c(long_lat_cols, "pa")]
      pred_layers <- fr_layers[[predictor_variables[[i]]]]
      fit_data <- extract_noNA_cov_values(x, pred_layers)
      fit_data <- fit_data %>%
        dplyr::select("pa", !names(x))

      if (scale_layers | is.null(suitable_habitat)) {
        set.seed(seed)
        bart_model <- dbarts::bart(x.train = fit_data[,names(pred_layers), drop = FALSE],
                                   y.train = fit_data[,"pa"],
                                   keeptrees = TRUE)

        invisible(bart_model$fit$state)
      } else {
        bart_model <- models_suitable_habitat[[i]]
      }

      fr <- response_curve_bart(bart_model = bart_model,
                                data = fit_data,
                                predictor_names = names(pred_layers))
      names(fr) <- names(pred_layers)
      return(fr)
    })
    names(other_results$response_curve) <- names(presence_absence_list$model_pa)

    # Free memory by removing fitted models
    if (!scale_layers){
      rm(models_suitable_habitat)
    }

    end_fr_time <- Sys.time()
    print(paste("Functional responses execution time:", difftime(end_fr_time, start_fr_time, units = "mins"), "mins"))
  }

  #=========================================================#
  # 9. Cross-validation ----
  #=========================================================#

  if ("cross_validation" %in% other_analysis){
    start_cv_time <- Sys.time()
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Performing cross-validation")))}
    print("Performing cross-validation...")

    if (!is.null(native_range)){
      other_results$cross_validation$native_range <- lapply(names(presence_absence_list$model_pa), function(i){
        cv_bart(pa_coords = presence_absence_list$model_pa[[i]],
                layers = c(layers[[predictor_variables[[i]]]], coords_layer),
                k = 10,
                seed = seed)
      })
      names(other_results$cross_validation$native_range) <- names(presence_absence_list$model_pa)
    }

    if (!is.null(suitable_habitat)){
      other_results$cross_validation$suitable_habitat <- lapply(names(presence_absence_list$model_pa), function(i){
        cv_bart(pa_coords = presence_absence_list$model_pa[[i]],
                layers = layers[[predictor_variables[[i]]]],
                k = 10,
                seed = seed)
      })
      names(other_results$cross_validation$suitable_habitat) <- names(presence_absence_list$model_pa)
    }

    end_cv_time <- Sys.time()
    print(paste("Cross-validation execution time:", difftime(end_cv_time, start_cv_time, units = "mins"), "mins"))
  }


  #=========================================================#
  # 10. Finalizing -----
  #=========================================================#
  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading..."), h4("Sit back, relax, and let us do the math!"),  h6("Finalizing")))}

  for (sp in names(presence_absence_list$model_pa)){
    presence_absence_list$model_pa[[sp]] <- cbind(presence_absence_list$model_pa[[sp]], design_matrix[[sp]])
  }

  end_time <- Sys.time()
  print(paste("GLOSSA analysis execution time:", difftime(end_time, start_time, units = "mins"), "mins"))

  # Return results to Shiny server
  return(list(
    presence_absence_list = presence_absence_list,
    covariate_list = covariate_list,
    projections_results = projections_results,
    other_results = other_results,
    pa_cutoff = pa_cutoff,
    habitat_suitability = habitat_suitability
  ))
}
