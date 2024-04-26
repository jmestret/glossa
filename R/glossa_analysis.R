#' Main Analysis Function for Glossa Package
#'
#' This function wraps all the analysis that the Glossa package performs. It processes presence-absence data,
#' environmental covariates, and performs species distribution modeling and prediction under historical, past, and future scenarios.
#'
#' @param pa_files A list of file paths containing presence-absence data.
#' @param historical_files A list of file paths containing historical environmental layers.
#' @param future_files A list of file paths containing future scenario environmental layers.
#' @param future_scenario_names A vector of names corresponding to future scenarios.
#' @param decimal_digits Integer; number of digits to round coordinates to if not NULL.
#' @param scale_layers Logical; if TRUE, covariate layers will be scaled based on historical data.
#' @param native_range A vector of scenarios ('historical', 'past', 'future') where native range modeling should be performed.
#' @param suitable_habitat A vector of scenarios ('historical', 'past', 'future') where habitat suitability modeling should be performed.
#' @param other_analysis A vector of additional analyses to perform (e.g., 'variable_importance', 'response_curve', 'cross_validation').
#' @param seed Optional; an integer seed for reproducibility of results.
#' @param waiter Optional; a waiter instance to update progress in a Shiny application.
#'
#' @return A list containing structured outputs from each major section of the analysis including model data, predictions,
#' variable importance scores, and habitat suitability assessments.
#'
#' @export
glossa_analysis <- function(
    pa_files = NULL, historical_files = NULL, future_files = NULL, future_scenario_names = NULL,
    predictor_variables = NULL, decimal_digits = NULL, scale_layers = FALSE,
    native_range = NULL, suitable_habitat = NULL, other_analysis = NULL,
    seed = NA, waiter = NULL) {

  #=========================================================#
  # 0. Check inputs and load necessary data ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Initializing objects...")))}

  # Set seed
  if (is.na(seed)){
    seed <- NULL
  }

  # Load global mask
  sf::sf_use_s2(FALSE)
  global_land_mask <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    sf::st_as_sfc() %>%
    sf::st_union() %>%
    sf::st_make_valid() %>%
    sf::st_wrap_dateline()

  global_ocean_mask <- invert_polygon(global_land_mask)

  # Initialize empty output
  presence_absence_list <- list(raw_pa = NULL, clean_pa = NULL, model_pa = NULL)
  covariate_list <- list(historical = NULL, past = NULL, future = NULL)
  prediction_results <- list(historical = NULL, past = NULL, future = NULL)
  other_results <- list(variable_importance = NULL, response_curve = NULL, cross_validation = NULL)
  pa_cutoff <- list(native_range = NULL, suitable_habitat = NULL)
  habitat_suitability <- list(historical = NULL, past = NULL, future = NULL)
  design_matrix <- list()

  #=========================================================#
  # 1. Load presence(/absence) data and environmental layers ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Loading input data...")))}


  # * Load presence(/absence) data ----
  presence_absence_list$raw_pa <- lapply(pa_files, read_glossa_pa)

  # Place species names
  sp_names <- sapply(presence_absence_list$raw_pa, function(x) {
    paste0(unique(x[, "species"]), collapse = "")
  })
  sp_names[which(duplicated(sp_names))] <- paste0(sp_names[which(duplicated(sp_names))], which(duplicated(sp_names)))
  names(pa_files) <- sp_names
  names(presence_absence_list$raw_pa) <- sp_names

  # * Load covariate layers ----
  # Historical layers
  covariate_list$past <- read_glossa_layers(historical_files)
  cov_names <- names(covariate_list$past)

  # Future layers
  if ("future" %in% native_range | "future" %in% suitable_habitat){
    if (length(future_files) <= 0){
      stop("Error: No future layers provided.")
    }
    covariate_list$future <- lapply(future_files, read_glossa_layers)
    if (is.null(future_scenario_names)){
      names(covariate_list$future) <- sub("\\.zip$", "", basename(future_files))
    } else {
      names(covariate_list$future) <- future_scenario_names
    }

    same_fut_cov <- all(sapply(covariate_list$future, function(x){
      all(names(x) == names(covariate_list$past))
    }))

    if (!same_fut_cov){
      stop("Error: Future covariate layers differ in the covariate names from historical layers.")
    }
  }

  # * Select predictor variables ----
  if (is.null(predictor_variables)){
    predictor_variables <- lapply(seq_along(presence_absence_list$raw_pa), function(x) names(covariate_list$past))
  }
  names(predictor_variables) <- names(presence_absence_list$raw_pa)

  #=========================================================#
  # 2. Clean coordinates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Processing P/A coordinates...")))}

  presence_absence_list$clean_pa <- lapply(presence_absence_list$raw_pa, function(x){
    clean_coordinates(
      data = x,
      sf_poly = global_land_mask,
      overlapping = TRUE,
      decimal_digits = decimal_digits,
      coords = c("decimalLongitude", "decimalLatitude")
    )
  })

  #=========================================================#
  # 3. Covariate layer processing ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Processing covariate layers...")))}

  # * Process historical covariate layers ----
  covariate_list$past <- lapply(covariate_list$past, function(x){
    global_mask(layers = x, sf_poly = global_ocean_mask)
  })

  # Get mean historical layer for model fitting or functional responses
  if (!(scale_layers) | "functional_responses" %in% other_analysis) {
    covariate_list$historical$not_scaled <- lapply(covariate_list$past, function(x){
      terra::mean(x, na.rm = TRUE)
    })
  }

  if (scale_layers) {
    # Compute mean and sd of the historical time series for each environmental variable
    historical_mean <- lapply(covariate_list$past, function(x){
      mean(as.vector(x), na.rm = TRUE)
    })

    historical_sd <- lapply(covariate_list$past, function(x){
      sd(as.vector(x), na.rm = TRUE)
    })

    # Scale historical layers with historical mean and sd
    covariate_list$past <- lapply(seq_along(covariate_list$past), function(x, n, i){
      terra::scale(x[[n[i]]], center = historical_mean[[n[i]]], scale = historical_sd[[n[i]]])},
      x = covariate_list$past,
      n = names(covariate_list$past))
    names(covariate_list$past) <- names(historical_mean)

    # Get mean of scaled historical layers for model fitting
    covariate_list$historical$scaled <- lapply(covariate_list$past, function(x){
      terra::mean(x, na.rm = TRUE)
    })
  }

  # If not predicting to past free memory emptying layers
  if (!"past" %in% native_range & !"past" %in% suitable_habitat) {
    covariate_list$past <- NULL
  }

  # * Process future covariate layers ----
  if ("future" %in% native_range | "future" %in% suitable_habitat) {
    covariate_list$future <- lapply(covariate_list$future, function(scenario){
      scenario <- lapply(scenario, function(x){
        global_mask(layers = x, sf_poly = global_ocean_mask)
      })
    })

    covariate_list$past <- lapply(covariate_list$past, function(x){
      global_mask(layers = x, sf_poly = global_ocean_mask)
    })

    if (scale_layers){
      covariate_list$future <- lapply(covariate_list$future, function(scenario){
        scenario <- lapply(seq_along(scenario), function(x, n, i){
          terra::scale(x[[n[i]]], center = historical_mean[[n[i]]], scale = historical_sd[[n[i]]])},
          x = scenario,
          n = names(scenario))
        names(scenario) <- names(historical_mean)
        return(scenario)
      })
    }
  }

  # Choose layers for analysis
  if (scale_layers) {
    layers <-  terra::rast(covariate_list$historical$scaled)
  } else {
    layers <- terra::rast(covariate_list$historical$not_scaled)
  }

  #=========================================================#
  # 4. Remove presences/absences with NA values in covariates ----
  #=========================================================#

  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Building model matrix...")))}

  # Remove points with NA values in any environmental variable
  presence_absence_list$model_pa <- lapply(seq_along(presence_absence_list$clean_pa), function(i){
    x <- presence_absence_list$clean_pa[[i]]
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
      x <- generate_pseudo_absences(x, global_ocean_mask, layers[[predictor_variables[[i]]]])
    }
    return(x)
  })
  names(presence_absence_list$model_pa) <- names(presence_absence_list$clean_pa)

  # * Create design matrix ----
  design_matrix <- lapply(seq_along(presence_absence_list$model_pa), function (i){
    terra::extract(
      layers[[predictor_variables[[i]]]],
      presence_absence_list$model_pa[[i]][, c("decimalLongitude", "decimalLatitude")]
    ) %>%
      select(!ID)
  })
  names(design_matrix) <- names(presence_absence_list$model_pa)

  #=========================================================#
  # 6. Native range  ----
  #=========================================================#

  # If Native Ranges include longitude and latitude for native ranges modeling
  if (!is.null(native_range)){
    # Create layer with longitude and latitude values
    coords_layer <- glossa::create_coords_layer(layers, global_ocean_mask, scale_layers = scale_layers)

    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Fitting native range models...")))}

    models_native_range <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        presence_absence_list$model_pa[[i]],
        layers = c(layers[[predictor_variables[[i]]]], coords_layer),
        seed = seed
      )
    })
    names(models_native_range) <- names(presence_absence_list$model_pa)

    # * Variable importance ----
    other_results$variable_importance$native_range <- lapply(models_native_range, variable_importance)

    # * Optimal cutoff ----
    pa_cutoff$native_range <- lapply(names(models_native_range), function(sp) {
      pa_optimal_cutoff(
        presence_absence_list$model_pa[[sp]],
        c(layers[[predictor_variables[[sp]]]], coords_layer),
        models_native_range[[sp]]
      )
    })
    names(pa_cutoff$native_range) <- names(models_native_range)

    # * Historical prediction ----
    if ("historical" %in% native_range) {
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting historical native range...")))}
      prediction_results$historical$native_range <- lapply(names(models_native_range), function(sp) {
        predict_bart(
          models_native_range[[sp]],
          c(layers[[predictor_variables[[sp]]]], coords_layer),
          pa_cutoff$native_range[[sp]]
        )
      })
      names(prediction_results$historical$native_range) <- names(models_native_range)
    }

    # * Past prediction ----
    if ("past" %in% native_range) {
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting past native range...")))}
      prediction_results$past$native_range <- lapply(names(models_native_range), function(sp) {
        prediction <- lapply(1:terra::nlyr(covariate_list$past[[1]]), function(i){
          # Stack covariates by year
          pred_layers <- lapply(covariate_list$past, function(y){
            return(y[[i]])
          })
          pred_layers <- terra::rast(pred_layers)
          pred_layers <- c(pred_layers[[predictor_variables[[sp]]]], coords_layer)

          predict_bart(models_native_range[[sp]], pred_layers, pa_cutoff$native_range[[sp]])
        })
        return(prediction)
      })
      names(prediction_results$past$native_range) <- names(models_native_range)
    }

    # * Future prediction ----
    if ("future" %in% native_range){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting future native range...")))}
      prediction_results$future$native_range <- lapply(names(models_native_range), function(sp) {
        prediction <- lapply(covariate_list$future, function(future_scenario){
          prediction_scenario <- lapply(1:terra::nlyr(future_scenario[[1]]), function(i){
            # Stack covariates by year
            pred_layers <- lapply(future_scenario, function(y){
              return(y[[i]])
            })
            pred_layers <- terra::rast(pred_layers)
            pred_layers <- c(pred_layers[[predictor_variables[[sp]]]], coords_layer)

            predict_bart(models_native_range[[sp]], pred_layers, pa_cutoff$native_range[[sp]])
          })
          return(prediction_scenario)
        })
      })
      names(prediction_results$future$native_range) <- names(models_native_range)
    }

    # Free memory by removing fitted models
    rm(models_native_range)
  }

  #=========================================================#
  # 7. Suitable habitat  ----
  #=========================================================#

  if (!is.null(suitable_habitat)){
    # * Fit bart ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Fitting suitable habitat models...")))}

    models_suitable_habitat <- lapply(seq_along(presence_absence_list$model_pa), function(i){
      fit_bart_model(
        presence_absence_list$model_pa[[i]],
        layers = layers[[predictor_variables[[i]]]],
        seed = seed
      )
    })
    names(models_suitable_habitat) <- names(presence_absence_list$model_pa)

    # * Variable importance ----
    other_results$variable_importance$suitable_habitat <- lapply(models_suitable_habitat, variable_importance)

    # * Optimal cutoff ----
    pa_cutoff$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
      pa_optimal_cutoff(
        presence_absence_list$model_pa[[sp]],
        layers[[predictor_variables[[sp]]]],
        models_suitable_habitat[[sp]]
      )
    })
    names(pa_cutoff$suitable_habitat) <- names(models_suitable_habitat)

    # * Historical prediction ----
    if ("historical" %in% suitable_habitat) {
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting historical suitable habitat...")))}
      prediction_results$historical$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        predict_bart(
          models_suitable_habitat[[sp]],
          layers[[predictor_variables[[sp]]]],
          pa_cutoff$suitable_habitat[[sp]]
        )
      })
      names(prediction_results$historical$suitable_habitat) <- names(models_suitable_habitat)
    }

    # * Past prediction ----
    if ("past" %in% suitable_habitat) {
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting past suitable habitat...")))}
      prediction_results$past$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        prediction <- lapply(1:terra::nlyr(covariate_list$past[[1]]), function(i){
          # Stack covariates by year
          pred_layers <- lapply(covariate_list$past, function(y){
            return(y[[i]])
          })
          pred_layers <- terra::rast(pred_layers)
          pred_layers <- pred_layers[[predictor_variables[[sp]]]]

          predict_bart(models_suitable_habitat[[sp]], pred_layers, pa_cutoff$suitable_habitat[[sp]])
        })
        return(prediction)
      })
      names(prediction_results$past$suitable_habitat) <- names(models_suitable_habitat)
    }

    # * Future prediction ----
    if ("future" %in% suitable_habitat){
      if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Predicting future suitable habitat...")))}
      prediction_results$future$suitable_habitat <- lapply(names(models_suitable_habitat), function(sp) {
        prediction <- lapply(covariate_list$future, function(future_scenario){
          prediction_scenario <- lapply(1:terra::nlyr(future_scenario[[1]]), function(i){
            # Stack covariates by year
            pred_layers <- lapply(future_scenario, function(y){
              return(y[[i]])
            })
            pred_layers <- terra::rast(pred_layers)
            pred_layers <- pred_layers[[predictor_variables[[sp]]]]

            predict_bart(models_suitable_habitat[[sp]], pred_layers, pa_cutoff$suitable_habitat[[sp]])
          })
          return(prediction_scenario)
        })
      })
      names(prediction_results$future$suitable_habitat) <- names(models_suitable_habitat)
    }

    # * Habitat suitability change ----
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Computing habitat suitability change...")))}
    if ("historical" %in% suitable_habitat) {
      # Covered area
      habitat_suitability$historical$covered_area <- lapply(names(models_suitable_habitat), function(sp){
        layer <- prediction_results$historical$suitable_habitat[[sp]]["mean"]
        area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
        area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
        return(area)
      })
      names(habitat_suitability$historical$covered_area) <- names(models_suitable_habitat)

      # Mean suitability probability
      habitat_suitability$historical$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
        layer <- prediction_results$historical$suitable_habitat[[sp]]
        prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
        return(as.numeric(prob))
      })
      names(habitat_suitability$historical$suit_prob) <- names(models_suitable_habitat)
    }

    if ("past" %in% suitable_habitat) {
      # Covered area
      habitat_suitability$past$covered_area <- lapply(names(models_suitable_habitat), function(sp){
        covered_area <- sapply(prediction_results$past$suitable_habitat[[sp]], function(layer) {
          layer <- layer["mean"]
          area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
          area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
          return(area)
        })
      })
      names(habitat_suitability$past$covered_area) <- names(models_suitable_habitat)

      # Mean suitability probability
      habitat_suitability$past$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
        suit_prob <- sapply(prediction_results$past$suitable_habitat[[sp]], function(layer) {
          prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
          return(as.numeric(prob))
        })
      })
      names(habitat_suitability$past$suit_prob) <- names(models_suitable_habitat)
    }

    if ("future" %in% suitable_habitat) {
      # Covered area
      habitat_suitability$future$covered_area <- lapply(names(models_suitable_habitat), function(sp){
        covered_area_scenarios <- lapply(prediction_results$future$suitable_habitat[[sp]], function(future_scenario) {
          covered_area <- sapply(future_scenario, function(layer) {
            layer <- layer["mean"]
            area <- terra::ifel(layer > pa_cutoff$suitable_habitat[[sp]], layer, NA)
            area <- sum(terra::values(terra::cellSize(area, mask = TRUE, unit = "km"), na.rm = TRUE))
            return(area)
          })
        })
      })
      names(habitat_suitability$future$covered_area) <- names(models_suitable_habitat)

      # Mean suitability probability
      habitat_suitability$future$suit_prob <- lapply(names(models_suitable_habitat), function(sp){
        suit_prob_scenarios <- lapply(prediction_results$future$suitable_habitat[[sp]], function(future_scenario) {
          suit_prob <- sapply(future_scenario, function(layer) {
            prob <- terra::global(layer["mean"], mean, na.rm = TRUE)
            return(as.numeric(prob))
          })
        })
      })
      names(habitat_suitability$future$suit_prob) <- names(models_suitable_habitat)
    }

    # Free memory by removing fitted models
    if (scale_layers | !"functional_responses" %in% other_analysis){
      rm(models_suitable_habitat)
    }
  }

  #=========================================================#
  # 8. Functional responses ----
  #=========================================================#

  if ("functional_responses" %in% other_analysis){
    if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Computing functional responses...")))}
    layers <- terra::rast(covariate_list$historical$not_scaled)
    other_results$response_curve <- lapply(names(presence_absence_list$model_pa), function(i){
      x <- presence_absence_list$model_pa[[i]]
      pred_layers <- layers[[predictor_variables[[i]]]]
      fit_data <- extract_noNA_cov_values(x, pred_layers)
      fit_data <- fit_data %>%
        select(pa, !names(x))

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
  }

  #=========================================================#
  # 9. Finalizing -----
  #=========================================================#
  if (!is.null(waiter)){waiter$update(html = tagList(img(src = "logo_glossa.gif", height = "200px"), h4("Finalizing...")))}

  for (sp in names(presence_absence_list$model_pa)){
    presence_absence_list$model_pa[[sp]] <- cbind(presence_absence_list$model_pa[[sp]], design_matrix[[sp]])
  }

  # Return results to Shiny server
  return(list(
    presence_absence_list = presence_absence_list,
    covariate_list = covariate_list,
    prediction_results = prediction_results,
    other_results = other_results,
    pa_cutoff = pa_cutoff,
    habitat_suitability = habitat_suitability
  ))
}