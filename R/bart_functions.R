#' Fit a BART Model Using Environmental Covariate Layers
#'
#' This function fits a Bayesian Additive Regression Trees (BART) model using
#' presence/absence data and environmental covariate layers.
#'
#' @param y A numeric vector indicating presence (1) or absence (0).
#' @param x A data frame with the same number of rows as the length of the vector `y`, containing the covariate values.
#' @param seed An optional integer value for setting the random seed for reproducibility.
#'
#' @return A BART model object.
#'
#' @export
fit_bart_model <- function(y, x, seed = NULL) {
  # Input validation
  if (!is.numeric(y) || !all(y %in% c(0, 1))) {
    stop("Argument 'y' must be a numeric vector containing only 0s and 1s.")
  }

  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a data frame.")
  }

  if (length(y) != nrow(x)) {
    stop("Length of 'y' must match the number of rows in 'x'.")
  }

  # Set random seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Fit the BART model
  bart_model <- dbarts::bart(x.train = x,
                             y.train = y,
                             keeptrees = TRUE)

  # Make the model fit state invisible so we can export it and use it to predict
  invisible(bart_model$fit$state)

  # Return the fitted BART model object
  return(bart_model)
}

#' Make Predictions Using a BART Model
#'
#' This function makes predictions using a Bayesian Additive Regression Trees (BART) model
#' on a stack of environmental covariates ('SpatRaster').
#'
#' @param bart_model A BART model object obtained from fitting BART using the 'dbarts' package.
#' @param layers A SpatRaster object containing environmental covariates for prediction.
#' @param cutoff An optional numeric cutoff value for determining potential presences. If NULL, potential presences and absences will not be computed.
#'
#' @return A SpatRaster containing the mean, median, standard deviation, and quantiles
#' of the posterior predictive distribution, as well as a potential presences layer if cutoff is provided.
#'
#' @export
predict_bart <- function(bart_model, layers, cutoff = NULL) {
  tryCatch({
    # Define quantiles for posterior predictive distribution
    quantiles <- c(0.025, 0.975)

    # Convert raster stack to matrix
    input_matrix <- terra::as.matrix(layers)

    # Initialize output data frame for predictions
    blank_output <- data.frame(matrix(ncol = (4 + length(quantiles) + ifelse(!is.null(cutoff), 1, 0)),
                                      nrow = terra::ncell(layers[[1]])))

    # Get indices of non-NA values in the input matrix
    which_vals <- which(complete.cases(input_matrix))

    # Remove NA values from the input matrix
    input_matrix <- input_matrix[complete.cases(input_matrix), , drop = FALSE]

    # Make predictions using the BART model
    pred <- dbarts:::predict.bart(bart_model, input_matrix)

    # Calculate summary statistics of predictions
    pred_data <- cbind(
      data.frame(matrixStats::colMeans2(pred)),
      data.frame(matrixStats::colMedians(pred)),
      data.frame(matrixStats::colSds(pred)),
      data.frame(matrixStats::colQuantiles(pred, probs = quantiles))
    )

    # Calculate the difference between upper and lower quantiles
    pred_data$diff <- pred_data$X97.5. - pred_data$X2.5.

    # Rename columns
    names(pred_data) <- c("mean", "median", "sd", "q0.025", "q0.975", "diff")

    # Calculate potential presences
    if (!is.null(cutoff)) {
      pred_data$potential_presences <- ifelse(pred_data$mean >= cutoff, 1, 0)
    }

    # Fill in the blank output with predictions
    blank_output[which_vals, ] <- as.matrix(pred_data)

    # Reshape output to SpatRaster format
    out_list <- lapply(1:ncol(blank_output), function(x) {
      output_matrix <- t(matrix(blank_output[, x], nrow = ncol(layers), ncol = nrow(layers)))
      return(terra::rast(output_matrix, extent = terra::ext(layers)))
    })

    # Convert list of matrices to raster stack
    out_list <- terra::rast(out_list)

    # Set names for the raster stack
    names(out_list) <- names(pred_data)

    # Set CRS
    terra::crs(out_list) <- terra::crs(layers)

    return(out_list)
  }, error = function(err) {
    message("Prediction failed: ", conditionMessage(err))
    return(NULL) # Return NULL if prediction fails
  })
}


#' Calculate Response Curve Using BART Model
#'
#' This function calculates the response curve (functional responses) using a Bayesian Additive Regression Trees (BART) model.
#'
#' @param bart_model A BART model object obtained from fitting BART ('dbarts::bart').
#' @param data A data frame containing the predictor variables (the design matrix) used in the BART model.
#' @param predictor_names A character vector containing the names of the predictor variables.
#'
#' @return A list containing a data frame for each independent variable with mean, 2.5th percentile, 97.5th percentile, and corresponding values of the variables.
#'
#' @export
response_curve_bart <- function(bart_model, data, predictor_names) {
  # Calculate level values for each predictor variable
  y_values <- lapply(predictor_names, function(name) {
    values <- na.omit(data[[name]])
    values <- c(quantile(values, prob = seq(0.05, 0.95, length.out = 10)),
                seq(min(values), max(values), length.out = 10))
    return(sort(values))
  })

  # Obtain variable response information using pdbart
  var_r <- dbarts::pdbart(bart_model, xind = predictor_names, levs = y_values, pl = FALSE)

  # Compute inverse probit values for each predictor variable
  inv_probit_df <- lapply(var_r$fd, function(fd) {
    data.frame(matrix(mcp::phi(fd), nrow = nrow(fd), ncol = ncol(fd)))
  })

  # Calculate mean, 2.5th percentile, 97.5th percentile, and values of predictor variables
  data_var <- lapply(seq_along(predictor_names), function(i) {
    prob_inv <- colMeans(inv_probit_df[[i]], na.rm = TRUE)
    quantile_values_q25 <- apply(inv_probit_df[[i]], 2, quantile, probs = 0.025, na.rm = TRUE)
    quantile_values_q975 <- apply(inv_probit_df[[i]], 2, quantile, probs = 0.975, na.rm = TRUE)
    value <- var_r$levs[[i]]
    data.frame(mean = prob_inv, q25 = quantile_values_q25, q975 = quantile_values_q975, value = value)
  })

  # Set names for the data frames
  names(data_var) <- predictor_names

  return(data_var)
}


#' Variable Importance in BART Model
#'
#' Calculate variable importance, measured as the proportion of total branches used for a given variable.
#'
#' @param bart_model A BART model object.
#' @return A numeric vector containing the name of the variable and its corresponding importance value. The variables are sorted such that the first variable listed is the one that has the greatest influence.
#'
#' @export
variable_importance <- function(bart_model) {
  importance <- colMeans(bart_model$varcount / rowSums(bart_model$varcount))
  importance <- sort(importance, decreasing = TRUE)
  return(importance)
}

#' Optimal Cutoff for Presence-Absence Prediction
#'
#' This function calculates the optimal cutoff for presence-absence prediction using a BART model.
#'
#' @param y Vector indicating presence (1) or absence (0).
#' @param x Dataframe with same number of rows as the length of the vector `y` with the covariate values.
#' @param model A BART model object.
#' @param seed Random seed for reproducibility.
#'
#' @return The optimal cutoff value for presence-absence prediction.
#'
#' @export
pa_optimal_cutoff <- function(y, x, model, seed = NULL) {
  set.seed(seed)
  pred_data <- dbarts:::predict.bart(model, newdata = x)
  pred_mean <- colMeans(pred_data)

  pa_cutoff <- optimalCutoff(
    actuals = y,
    predictedScores = pred_mean
  )

  return(pa_cutoff)
}

#' Cross-Validation for BART Model
#'
#' This function performs k-fold cross-validation for a Bayesian Additive Regression Trees (BART) model
#' using presence-absence data and environmental covariate layers. It calculates various performance metrics
#' for model evaluation.
#'
#' @param data Data frame with a column (named 'pa') indicating presence (1) or absence (0) and columns for the predictor variables.
#' @param k Integer; number of folds for cross-validation (default is 10).
#' @param seed Optional; random seed.
#'
#' @return A data frame containing the true positives (TP), false positives (FP), false negatives (FN), true negatives (TN),
#' and various performance metrics including precision (PREC), sensitivity (SEN), specificity (SPC), false discovery rate (FDR),
#' negative predictive value (NPV), false negative rate (FNR), false positive rate (FPR), F-score, accuracy (ACC), balanced accuracy (BA),
#' and true skill statistic (TSS) for each fold.
#'
#' @export
cv_bart <- function(data, k = 10, seed = NULL){
  set.seed(seed)
  n <- nrow(data)
  # Create index vector
  k_index <- rep(1:k, length.out = n)
  # Randomize vector
  k_index <- sample(k_index)
  data$k <- k_index
  TP <- c()
  FP <- c()
  FN <- c()
  TN <- c()
  for (i in 1:k){
    # Split train-test
    train <- data[data$k != i, ]
    test <- data[data$k == i, ]

    # Fit model
    bart_model <- fit_bart_model(
      y = train[, "pa"],
      x = train[, colnames(train) != "pa", drop = FALSE],
      seed = seed
    )

    # Compute optimal cutoff to predict presence/absence
    cutoff <- pa_optimal_cutoff(
      y = train[, "pa"],
      x = train[, colnames(train) != "pa", drop = FALSE],
      bart_model
    )

    pred <- dbarts:::predict.bart(bart_model, test[, colnames(test) != "pa", drop = FALSE])
    pred <- colMeans(pred)
    potential_presences <- ifelse(pred >= cutoff, 1, 0)

    # Confusion matrices
    TP <- c(TP, sum(test$pa == 1 & potential_presences == 1))
    FP <- c(FP, sum(test$pa == 0 & potential_presences == 1))
    FN <- c(FN, sum(test$pa == 1 & potential_presences == 0))
    TN <- c(TN, sum(test$pa == 0 & potential_presences == 0))
  }

  # Metrics
  PREC = TP / (TP + FP)
  SEN = TP / (TP + FN)
  SPC = TN / (TN + FP)
  FDR = FP / (TP + FP)
  NPV = TN / (FN + TN)
  FNR = FN / (TP + FN)
  FPR = FP / (FP + TN)
  Fscore = 2 * ((PREC * SEN) / (PREC + SEN))
  ACC = (TP + TN)/(TP + FP + FN + TN)
  BA = (SEN + SPC) / 2
  TSS = SEN + SPC - 1

  cv_res <- data.frame(
    TP = TP, FP = FP, FN = FN, TN = TN, PREC = PREC, SEN = SEN, SPC = SPC,
    FDR = FDR, NPV = NPV, FNR = FNR, FPR = FPR, Fscore = Fscore, ACC = ACC,
    BA = BA, TSS = TSS
  )

  return(cv_res)
}
