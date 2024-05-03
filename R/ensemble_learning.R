ensemble_regression <- function(data, target, models, weights = NULL) {

  if (any(sapply(data, function(x) !is.numeric(x)))) {
    # Print a warning message
    cat("Warning: Non-numeric column(s) detected. Please convert to numeric.\n")
  }

  if(sum(weights)>1){
    stop("Weights are total greater than 1")
  }
  data <- na.omit(data)
  if (nrow(data) == 0) {
    stop("All data removed after omitting NAs.")
  }

  predictions <- list()
  model_objects <- list()
  model_coefs <- list()

  print("Starting model training.")

  for (model_name in models) {
    model_name <- tolower(model_name)
    if (model_name == "linear") {
      print("Training linear regression.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "linear"
      )
      model_objects[[model_name]] <- model_info
      predictions[[model_name]] <- predict(model_info, newdata = data)
      model_coefs[[model_name]] <- summary(model_info)
    } else if (model_name == "logistic") {
      print("Training logistic regression.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "logistic"
      )
      model_objects[[model_name]] <- model_info
      # print(model_objects)
      predictions[[model_name]] <- predict(model_info, newdata = data, type = "response")
      model_coefs[[model_name]] <- summary(model_info)
      # print(model_coefs)
    }else if (model_name == "lasso") {
      print("Training Lasso regression.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "lasso"
      )
      model_objects[[model_name]] <- model_info
      newx <- model.matrix(as.formula(paste(target, "~ .")), data)
      predictions[[model_name]] <- predict(model_info, newx = newx, s = model_info$model$lambda.min)
      model_coefs[[model_name]] <- coef(model_info)
    } else if (model_name == "ridge") {
      print("Training Ridge regression.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "ridge"
      )
      model_objects[[model_name]] <- model_info
      predictions[[model_name]] <- predict(model_info, newx = model.matrix(as.formula(paste(target, "~ .")), data), s = model_info$model$lambda.min)
      model_coefs[[model_name]] <- coef(model_info)
    } else if (model_name == "elastic_net") {
      print("Training ElasticNet regression.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "elastic_net"
      )
      model_objects[[model_name]] <- model_info
      predictions[[model_name]] <- predict(model_info, newx = model.matrix(as.formula(paste(target, "~ .")), data), s = model_info$model$lambda.min)
      model_coefs[[model_name]] <- coef(model_info)
    } else if (model_name == "random_forest") {
      print("Training Random Forest.")
      model_info <- train_model(
        formula = as.formula(paste(target, "~ .")),
        data = data,
        model_type = "random_forest"
      )
      model_objects[[model_name]] <- model_info
      predictions[[model_name]] <- predict(model_info, newdata = data)
      model_coefs[[model_name]] <- coef(model_info)
    }
  }


  print("All requested models trained")
  pred_lengths <- sapply(predictions, length)
  if (any(pred_lengths != pred_lengths[1])) {
    stop("Mismatch between prediction lengths among models.")
  }

  if (is.null(weights)) {
    warning("Weights not provided. Defaulting to equal weights for all models.")
    weights <- rep(1 / length(models), length(models))
  } else {
    if (length(weights) != length(models)) {
      stop("The length of 'weights' must match the number of models.")
    }
    if (sum(weights) != 1) {
      warning("The sum of weights is not equal to 1. Normalizing weights.")
      weights <- weights / sum(weights)
    }
  }

  # print("Weights normalized.")

  combined_predictions <- do.call(cbind, predictions)
  if (is.null(combined_predictions) || ncol(combined_predictions) != length(models)) {
    stop("Mismatch between combined predictions and the number of models.")
  }

  weighted_prediction <- rowSums(combined_predictions * weights)

  actuals <- data[[target]]

  rmse <- sqrt(mean((weighted_prediction - actuals)^2))

  return(list(
    "weighted_prediction" = weighted_prediction,
    "predictions" = predictions,
    "model_coefficients" = model_coefs,
    "rmse" =rmse
  ))
}
