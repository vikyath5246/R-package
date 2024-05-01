library(caret)
library(randomForest)
library(glmnet)

ensemble_regression <- function(data, target, models, weights = NULL) {
  predictions <- list()  # List to store predictions of each model
  model_objects <- list()  # List to store full model objects for external use
  model_coefs <- list()  # List to store model coefficients or parameters
  performance_metrics <- list()  # List to store performance metrics

  # Train each specified model
  for (model_name in models) {
    if (tolower(model_name) == "linear") {
      model <- lm(as.formula(paste(target, "~ .")), data = data)
      model_objects[[model_name]] <- model
      predictions[[model_name]] <- predict(model, newdata = data)
      model_coefs[[model_name]] <- coef(model)
      performance_metrics[[model_name]] <- summary(model)$r.squared
    } else if (tolower(model_name) == "lasso") {
      x <- as.matrix(data[, !(colnames(data) %in% target)])
      y <- data[[target]]
      model <- cv.glmnet(x, y, alpha = 1)  # Alpha = 1 for Lasso
      model_objects[[model_name]] <- model
      best_lambda <- model$lambda.min
      predictions[[model_name]] <- predict(model, newx = x, s = best_lambda, type = "response")
      model_coefs[[model_name]] <- coef(model, s = best_lambda)
      performance_metrics[[model_name]] <- min(model$cvm)
    } else if (tolower(model_name) == "ridge") {
      x <- as.matrix(data[, !(colnames(data) %in% target)])
      y <- data[[target]]
      model <- cv.glmnet(x, y, alpha = 0)  # Alpha = 0 for Ridge
      model_objects[[model_name]] <- model
      best_lambda <- model$lambda.min
      predictions[[model_name]] <- predict(model, newx = x, s = best_lambda, type = "response")
      model_coefs[[model_name]] <- coef(model, s = best_lambda)
      performance_metrics[[model_name]] <- min(model$cvm)
    } else if (tolower(model_name) == "elasticnet") {
      x <- as.matrix(data[, !(colnames(data) %in% target)])
      y <- data[[target]]
      model <- cv.glmnet(x, y, alpha = 0.5)  # Alpha = 0.5 for Elastic Net
      model_objects[[model_name]] <- model
      best_lambda <- model$lambda.min
      predictions[[model_name]] <- predict(model, newx = x, s = best_lambda, type = "response")
      model_coefs[[model_name]] <- coef(model, s = best_lambda)
      performance_metrics[[model_name]] <- min(model$cvm)
    } else if (tolower(model_name) == "randomforest") {
      model <- randomForest(as.formula(paste(target, "~ .")), data = data, ntree = 500)
      model_objects[[model_name]] <- model
      predictions[[model_name]] <- predict(model, newdata = data)
      model_coefs[[model_name]] <- importance(model)
      performance_metrics[[model_name]] <- mean(model$mse)  # Using mean squared error
    }
  }

  # Normalize weights
  if (is.null(weights)) {
    weights <- rep(1 / length(models), length(models))
  } else {
    weights <- weights / sum(weights)  # Ensure weights sum to 1
  }

  # Combine predictions using weights
  combined_predictions <- do.call(cbind, predictions)
  weighted_prediction <- rowSums(combined_predictions * weights)

  # Calculate an ensemble performance metric if needed (e.g., mean of metrics)
  ensemble_performance_metric <- mean(unlist(performance_metrics))

  # Return a list containing all relevant results and model information
  return(list(
    "weighted_prediction" = weighted_prediction,
    "predictions" = predictions,
    "model_coefficients" = model_coefs,
    "performance_metrics" = performance_metrics,
    "ensemble_performance_metric" = ensemble_performance_metric,
    "model_objects" = model_objects
  ))
}


# ensemble_predict <- function(test_data, ensemble_model) {
#   predictions <- list()
#   weights <- ensemble_model$weights  # Extract weights from the ensemble model object
#
#   # Iterate through each model stored in the ensemble_model object
#   for (model_name in names(ensemble_model$model_objects)) {
#     model <- ensemble_model$model_objects[[model_name]]
#
#     if (tolower(model_name) == "linear" || tolower(model_name) == "logistic") {
#       # Predict using linear or logistic regression models
#       predictions[[model_name]] <- predict(model, newdata = test_data)
#     } else if (tolower(model_name) %in% c("lasso", "ridge", "elasticnet")) {
#       # Check and prepare features for glmnet
#       # Ensure only the features used in training are in test_data
#       coef_names <- names(coef(model, s = "lambda.min")[ -1 ])  # -1 to exclude intercept
#       if (all(coef_names %in% names(test_data))) {
#         # Subsetting test_data to only include the features used in training
#         x_new <- as.matrix(test_data[, coef_names, drop = FALSE])
#         x_new <- scale(x_new)  # Apply scaling
#         predictions[[model_name]] <- predict(model, newx = x_new, s = "lambda.min", type = "response")
#       } else {
#         stop("Variable mismatch: test data does not have the same variables as the training data for glmnet models.")
#       }
#     } else if (tolower(model_name) == "randomforest") {
#       # Predict using random forest models
#       predictions[[model_name]] <- predict(model, newdata = test_data)
#     }
#   }
#
#   # Combine predictions using weights
#   combined_predictions <- Reduce(`+`, lapply(names(predictions), function(name) {
#     predictions[[name]] * weights[names(weights) == name]
#   }))
#
#   return(combined_predictions)
# }



# Note: Make sure that the ensemble_model object has 'weights' and 'target' appropriately set up to be used here.

# data(mtcars)
#
# # Since `mpg` is the target variable (miles per gallon),
# # we'll use it as our target variable
# target <- "mpg"
#
# # Select the predictors (features) excluding the target variable
# predictors <- setdiff(names(mtcars), target)
#
# # Combine the target variable and predictors into a dataframe
# data <- mtcars
#
# # Define the models to include in the ensemble
# models <- c("linear", "lasso", "ridge", "elasticnet", "randomforest")
#
# # Define weights for each model
# weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)  # Equal weights for demonstration purposes
#
# # Now, let's test the `ensemble_regression` function with weights
# result <- ensemble_regression(data = data, target = target, models = models, weights = weights)
#
# # Print the ensemble performance metric
# print(result$ensemble_performance_metric)

