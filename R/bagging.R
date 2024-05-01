# Load necessary libraries
library(MASS)
library(glmnet)

# Define the fit_model and bag_model functions (definitions copied from previous discussions)

fit_model <- function(data, formula, model_type) {
  if (model_type == "linear") {
    return(lm(formula, data = data))
  } else if (model_type == "logistic") {
    return(glm(formula, data = data, family = binomial()))
  } else if (model_type %in% c("ridge", "lasso", "elasticnet")) {
    y <- as.matrix(data[[all.vars(formula)[1]]])  # Ensure the response variable is correctly identified
    x <- model.matrix(formula, data)[, -1]  # Make sure to remove the intercept
    if (ncol(x) < 2) stop("Not enough predictors to fit a model.")
    alpha <- ifelse(model_type == "ridge", 0, ifelse(model_type == "lasso", 1, 0.5))
    cv_fit <- cv.glmnet(x, y, alpha = alpha)
    return(cv_fit)
  } else {
    stop("Unsupported model type")
  }
}

bag_model <- function(data, formula, model_type, R = 10) {
  models <- vector("list", R)
  predictions <- vector("list", R)
  n <- nrow(data)

  if (model_type %in% c("lasso", "elasticnet")) {
    # Correctly creating a model matrix and response vector from the actual data
    x <- model.matrix(formula, data)[, -1]
    y <- as.matrix(data[[all.vars(formula)[1]]])
    # Using glmnet to fit a model on actual data to initialize variable importance correctly
    initial_model <- glmnet(x, y, alpha = ifelse(model_type == "lasso", 1, 0.5))
    variable_importance <- rep(0, length(coef(initial_model)) - 1)
    colnames <- names(coef(initial_model))[-1]
  }

  for (i in 1:R) {
    sampled_data <- data[sample(n, replace = TRUE), ]
    model <- fit_model(sampled_data, formula, model_type)
    models[[i]] <- model

    if (model_type %in% c("ridge", "lasso", "elasticnet")) {
      lambda <- model$lambda.min
      pred <- stats::predict(model, newx = model.matrix(formula, data)[, -1], s = lambda, type = "response")
    } else {
      pred <- stats::predict(model, newdata = data, type = "response")
    }
    predictions[[i]] <- pred[,1]

    if (model_type %in% c("lasso", "elasticnet")) {
      coefs <- coef(model, s = "lambda.min")
      selected_vars <- which(coefs[-1] != 0)
      variable_importance[selected_vars] <- variable_importance[selected_vars] + 1
    }
  }

  avg_predictions <- rowMeans(do.call(cbind, predictions))

  if (model_type %in% c("lasso", "elasticnet")) {
    names(variable_importance) <- colnames
    variable_importance <- (variable_importance / R) * 100
    return(list(models = models, avg_predictions = avg_predictions, variable_importance = variable_importance))
  } else {
    return(list(models = models, avg_predictions = avg_predictions))
  }
}



# Load Boston dataset
# data("Boston")
# Boston$medv_binary <- as.numeric(Boston$medv > median(Boston$medv))
# formula_linear <- medv ~ .
# formula_logistic <- medv_binary ~ .
#
# # Execute tests manually
# # Test linear regression
# linear_model <- fit_model(Boston, formula_linear, "linear")
# print(linear_model)
#
# # Scale predictors to improve convergence
# Boston_scaled <- as.data.frame(scale(Boston[, -which(names(Boston) == "medv_binary")]))
# Boston_scaled$medv_binary <- Boston$medv_binary
#
# # Fit logistic model on scaled data
# logistic_model_scaled <- fit_model(Boston_scaled, formula_logistic, "logistic")
# print(summary(logistic_model_scaled))
#
#
# # Check the model matrix directly
# x <- model.matrix(formula_linear, Boston)[, -1]  # Ensure intercept is excluded
# y <- Boston$medv  # Ensure this is the response variable you intend to use
#
# # Directly test glmnet to make sure the matrix x is correct
# test_model <- glmnet(x, y, alpha = 1)
# print(test_model)
#
# # If the above works, retest the bag_model
# lasso_results <- bag_model(Boston, formula_linear, "lasso", R = 10)
# print(lasso_results)
#
#
# # Test elasticnet regression
# elasticnet_results <- bag_model(Boston, formula_linear, "elasticnet", R = 10)
# print(elasticnet_results)
