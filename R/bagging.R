if (!require(glmnet, quietly = TRUE)) { install.packages("glmnet") }
library(glmnet)

bagged_models <- function(formula, data, family = NULL, model_type, R) {
  print("Bagging is selected.")
  # predictions <- rep(0, nrow(X_test))
  # variable_importance <- numeric()

  # # Train R bagged models
  # for (i in 1:R) {
  #   # Perform bootstrap sampling
  #   indices <- sample(1:nrow(data), replace = TRUE)
  #   data_bagged <- data[indices, ]
  #   #y_train_bagged <- y_train[indices]

  #   # Train model
  #   if (model_type == 'linear') {
  #     model <- lm(formula, data = data)
  #   } else if (model_type == 'logistic') {
  #     model <- glm(formula, data = data, family = binomial(link = "logit"))
  #   } else if (model_type == 'ridge') {
  #     model <- glmnet(X_train_bagged, y_train_bagged, alpha = 0)
  #   } else if (model_type == 'lasso') {
  #     model <- glmnet(X_train_bagged, y_train_bagged, alpha = 1)
  #   } else if (model_type == 'elastic_net') {
  #     model <- glmnet(X_train_bagged, y_train_bagged, alpha = 0.5)
  #   } else {
  #     warning("Bagging is supported only in the following models: linear, logistic, ridge, lasso, elastic_net. Please choose one of the above models.")
  #   }

  #   # Make predictions
  #   y_pred <- predict(model, newx = X_test)
  #   predictions <- predictions + y_pred

  #   # Update variable importance score
  #   variable_importance <- update_variable_importance(X_train, indices, variable_importance)
  # }

  # # Average predictions
  # predictions <- predictions / R

  # return(list(predictions = predictions, variable_importance = variable_importance))
}

# Update variable importance scores based on the number of times each variable is selected in the bagging process
update_variable_importance <- function(X_train, indices, variable_importance) {
  selected_variables <- unique(indices)
  for (variable in selected_variables) {
    if (variable %in% names(variable_importance)) {
      variable_importance[as.character(variable)] <- variable_importance[as.character(variable)] + 1
    } else {
      variable_importance[as.character(variable)] <- 1
    }
  }
  return(variable_importance)
}

# Example usage
# result <- bagged_models(X_train, y_train, X_test, 'linear', R = 10)
# predictions <- result$predictions
# variable_importance <- result$variable_importance
