ensemble_predict <- function(models, newdata) {
  # Define a function to select models based on numbers
  select_models <- function() {
    cat("Select models from the following options:\n")
    cat("1) linear\n")
    cat("2) logistic\n")
    cat("3) random_forest\n")

    selected_models_input <- readline(prompt = "Enter selected models (comma-separated numbers): ")
    selected_model_numbers <- as.numeric(strsplit(selected_models_input, ",")[[1]])
    selected_models <- names(models)[selected_model_numbers]

    if (any(is.na(selected_model_numbers)) || any(selected_model_numbers < 1) || any(selected_model_numbers > length(models))) {
      stop("Invalid model numbers selected.")
    }

    return(selected_models)
  }

  # Define a function to input corresponding weights
  input_weights <- function(selected_models) {
    weights_input <- readline(prompt = "Enter corresponding weights (comma-separated): ")
    selected_weights <- as.numeric(strsplit(weights_input, ",")[[1]])

    # Check if the number of weights matches the number of selected models
    if (length(selected_models) != length(selected_weights)) {
      stop("Number of weights does not match the number of selected models.")
    }

    # Normalize weights to sum to 1
    selected_weights <- selected_weights / sum(selected_weights)

    return(selected_weights)
  }

  # Select models
  selected_models <- select_models()

  # Input corresponding weights
  selected_weights <- input_weights(selected_models)

  # Make predictions for selected models
  predictions <- numeric(nrow(newdata))
  for (model_name in selected_models) {
    if (model_name %in% c("linear", "ridge", "lasso", "elastic_net", "svm")) {
      predictions <- predictions + predict(models[[model_name]], newdata = newdata) * selected_weights[model_name]
    } else if (model_name %in% c("logistic")) {
      prob <- predict(models[[model_name]], newdata = newdata, type = "response")
      predictions <- predictions + ifelse(prob > 0.5, 1, 0) * selected_weights[model_name]
    } else if (model_name %in% c("random_forest")) {
      predictions <- predictions + predict(models[[model_name]], newdata = newdata) * selected_weights[model_name]
    }
  }

  return(predictions)
}
