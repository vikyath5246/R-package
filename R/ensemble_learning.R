'Now lets begin with our ensemble regression.'
'So ensemble regression essetially qualifies like if we multiply our weight * prediction and then sum it up we get our model'
'Now lets what we have tried.'

ensemble_prediction <- function(X, y, models, weights = NULL) {
  'Check for library, or else ignore it'
  required_libraries <- c("glmnet", "randomForest")
  missing_libraries <- required_libraries[!required_libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries, dependencies = TRUE)
    message("Installed the required libraries.")
  }
  lapply(required_libraries, library, character.only = TRUE)

  'We check for whether our dataset has non-numeric, categorical and all that stuff'
  if (any(sapply(X, function(x) !is.numeric(x)))) {
    warning("Non-numeric column(s) detected in predictors. Please convert to numeric. Please check your dataset again\n")
  }

  'Now we check for whether the sum of weights > 1 or not'
  if(sum(weights)>1){
    stop("Weights are total greater than 1")
  }

  'Now we check for null values and if there is anything wrong we print it.'
  if (anyNA(X) || anyNA(y)) {
    warning("The dataset contains NA values. We are removing them. You can also compuet it statistically and then run the function")
    complete_cases <- complete.cases(X, y)
    X <- X[complete_cases, ]
    y <- y[complete_cases]
  }


  'We store the following like the predictions, model_objects, and model_coefs so that we can return them.'
  predictions <- list()
  model_objects <- list()
  model_coefs <- list()

  print("Starting model training.")
  'Now for various names in models we are going to check for each and every model its weight and then its prediction'
  'We are gong to store each and evry model prediction'
  'And also we store each and every model coefs and the other stuff depending on the model.'
  for (model_name in models) {
    model_name <- tolower(model_name)
    if (model_name == "linear" || model_name == "logistic" || model_name == "lasso" || model_name == "ridge" || model_name == "elastic_net" || model_name == "random_forest") {
      print(paste("Training", model_name, "regression."))
      model_info <- trainmodel(
        y = y,
        X = X,
        model_type = model_name
      )
      model_objects[[model_name]] <- model_info
      if (model_name %in% c("linear", "logistic")) {

        predictions[[model_name]] <- predict(model_info, newdata = as.data.frame(X))
      } else if (model_name %in% c("lasso", "ridge", "elastic_net")) {
        predictions[[model_name]] <- predict(model_info, newx = model.matrix(~.-1, data = as.data.frame(X)), s = model_info$model$lambda.min)
      } else if (model_name == "random_forest") {
        predictions[[model_name]] <- predict(model_info, newdata = as.data.frame(X))
      }
      if (model_name %in% c("linear", "logistic")) {
        model_coefs[[model_name]] <- summary(model_info)
      } else if(model_name %in% c("random_forest")) {
        model_coefs[[model_name]] <- importance(model_info)
      }else{
        model_coefs[[model_name]] <- coef(model_info)
      }
    } else {
      stop(paste("Invalid model type:", model_name))
    }
  }
  'We check for the mismatch among models'
  print("All requested models trained")
  pred_lengths <- sapply(predictions, length)
  if (any(pred_lengths != pred_lengths[1])) {
    stop("Mismatch between prediction lengths among models.")
  }

  'We check if weights are null we provide various messages and warnings depending on the types of weights.'
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

  'Now we combine predictions andmultiply it into various weights'
  combined_predictions <- do.call(cbind, predictions)
  if (is.null(combined_predictions) || ncol(combined_predictions) != length(models)) {
    stop("Mismatch between combined predictions and the number of models.")
  }

  weighted_prediction <- rowSums(combined_predictions * weights)

  actuals <- y

  i=TRUE

  while(i){

    cat("Please select the output you want to view:\n")
    cat("1: Model Coefficients\n")
    cat("2: Weighted Predictions\n")
    cat("3: Predictions\n")
    cat("4: Exit\n")
    choice <- as.integer(readline(prompt="Enter your choice (number): "))

    # Process user input and display the requested output
    if (!is.na(choice)) {
      if (choice == 1) {
        cat("Available models' coefficients:\n")
        names <- names(model_coefs)
        for (i in seq_along(names)) {
          cat(paste(i, ": ", names[i], "\n", sep=""))
        }
        model_choice <- as.integer(readline(prompt="Select a model number for coefficients: "))
        if (!is.na(model_choice) && model_choice %in% 1:length(names)) {
          print(model_coefs[[names[model_choice]]])
        } else {
          cat("Invalid model number selected.\n")
        }
      } else if (choice == 2) {
        print(weighted_prediction)
      }else if(choice==3){
        cat("Available models' predictions:\n")
        names <- names(predictions)
        for (i in seq_along(names)) {
          cat(paste(i, ": ", names[i], "\n", sep=""))
        }
        model_choice <- as.integer(readline(prompt="Select a model number for predictions: "))
        if (!is.na(model_choice) && model_choice %in% 1:length(names)) {
          print(model_coefs[[names[model_choice]]])
        } else {
          cat("Invalid model number selected.\n")
        }
      }
      else if(choice==4){
        i=FALSE
      }
      else {
        cat("Invalid choice. Please run the function again and select a valid option.\n")
      }
    } else {
      cat("No valid input provided. Please run the function again and enter a valid choice.\n")
    }
  }

  return(list(
    "weighted_prediction" = weighted_prediction,
    "predictions" = predictions,
    "model_coefficients" = model_coefs
  ))
}
