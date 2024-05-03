predictor_selection <- function(data, target_col, technique = "correlation", method = "pearson", threshold = 0.05, top_k = 2) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    install.packages("glmnet")
    stop("The 'glmnet' package is required but not installed. Please install it before using the Lasso technique.")
  }

  if (!target_col %in% names(data)) {
    stop("The target column specified does not exist in the dataset.")
  }

  if (anyNA(data)) {
    warning("The data contains NA values. Omitting these observations. Alternatively, you can also handle NA values using imputation techniques like mean, median, or mode.")
    data <- na.omit(data)
  }

  if (threshold > 0.9) {
    warning("The threshold value for correlation is very high. Consider using a lower threshold for better predictor selection.")
  }

  if (top_k > ncol(data) - 1) {  # Subtract 1 for the target column
    stop("The specified value of top_k exceeds the number of predictors. Setting top_k to the maximum possible value.")
  }

  if (is.numeric(data[[target_col]])) {
    y <- data[[target_col]]
  } else {
    y <- factor(data[[target_col]])
  }

  if (technique == "correlation") {
    correlations <- cor(data[, -which(names(data) == target_col)], y, method = method)
    if (is.matrix(correlations)) {
      correlations <- setNames(as.vector(correlations), colnames(data[, -which(names(data) == target_col)]))
    }
    if (any(is.na(correlations))) {
      correlations <- na.omit(correlations)
    }
    correlations <- sort(abs(correlations), decreasing = TRUE)  # Sort correlation values in descending order
    strong_predictors <- names(correlations)[1:min(top_k, length(correlations))]  # Select top_k predictors
    strong_predictors <- strong_predictors[abs(correlations) >= threshold]  # Select predictors with correlation absolute value >= threshold
  } else if (technique == "univariate_fs") {
    p_values <- sapply(names(data)[-which(names(data) == target_col)], function(x) {
      if (is.numeric(data[[x]])) {
        test_result <- t.test(data[[x]] ~ data[[target_col]])
        return(test_result$p.value)
      } else {
        return(1)
      }
    })
    p_values <- sort(p_values)  # Sort the p-values in ascending order
    strong_predictors <- names(p_values)[1:min(top_k, length(p_values))]  # Select top_k predictors
    strong_predictors <- strong_predictors[p_values < threshold]  # Select predictors with p-value less than threshold
  }else if (technique == "lasso") {
    x <- as.matrix(data[, names(data) != target_col])
    y <- data[[target_col]]
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10, family = "gaussian")
    lambda_optimal <- cv_model$lambda.min
    final_model <- glmnet(x, y, alpha = 1, lambda = lambda_optimal, intercept = FALSE)
    coef_info <- coef(final_model, s = "lambda.min")
    strong_predictors <- rownames(coef_info)[coef_info[, 1] != 0, drop = FALSE]

  } else {
    stop("Invalid technique. Please choose 'correlation', 'univariate_fs', or 'lasso'.")
  }

  if (length(strong_predictors) == 0) {
    cat("No predictors selected based on the specified threshold and technique.\n")
    return(NULL)
  }

  top_predictors <- strong_predictors[1:min(top_k, length(strong_predictors))]
  print(top_predictors)

  build_model <- ""
  while (build_model != "yes" && build_model != "no") {
    build_model <- tolower(readline(prompt = "Do you want to build the train_model function? (yes/no): "))
    if (build_model != "yes" && build_model != "no") {
      print("Invalid input. Please enter 'yes' or 'no'.")
    }
  }

  if (build_model == "yes") {
    cat("Select a model type for training:\n")
    cat("1. Linear\n")
    cat("2. Logistic\n")
    cat("3. Ridge\n")
    cat("4. Lasso\n")
    cat("5. Elastic_net\n")
    cat("6. Random_forest\n")

    model_type_input <- as.integer(readline(prompt = "Enter the model type (1-6): "))
    model_types <- c("linear", "logistic", "ridge", "lasso", "elastic_net", "random_forest")

    if (model_type_input < 1 || model_type_input > length(model_types)) {
      stop("Invalid model type selected.")
    }

    chosen_model_type <- model_types[model_type_input]
    cat("You have chosen to train a", chosen_model_type, "model.\n")

    # Train the chosen model using the selected predictors
    train_data <- data[, c(top_predictors, target_col)]
    model_res <- train_model(as.formula(paste(target_col, "~ .")), train_data, model_type = chosen_model_type)

    if (chosen_model_type %in% c("ridge", "lasso", "elastic_net")) {
      model_summary <- coef(model_res)
    } else if (chosen_model_type %in% c("linear", "logistic")) {
      model_summary <- summary(model_res)
    } else if (chosen_model_type == "random_forest") {
      model_summary <- importance(model_res)
    }

    return(list(selected_predictors = top_predictors, model = model_res, summary = model_summary))
  } else {
    print("Okay, moving on without building the train_model function, just returning the top predictors")
    return(selected_predictors = top_predictors)
  }
}
