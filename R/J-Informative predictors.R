predictor_selection <- function(data, target_col, technique = "correlation", method = "pearson", threshold = 0.05, top_k = 2) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    install.packages("glmnet")
    warning("The 'glmnet' package is required but not installed. Installing it for you...")
  }

  if (!target_col %in% names(data)) {
    stop("The target column specified does not exist in the dataset.")
  }

  if (anyNA(data)) {
    warning("The data contains NA values. Omitting these observations. Alternatively, you can also handle NA values using imputation techniques like mean, median, or mode.")
    data <- na.omit(data)
  }

  if (technique == "correlation") {
    correlations <- cor(data[, -which(names(data) == target_col)], data[[target_col]], method = method)
    if (is.matrix(correlations)) {
      correlations <- setNames(as.vector(correlations), colnames(data[, -which(names(data) == target_col)]))
    }
    if (any(is.na(correlations))) {
      correlations <- na.omit(correlations)
    }

    if (threshold > 0.9) {
      warning("The threshold value for correlation is very high. Consider using a lower threshold for better predictor selection.")
    }

    strong_predictors <- names(correlations)[abs(correlations) >= threshold]

  } else if (technique == "univariate_fs") {
    p_values <- sapply(names(data)[-which(names(data) == target_col)], function(x) {
      model <- lm(formula = paste(target_col, "~", x), data = data)
      coef_summary <- summary(model)$coefficients
      if (is.na(coef_summary[x, 4])) return(1)
      coef_summary[x, 4]
    })
    strong_predictors <- names(p_values)[p_values < threshold]

  } else if (technique == "lasso") {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("The 'glmnet' package is required but not installed. Please install it before using the Lasso technique.")
    }

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

  top_predictors_list <- as.list(top_predictors)
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
    print(top_predictors)
    # Train the chosen model using the selected predictors
    train_data <- data[, c(top_predictors, target_col)]
    print(train_data)
    model <- train_model(as.formula(paste(target_col, "~ .")), train_data, model_type = chosen_model_type)

    model_summary <- summary(model)
    return(list(selected_predictors = top_predictors, model = model,summary = model_summary))
  } else {
    print("Okay, moving on without building the train_model function.")
    return(selected_predictors = top_predictors)
  }
}

# data(Boston)
#
# # Preview the dataset
# head(Boston)
#
# # Define the target column
# target_col <- "medv"  # Median value of owner-occupied homes in $1000s
#
# # Test the predictor_selection function with correlation technique
# selected_predictors_correlation <- predictor_selection(Boston, target_col, technique = "correlation", method = "pearson", threshold = 0.2, top_k = 4)
# print("Selected predictors using correlation technique:")
# print(selected_predictors_correlation)
#
# # Test the predictor_selection function with univariate feature selection technique
# selected_predictors_univariate_fs <- predictor_selection(Boston, target_col, technique = "univariate_fs", threshold = 0.05, top_k = 3)
# print("Selected predictors using univariate feature selection technique:")
# print(selected_predictors_univariate_fs)
#
# # Test the predictor_selection function with lasso technique
# selected_predictors_lasso <- predictor_selection(Boston, target_col, technique = "lasso",top_k=5)
# print("Selected predictors using lasso technique:")
# print(selected_predictors_lasso)
