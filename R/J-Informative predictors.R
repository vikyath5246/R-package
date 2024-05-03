'Here we are going to use the top_k selection, since this would be mostly when the numbver of rows are greater than the number of columns'
'So here we would discuss 3 methods correlation, lasso and t-test'
'So lets each of these.'

topk_selector <- function(X, y, technique = "correlation", method = "pearson", threshold = 0.05, top_k = 2) {

  'We will check if our libraries are installed or not, if they are not installed, then we would print error messages.'
  required_libraries <- c("glmnet", "randomForest", "stats")
  missing_libraries <- required_libraries[!required_libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries, dependencies = TRUE)
  }
  lapply(required_libraries, library, character.only = TRUE)

  'If we have any NA values, we are going to omit them in this dataset'
  'We are going to adjust the sizes'
  if (anyNA(X) || anyNA(y)) {
    warning("The dataset contains NA values. We are removing them. You can also compute it statistically and then run the function.")
    complete_cases <- complete.cases(X, y)
    X <- X[complete_cases, ]
    y <- y[complete_cases]
  }

  'If the threshold increases our expectations then we would print a warning'
  if (threshold > 0.9) {
    warning("The threshold value is very high. Consider using a lower threshold for better predictor selection.")
  }

  'We will stop if the number of columns of top k are exceeding our columns in dataset.'
  if (top_k > ncol(X)) {
    stop("The specified value of top_k exceeds the number of predictors. Please check ")
  }

  results <- list()

  if (technique == "correlation") {
    'for correlation what we did we tried to find the correlation between each and evry variable'
    ' Now the next that we did is we sorted them in an order'
    'The next thing is we just took out the top 1:k predictors and we let the results out'
    correlations <- cor(X, y, method = method)
    if (is.matrix(correlations)) {
      correlations <- setNames(as.vector(correlations), colnames(X))
    }
    correlations <- sort(abs(correlations), decreasing = TRUE)
    top_indices <- which(correlations >= threshold)
    top_predictors <- names(correlations)[top_indices][1:min(top_k, length(top_indices))]
    results$Predictors <- top_predictors
    results$Values <- correlations[top_predictors]
  } else if (technique == "t-test") {
    'We are now going to try t-test'
    'So the t test basically what we are trying to do it we will compute each column against the target variable'
    'After that accordingly we will check the p-values'
    'Generally t-test is good for binary classified dataset'
    'Here what we are going to do we are going to sort the p-values'
    'Then accordingly we are going to choos ethe top_k in that function'
    p_values <- sapply(colnames(X), function(col_name) {
      if (length(unique(y)) != 2) {
        stop("Target variable must have exactly 2 levels for t-test. Sincet-test is only done for binary setting")
      }
      test_result <- t.test(X[[col_name]] ~ y)
      return(test_result$p.value)
    })
    p_values <- sort(p_values)
    top_indices <- which(p_values < threshold)
    top_predictors <- names(p_values)[top_indices][1:min(top_k, length(top_indices))]
    results$Predictors <- top_predictors
    results$Values <- p_values[top_predictors]
  } else if (technique == "lasso") {
    'We are now going to try our lasso'
    'So first what we will see is we will compute the family and the lambda values'
    'Compute the lasso regression'
    'So generally the speciality of lasso regression is it is used to find the topk predictors.'
    'Now, the next step is we have to fit in the lasso regression'
    'and then accordingly we will compute the topk values'

    x <- as.matrix(X)
    if (is.factor(y) || all(y %in% c(0, 1))) {
      family_type <- "binomial"
    } else {
      family_type <- "gaussian"
    }
    # print(family_type)
    cv_model <- cv.glmnet(x, y, alpha = 1, nfolds = 10, family = family_type)
    lambda_optimal <- cv_model$lambda.min
    final_model <- glmnet(x, y, alpha = 1, lambda = lambda_optimal, intercept = FALSE)
    coef_info <- coef(final_model, s = "lambda.min")
    significant_coefs <- which(abs(coef_info[, 1]) > 0)
    sorted_coefs <- sort(abs(coef_info[significant_coefs, 1]), decreasing = TRUE)
    top_predictors <- names(sorted_coefs[length(sorted_coefs):max(length(sorted_coefs) - top_k + 1, 1)])
    results$Predictors <- top_predictors
    results$Values <- coef_info[top_predictors, 1]
  }

  else {
    stop("Invalid technique. Please choose 'correlation', 't-test', or 'lasso'.")
  }

  if (length(results$Predictors) == 0) {des
    cat("No predictors were selected based on the threshold. Please try changing the threshold.")
    return(NULL)
  }
  'We are going to return them in a list of values and predictors'
  return(results)
}
