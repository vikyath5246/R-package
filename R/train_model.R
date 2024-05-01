train_model <- function(formula, data, model_type = "random_forest", family = NULL, lambda = NULL) {
  install_required_packages <- function(packages) {
    for (package in packages) {
      if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
        warning(paste("The '", package, "' package is required but not installed. Installing it for you...", sep = ""))
      }
    }
  }

  install_required_packages(c("glmnet", "randomForest"))
  library(glmnet)
  library(randomForest)

  if (anyNA(data)) {
    warning("The data contains NA values. Omitting these observations. Alternatively, you can also handle NA values using imputation techniques like mean, median, or mode.")
    data <- na.omit(data)
  }

  target_variable <- all.vars(formula)[[1]]

  if (model_type %in% c("ridge", "lasso", "elastic_net")) {
    if (is.null(family)) {
      unique_values <- unique(data[[target_variable]])
      num_unique <- length(unique_values)
      num_observations <- length(data[[target_variable]])

      if (num_unique == 2) {
        family <- "binomial"
      } else{
        family <- "gaussian"
      }
      cat("Family used for", model_type, "regression:", family, "\n")
    }
  }

  if (model_type %in% c("ridge", "lasso", "elastic_net") && is.null(lambda)) {
    x <- model.matrix(formula, data)
    if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
    cv_fit <- glmnet::cv.glmnet(x, y = data[[target_variable]], alpha = ifelse(model_type == "ridge", 0, ifelse(model_type == "lasso", 1, 0.5)), family = family)
    lambda <- cv_fit$lambda.min
  }

  model <- switch(
    model_type,
    "linear" = {
      lm_model <- lm(formula, data = data)
      f_statistic <- summary(lm_model)$fstatistic[1]
      r_squared <- summary(lm_model)$r.squared
      return(list(model = lm_model, f_statistic = f_statistic, r_squared = r_squared, summary = summary(lm_model)))
    },
    "logistic" = {
      data[[target_variable]] <- factor(data[[target_variable]])
      glm(formula, data = data, family = binomial())
    },
    "ridge" = {
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[target_variable]], alpha = 0, lambda = lambda, family = family)
    },
    "lasso" = {
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[target_variable]], alpha = 1, lambda = lambda, family = family)
    },
    "elastic_net" = {
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[target_variable]], alpha = 0.5, lambda = lambda, family = family)
    },
    "random_forest" = {
      model <- randomForest::randomForest(formula, data = data, ntree = 500, mtry = sqrt(ncol(data) - 1), importance = TRUE, classWeight = NULL, keep.inbag = FALSE)
    },
    stop("Invalid model type")
  )

  return(list(model = model))
}
