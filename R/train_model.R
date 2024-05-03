train_model <- function(formula, data, model_type = "random_forest", family = NULL, lambda = NULL) {

  if (any(sapply(data, function(x) !is.numeric(x)))) {
    # Print a warning message
    cat("Warning: Non-numeric column(s) detected. Please convert to numeric.\n")
  }

  required_libraries <- c("glmnet", "randomForest")
  missing_libraries <- required_libraries[!required_libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries, dependencies = TRUE)
  }

  lapply(required_libraries, library, character.only = TRUE)

  if (!is.element(all.vars(formula)[[1]], names(data))) {
    stop("Target variable not found in the dataset.")
  }



  if (anyNA(data)) {
    warning("The data contains NA values. Omitting these observations.")
    data <- na.omit(data)
  }

  target_variable <- all.vars(formula)[[1]]
  x <- model.matrix(formula, data)
  y <- data[[target_variable]]



  maxit <- 1000

  if (model_type %in% c("ridge", "lasso", "elastic_net")) {
    if (is.null(family)) {
      family <- ifelse(length(unique(y)) <= 5, "binomial", "gaussian")
    }
    if (is.null(lambda)) {
      alpha <- ifelse(model_type == "lasso", 1, ifelse(model_type == "ridge", 0, 0.5))  # Default alpha for elastic net
      cv_fit <- glmnet::cv.glmnet(x, y, alpha = alpha, family = family)

      if (any(is.infinite(cv_fit$cvm))) {
        warning(paste("Infinite CV error encountered in", model_type, "model. Adjusting lambda selection."))
        lambda <- cv_fit$lambda.1se
      } else {
        lambda <- cv_fit$lambda.min
      }
    }

    if (model_type == "ridge") {
      alpha <- 0
    } else if (model_type == "lasso") {
      alpha <- 1
    } else if (model_type == "elastic_net") {
      alpha <- 0.5
    }
    # print(family)
    model <- glmnet::glmnet(x, y, alpha = alpha, lambda = lambda, family = family)
  } else {
    if (model_type == "logistic") {
      data[[target_variable]] <- factor(data[[target_variable]])
      # model <- glm(formula, data = data, family = binomial(), maxit = maxit)
    }
      if(model_type == "random_forest") {
      if (length(unique(y)) <= 4) {
        data[[target_variable]] <- factor(data[[target_variable]])
        model <- randomForest::randomForest(formula, data = data, ntree=500, mtry=sqrt(ncol(data)), classification = TRUE)
      } else {
        model <- randomForest::randomForest(formula, data = data, ntree = 500, mtry = sqrt(ncol(data)))
      }
    } else {
      model <- switch(
        model_type,
        "linear" = lm(formula, data = data),
        "logistic" = glm(formula, data = data, family = binomial(), maxit = maxit),
        stop("Invalid model type")
      )
    }
  }

  return(model)
}

