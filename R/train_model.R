train_model <- function(formula, data, model_type = "random_forest", family = NULL, lambda = NULL) {

  model <- switch(
    model_type,
    linear = lm(formula, data = data),
    logistic = {
      if (length(unique(data[[all.vars(formula)[[1]]]])) > 2)
        warning("If the response variable is binary, it would give great results")
      glm(formula, data = data, family = binomial)
    },
    ridge = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for ridge regression")
      if (is.null(lambda)) stop("lambda argument must be provided for ridge regression")
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[all.vars(formula)[[1]]]], alpha = 0, lambda = lambda, family = family)
    },
    lasso = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for lasso regression")
      if (is.null(lambda)) stop("lambda argument must be provided for lasso regression")
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[all.vars(formula)[[1]]]], alpha = 1, lambda = lambda, family = family)
    },
    elastic_net = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for elastic net")
      if (is.null(lambda)) stop("lambda argument must be provided for elastic net")
      x <- model.matrix(formula, data)
      if (ncol(x) < 2) warning("x should be a matrix with 2 or more columns")
      glmnet::glmnet(x, y = data[[all.vars(formula)[[1]]]], alpha = 0.5, lambda = lambda, family = family)
    },
    svm = {
      if (!requireNamespace("e1071", quietly = TRUE)) stop("Required package 'e1071' not available")
      e1071::svm(formula, data = data)
    },
    random_forest = {
      randomForest::randomForest(formula, data = data,cv=TRUE)
    },
    stop("Invalid model type")
  )
  return(model)
}
