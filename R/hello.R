# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @param formula formula specifying the model (e.g., y ~ x1 + x2)
#' @param data data frame containing the variables
#' @param model_type type of model to train ('linear', 'logistic', 'ridge', 'lasso', 'elastic_net', 'svm', 'random_forest', or 'boosted_trees')
#' @param family family for glm models ('gaussian' for linear regression, 'binomial' for logistic regression)
#' @param lambda regularization parameter for ridge, lasso, and elastic net
#' @return fitted regression model
#' @export
choose_models <- function(formula, data, model_type = "random_forest", family = NULL, lambda = NULL) {
  model <- switch(
    model_type,
    linear = lm(formula, data = data),
    logistic = {
      if (length(unique(data[[all.vars(formula)[[1]]]])) > 2)
        stop("Response variable for logistic regression must be binary")
      glm(formula, data = data, family = binomial)
    },
    ridge = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for ridge regression")
      if (is.null(lambda)) stop("lambda argument must be provided for ridge regression")
      glmnet::glmnet(formula, data = as.matrix(data), alpha = 0, lambda = lambda, family = family)
    },
    lasso = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for lasso regression")
      if (is.null(lambda)) stop("lambda argument must be provided for lasso regression")
      glmnet::glmnet(formula, data = as.matrix(data), alpha = 1, lambda = lambda, family = family)
    },
    elastic_net = {
      if (is.null(family)) family <- "gaussian"
      if (!family %in% c("gaussian", "binomial")) stop("Invalid family argument for elastic net")
      if (is.null(lambda)) stop("lambda argument must be provided for elastic net")
      glmnet::glmnet(formula, data = as.matrix(data), alpha = 0.5, lambda = lambda, family = family)
    },
    svm = {
      if (!requireNamespace("e1071", quietly = TRUE)) stop("Required package 'e1071' not available")
      e1071::svm(formula, data = data)
    },
    random_forest = {
      randomForest::randomForest(formula, data = data)
    },
    stop("Invalid model type")
  )
  return(model)
}
## Anyc hanges that I can do to this?
## Team please do rigourous testing on this.
## Then next I'll show the TA and then we can discuss the next steps
##Tuesday doubt session with prof
##Baaki dekh lenge

##
