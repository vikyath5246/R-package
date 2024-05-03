#' ensemble prediction
#'
#' The `ensemble_prediction` function implements ensemble prediction, combining predictions from multiple models with optional weights.
#'
#' @param X The dataset to train the ensemble prediction model on without the dependent variable
#' @param y The dependent variable of our dataset.
#' @param models A character vector specifying the types of base models to be used for ensemble prediction.
#' @param weights Optional weights for combining predictions from different models. If not provided, equal weights are used.
#' @param output To exit from the console return "4"
#' @return A list containing the weighted predictions, predictions from individual models, and model coefficients.
#'
#' @examples
#' library(MASS)
#' data("Boston")
#' y <- Boston$medv
#' X <- Boston[, !names(Boston) %in% "medv"]
#' ensemble_prediction(X, y, models = c("linear", "lasso", "random_forest"))
#'
#'
#' @export
ensemble_prediction <- function(X, y, models, weights = NULL) {
  # Function body...
}

