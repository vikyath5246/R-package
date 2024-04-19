main_model <- function(formula, data, model_type = "random_forest", family = NULL, lambda = NULL, bagging = 1) {

  # When bagging is selected
  if(bagging > 1) {
    model <- bagged_models(formula, data, family, model_type, bagging)
  }
  # When bagging is not selected
  else {
    model <- train_model(formula, data, model_type, family, lambda)
  }
  return(model);
}

