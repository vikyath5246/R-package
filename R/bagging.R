bag_model <- function(data, formula, model_type, R = 10) {

  if (any(sapply(data, function(x) !is.numeric(x)))) {
    cat("Warning: Non-numeric column(s) detected. Please convert to numeric.\n")
  }

  if(model_type %in% c("random_forest","linear","logistic")){
    if(model_type=="random_forest"){
      stop("The random forest model does not apply to bagging. Random Forest itself is an ensembling technique, which consists of bootstrapping.")
      stop("Please try to implement penalized regression. ")
    }
    if(model_type=="linear" || model_type=="logistic"){
      stop("Both linear and logistic regression models cannot be used for bagging. Bagging is typically applied to high variance models, but linear and logistic both are low variance models. Moreover you would get same coefficients for both of these models.")
    }
  }
  models <- vector("list", R)
  predictions <- vector("list", R)
  coefficients_list <- vector("list", R)
  n <- nrow(data)

  presence_matrix <- NULL
  x <- NULL

  if (anyNA(data)) {
    warning("The data contains NA values. Omitting these observations.")
    data <- na.omit(data)
  }


  if (model_type %in% c("lasso", "elastic_net", "ridge")) {
    x <- model.matrix(formula, data)
    presence_matrix <- matrix(0, ncol = ncol(x), nrow = R)
  }
  # print("finished")

  for (i in 1:R) {
    sampled_data <- data[sample(1:n, size = n, replace = TRUE), ]
    # print(sampled_data)
    model <- train_model(formula, sampled_data, model_type = model_type)
    models[[i]] <- model

    # print("finished")
    # if (model_type %in% c("linear", "logistic")) {
    #   pred <- predict(model, newdata = sampled_data, type="response")
    # } else {
    #   pred <- predict(model, newx = x, type = "response")
    # }
    pred <- predict(model, newx = x, type = "response")
    predictions[[i]] <- pred

    if (model_type %in% c("lasso", "elastic_net", "ridge")) {
      coefs <- coef(model, s = "lambda.min")[-1]  # Exclude intercept
      active_coefs <- which(coefs != 0)
      presence_matrix[i, active_coefs] <- 1
      coefficients_list[[i]] <- coefs
    }
  }

  avg_predictions <- rowMeans(do.call(cbind, predictions), na.rm = TRUE)
  # print(avg_predictions)
  if(model_type %in% c("linear","logistic")){
    warning("Since, there is no way to check for variable selection when we perform bagging, so it is recommneded that you do not do bagging for this type of models, Instead we recommend you that please try the penalized models for bagging.")
    warning("There are going to be NA Values as well, as there is no proper way to handle linear and logistic when it comes to bagging. Please try penalized regression.")
  }
  if (model_type %in% c("lasso", "elastic_net", "ridge")) {
    naive_importance <- colSums(presence_matrix) / R
    colnames_naive_importance <- colnames(x)

    avg_coefficients <- colMeans(do.call(rbind, lapply(coefficients_list, function(coef) {
      as.numeric(coef[!is.na(coef)])
    })), na.rm = TRUE)
    # print("Variables and their naive importance scores:")
    # print(data.frame(Variable = colnames_naive_importance, Importance = naive_importance))

    return(list(
      avg_predictions = avg_predictions,
      avg_coefficients = data.frame(Variable = colnames_naive_importance, avg_coefficients = avg_coefficients),
      naive_importance = data.frame(Variable = colnames_naive_importance, Importance = naive_importance)
    ))
  # } else if(model_type %in% c("linear", "logistic")) {
  #
      # return(list(
  #     avg_predictions = avg_predictions
  #   ))
  # }
  }
}
