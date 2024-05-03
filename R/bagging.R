bagging <- function(X, y, model_type="lasso", R = 10) {
  'Chceck for libraries again'
  required_libraries <- c("glmnet", "randomForest")
  missing_libraries <- required_libraries[!required_libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries, dependencies = TRUE)
    message("Installed the required libraries.")
  }
  lapply(required_libraries, library, character.only = TRUE)

  'Now here we check valid models, so that we can build for invalid model_type'
  valid_models <- c("lasso", "elastic_net", "ridge", "linear", "logistic", "random_forest")
  if (!model_type %in% valid_models) {
    stop("Invalid model type. Please use one of the following: ", paste(valid_models, collapse = ", "))
  }
  'First we make a dataframe of our data X and Y  for easiness'
  data <- data.frame(X, y)
  'Now then we set the formula of our glmnet models that we are going to use. '
  formula <- as.formula(paste("y ~", paste(names(X), collapse = "+")))

  'We then find the family for our binomial and gaussian data'
  'We can then try set the family'
  unique_y <- unique(y)
  if (all(unique_y %in% c(0, 1)) && length(unique_y) == 2) {
    family <- "binomial"
  } else {
    family <- "gaussian"
  }

  'Now as usual we check out our categorical variables'
  if (any(sapply(X, function(x) !is.numeric(x)))) {
    warning("The columns contain categorical values. Please convert them to numeric values, else the regression might not fit well.\n")
  }

  'If model is random forest, linear, logistic we stop the code'
  if (model_type == "random_forest") {
    stop("Random Forest itself is an ensembling technique, which includes bagging. Please use another model type for bagging.")
  }
  if(model_type=="linear"||model_type=="logistic"){
    stop("The linear and logistic generally have low variance bias. So generally we do not do bootstrapping for linear and logistic. Moreover the naive score is always 1 when cmomputed. So please try penalized regression")
  }

  'Now we are ready to fit the models , so we take a bunch of new variables for our storage of data'
  'Since we are passing our code into glmnet we are trying to fit in our data'
  fitted_models <- vector("list", R)
  predictions <- vector("list", R)
  coefficients_list <- vector("list", R)
  X <- model.matrix(formula, data)[,-1]  # Exclude intercept

  ' Now lets see if we are now running intopenalized regression we will set the matrix_coef to 0 for updation of every value'
  if (model_type %in% c("lasso", "elastic_net", "ridge")) {
    matrix_coef <- matrix(0, ncol = ncol(X), nrow = R)
  }

  'Now we are going to do is bootstrapping'
  'So probably we will keep taking R samples and then we keep running them each and every time with R samples'
  'So probably here we keep fitting the model, first we look for the lambda value and then we try to fit the model'
  'Once we fit the model we try to gasp some certain parameters of it.'
  'Once we are we done we start calculating avg_predictions of R times'
  for (i in 1:R) {
    sampled_data <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    x_sampled <- X[sampled_data, , drop = FALSE]
    y_sampled <- y[sampled_data]

    cv_fit <- cv.glmnet(x_sampled, y_sampled, alpha = ifelse(model_type == "ridge", 0, ifelse(model_type == "lasso", 1, 0.5)), family = family)
    fitted_models[[i]] <- cv_fit
    pred <- predict(cv_fit, newx = X, s = "lambda.min", type = "response")
    predictions[[i]] <- pred

    if (model_type %in% c("lasso", "elastic_net", "ridge")) {
      coefs <- coef(cv_fit, s = "lambda.min")[-1]
      active_coefs <- which(coefs != 0)
      matrix_coef[i, active_coefs] <- 1
      coefficients_list[[i]] <- coefs
    }
  }

  'Now here we will calculate naive importance score, which is like the column being repeated by R times * 100'
  'We are trying to build the avg_coefficients as well and then we return them. '
  avg_predictions <- rowMeans(do.call(cbind, predictions), na.rm = TRUE)
  if (model_type %in% c("lasso", "elastic_net", "ridge")) {
    naive_importance <- (colSums(matrix_coef) / R)*100
    colnames_naive_importance <- colnames(X)

    avg_coefficients <- colMeans(do.call(rbind, lapply(coefficients_list, function(coef) {
      as.numeric(coef[!is.na(coef)])
    })), na.rm = TRUE)

    return(list(
      avg_predictions = avg_predictions,
      avg_coefficients = data.frame(Variable = colnames_naive_importance, avg_coefficients = avg_coefficients),
      naive_importance = data.frame(Variable = colnames_naive_importance, Importance = naive_importance)
    ))
  }
}
