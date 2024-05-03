trainmodel <- function(y, X, model_type = "random_forest", family = NULL, lambda = NULL) {
  'We will also check for the column if they ar esignificantly greater than rows than we look for other options'
  data <- data.frame(X, y)
  if (ncol(data)/nrow(data) >5) {
    warning("The number of rows is significantly greater than the number of columns, which might affect model performance. We advise you to do penalized regression. The normal regression would not fit so well. You can even try top k predictors")
  }
  'This helps us find if there is any categorical data, if there is we print a warning there.'
  if (any(sapply(X, function(x) !is.numeric(x)))) {
    warning("There are categorical columns. Please convert them to numerical columns, otherwise the model may not fit correctly.")
  }

  'We check for missing libraries, if there are any missing libraries we try to install them.'
  'We only install two libraries glmnet and randomForest, since we require them to build ur models'
  required_libraries <- c("glmnet", "randomForest")
  missing_libraries <- required_libraries[!required_libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries, dependencies = TRUE)
    message("Installed the required libraries.")
  }
  lapply(required_libraries, library, character.only = TRUE)

  'We check for any NA Values, if there are any NA values we omit ourNA values from our dataset.'
  if (anyNA(X) || anyNA(y)) {
    warning("The dataset contains NA values. We are removing them. You can also compuet it statistically and then run the function")
    complete_cases <- complete.cases(X, y)
    X <- X[complete_cases, ]
    y <- y[complete_cases]
  }


  'The reason why we are doing model.matrix is to fit the the glmnet'
  X <- as.matrix(X)


  'This is a parameter used for logistic regression building'
  maxit =100000


  'Now lets start with ridge lasso and elastic_net'
  'we check for if lambda is null, if it is null we try to fit our own lambda which can be found using cv.'
  'Now also for family as well, we check if the value is less than 3, it is we give it binomial or else gaussian'
  'This helps us handle the binary and continuous datatype better.'
  'If we get our lambda infinite, we assign it a small value'
  'Then we fit our model alpha =0 for ridge, alpha =1 for lasso, alpha =0.5 for elastic_net'
  'Once we identify our model type we build our model using glmnet'
  if (model_type %in% c("ridge", "lasso", "elastic_net")) {
    alpha <- ifelse(model_type == "lasso", 1, ifelse(model_type == "ridge", 0, 0.5))
    if (is.null(family)) {
      family <- ifelse(length(unique(y)) <= 3, "binomial", "gaussian")
    }
    if (is.null(lambda)) {
      cv_fit <- glmnet::cv.glmnet(X, y, alpha = alpha, family = family)
      lambda <- if (any(is.infinite(cv_fit$cvm))) {
        warning(paste("Infinite CV error encountered in", model_type, "model. Adjusting lambda selection."))
        cv_fit$lambda.1se
      } else {
        cv_fit$lambda.min
      }
    }
    model <- glmnet::glmnet(X, y, alpha = alpha, lambda = lambda, family = family,intercept=FALSE)
  } else if (model_type == "linear") {
    'This here we simply handle linear regression'
    'So heree what we do is we simply try to use lm to build our linear regression model'
    data <- as.data.frame(cbind(y,X))
    model <- lm(y ~ ., data = data)
  } else if (model_type == "logistic") {
    'We check if our data is numeric or not, if not we try to you know build in the factor'
    'Then we build our nice little logistic regression.'
    if(is.numeric(y)) {
      y <- y
    } else {
      y <- factor(y)
    }
    model <- glm(y ~ ., data = data.frame(X,y), family = binomial(), maxit=maxit)
  } else if (model_type == "random_forest") {
    'So we check for first whther its a classification data or continuous data'
    'Then depending on that we give whether classification is True or not.'
    data <- as.data.frame(cbind(y,X))
    if (length(unique(y)) <= 4) {
      data[[1]] <- factor(data[[1]])
      model <- randomForest::randomForest(y ~ ., data = data, ntree = 500, mtry = sqrt(ncol(data)), classification = TRUE)
    } else {
      model <- randomForest::randomForest(y ~ ., data = data, ntree = 500, mtry = sqrt(ncol(data)))
    }
  }else {
    'If there is no such input we give invalid model type'
    stop("Invalid model type")
  }
  return(model)
}
