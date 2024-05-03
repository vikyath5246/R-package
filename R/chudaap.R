# results <- ensemble_regression(airquality, "Ozone", c("linear", "lasso", "ridge", "random_forest"))
# results <- ensemble_regression(mtcars,"am",c("logistic","lasso","ridge","random_forest"))


#kaam karra
# select <- predictor_selection(airquality, "Ozone", technique="correlation",threshold=0, top_k=6)
# select <- predictor_selection(ChickWeight, "weight", technique="correlation",threshold=0, top_k=6)
# select <- predictor_selection(swiss, "Fertility", technique="correlation",threshold=0, top_k=6)
# select <- predictor_selection(mtcars, "am", technique="correlation",threshold=0, top_k=3)
#
# select <- predictor_selection(airquality, "Ozone", technique="univariate_fs",threshold=0, top_k=3)
# select <- predictor_selection(ChickWeight, "weight", technique="univariate_fs",threshold=0, top_k=3)
# select <- predictor_selection(swiss, "Fertility", technique="univariate_fs",threshold=0, top_k=3)
# select <- predictor_selection(mtcars, "am", technique="univariate_fs",threshold=0, top_k=3)
#
# select <- predictor_selection(airquality, "Ozone", technique="lasso",threshold=0, top_k=3)
# select <- predictor_selection(ChickWeight, "weight", technique="lasso",threshold=0, top_k=3)
# select <- predictor_selection(swiss, "Fertility", technique="lasso",threshold=0, top_k=3)
# select <- predictor_selection(mtcars, "am", technique="lasso",threshold=0, top_k=3)


#train log ko chod k w
# data("airquality")
# result <- train_model(Ozone ~.,airquality,model_type="linear")
# result <- train_model(Ozone ~ .,airquality,model_type="logistic")
# result <- train_model(Ozone ~.,airquality,model_type="ridge")
# result <- train_model(Ozone ~.,airquality,model_type="lasso")
# result <- train_model(Ozone ~.,airquality,model_type="elastic_net")
# result <- train_model(Ozone ~.,airquality,model_type="random_forest")

# data("faithful")
# result <- train_model(waiting ~ eruptions, faithful, model_type="linear")
# result <- train_model(waiting ~ eruptions, faithful, model_type="logistic")
# result <- train_model(waiting ~ eruptions, faithful, model_type="ridge")
# result <- train_model(waiting ~ eruptions, faithful, model_type="lasso")
# result <- train_model(waiting ~ eruptions, faithful, model_type="elastic_net")
# result <- train_model(waiting ~ eruptions, faithful, model_type="random_forest")

# data("ChickWeight")
# result <- train_model(weight ~ ., ChickWeight, model_type="linear")
# result <- train_model(weight ~ ., ChickWeight, model_type="logistic")
# result <- train_model(weight ~ ., ChickWeight, model_type="ridge")
# result <- train_model(weight ~ ., ChickWeight, model_type="lasso")
# result <- train_model(weight ~ ., ChickWeight, model_type="elastic_net")
# result <- train_model(weight ~ ., ChickWeight, model_type="random_forest")

# data("swiss")
# result <- train_model(Fertility ~ ., swiss, model_type="linear")
# # result <- train_model(Fertility ~ ., swiss, model_type="logistic")
# result <- train_model(Fertility ~ ., swiss, model_type="ridge")
# result <- train_model(Fertility ~ ., swiss, model_type="lasso")
# result <- train_model(Fertility ~ ., swiss, model_type="elastic_net")
# result <- train_model(Fertility ~ ., swiss, model_type="random_forest")

# data("mtcars")
# result <- train_model(am ~ ., mtcars, model_type="linear")
# result <- train_model(am ~ ., mtcars, model_type="logistic")
# result <- train_model(am ~ ., mtcars, model_type="ridge")
# result <- train_model(am ~ ., mtcars, model_type="lasso")
# result <- train_model(am ~ ., mtcars, model_type="elastic_net")
# result <- train_model(am ~ ., mtcars, model_type="random_forest")


# lasso_results <- bag_model(airquality, Ozone~., "lasso", R = 10)
# lasso_results <- bag_model(airquality, Ozone~., "ridge", R = 100)
