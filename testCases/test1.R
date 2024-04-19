# Load required libraries
library(randomForest)  # For generating random forest models
library(glmnet)        # For ridge, lasso, and elastic net regression

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n_rows <- 100
n_cols <- 10  # Reduced number of columns for demonstration

# Generate predictor variables (columns) with different distributions
predictor_data <- data.frame(
  age = sample(18:80, n_rows, replace = TRUE),                # Age distributed uniformly between 18 and 80
  income = rnorm(n_rows, mean = 50000, sd = 10000),           # Income normally distributed with mean 50000 and sd 10000
  education_years = rpois(n_rows, lambda = 12),              # Years of education Poisson distributed with mean 12
  employment_status = sample(c("Employed", "Unemployed", "Self-employed"), n_rows, replace = TRUE),  # Categorical variable for employment status
  marital_status = sample(c("Single", "Married", "Divorced", "Widowed"), n_rows, replace = TRUE),     # Categorical variable for marital status
  health_condition = sample(c("Excellent", "Good", "Fair", "Poor"), n_rows, replace = TRUE),           # Categorical variable for health condition
  num_children = sample(0:5, n_rows, replace = TRUE),        # Number of children distributed between 0 and 5
  credit_score = runif(n_rows, min = 300, max = 850),        # Credit score uniformly distributed between 300 and 850
  has_loan = sample(c(TRUE, FALSE), n_rows, replace = TRUE), # Binary variable indicating whether the individual has a loan
  num_credit_cards = sample(0:5, n_rows, replace = TRUE)     # Number of credit cards distributed between 0 and 5
)

# Generate target variable (response) based on predictor variables
# For example, let's create a target variable representing monthly expenses
data <- predictor_data
data$monthly_expenses <- with(data, 500 + 100 * age + rnorm(n_rows, mean = 0, sd = 5000))

# Test the train_model function with different model types
# For example, train a linear regression model
linear_model <- train_model(monthly_expenses ~ ., data = data)
print(summary(linear_model))

# Train a random forest model
random_forest_model <- train_model(monthly_expenses ~ ., data = data, model_type = "random_forest")
print(random_forest_model)

# Train a ridge regression model
ridge_model <- train_model(monthly_expenses ~ ., data = data, model_type = "ridge", family = "gaussian", lambda = 0.1)
print(summary(ridge_model))

# Train a logistic regression model (for classification tasks)
# Note: Ensure the target variable is binary for logistic regression
# For demonstration purposes, let's convert monthly expenses to a binary variable indicating high or low expenses
data$high_expenses <- ifelse(data$monthly_expenses > median(data$monthly_expenses), TRUE, FALSE)
logistic_model <- train_model(high_expenses ~ ., data = data, model_type = "logistic")
print(summary(logistic_model))
