# P2 ----------------------------------------------------------------------

#  Fit linear model with glm from boot lib. Param deg is an int.
glm_fit <- function(train_data, deg, x_col = "x", y_col = "y", seed = 1) {
  set.seed(seed)
  x <- train_data[[x_col]]
  y <- train_data[[y_col]]
  
  if(deg == 0) {
    glm_fit_result <- glm(y ~ 1, data = train_data)
  } else {
    glm_fit_result <- glm(y ~ poly(x, degree = deg), data = train_data) 
  }
  
  glm_fit_result
}

# Calculate mean squared error.
calc_mse <- function(data, glm_fit, y_col = "y", seed = 1) {
  set.seed(seed)
  y <- data[[y_col]]
  mse <- mean((y - predict(glm_fit, data))^2)
  
  mse
}

calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate k-fold cross-validation on a linear model computed using
# the above glm_fit function
calc_cv_error <- function(data, glm_fit, deg, k = 10, seed = 1) {
  set.seed(seed)
  cv_err <- with(list(deg = deg), cv.glm(data, glm_fit, K = k))
  
  cv_err$delta[1]
}

# Fit polynomial model with order = deg and return data.table with deg, name and MSE.
# The MSE is calculated from either the fitted model or from a k-fold cross-validation function's output.
p2_controller <- function(train_data, test_data, deg, name, cv_err = FALSE, x_col = "x", y_col = "y", k = 10, seed = 1) {
  
  set.seed(seed)
  
  glm_fit_result <- glm_fit(train_data = train_data, deg = deg, x_col = x_col, y_col = y_col, seed = seed)

  if(cv_err) {
    err <- calc_cv_error(data = train_data, glm_fit =  glm_fit_result, deg = deg, k = k, seed = seed)
  } else {
    err <- calc_mse(data = test_data, glm_fit = glm_fit_result, seed = seed)
  }
  data.table(deg = deg, name = name, err = err)
}

# Fit a model (using the caret library) defined in paramater caret_model and optionally use 10-fold cross_validation (cv_err = TRUE).
# Return the RMSE in a data.table also including name and regressor.
p2_c_fit_controller <- function(
    data1,
    data2,
    name,
    regressor,
    y_col = "Next_Tmax",
    formula = NULL,
    caret_method = "lm",
    cv_err = FALSE,
    seed = 1,
    ...
) {
  set.seed(seed)
  
  if(is.null(formula)) {
    formula <- as.formula(paste(y_col, "~ ."))
  }
  
  ctrl <- if (cv_err) {
    trainControl(method = "cv", number = 10)
  } else {
    trainControl(method = "none")
  }
  
  model <- train(form = formula, data = data1, method = caret_method, trControl = ctrl, ...)
  
  rmse <- if(cv_err) {
    mean(model$resample$RMSE)
  } else {
    pred <- predict(model, newdata = data2)
    actual <- data2[[y_col]]
    sqrt(mean((actual - pred)^2))
  }
  
  data.table(regressor = regressor, name = name, rmse = rmse)
}











