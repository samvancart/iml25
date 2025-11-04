# P2 ----------------------------------------------------------------------

#  Fit linear model with glm from boot lib. Deg is an int.
glm_fit <- function(train_data, deg, x_col = "x", y_col = "y", seed = 1) {
  set.seed(seed)
  x <- train_data[[x_col]]
  y <- train_data[[y_col]]
  glm_fit_result <- glm(y ~ poly(x, degree = deg), data = train_data)
  
  glm_fit_result
}

# Calculate mean squared error.
calc_mse <- function(data, glm_fit, y_col = "y", seed = 1) {
  set.seed(seed)
  y <- data[[y_col]]
  mse <- mean((y - predict(glm_fit, data))^2)
  
  mse
}

# Calculate k-fold cross-validation on a linear model computed using
# the above glm_fit function
calc_cv_error <- function(data, glm_fit, deg, k = 10, seed = 1) {
  set.seed(seed)
  cv_err <- with(list(deg = deg), cv.glm(data, glm_fit, K = k))
  
  cv_err$delta[1]
}


p2_controller <- function(train_data, test_data, deg, cv_err = FALSE, x_col = "x", y_col = "y", k = 10, seed = 1) {
  
  set.seed(seed)
  
  glm_fit_result <- glm_fit(train_data = train_data, deg = deg, seed = seed)
  if(cv_err) {
    err <- calc_cv_error(data = train_data, glm_fit =  glm_fit_result, deg = deg, k = k, seed = seed)
  } else {
    err <- calc_mse(data = test_data, glm_fit = glm_fit_result, seed = seed)
  }
  err
}













