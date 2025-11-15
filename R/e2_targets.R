# P8 ----------------------------------------------------------------------


e2p8_targets <- function() {
  tar_plan(
    penguins_train = fread("data/ex_2/penguins_train.csv"),
    penguins_test = fread("data/ex_2/penguins_test.csv"),
    tar_target(
      e2p8_a,
      {
        # to factor
        penguins_train[, species := as.factor(species)]
        penguins_test[, species := as.factor(species)]
        
        # fit
        fit <- glm(species ~ ., data = penguins_train, family = "binomial")
        coef_fit <- coef(fit)
        
        # predict
        train_probs <- predict(fit, newdata = penguins_train, type = "response")
        train_pred <- ifelse(train_probs > 0.5 , "notAdelie", "Adelie")
        train_acc <- mean(train_pred == penguins_train$species)
        
        test_probs <- predict(fit, newdata = penguins_test, type = "response")
        test_pred <- ifelse(test_probs > 0.5, "notAdelie", "Adelie")
        test_acc <- mean(test_pred == penguins_test$species)
        
        train_probs_lin <- predict(fit, newdata = penguins_train, type = "link")
        
        list(
          penguins_train = penguins_train,
          penguins_test = penguins_test,
          fit = fit,
          train_probs = train_probs,
          train_pred = train_pred,
          train_acc = train_acc,
          test_probs = test_probs,
          test_pred = test_pred,
          test_acc = test_acc,
          train_probs_lin = train_probs_lin
        )
      }
    ),
    tar_target(
      e2p8_a_plot,
      {
        t_train <- e2p8_a$train_probs_lin
        p_train <- e2p8_a$train_probs
        penguins_train <- e2p8_a$penguins_train
        
        plot_path <- "data/plots/e2p8a.png"
        png(plot_path, width = 800, height = 600)
        
        plot(t_train, p_train,
             col = ifelse(penguins_train$species == "Adelie", "blue", "red"),
             pch = ifelse(penguins_train$species == "Adelie", 16, 4),
             xlab = "Linear response",
             ylab = "Probability",
             main = "Penguins Train Logistic Regression Fit")
        legend("topleft", legend = c("Adelie", "Not Adelie"),
               col = c("blue", "red"), pch = c(16, 4))
        
        
        dev.off()
        plot_path
      },
      format = "file" 
    ),
    tar_target(
      e2p8_b,
      {
        train <- e2p8_a$penguins_train
        test <- e2p8_a$penguins_test
        
        # Cross validate to get lambda
        set.seed(1)
        cv_fit <- cv.glmnet(species ~ ., data  = train, family = "binomial")
        
        # Try a lambda value
        lambda <- cv_fit$lambda[35]
        
        fit_lasso_reg <- glmnetUtils:::glmnet.formula(species ~ ., data  = train, family = "binomial", lambda = lambda)
        coef_lasso <- coef(fit_lasso_reg)
        
        # Training accuracy
        train_probs <- predict(fit_lasso_reg, newdata = train, type = "response")
        train_pred <- ifelse(train_probs > 0.5 , "notAdelie", "Adelie")
        train_acc <- mean(train_pred == train$species)
        
        # Testing accuracy
        test_probs <- predict(fit_lasso_reg, newdata = test, type = "response")
        test_pred <- ifelse(test_probs > 0.5, "notAdelie", "Adelie")
        test_acc <- mean(test_pred == penguins_test$species)
        
        list(
          train_acc = train_acc,
          test_acc = test_acc,
          coef_lasso = coef_lasso,
          fit = fit_lasso_reg
        )
        
      }
    )
  )
}


# E2 ALL ------------------------------------------------------------------


e2_targets <- function() {
  list(
    e2p8_targets(),
    tar_quarto(e2_report, "reports/exercise-set-2.qmd")
  )
}
