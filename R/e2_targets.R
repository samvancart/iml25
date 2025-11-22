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
    ),
    tar_target(
      e2p10a,
      {
        train <- e2p8_a$penguins_train
        
        means <- train[, lapply(.SD, mean), by = species]
        means[, variable := "mean"]
        sds <- train[,lapply(.SD, sd), by = species]
        sds[, variable := "sd"]
        
        counts <- train[, .N, by = "species"]
        pseudo <- 1
        trials <- sum(counts$N)
        counts[, smoothed := (N + pseudo/trials + pseudo * 2), by = "species"]
        counts[, N := NULL]
        counts[, total := sum(smoothed)]
        counts[, class_prob := smoothed/total, by = "species"]
        counts <- counts[, c("species", "class_prob")]
        counts[, variable := "laplace"]
        
        means_long <- melt.data.table(means, id.vars = c("species", "variable"), variable.name = "name")
        sd_long <- melt.data.table(sds, id.vars = c("species", "variable"), variable.name = "name")
        counts_long <- melt.data.table(counts, id.vars = c("species", "variable"), variable.name = "name")
        
        rbindlist(list(means_long, sd_long, counts_long))
        
      }
    ),
    tar_target(
      e2p10c,
      {
        
        # Extract only the mean and sd rows from training stats
        feature_dt <- e2p10a[variable %in% c("mean", "sd")]
        
        # Extract the prior (laplace-smoothed class probabilities)
        prob_dt <- e2p10a[variable %in% c("laplace")]
        
        # Reshape training stats wide: one row per species × feature,
        # with columns "mean" and "sd"
        params <- dcast(e2p10a[variable %in% c("mean","sd")],
                        species + name ~ variable,
                        value.var="value")
        
        # Load the test set
        test <- e2p8_a$penguins_test
        
        # Ensure numeric columns are properly typed
        test[, flipper_length_mm := as.numeric(flipper_length_mm)]
        test[, body_mass_g := as.numeric(body_mass_g)]
        
        # Add a unique id for each test observation
        test[, id := as.numeric(.I)]
        
        # Melt test set into long format: one row per id × feature
        # Columns: species (true label), id, name (feature name), x (observed value)
        melted_test <- melt.data.table(test,
                                       id.vars = c("species", "id"),
                                       variable.name = "name",
                                       value.name = "x")
        
        # Create a Cartesian grid of all test ids × all species
        # This ensures each test observation will be evaluated under both class models
        grid <- CJ(id = unique(melted_test$id),
                   species = unique(params$species))
        
        # Join grid with test features (on id) → duplicates each test row for each species
        # Then join with training parameters (on species + feature name)
        # Result: fk_dt has id, species, feature name, observed x, mean, sd
        fk_dt <- grid[melted_test, on="id", allow.cartesian=TRUE][
          params, on=.(species, name)
        ]
        
        # Compute densities
        fk_dt[, fk := dnorm(x, mean, sd)]
        
        # Collapse across features per id/species
        likelihoods <- fk_dt[, .(likelihood = prod(fk)), by=.(id, species)]
        
        # Attach priors
        priors <- prob_dt[, .(species, prior=value)]
        likelihoods <- merge(likelihoods, priors, by="species")
        
        # Score = likelihood × prior
        likelihoods[, score := likelihood * prior]
        
        # Normalize across species per id
        likelihoods[, posterior := score / sum(score), by=id]
        
        # Predicted class per id
        pred <- likelihoods[, .SD[which.max(posterior)], by=id]
        
        # Merge predicted species with true species from test set
        results <- merge(pred[, .(id, predicted_species = species)],
                         test[, .(id, true_species = species)],
                         by="id")
        
        # Flag correct predictions
        results[, correct := (predicted_species == true_species)]
        
        # Overall accuracy
        accuracy <- results[, mean(correct)]
        
        list(
          accuracy = accuracy,
          prediction = pred
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
