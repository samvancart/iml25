# P1 ----------------------------------------------------------------------


e1p1_targets <- function() {
  list(
    tar_target(
      name = e1p1_dt,
      command = fread("data/iml25-term-project/train.csv")
    ),
    tar_target(
      e1p1_a,
      e1p1_dt[, c("id", "partlybad") := NULL]
    ),
    tar_target(
      e1p1_b,
      summary(e1p1_a[, c("T84.mean","UV_A.mean","CS.mean")])
    ),
    tar_target(
      e1p1_c,
      {
        t84 <- e1p1_a$T84.mean
        t84_mean <- mean(t84)
        t84_sd <- sd(t84)
        list(t84_mean = t84_mean, t84_sd = t84_sd)
      }
    ),
    tar_target(
      e1p1_d,
      {
        file <- "data/plots/ex1_p1_d.PNG"
        png(file, width = 600, height = 400)
        par(mfrow = c(1, 2))
        barplot(table(e1p1_a$class4), main = "Barplot of class4")
        hist(e1p1_a$CO242.mean, main = "Histogram of CO242.mean", xlab = "CO242.mean")
        dev.off()
        file
      },
      format = "file"
    ),
    tar_target(
      e1p1_e,
      {
        dt <- e1p1_a[, c("UV_A.mean","T84.mean","H2O84.mean")]
        file <- "data/plots/ex1_p1_e.PNG"
        png(file, width = 600, height = 400)
        pairs(dt) # Scatterplot matrix
        dev.off()
        file
      }
    ),
    tar_target(
      e1p1_f,
      {
        train_dt <- copy(e1p1_dt)
        test_dt <- fread("data/iml25-term-project/test.csv")
        
        most_common_class <- names(sort(table(train_dt$class4), decreasing = TRUE))[1]
        
        event_count <- sum(train_dt$class4 != "nonevent") # Count how many rows are events (class4 != "nonevent")
        total_count <- nrow(train_dt) # Calculate relative frequency
        
        p_event <- event_count / total_count
        
        test_dt$class4 <- most_common_class # Assign the most common class to all rows
        test_dt$p <- p_event # Assign the same event probability to all rows
        test_dt[, c("id", "class4", "p")]
      }
    ),
    tar_target(
      e1p1_f_file,
      {
        out_path <- paste0("data/iml25-term-project/dummy_predictions.csv")
        fwrite(e1p1_f, file = out_path)
        out_path
      },
      format = "file"
    )
  )
}


# P2 ----------------------------------------------------------------------


e1p2_targets <- function() {
  tar_plan(
    e1p2_train_syn = fread("data/ex_1/train_syn.csv"),
    e1p2_test_syn = fread("data/ex_1/test_syn.csv"),
    e1p2_valid_syn = fread("data/ex_1/valid_syn.csv"),
    e1p2_trva = rbind(e1p2_train_syn, e1p2_valid_syn),
    e1p2_deg = 0:8,
    tar_group_by(
      e1p2_params_dt,
      {
        dt <- data.table(
          train_data = list(e1p2_train_syn, e1p2_train_syn, e1p2_train_syn, e1p2_trva, e1p2_trva),
          test_data = list(e1p2_train_syn, e1p2_valid_syn, e1p2_test_syn, e1p2_test_syn, e1p2_trva),
          name = c("Train", "Validation", "Test", "TestTRVA", "CV"),
          cv_err = c(rep(FALSE, 4), TRUE),
          k = c(rep(1,4), 10)
        )
        dt[, row_id := .I]
        dt_expanded <- CJ(row_id = dt$row_id, deg = e1p2_deg)[dt, on = "row_id"]
        dt_expanded[, row_id := .GRP, by = .I]
      },
      row_id
    ),
    tar_target(
      e1p2_results,
      {
        params <- list(train_data = e1p2_params_dt$train_data[[1]],
                       test_data = e1p2_params_dt$test_data[[1]],
                       deg = e1p2_params_dt$deg,
                       name = e1p2_params_dt$name,
                       cv_err = e1p2_params_dt$cv_err,
                       k = e1p2_params_dt$k)
        do.call(p2_controller, params)
      },
      pattern = map(e1p2_params_dt),
      iteration = "list"
    ),
    tar_target(
      e1p2_a,
      {
        dt_long <- rbindlist(e1p2_results)
        dt <- dcast.data.table(dt_long, as.formula("deg ~ name"))
        setnames(dt, old = "deg", new = "Degree")
        setcolorder(dt, c("Degree", "Train", "Validation", "Test", "TestTRVA", "CV"))
        setkey(dt, NULL)
      }
    ),
    tar_target(
      e1p2a_min_vals,
      {
        cols <- c("Validation", "CV")
        
        rbindlist(lapply(cols, function(col) {
          e1p2_a[which.min(get(col)), .(Metric = col, Degree, Value = get(col))]
        }))
      }
    ),
    tar_target(
      e1p2_b_plots,
      {
        p <- c(0:4, 8)
        plot_path <- "data/plots/e1p2_b_grid.png"
        png(plot_path, width = 1200, height = 800)
        par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
        
        for (deg in p) {
          glm_fit_result <- glm_fit(train_data = e1p2_train_syn, deg = deg)
          x <- e1p2_train_syn$x
          y <- e1p2_train_syn$y
          x_grid <- seq(from = -3, to = 3, length.out = 256)
          y_pred <- predict(glm_fit_result, newdata = data.table(x = x_grid))
          
          plot(x, y, main = paste("p =", deg), xlab = "x", ylab = "y", pch = 19, col = "blue", cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.8)
          lines(x_grid, y_pred, col = "black", lwd = 2)
        }
        
        dev.off()
        
        plot_path
      },
      format = "file"
    ),
    e1p2_train_real = fread("data/ex_1/train_real.csv"),
    e1p2_test_real = fread("data/ex_1/test_real.csv"),
    tar_group_by(
      e1p2_c_params_dt,
      {
        dt <- data.table(
          data1 = list(e1p2_train_real, e1p2_train_real, e1p2_train_real),
          data2 = list(e1p2_train_real, e1p2_test_real, e1p2_train_real),
          name = c("Train", "Test", "CV"),
          cv_err = c(rep(FALSE, 2), TRUE)
        )
        dt[, row_id := .I]
        dt2 <- data.table(
          regressor = c("Dummy", "OLS", "RF", "SVR", "GBM"),
          caret_method = c("null", "lm", "rf", "svmRadial", "gbm")
        )
        dt[, key := 1]
        dt2[, key := 1]
        
        # Cross join row_id with model_map
        dt_expanded <- merge(dt, dt2, by = "key", allow.cartesian = TRUE)[, key := NULL]
        dt_expanded[, row_id := .GRP, by = .I]
      },
      row_id
    ),
    tar_target(
      e1p2_c_results,
      {
        params <- list(data1 = e1p2_c_params_dt$data1[[1]],
                       data2 = e1p2_c_params_dt$data2[[1]],
                       name = e1p2_c_params_dt$name,
                       cv_err = e1p2_c_params_dt$cv_err,
                       regressor = e1p2_c_params_dt$regressor,
                       caret_method = e1p2_c_params_dt$caret_method
                       )
        do.call(p2_c_fit_controller, params)
      },
      pattern = map(e1p2_c_params_dt),
      iteration = "list"
    ),
    tar_target(
      e1p2_c,
      {
        dt_long <- rbindlist(e1p2_c_results)
        dt <- dcast.data.table(dt_long, as.formula("regressor ~ name"), value.var = "rmse")
        regressor_order <- c("Dummy", "OLS", "RF", "SVR", "GBM")
        dt[, regressor := factor(regressor, levels = regressor_order)]
        setorder(dt, regressor)
        setnames(dt, old = "regressor", new = "Regressor")
        setcolorder(dt, c("Regressor", "Train", "Test", "CV"))
        setkey(dt, NULL)
      }
    ),
    tar_target(
      e1p2c_min_vals,
      {
        cols <- c("Train", "Test","CV")
        
        rbindlist(lapply(cols, function(col) {
          e1p2_c[which.min(get(col)), .(Metric = col, Regressor, Value = get(col))]
        }))
      }
    )
  )
}


# P3 ----------------------------------------------------------------------


e1p3_targets <- function() {
  tar_plan(
    tar_target(
      e1p3_train,
      {
        set.seed(1)
        n <- 1000
        
        train_list <- lapply(seq(n), function(i) {
          x <- runif(n = 10, -3, 3)
          e <- rnorm(10, 0, 0.4)
          fx <- get_e1p3_fx(x)
          y <- fx+e
          data.table(y = y, x = x)
        })
        train_list
      }
    ),
    tar_group_by(
      e1p3_runtable,
      {
        set.seed(1)
        e0 <- rnorm(1000, 0, 0.4)
        deg <- c(0:6)
        dt1 <- as.data.table(expand.grid(train=e1p3_train, deg=deg))
        dt1[, row_id := .GRP, .I]
        dt2 <- as.data.table(expand.grid(e0=e0, deg=deg))
        dt2[, row_id := .GRP, .I]
        
        dt <- merge(dt1[, c("row_id", "train")], dt2, by = c("row_id"))
        
      },
      row_id
    ),
    tar_target(
      e1p3_pred,
      {
        set.seed(1)
        
        f0 <- get_e1p3_fx(0)
        e0 <- e1p3_runtable$e0
        y0 <- f0 + e0
        fit_args <- list(train_data = e1p3_runtable$train[[1]], deg = e1p3_runtable$deg)
        fit <- do.call(glm_fit, fit_args)
        f_hat0 <- predict(fit, newdata = data.table(x = 0))
        data.table(f0 = f0, y0 = y0, f_hat0 = f_hat0, deg = e1p3_runtable$deg)
      },
      pattern = map(e1p3_runtable)
    ),
    tar_target(
      e1p3_b1,
      {
        dt <- e1p3_pred[, .(
          BiasSq = (mean(f_hat0) - mean(f0))^2,
          Variance = var(f_hat0),
          Irreducible = var(y0 - f0)
        ), by = deg]
        dt[, Total := BiasSq + Variance + Irreducible]
        dt[, MSE := BiasSq + Variance + Irreducible]
        setnames(dt, old = "deg", new = "Degree")
      }
    ),
    tar_target(
      e1p3_b2,
      {
        png_path <- "data/plots/e1p3_b2.png"
        png(png_path, width = 800, height = 800)
        
        par(mfrow = c(2, 2))
        
        # Squared Loss (MSE)
        plot(e1p3_b1$Degree, e1p3_b1$MSE, type = "n",
             main = "Squared Loss (MSE)", xlab = "Polynomial Degree", ylab = "MSE")
        lines(spline(e1p3_b1$Degree, e1p3_b1$MSE, n = 256), lwd = 2)
        points(e1p3_b1$Degree, e1p3_b1$MSE, pch = 16)
        
        # Irreducible Error
        plot(e1p3_b1$Degree, e1p3_b1$Irreducible, type = "n",
             main = "Irreducible Error", xlab = "Polynomial Degree", ylab = "Error")
        lines(spline(e1p3_b1$Degree, e1p3_b1$Irreducible, n = 256), col = "red", lwd = 2)
        points(e1p3_b1$Degree, e1p3_b1$Irreducible, col = "red", pch = 16)
        
        # Bias Squared
        plot(e1p3_b1$Degree, e1p3_b1$BiasSq, type = "n",
             main = "Bias Squared", xlab = "Polynomial Degree", ylab = "Bias²")
        lines(spline(e1p3_b1$Degree, e1p3_b1$BiasSq, n = 256), col = "blue", lwd = 2)
        points(e1p3_b1$Degree, e1p3_b1$BiasSq, col = "blue", pch = 16)
        
        # Variance
        plot(e1p3_b1$Degree, e1p3_b1$Variance, type = "n",
             main = "Variance", xlab = "Polynomial Degree", ylab = "Variance")
        lines(spline(e1p3_b1$Degree, e1p3_b1$Variance, n = 256), col = "green", lwd = 2)
        points(e1p3_b1$Degree, e1p3_b1$Variance, col = "green", pch = 16)
        
        dev.off()
        
        png_path
        
      },
      format = "file"
    )
  )
}


# P5 ----------------------------------------------------------------------


e1p5_targets <- function() {
  tar_plan(
    d_paths = list.files("data/ex_1/", pattern = "^d[0-9]+\\.csv$", full.names = T),
    tar_target(
      d_data,
      {
       list(
         data =  fread(d_paths),
         name = unlist(tstrsplit(basename(d_paths), split = "\\.", keep = 1))
       )
      },
      pattern = map(d_paths),
      iteration = "list"
    ),
    tar_target(
      e1p5a_fit,
      {
        data = d_data$data
        fit = lm(data[["y"]] ~ data[["x"]])
        list(
          data = d_data$data,
          fit = fit,
          name = d_data$name
        )
      },
      pattern = map(d_data),
      iteration = "list"
    ),
    tar_target(
      e1p5a_dts,
      {
        fit = e1p5a_fit$fit
        summ_fit <- summary(fit)
        dt <- data.table(
          intercept = summ_fit$coefficients[1],
          slope_term = summ_fit$coefficients[2],
          std_error_int = summ_fit$coefficients[3],
          std_error_slope = summ_fit$coefficients[4],
          p_value = summ_fit$coefficients[8],
          r_squared = summ_fit$r.squared
        )
        list(
          data = d_data$data,
          fit = fit,
          summary_dt = dt,
          summ_fit = summ_fit,
          name = d_data$name
        )
      },
      pattern = map(e1p5a_fit,d_data),
      iteration = "list"
    ),
    tar_target(
      e1p5a_dt,
      {
        dts <- lapply(e1p5a_dts, function(item) {
          dt <- item$summary_dt
          dt[, data_name := item$name]
          dt
        })
        rbindlist(dts)
      }
    ),
    tar_target(
      e1p5b,
      {
        plot_path <- "data/plots/e1p5.png"
        png(plot_path, width = 800, height = 800)
        par(mfrow = c(2, 2))
        
        for(i in seq(e1p5a_dts)) {
          item <- e1p5a_dts[[i]]
          data <- item$data
          name <- item$name
          summary_dt <- item$summary_dt
          intercept <- summary_dt$intercept
          slope_term <- summary_dt$slope_term
          
          plot(data$x, data$y, main = name, xlab = "x", ylab = "y", pch = 19, col = "blue")
          
          abline(a = intercept, b = slope_term, col = "black", lwd = 2)
        }
        
        dev.off()
        plot_path
      },
      format = "file"
    ),
    tar_target(
      e1p5c,
      {
        plot_path <- "data/plots/e1p5c_residuals.png"
        png(plot_path, width = 800, height = 800)
        par(mfrow = c(2, 2))
        
        for(i in seq(e1p5a_dts)) {
          item <- e1p5a_dts[[i]]
          data <- item$data
          name <- item$name
          fit <- item$fit
          
          res <- residuals(fit)
          
          x <- data$x
          
          plot(x, res,
               xlab = "Predictor x",
               ylab = "Residuals",
               main = "Residuals vs Predictor",
               pch = 19, col = "blue")
          
          lines(lowess(x, res), col = "black", lwd = 2)
          
          abline(h = 0, col = "red", lwd = 2, lty = 2)
        }
        
        dev.off()
        plot_path
      },
      format = "file"
    )
  )
}


# E1 ALL ------------------------------------------------------------------


# List all e1 targets
e1_targets <- function() {
  list(
    e1p1_targets(),
    e1p2_targets(),
    e1p3_targets(),
    e1p5_targets(),
    tar_quarto(e1_report, "data/reports/exercise-set-1.qmd")

  )
}





