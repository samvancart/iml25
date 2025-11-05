# P1 ----------------------------------------------------------------------


e1p1_targets <- function() {
  list(
    tar_target(
      name = p1_dt,
      command = fread("data/iml25-term-project/train.csv")
    ),
    tar_target(
      p1_a,
      p1_dt[, c("id", "partlybad") := NULL]
    ),
    tar_target(
      p1_b,
      summary(p1_a[, c("T84.mean","UV_A.mean","CS.mean")])
    ),
    tar_target(
      p1_c,
      {
        t84 <- p1_a$T84.mean
        t84_mean <- mean(t84)
        t84_sd <- sd(t84)
        list(t84_mean = t84_mean, t84_sd = t84_sd)
      }
    ),
    tar_target(
      p1_d,
      {
        file <- "data/plots/ex1_p1_d.PNG"
        png(file, width = 600, height = 400)
        par(mfrow = c(1, 2))
        barplot(table(p1_a$class4), main = "Barplot of class4")
        hist(p1_a$CO242.mean, main = "Histogram of CO242.mean", xlab = "CO242.mean")
        dev.off()
        file
      },
      format = "file"
    ),
    tar_target(
      p1_e,
      {
        dt <- p1_a[, c("UV_A.mean","T84.mean","H2O84.mean")]
        file <- "data/plots/ex1_p1_e.PNG"
        png(file, width = 600, height = 400)
        pairs(dt) # Scatterplot matrix
        dev.off()
        file
      }
    ),
    tar_target(
      p1_f,
      {
        train_dt <- copy(p1_dt)
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
      p1_f_file,
      {
        out_path <- paste0("data/iml25-term-project/dummy_predictions.csv")
        fwrite(p1_f, file = out_path)
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



# E1 ALL ------------------------------------------------------------------


# List all e1 targets
e1_targets <- function() {
  list(
    e1p1_targets(),
    e1p2_targets(),
    tar_quarto(e1_report, "data/reports/exercise-set-1.qmd")
  )
}





