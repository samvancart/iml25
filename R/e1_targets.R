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
    ),
    tar_quarto(e1_report, "data/reports/exercise-set-1.qmd")
  )
}

e1p2_targets <- function() {
  
}



# List all e1 targets
e1_targets <- function() {
  list(
    e1p1_targets()
  )
}





