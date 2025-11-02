problem1_targets <- function() {
  list(
    tar_target(
      name = prob1_dt,
      command = fread("data/iml25-term-project/train.csv")
    ),
    tar_target(
      task_a_dt,
      prob1_dt[, c("id", "partlybad") := NULL]
    ),
    tar_target(
      task_b,
      summary(task_a_dt[, c("T84.mean","UV_A.mean","CS.mean")])
    ),
    tar_target(
      task_c,
      {
        t84 <- task_a_dt$T84.mean
        t84_mean <- mean(t84)
        t84_sd <- sd(t84)
        list(t84_mean = t84_mean, t84_sd = t84_sd)
      }
    ),
    tar_target(
      task_d,
      {
        file <- "data/plots/ex1_p1_d.PNG"
        png(file)
        par(mfrow = c(1, 2))
        barplot(table(task_a_dt$class4))
        hist(task_a_dt$CO242.mean, main = "Histogram of CO242.mean", xlab = "CO242.mean")
        dev.off()
        file
      },
      format = "file"
    ),
    tar_quarto(e1_report, "data/reports/exercise-set-1.qmd")
  )
}










