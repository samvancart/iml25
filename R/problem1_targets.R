problem1_targets <- function() {
  list(
    tar_target(
      name = prob1_dt,
      command = fread("data/iml25-term-project/train.csv")
    ),
    tar_target(
      task_a_dt,
      prob1_dt[, c("id", "partlybad") := NULL]
    )
  )
}