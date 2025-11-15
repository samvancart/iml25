# LOAD LIBS ---------------------------------------------------------------


# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)


# SET TARGET OPTIONS ------------------------------------------------------


tar_option_set(
  packages = c("data.table", "tinytex", "quarto", "knitr", 
               "boot", "randomForest", "e1071", "caret", "kernlab", "gbm",
               "glmnet", "glmnetUtils"),
  controller = crew::crew_controller_local(workers = 8, seconds_idle = 60)

)


# SOURCE ------------------------------------------------------------------


tar_source()


# TARGETS -----------------------------------------------------------------


list(
  e1_targets(),
  e2_targets()
)
