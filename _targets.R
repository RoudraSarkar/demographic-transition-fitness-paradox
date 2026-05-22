library(targets)
library(tarchetypes)

tar_source("R/")

tar_option_set(
  packages = c("tidyverse", "haven", "readxl", "fixest", "mgcv",
               "xgboost", "iml", "brms", "DoubleML",
               "countrycode", "mice", "wcde"),
  format = "rds"
)

# ============================================
# PIPELINE
# ============================================
list(
  # Targets will be added here as we build the pipeline
)
