install.packages(c("targets", "tarchetypes", "visNetwork"))
setwd("~/Desktop/dissertation")
getwd()

# ============================================
# Dissertation project scaffolding
# Safe to run multiple times — won't overwrite existing files
# ============================================

# 1. Create directories (recursive, no error if exists)
dirs <- c(
  "R",
  "data/derived",
  "output/figures",
  "output/tables",
  "output/models",
  "writing"
)

for (d in dirs) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 2. Create R/ function file stubs
r_files <- c(
  "R/load.R",       # Raw data ingestion functions
  "R/clean.R",      # Cleaning functions
  "R/harmonise.R",  # Country code, education crosswalks
  "R/merge.R",      # Dataset construction
  "R/analyse.R",    # H1-H4 model functions
  "R/plot.R"        # Figures
)

for (f in r_files) {
  if (!file.exists(f)) {
    writeLines(
      paste0("# ", basename(f), " — functions for ", 
             tools::file_path_sans_ext(basename(f)), " step\n"),
      f
    )
  }
}

# 3. Create _targets.R skeleton (only if it doesn't exist)
if (!file.exists("_targets.R")) {
  writeLines(c(
    "library(targets)",
    "library(tarchetypes)",
    "",
    "tar_source(\"R/\")",
    "",
    "tar_option_set(",
    "  packages = c(\"tidyverse\", \"haven\", \"readxl\", \"fixest\", \"mgcv\",",
    "               \"xgboost\", \"iml\", \"brms\", \"DoubleML\",",
    "               \"countrycode\", \"mice\", \"wcde\"),",
    "  format = \"rds\"",
    ")",
    "",
    "# ============================================",
    "# PIPELINE",
    "# ============================================",
    "list(",
    "  # Targets will be added here as we build the pipeline",
    ")"
  ), "_targets.R")
}

# 4. Create a .gitignore so we don't commit large data or auto-generated files
if (!file.exists(".gitignore")) {
  writeLines(c(
    "# Targets internals",
    "_targets/",
    "",
    "# Raw data (too big for git)",
    "data/raw/",
    "",
    "# Derived data (regenerable from pipeline)",
    "data/derived/",
    "",
    "# Model outputs",
    "output/models/",
    "",
    "# R/RStudio",
    ".Rhistory",
    ".RData",
    ".Rproj.user/",
    "",
    "# macOS",
    ".DS_Store"
  ), ".gitignore")
}

# 5. Report
cat("\n✓ Project structure created.\n\n")
cat("Folder tree:\n")
fs::dir_tree(".", recurse = 1)

# Show ALL files including hidden ones in current directory
list.files(".", all.files = TRUE, no.. = TRUE)
usethis::create_project(".", open = FALSE)

