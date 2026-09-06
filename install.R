# Install every R package the ETL and the Shiny app need.
# Usage:  Rscript install.R
options(repos = c(CRAN = "https://cloud.r-project.org"))

pkgs <- c(
  # app
  "shiny", "bslib", "plotly", "reactable", "shinyWidgets", "htmlwidgets",
  # ETL / data
  "data.table", "dplyr", "tidyr", "lubridate", "stringr", "readr",
  "arrow", "rlang", "tibble", "scales", "R.utils"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All required packages already installed.")
}

# arrow is optional -- data_prep.R falls back to .rds if it is missing
missing_after <- setdiff(pkgs, rownames(installed.packages()))
missing_after <- setdiff(missing_after, "arrow")
if (length(missing_after)) {
  stop("Failed to install: ", paste(missing_after, collapse = ", "))
}
message("Dependencies ready.")
