# install.R — one-off setup. Run once before launching the app:
#   Rscript install.R
#
# Installs every CRAN package the app needs. Safe to re-run; already-
# installed packages are skipped.

required <- c(
  "shiny",       # the Shiny framework
  "leaflet",     # interactive map
  "dplyr",       # data wrangling
  "sf",          # spatial data for the choropleth
  "networkD3",   # Sankey diagram in the constituency detail
  "testthat",    # for running the unit tests
  "rsconnect"    # for deploying to shinyapps.io
)

missing <- required[!(required %in% installed.packages()[, "Package"])]
if (length(missing) == 0) {
  message("All required packages already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
