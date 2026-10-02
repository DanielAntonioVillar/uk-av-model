# deploy.R -------------------------------------------------------------
# One-off local script to publish the app to shinyapps.io.
#
# Before first use, get your account token from
# https://www.shinyapps.io/admin/#/tokens  and run (once):
#
#   rsconnect::setAccountInfo(
#     name   = "<your-username>",
#     token  = "<your-token>",
#     secret = "<your-secret>"
#   )
#
# Then run this script from the app root.
# ----------------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

# Confirm the baked data file exists — without it the deployed app
# will fall back to the synthetic fixture.
stopifnot(file.exists("data/baked.rds"))

rsconnect::deployApp(
  appDir       = ".",
  appName      = "uk-av-model",
  appTitle     = "UK 2024 under AV",
  # Only ship what the app actually needs; keep the raw sources local.
  appFiles     = c(
    "app.R",
    list.files("R", full.names = TRUE),
    "data/baked.rds"
  ),
  forceUpdate  = TRUE
)
