# Set up renv
Sys.setenv(
  RENV_PATHS_RENV = file.path("./services/r/ascent-operations/renv/"),
  RENV_PATHS_LOCKFILE = file.path("./services/r/ascent-operations/renv.lock")
)

renv::init(bare = TRUE, restart = FALSE)

# Deploy to shinyapps.io

rsconnect::deployApp(
  appDir = "./services/r/ascent-operations/",
  appName = "ascent-operations",
  account = "ascent"
)
