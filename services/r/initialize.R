# Set up renv
Sys.setenv(
  RENV_PATHS_RENV = file.path("./services/r/renv/"),
  RENV_PATHS_LOCKFILE = file.path("./services/r/renv.lock")
)

renv::init(bare = TRUE, restart = FALSE)

# Load package
devtools::load_all("./services/r/ascentr/")