#' Deploy app within inst directory to shinyapps.io via rsconnect
#'
#' @param file Folder location for app (e.g., "/inst/apps/ascent-operations")
#' @param title Name of the app (will be part of the url)
#' @param ... Further parameters passed to rsconnect::deployApp (e.g., account)
#'
#' @returns Invisibly returns `NULL`; called for its side effect of deploying
#'   the app to shinyapps.io
#' @export
#'
#' @examples deploy("inst/apps/ascent-operations", "ascent_ops", account = "ascent")
deploy <- function(file, title, ...) {
  writeLines(
    c(
      'pkgload::load_all("./services/r/ascentr/")',
      #'options(shiny.autoload.r = FALSE)',
      sprintf('shiny::shinyAppDir("%s")', file)
    ),
    "app.R"
  )
  on.exit(unlink("app.R"))
  rsconnect::deployApp(
    appPrimaryDoc = "app.R",
    appName = title,
    appTitle = title,
    ...
  )
}


#' Run an app within the inst directory locally
#'
#' Writes a temporary `app.R` that loads the package and launches the
#' specified Shiny app directory, then runs it locally via
#' `shiny::runApp()`.
#'
#' @param file Folder location for app (e.g., "/inst/apps/ascent-operations")
#' @param ... Further parameters passed to `shiny::runApp()`
#'
#' @returns Invisibly returns `NULL`; called for its side effect of running
#'   the app
#' @export
#'
#' @examples
deploy_local <- function(file, ...) {
  writeLines(
    c(
      'pkgload::load_all("./services/r/ascentr/")',
      #'options(shiny.autoload.r = FALSE)',
      sprintf('shiny::shinyAppDir("%s")', file)
    ),
    "app.R"
  )
  on.exit(unlink("app.R"))
  shiny::runApp(
    appDir = "app.R",
    ...
    )
}
