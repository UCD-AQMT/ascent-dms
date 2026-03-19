#' Deploy app within inst directory to shinyapps.io via rsconnect
#'
#' @param file Folder location for app (e.g., "/inst/apps/ascent-operations")
#' @param title Name of the app (will be part of the url)
#' @param ... Further parameters passed to rsconnect::deployApp (e.g., account)
#'
#' @returns
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


#' Title
#'
#' @param file 
#' @param ... 
#'
#' @returns
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