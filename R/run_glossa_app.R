#' Run Glossa Shiny App
#'
#' This function launches the Glossa Shiny web application.
#'
#' @param launch.browser launch browser or local R
#' @param port port for browse
#'
#' @details The Glossa Shiny app provides an interactive interface for users to access Glossa functionalities.
#'
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' run_glossa_app()
#' }
run_glossa_app <- function(request_size_mb = 2000, launch.browser = TRUE, port = getOption("shiny.port")) {
  options(shiny.maxRequestSize = request_size_mb * (1024^2))
  on.exit(rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv))
  return(shiny::runApp(appDir = system.file("app", package = "glossa"), launch.browser = launch.browser, port = port))
}
