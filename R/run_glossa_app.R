#' Run Glossa Shiny App
#'
#' This function launches the Glossa Shiny web application.
#'
#' @details The Glossa Shiny app provides an interactive interface for users to access Glossa functionalities.
#'
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' run_glossa_app()
#' }
run_glossa_app <- function(maxRequestSize = NULL) {
  if (!is.null(maxRequestSize)){
    options(shiny.maxRequestSize = maxRequestSize)
  }

  shiny::runApp(appDir = system.file("app", package = "glossa"))
}
