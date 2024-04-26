#' Run Glossa Shiny App
#'
#' This function launches the Glossa Shiny web application.
#'
#' @param maxRequestSize To increase the maximum file size allowed for uploading in Shiny (default 5Mb)
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
