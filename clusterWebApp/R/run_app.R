utils::globalVariables(c("Dim1", "Dim2", "Cluster", "name", "value", "k"))
#' Launch the Shiny Clustering Web App
#'
#' This function launches the Shiny web application located in the \code{inst/app} directory
#' of the installed package. The application provides an interactive interface for clustering analysis.
#'
#' @return No return value. This function is called for its side effect (launching the app).
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#'
#' @importFrom shiny runApp
#' @importFrom utils packageName
#' @importFrom magrittr %>%
#' @importFrom stats na.omit dist hclust cutree
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom shinythemes shinytheme
#' @importFrom shinycssloaders withSpinner
#' @export
run_app <- function() {
  pkg <- utils::packageName()
  app_dir <- system.file("app", package = pkg)

  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Unable to find the Shiny app directory. Please ensure that 'inst/app' contains ui.R and server.R.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
