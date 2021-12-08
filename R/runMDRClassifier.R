#' Launch Shiny App for MDRClassifier
#'
#' A function that launches the Shiny app for MDRClassifier.
#' The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' MDRClassifier::runMDRClassifier()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp

runMDRClassifier <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "MDRClassifier")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
# [END]
