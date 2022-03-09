#' Run the Shiny Application
#' @param ... additional options passed to shinyApp()
#' @export
#' @importFrom shiny shinyApp
run_app <- function(...) {
  shinyApp(ui, server, options = list(launch.browser = TRUE), ...)
}
