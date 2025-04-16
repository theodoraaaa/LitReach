#' LitReach
#'
#' Runs the LitReach App
#' @return The LitReach shiny app
#' @examples
#' LitReach()
#' @export
LitReach <- function() {
  app.path <- fs::path_package("R", "app.R", package = "LitReach")

  source(app.path)
  addResourcePath("assets", system.file("www", package = "LitReach"))
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
