#' LitReach
#'
#' Runs the LitReach App
#' @return The LitReach shiny app
#' @examples
#' LitReach()
#' @export
LitReach <- function() {
  source(here::here("R", "app.R"))
  addResourcePath("assets", system.file("www", package = "LitReach"))
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
