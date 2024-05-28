#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @import DT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  output$data_table <- DT::renderDT({
    random_DT(5, 5)
  })
  output$plot <- renderPlot({
    random_ggplot()
  })
  output$text <- renderText({
    random_text(nwords = 50)
  })
}
