#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id, dat, selected_channels) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({
      req(dat(), selected_channels())
      g <-
        EGM::ggm(dat()) +
        EGM::theme_egm_dark()

      g$data <- g$data[g$data$label %in% selected_channels(),]

      # Return plot
      g
    })

  })
}

## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
