#' zoom_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zoom_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("zoom_plot"))
  )
}

#' zoom_plot Server Functions
#'
#' @noRd
mod_zoom_plot_server <- function(id, dat, selected_channels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shared_data <- reactive({
      req(dat())
      req(selected_channels())
      dat()
    })

    output$zoom_plot <- plotly::renderPlotly({
      shared_data_obj <- crosstalk::SharedData$new(shared_data(), key = ~channel)

      plot_data <- shared_data()
      channels <- selected_channels()

      # Create ggplot
      g <-
        EGM::ggm(plot_data, channels) +
        theme_egm_dark()

      # Convert to plotly with crosstalk for interactive zooming and panning
      plotly::ggplotly(g) %>%
        plotly::highlight("plotly_selected") |>
        plotly::layout(dragmode = "zoom")
    })
  })
}

## To be copied in the UI
# mod_zoom_plot_ui("zoom_plot_1")

## To be copied in the server
# mod_zoom_plot_server("zoom_plot_1")
