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
    plotly::plotlyOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @param id Internal parameter for {shiny}.
#' @param dat A reactive expression that returns the data to be plotted.
#' @param selected_channels A reactive expression that returns the selected channels.
#' @param plotly_source The source id for plotly events.
#' @noRd
mod_plot_server <- function(id, dat, selected_channels, plotly_source) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$plot <- plotly::renderPlotly({
      req(dat())
      req(selected_channels())

      # Extract plot data from a `ggm` object
      plotData <- ggm(dat())$data
      channelNames <- selected_channels()

      # Simplify dataset and create simple ggplot
      # This is a data.table operation
      # Color is set to white to match the dark theme
      g <-
        plotData[plotData$label %in% channelNames, ] |>
        ggplot(aes(x = sample, y = mV)) +
        geom_line(color = "white") +
        facet_wrap(~ label, ncol = 1, scales = "free_y", strip.position = "left") +
        EGM::theme_egm_dark() # Simplify the background colors


      # Make plotly
      p <-
        g |>
        plotly::ggplotly(source = plotly_source,
                         dynamicTicks = TRUE,
                         originalData = TRUE)

      # Adjust settings and anchoring of text in the layout
      #   The first layer is always "sample", the second is "mV"
      for (i in 1:length(p$x$layout$annotations)) {

        ann <- p$x$layout$annotations[[i]]

        # Only apply edits to labels for facets
        if (ann$text %in% channelNames) {

          # Color, shape, size
          ann$textangle <- 0
          ann$font$color <- "rgba(255,255,255,1)"

          # X position
          ann$xref <- "paper"
          ann$x <- -0.01
          ann$xanchor <- "right"

          # Y position
          ann$yref <- "paper"
          ann$yanchor <- "middle"


        }

        p$x$layout$annotations[[i]] <- ann
      }

      # Add final flourishes prior to returning
      p |>
        plotly::layout(
          # Add left spacing margin
          margin = list(l = 80)
        ) |>
        plotly::layout(dragmode = "zoom") |>
        plotly::event_register("plotly_click")

    })

  })
}

## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
