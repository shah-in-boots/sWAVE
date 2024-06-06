#' annotate_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotate_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("annotation_button"), "Enable Annotation"),
    DT::DTOutput(ns("annotation_table"))
  )
}

#' annotate_plot Server Functions
#'
#' @noRd
mod_annotate_plot_server <- function(id, selected_channels, plotly_source) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    annotation_mode <- reactiveVal(FALSE)
    annotations <- reactiveVal(data.frame(Channel = character(), Time = numeric()))

    # Switch annotation mode to TRUE if clicked
    observeEvent(input$annotation_button, {
      annotation_mode(!annotation_mode())
    })

    observeEvent(plotly::event_data("plotly_click", source = plotly_source), {
      req(annotation_mode())
      req(selected_channels())

      clickData <- plotly::event_data("plotly_click", source = plotly_source)

      # Check to see if click_data is correct
      print("Click data:")
      print(clickData)

      # Get ordered channels
      orderedLabels <- EGM:::.labels
      channelNames <- selected_channels()
      orderedChannels <- channelNames[match(orderedLabels, channelNames, nomatch = 0)]

      print("Ordered channels: ")
      print(orderedChannels)

      curveNumber <- clickData$curveNumber + 1

      if (curveNumber <= length(orderedChannels)) {
        selectedChannel <- orderedChannels[curveNumber]
      } else {
        selectedChannel <- "Unknown"
      }

      print("Channel: ")
      print(selectedChannel)

      newAnnotation <- data.frame(Channel = selectedChannel,
                                  Time = clickData$x)

      print("New annotation:")
      print(newAnnotation)

      # Update annotations
      currentAnnotations <- annotations()
      updatedAnnotations <- rbind(currentAnnotations, newAnnotation)
      annotations(updatedAnnotations)

      print("Annotations reactive value after update:")  # Debug print
      print(annotations())  # Debug print to verify update
    })

    output$annotation_table <- DT::renderDT({
      DT::datatable(annotations(), selection = "single", editable = TRUE)
    })

  })
}

## To be copied in the UI
# mod_annotate_plot_ui("annotate_plot_1")

## To be copied in the server
# mod_annotate_plot_server("annotate_plot_1")
