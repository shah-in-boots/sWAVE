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
    # Add color to the active annotation button
    tags$head(
      tags$style(HTML("
        .annotation-active {
          background-color: #fd9a44 !important;
          color: #FFFFFF !important;
          border: none
        }
        .delete-button {
          background-color: #d11807 !important;
          color: #FFFFFF !important;
          border: none
        }
      "))
    ),
    actionButton(ns("annotation_button"),
      label = "Annotation Mode",
      class = "annotation-button"
    ),
    actionButton(ns("delete_button"),
      label = "Delete Annotation",
      class = "delete-button"
    ),
    DT::DTOutput(ns("annotation_table"))
  )
}

#' annotate_plot Server Functions
#'
#' @noRd
mod_annotate_plot_server <- function(id, selected_channels, plotly_source, annotation_settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    annotation_mode <- reactiveVal(FALSE)
    annotations <- reactiveVal(
      data.frame(
        Channel = character(),
        Time = numeric(),
        Type = character()
      )
    )

    # Switch annotation mode to TRUE if clicked
    observeEvent(input$annotation_button, {
      annotation_mode(!annotation_mode())
      shinyjs::toggleClass(
        selector = paste0("#", ns("annotation_button")),
        class = "annotation-active"
      )
    })

    # Add annotations based on clicking on the plot
    observeEvent(plotly::event_data("plotly_click", source = plotly_source), {
      req(annotation_mode())
      req(selected_channels())

      clickData <- plotly::event_data("plotly_click", source = plotly_source)

      # Get ordered channels
      orderedLabels <- EGM:::.labels
      channelNames <- selected_channels()
      orderedChannels <- channelNames[match(orderedLabels, channelNames, nomatch = 0)]

      # Map curve number to the channel name
      curveNumber <- clickData$curveNumber + 1
      if (curveNumber <= length(orderedChannels)) {
        selectedChannel <- orderedChannels[curveNumber]
      } else {
        selectedChannel <- "Unknown"
      }

      # Add annotation choice to annotation table
      annotationChoice <- annotation_settings$annotation_choice()

      # TODO
      # Eventually this need to be compatabile with `annotation_table()`
      newAnnotation <-
        data.frame(
          Channel = selectedChannel,
          Time = clickData$x,
          Type = annotationChoice
        )

      # Update annotations
      currentAnnotations <- annotations()
      updatedAnnotations <- rbind(currentAnnotations, newAnnotation)
      annotations(updatedAnnotations)
    })

    # Delete annotations based on selections
    # DT::datatable and DT::renderDT() create a variable for input
    #   That is the name of the table with "_rows_selected" appended
    observeEvent(input$delete_button, {
      rowSelection <- input$annotation_table_rows_selected
      if (!is.null(rowSelection) & length(rowSelection) > 0) {
        currentAnnotations <- annotations()
        updatedAnnotations <- currentAnnotations[-rowSelection, ]
        annotations(updatedAnnotations)
      }
    })

    output$annotation_table <- DT::renderDT(
      {
        DT::datatable(annotations(), selection = "single", editable = TRUE)
      },
      server = FALSE
    )
  })
}

## To be copied in the UI
# mod_annotate_plot_ui("annotate_plot_1")

## To be copied in the server
# mod_annotate_plot_server("annotate_plot_1")
