#' channel_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_channel_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("channel_select"))
  )
}

#' channel_select Server Functions
#'
#' @noRd
mod_channel_select_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    channels <- reactive({
      req(dat())
      names(dat()$signal[-1])
    })

    output$channel_select <- renderUI({
      req(channels())
      checkboxGroupInput(
        ns("selected_channels"),
        "Select Channels",
        choices = channels(),
        selected = channels()
      )
    })

    reactive({
      req(input$selected_channels)
      input$selected_channels
    })
  })
}

## To be copied in the UI
# mod_channel_select_ui("channel_select_1")

## To be copied in the server
# mod_channel_select_server("channel_select_1")
