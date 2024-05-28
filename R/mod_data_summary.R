#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("summary"))
  )
}

#' data_summary Server Functions
#'
#' @noRd
mod_data_summary_server <- function(id, dat) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$summary <- renderPrint({
      summary(dat())
    })
  })
}

## To be copied in the UI
# mod_data_summary_ui("data_summary_1")

## To be copied in the server
# mod_data_summary_server("data_summary_1")
