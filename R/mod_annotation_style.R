#' annotation_style UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annotation_style_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Allows them to pick an annotation style once data is loaded
    uiOutput(ns("annotation_style_ui")),

    # Allows them to see choices based on the style
    uiOutput(ns("annotation_choice_ui"))

  )
}

#' annotation_style Server Functions
#'
#' @noRd
mod_annotation_style_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # This annotation style will eventually need to be overhauled
    # An entire annotation system should likely exist within the `EGM` package

    # First let's add the annotation input box to only render once data is in
    output$annotation_style_ui <- renderUI({
      req(dat())
      tagList(
        selectInput(
          ns("annotation_style"),
          label = "Select Annotation Style",
          choices = c("intracardiac"),
          selected = "intracardiac"
        )
      )
    })


    # Second can see annotation options once picked
    output$annotation_choice_ui <- renderUI({
      req(dat())
      req(input$annotation_style)
      if (input$annotation_style == "intracardiac") {
        radioButtons(
          ns("annotation_choice"),
          label = "Select Annotation",
          choices = c("A (atrial)", "H (His)", "V (ventricular)"),
          selected = "A (atrial)"
        )
      }
    })

    # Return reactive values
    return(
      list(
        annotation_style = reactive(input$annotation_style),
        annotation_choice = reactive(input$annotation_choice)
      )
    )

  })
}

## To be copied in the UI
# mod_annotation_style_ui("annotation_style_1")

## To be copied in the server
# mod_annotation_style_server("annotation_style_1")
