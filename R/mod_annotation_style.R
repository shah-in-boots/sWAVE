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
    selectInput(
      ns("annotation_style"),
      label = "Select Annotation Style",
      choices = c("intracardiac"),
      selected = "intracardiac"
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'intracardiac'", ns("annotation_style")),
      radioButtons(
        ns("annotation_choice"),
        label = "Select Annotation",
        choices = c("A (atrial)", "H (His)", "V (ventricular)"),
        selected = "A (atrial)"
      )
    )
  )
}

#' annotation_style Server Functions
#'
#' @noRd
mod_annotation_style_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # This annotation style will eventually need to be overhauled
    # An entire annotation system should likely exist within the `EGM` package

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
