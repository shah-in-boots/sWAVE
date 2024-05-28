#' file_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("fileType"), "Select File Type",
                choices = c("LS Pro (.txt)" = "LSP", "ECG (.xml)" = "ECG")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'ECG'", ns("fileType")),
      fileInput(ns("fileECG"), "Upload ECG File", accept = ".xml")
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'LSP'", ns("fileType")),
      fileInput(ns("fileLSP"), "Upload LS Pro File", accept = ".txt")
    ),
  )
}

#' file_upload Server Functions
#'
#' @noRd
mod_file_upload_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read in data conditionally
    reactive({
      req(input$fileType)

      if (input$fileType == "ECG") {
        req(input$fileECG)
        EGM::read_muse(input$fileECG$datapath)
      } else if (input$fileType == "LSP") {
        req(input$fileLSP)
        EGM::read_lspro(input$fileLSP$datapath)
      }

    })

  })

}


## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
