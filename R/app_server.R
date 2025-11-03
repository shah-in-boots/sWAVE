#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  # File upload module (ID must match UI)
  uploaded <- mod_file_upload_server("file_upload_1")

  # Wire uploaded EGM into plot module
  mod_egm_plot_server("plot", egm = uploaded$egm)
  
  # Future modules can use the uploaded_data reactive here
}
