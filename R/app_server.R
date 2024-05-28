#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import DT
#' @import EGM
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dat <- mod_file_upload_server("file_upload")
  mod_data_summary_server("data_summary", dat)
  mod_plot_server("plot", dat)


}
