#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import DT
#' @import EGM
#' @noRd
app_server <- function(input, output, session) {
  # Application server logic

  # File uploads
  dat <- mod_file_upload_server("file_upload")

  # Channel selection
  selected_channels <- mod_channel_select_server("channel_select", dat)
  annotation_settings <- mod_annotation_style_server("annotation_style", dat)

  # Plotting
  plotly_source <- "plot_annotation" # Define a source ID for plotly
  mod_plot_server("plot", dat, selected_channels, plotly_source)
  mod_annotate_plot_server("annotation", selected_channels, plotly_source)

  # Render annotation table after data uploaded (conditional)
  output$delayed_annotation <- renderUI({
    req(dat()) # Ensures that dat() is not NULL, meaning file has been processed
    mod_annotate_plot_ui("annotation") # Add annotation module below
  })

}
