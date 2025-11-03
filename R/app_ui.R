#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # Application title
      titlePanel("sWAVE - Shiny Waveform Annotator, Visualizer, and Editor"),
      
      # Sidebar layout
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h3("File Upload"),
          mod_file_upload_ui("file_upload_1")
        ),
        
        mainPanel(
          width = 9,
          h3("Waveform Viewer"),
          uiOutput("placeholder_text"),
          mod_egm_plot_ui("plot")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sWAVE"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
