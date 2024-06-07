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
      titlePanel("sWAVE: Shiny Waveform Annotator, Visualizer, and Editor"),
      sidebarLayout(
        sidebarPanel(
          mod_file_upload_ui("file_upload"),
          mod_annotation_style_ui("annotation_style"),
          mod_channel_select_ui("channel_select"), # Add channel select module
        ),
        mainPanel(
          mod_plot_ui("plot"),
          uiOutput("delayed_annotation") # Annotation only after data uploaded
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
