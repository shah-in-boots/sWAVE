#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Enable
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("sWAVE: Shiny Waveform Annotator, Visualizer, and Editor"),
      sidebarLayout(
        sidebarPanel(
          mod_egm_source_ui("egm_source"),
          tags$hr(),
          tags$h4("Sweep Navigation"),
          numericInput("sweep_index", "Sweep index", value = 1, min = 1, step = 1),
          shiny::div(
            class = "d-flex gap-2",
            actionButton("prev_sweep", "Previous"),
            actionButton("next_sweep", "Next")
          ),
          tags$hr(),
          tags$h4("Window"),
          numericInput("window_start", "Window start (sample)", value = 0, min = 0, step = 1),
          numericInput("window_duration", "Window duration (samples)", value = 2000, min = 1, step = 1)
        ),
        mainPanel(
          mod_waveform_viewer_ui("waveform_viewer")
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # Add shiny JS here
    shinyjs::useShinyjs()
  )
}
