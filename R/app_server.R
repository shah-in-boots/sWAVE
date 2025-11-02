#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import EGM
#' @noRd
app_server <- function(input, output, session) {
  # Application server logic

  initial_egm <- golem::get_golem_options("initial_egm")
  egm_source <- mod_egm_source_server("egm_source", initial_egm = initial_egm)
  egm <- egm_source$egm

  observeEvent(input$prev_sweep, {
    current <- input$sweep_index
    if (is.null(current) || is.na(current)) {
      current <- 1
    }
    updateNumericInput(session, "sweep_index", value = max(1, current - 1))
  })

  observeEvent(input$next_sweep, {
    current <- input$sweep_index
    if (is.null(current) || is.na(current)) {
      current <- 1
    }
    updateNumericInput(session, "sweep_index", value = current + 1)
  })

  viewer_controls <- reactive({
    list(
      sweep_index = input$sweep_index,
      window_start = input$window_start,
      window_duration = input$window_duration
    )
  })

  mod_wave_viewer_server("wave_viewer", egm = egm, controls = viewer_controls)


}
