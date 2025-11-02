#' waveform_viewer UI Function
#'
#' @description A shiny Module that wraps the waveform viewer controls and
#'   plotting surface.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param egm A reactive expression that returns the `EGM` object currently
#'   loaded in the application.
#' @param controls A reactive expression that returns a list of viewer control
#'   values (for example sweep index and window settings).
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div fluidRow column h4
mod_waveform_viewer_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::div(
      class = "waveform-viewer__controls",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4("Channel Selection"),
          mod_channel_select_ui(ns("channel_select"))
        )
      )
    ),
    shiny::div(
      class = "waveform-viewer__plot",
      plotly::plotlyOutput(ns("plot"))
    )
  )
}

#' waveform_viewer Server Functions
#'
#' @noRd
mod_waveform_viewer_server <- function(id, egm, controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_channels <- mod_channel_select_server("channel_select", egm)

    plot_data <- reactive({
      req(egm())
      req(selected_channels())

      ctrl <- controls()
      if (is.null(ctrl)) {
        ctrl <- list()
      }

      plotData <- ggm(egm())$data
      plotData <- plotData[plotData$label %in% selected_channels(), , drop = FALSE]

      if (nrow(plotData) == 0) {
        return(plotData)
      }

      if ("sweep" %in% names(plotData) && !is.null(ctrl$sweep_index)) {
        sweeps <- sort(unique(plotData$sweep))
        if (length(sweeps) > 0) {
          index <- max(1, min(as.integer(ctrl$sweep_index), length(sweeps)))
          plotData <- plotData[plotData$sweep == sweeps[index], , drop = FALSE]
        }
      }

      if (!is.null(ctrl$window_start) && !is.null(ctrl$window_duration) &&
          "sample" %in% names(plotData)) {
        start <- as.numeric(ctrl$window_start)
        duration <- max(0, as.numeric(ctrl$window_duration))
        end <- start + duration
        plotData <- plotData[plotData$sample >= start & plotData$sample <= end, , drop = FALSE]
      }

      plotData
    })

    output$plot <- plotly::renderPlotly({
      data <- plot_data()
      req(nrow(data) > 0)

      channelNames <- unique(data$label)

      g <-
        data |>
        ggplot2::ggplot(ggplot2::aes(x = sample, y = mV)) +
        ggplot2::geom_line(color = "white") +
        ggplot2::facet_wrap(~ label, ncol = 1, scales = "free_y", strip.position = "left") +
        EGM::theme_egm_dark()

      p <-
        g |>
        plotly::ggplotly(dynamicTicks = TRUE, originalData = TRUE)

      if (!is.null(p$x$layout$annotations)) {
        for (i in seq_along(p$x$layout$annotations)) {
          ann <- p$x$layout$annotations[[i]]

          if (ann$text %in% channelNames) {
            ann$textangle <- 0
            ann$font$color <- "rgba(255,255,255,1)"
            ann$xref <- "paper"
            ann$x <- -0.01
            ann$xanchor <- "right"
            ann$yref <- "paper"
            ann$yanchor <- "middle"
          }

          p$x$layout$annotations[[i]] <- ann
        }
      }

      p |>
        plotly::layout(margin = list(l = 80)) |>
        plotly::layout(dragmode = "zoom")
    })
  })
}

## To be copied in the UI
# mod_waveform_viewer_ui("waveform_viewer_1")

## To be copied in the server
# mod_waveform_viewer_server("waveform_viewer_1")
