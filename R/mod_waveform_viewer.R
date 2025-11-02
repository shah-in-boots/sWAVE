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

      downsample_waveform(plotData)
    })

    output$plot <- plotly::renderPlotly({
      data <- plot_data()
      req(nrow(data) > 0)

      channelNames <- unique(data$label)

      axis_template <- list(
        color = "rgba(255,255,255,1)",
        gridcolor = "rgba(255,255,255,0.1)",
        zerolinecolor = "rgba(255,255,255,0.2)",
        tickfont = list(color = "rgba(255,255,255,0.9)")
      )

      plots <- lapply(channelNames, function(channel) {
        channel_data <- data[data$label == channel, , drop = FALSE]

        plotly::plot_ly(
          channel_data,
          x = ~sample,
          y = ~mV,
          type = "scatter",
          mode = "lines",
          name = channel,
          line = list(color = "rgba(255,255,255,1)"),
          hoverinfo = "text",
          text = ~paste0(
            "Channel: ", channel,
            "<br>Sample: ", sample,
            "<br>mV: ", round(mV, 3)
          )
        ) |>
          plotly::layout(
            yaxis = c(axis_template, list(title = channel)),
            xaxis = axis_template
          )
      })

      subplot <- plotly::subplot(
        plots,
        nrows = length(channelNames),
        shareX = TRUE,
        titleX = FALSE,
        titleY = TRUE,
        which_layout = "merge",
        margin = 0.02
      )

      subplot |>
        plotly::layout(
          showlegend = FALSE,
          dragmode = "zoom",
          plot_bgcolor = "rgba(17,17,17,1)",
          paper_bgcolor = "rgba(17,17,17,1)",
          margin = list(l = 80, r = 20, t = 20, b = 40)
        ) |>
        plotly::config(displaylogo = FALSE)
    })
  })
}

downsample_waveform <- function(data, max_points_per_channel = 5000) {
  if (!is.data.frame(data)) {
    return(data)
  }

  required_cols <- c("label", "sample")
  if (!all(required_cols %in% names(data))) {
    return(data)
  }

  max_points_per_channel <- max(1L, as.integer(max_points_per_channel))

  label_levels <- unique(data$label)

  dt <- data.table::as.data.table(data)
  data.table::setorderv(dt, c("label", "sample"))

  downsampled <- dt[, {
    if (.N <= max_points_per_channel) {
      .SD
    } else {
      idx <- unique(as.integer(floor(seq(1, .N, length.out = max_points_per_channel))))
      .SD[idx]
    }
  }, by = "label"]

  downsampled <- as.data.frame(downsampled)
  downsampled$label <- factor(downsampled$label, levels = label_levels)
  downsampled <- downsampled[order(downsampled$label, downsampled$sample), , drop = FALSE]
  downsampled$label <- as.character(downsampled$label)

  downsampled
}

## To be copied in the UI
# mod_waveform_viewer_ui("waveform_viewer_1")

## To be copied in the server
# mod_waveform_viewer_server("waveform_viewer_1")
