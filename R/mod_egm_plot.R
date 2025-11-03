#' EGM Plot module
#'
#' A Plotly WebGL viewer for EGM signal data.
#'
#' @name mod_egm_plot
#' @param id Module id
#' @param egm Reactive returning an EGM object
#' @return None; used for side effects in Shiny
#' @importFrom shiny NS moduleServer reactive req renderUI uiOutput
#' @importFrom plotly plot_ly layout renderPlotly plotlyOutput
#' @importFrom dplyr mutate select group_by ungroup slice any_of n row_number rename
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @examples
#' # In app_server: mod_egm_plot_server("plot", egm = uploaded$egm)
#' # In app_ui: mod_egm_plot_ui("plot")
NULL

# UI ----
mod_egm_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(ns("plot"), height = "60vh")
  )
}

# Helpers ----
.pick_fs <- function(egm) {
  # EGM objects always store frequency in attributes(header)$record_line$frequency
  rec <- attr(egm$header, "record_line", exact = TRUE)
  if (!is.null(rec) && !is.null(rec$frequency)) {
    return(as.numeric(rec$frequency))
  }
  stop("No frequency found in attributes(header)$record_line$frequency")
}

.prepare_signal <- function(egm) {
  sig <- egm$signal
  if (is.null(sig) || !nrow(sig)) {
    stop("EGM signal table is empty or NULL")
  }

  # Get channel names from header$label (these match signal column names)
  channel_labels <- as.character(egm$header$label)
  
  # Signal is wide: sample + channel columns
  # Pivot to long format: sample, label, value
  sig_long <- tidyr::pivot_longer(
    sig,
    cols = dplyr::all_of(channel_labels),
    names_to = "label",
    values_to = "value"
  )
  
  # Calculate time from sample and frequency
  fs <- .pick_fs(egm)
  sig_long <- dplyr::mutate(sig_long, time = .data$sample / fs)
  
  # Return time, value, label (and optionally sample)
  dplyr::select(sig_long, time, value, label, sample)
}

.downsample_by <- function(sig, target_n = 2e5) {
  if (is.null(sig) || !nrow(sig)) return(sig)
  step_by <- max(1L, as.integer(nrow(sig) / target_n))
  sig %>%
    dplyr::group_by(.data$label) %>%
    dplyr::slice(seq(1L, dplyr::n(), by = step_by)) %>%
    dplyr::ungroup()
}

# Server ----
mod_egm_plot_server <- function(id, egm) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::req(egm)

    sig_ds <- shiny::reactive({
      x <- egm()
      shiny::req(x)
      s <- .prepare_signal(x)
      .downsample_by(s)
    })

    output$plot <- plotly::renderPlotly({
      s <- sig_ds()
      shiny::req(s)
      plotly::plot_ly(
        s,
        x = ~time,
        y = ~value,
        color = ~label,
        split = ~label,
        type = "scattergl",
        mode = "lines",
        hoverinfo = "text",
        text = ~paste0(
          "label: ", label,
          "<br>time: ", signif(time, 6),
          "<br>value: ", signif(value, 6)
        )
      ) %>%
        plotly::layout(
          xaxis = list(title = if (inherits(s$time, "POSIXt")) "Time" else "Time (s)"),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", x = 0, y = 1.05),
          hovermode = "x unified"
        )
    })
  })
}
