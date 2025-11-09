#' EGM Plot module
#'
#' A Plotly WebGL viewer for EGM signal data.
#'
#' @name mod_egm_plot
#' @param id Module id
#' @param egm Reactive returning an EGM object
#' @return None; used for side effects in Shiny
#' @importFrom shiny NS moduleServer reactive reactiveVal req renderUI uiOutput
#' @importFrom shiny selectInput actionButton observeEvent
#' @importFrom plotly plot_ly layout renderPlotly plotlyOutput
#' @importFrom dplyr mutate select group_by ungroup slice any_of n row_number rename
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' # In app_server: mod_egm_plot_server("plot", egm = uploaded$egm)
#' # In app_ui: mod_egm_plot_ui("plot")
NULL

# UI ----
mod_egm_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex gap-2 align-items-center mb-2 flex-wrap",
      shiny::selectInput(
        ns("speed"),
        label = "Sweep speed",
        choices = c("25 mm/s" = 25, "50 mm/s" = 50, "100 mm/s" = 100, "200 mm/s" = 200),
        selected = 25
      ),
      shiny::actionButton(ns("prev_window"), label = "Previous"),
      shiny::actionButton(ns("next_window"), label = "Next")
    ),
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

  # Get channel names from header$label in order (these match signal column names)
  channel_labels <- as.character(egm$header$label)
  
  # Signal is wide: sample + channel columns
  # Pivot to long format: sample, label, value
  sig_long <- tidyr::pivot_longer(
    sig,
    cols = dplyr::all_of(channel_labels),
    names_to = "label",
    values_to = "value"
  )
  
  # Convert label to factor to preserve order from header
  sig_long <- dplyr::mutate(sig_long, label = factor(.data$label, levels = channel_labels))
  
  # Calculate time from sample and frequency
  fs <- .pick_fs(egm)
  sig_long <- dplyr::mutate(sig_long, time = .data$sample / fs)
  
  # Normalize each channel and add vertical offset
  # Each channel gets its own vertical space
  sig_long <- sig_long %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(
      # Normalize to roughly -0.4 to +0.4 range per channel
      value_norm = (.data$value - mean(.data$value, na.rm = TRUE)) / 
                   (max(abs(.data$value - mean(.data$value, na.rm = TRUE)), na.rm = TRUE) + 1e-10),
      value_norm = .data$value_norm * 0.4,
      # Add vertical offset: channel number (1-indexed from bottom)
      channel_num = as.numeric(.data$label),
      value_offset = .data$value_norm + .data$channel_num
    ) %>%
    dplyr::ungroup()
  
  # Return time, value_offset, label (and original value for hover)
  dplyr::select(sig_long, time, value_offset, label, value, sample, channel_num)
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

    default_speed <- 25
    default_duration <- 10

    selected_speed <- shiny::reactive({
      req_speed <- input$speed
      if (is.null(req_speed)) {
        return(default_speed)
      }
      as.numeric(req_speed)
    })

    window_duration <- shiny::reactive({
      sel <- selected_speed()
      default_duration * (default_speed / sel)
    })

    sig_full <- shiny::reactive({
      x <- egm()
      shiny::req(x)
      .prepare_signal(x)
    })

    window_start <- shiny::reactiveVal(0)

    .update_window_start <- function(start_candidate) {
      s <- sig_full()
      shiny::req(s)
      dur <- window_duration()
      if (!is.finite(dur) || dur <= 0) {
        dur <- default_duration
      }
      max_time <- max(s$time, na.rm = TRUE)
      max_start <- max(0, max_time - dur)
      start_adj <- min(max(start_candidate, 0), max_start)
      window_start(start_adj)
    }

    shiny::observeEvent(sig_full(), {
      window_start(0)
    })

    shiny::observeEvent(window_duration(), {
      .update_window_start(window_start())
    })

    shiny::observeEvent(input$next_window, {
      shiny::req(sig_full())
      dur <- window_duration()
      current_start <- window_start()
      step <- dur * 0.8
      if (!is.finite(step) || step <= 0) {
        step <- default_duration * 0.8
      }
      .update_window_start(current_start + step)
    })

    shiny::observeEvent(input$prev_window, {
      shiny::req(sig_full())
      dur <- window_duration()
      current_start <- window_start()
      step <- dur * 0.8
      if (!is.finite(step) || step <= 0) {
        step <- default_duration * 0.8
      }
      .update_window_start(current_start - step)
    })

    sig_window <- shiny::reactive({
      s <- sig_full()
      shiny::req(s)
      dur <- window_duration()
      if (!is.finite(dur) || dur <= 0) {
        dur <- default_duration
      }
      start <- window_start()
      max_time <- max(s$time, na.rm = TRUE)
      max_start <- max(0, max_time - dur)
      start <- min(max(start, 0), max_start)
      end <- start + dur
      filtered <- dplyr::filter(s, .data$time >= start & .data$time <= end)
      .downsample_by(filtered)
    })

    window_range <- shiny::reactive({
      s <- sig_full()
      shiny::req(s)
      dur <- window_duration()
      if (!is.finite(dur) || dur <= 0) {
        dur <- default_duration
      }
      start <- window_start()
      max_time <- max(s$time, na.rm = TRUE)
      max_start <- max(0, max_time - dur)
      start <- min(max(start, 0), max_start)
      c(start, start + dur)
    })

    output$plot <- plotly::renderPlotly({
      s <- sig_window()
      shiny::req(s)

      # Get unique channels in order
      channels <- levels(s$label)
      n_channels <- length(channels)
      
      # Use white color for all traces for better legibility on black background
      colors <- rep("white", n_channels)
      
      plotly::plot_ly(
        s,
        x = ~time,
        y = ~value_offset,
        color = ~label,
        colors = colors,
        split = ~label,
        type = "scattergl",
        mode = "lines",
        line = list(color = "white", width = 1),
        hoverinfo = "text",
        text = ~paste0(
          "Channel: ", label,
          "<br>Time: ", signif(time, 6), " s",
          "<br>Value: ", signif(value, 6)
        )
      ) %>%
        plotly::layout(
          plot_bgcolor = "black",
          paper_bgcolor = "black",
          font = list(color = "white"),
          xaxis = list(
            title = if (inherits(s$time, "POSIXt")) "Time" else "Time (s)",
            gridcolor = "#333333",
            zerolinecolor = "#555555",
            range = window_range()
          ),
          yaxis = list(
            title = "",
            tickmode = "array",
            tickvals = seq_len(n_channels),
            ticktext = channels,
            range = c(0.5, n_channels + 0.5),
            gridcolor = "#333333",
            zerolinecolor = "#555555"
          ),
          legend = list(
            orientation = "h", 
            x = 0, 
            y = 1.05,
            font = list(color = "white"),
            bgcolor = "rgba(0,0,0,0.5)"
          ),
          hovermode = "x unified",
          showlegend = TRUE
        )
    })
  })
}
