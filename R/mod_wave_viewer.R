#' wave_viewer UI Function
#'
#' @description A shiny Module that wraps the waveform viewer controls and
#'   plotting surface using a WebGL capable widget.
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
#' @importFrom shinyjs runjs
mod_wave_viewer_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::div(
      id = ns("interaction_root"),
      class = "waveform-viewer d-flex flex-column gap-3 h-100",
      tabindex = "0",
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
        class = "waveform-viewer__plot flex-grow-1",
        style = "width: 100%; height: 100%; min-height: 500px;",
        plotly::plotlyOutput(ns("plot"), width = "100%", height = "100%")
      )
    )
  )
}

#' wave_viewer Server Functions
#'
#' @noRd
mod_wave_viewer_server <- function(id, egm, controls) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$onFlushed(function() {
      bind_script <- sprintf(
        "(function(){\n  var ns = '%s';\n  var rootId = '%s';\n  window.waveViewerBindings = window.waveViewerBindings || {};\n  if (window.waveViewerBindings[rootId]) { return; }\n  var root = document.getElementById(rootId);\n  if (!root) { return; }\n  root.setAttribute('tabindex', '0');\n  var sendNudge = function(direction, evt) {\n    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) { return; }\n    Shiny.setInputValue(ns + 'window_nudge', {\n      direction: direction,\n      shift: !!(evt && evt.shiftKey),\n      alt: !!(evt && evt.altKey),\n      ctrl: !!(evt && evt.ctrlKey),\n      nonce: Date.now()\n    }, {priority: 'event'});\n  };\n  root.addEventListener('wheel', function(evt) {\n    if (evt.ctrlKey) { return; }\n    evt.preventDefault();\n    sendNudge(evt.deltaY < 0 ? 'back' : 'forward', evt);\n  }, {passive: false});\n  root.addEventListener('keydown', function(evt) {\n    if (evt.key === 'ArrowLeft' || evt.key === 'ArrowRight') {\n      evt.preventDefault();\n      sendNudge(evt.key === 'ArrowLeft' ? 'back' : 'forward', evt);\n    } else if (evt.key === ' ' || evt.code === 'Space') {\n      evt.preventDefault();\n      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {\n        Shiny.setInputValue(ns + 'toggle_play', {nonce: Date.now()}, {priority: 'event'});\n      }\n    }\n  });\n  root.addEventListener('focus', function() {\n    root.classList.add('waveform-viewer--focused');\n  });\n  root.addEventListener('blur', function() {\n    root.classList.remove('waveform-viewer--focused');\n  });\n  setTimeout(function(){ root.focus(); }, 200);\n  window.waveViewerBindings[rootId] = true;\n})();",
        session$ns(""),
        ns("interaction_root")
      )

      shinyjs::runjs(bind_script)
    }, once = TRUE)

    selected_channels <- mod_channel_select_server("channel_select", egm)

    waveform_cache <- reactiveVal(NULL)
    plot_ready <- reactiveVal(FALSE)

    build_cache <- function(egm_object) {
      waveform <- ggm(egm_object)
      data <- waveform$data

      if (!is.data.frame(data) || nrow(data) == 0) {
        return(NULL)
      }

      sr_candidates <- c(
        if (!is.null(waveform$metadata) && !is.null(waveform$metadata$sampling_rate)) {
          waveform$metadata$sampling_rate
        } else {
          NULL
        },
        if (!is.null(waveform$config) && !is.null(waveform$config$sampling_rate)) {
          waveform$config$sampling_rate
        } else {
          NULL
        },
        if (!is.null(attr(data, "sampling_rate"))) {
          attr(data, "sampling_rate")
        } else {
          NULL
        }
      )
      sample_rate <- suppressWarnings(as.numeric(sr_candidates[!is.na(sr_candidates)][1]))

      channel_groups <- split(data, data$label, drop = TRUE)
      channel_cache <- lapply(channel_groups, function(channel_df) {
        sweeps <- if ("sweep" %in% names(channel_df)) {
          split(channel_df, channel_df$sweep, drop = TRUE)
        } else {
          list(`1` = channel_df)
        }

        lapply(sweeps, function(sweep_df) {
          sweep_df <- sweep_df[order(sweep_df$sample), , drop = FALSE]
          list(
            samples = sweep_df$sample,
            values = sweep_df$mV
          )
        })
      })

      sweep_ids <- if ("sweep" %in% names(data)) {
        sort(unique(data$sweep))
      } else {
        1
      }

      list(
        channels = channel_cache,
        sweeps = as.character(sweep_ids),
        sample_rate = sample_rate
      )
    }

    observeEvent(egm(), {
      cache <- build_cache(egm())
      waveform_cache(cache)
      plot_ready(FALSE)
    }, ignoreNULL = TRUE)

    get_view_data <- function(cache, selected, ctrl) {
      if (is.null(cache) || length(cache$channels) == 0 || length(selected) == 0) {
        sample_rate <- if (!is.null(cache)) cache$sample_rate else NA_real_
        return(list(traces = list(), sweep_id = NULL, sample_rate = sample_rate, window = NULL))
      }

      sweeps <- cache$sweeps
      if (length(sweeps) == 0) {
        sweeps <- "1"
      }

      ctrl <- ctrl %||% list()
      sweep_index <- ctrl$sweep_index %||% 1
      sweep_index <- suppressWarnings(as.integer(sweep_index))
      if (is.na(sweep_index) || sweep_index < 1) {
        sweep_index <- 1L
      }
      if (sweep_index > length(sweeps)) {
        sweep_index <- length(sweeps)
      }
      sweep_id <- sweeps[[sweep_index]]

      window_start <- suppressWarnings(as.numeric(ctrl$start_time %||% ctrl$window_start %||% 0))
      if (is.na(window_start)) {
        window_start <- 0
      }
      window_duration <- suppressWarnings(as.numeric(ctrl$window_length %||% ctrl$window_duration %||% Inf))
      if (is.na(window_duration) || window_duration <= 0) {
        window_duration <- Inf
      }
      window_end <- window_start + window_duration

      traces <- lapply(selected, function(channel) {
        channel_entry <- cache$channels[[channel]]
        if (is.null(channel_entry)) {
          return(NULL)
        }

        sweep_key <- as.character(sweep_id)
        sweep_data <- channel_entry[[sweep_key]] %||% channel_entry[[1]]
        if (is.null(sweep_data)) {
          return(NULL)
        }

        samples <- sweep_data$samples
        values <- sweep_data$values
        keep <- if (is.finite(window_end)) {
          samples >= window_start & samples <= window_end
        } else {
          samples >= window_start
        }

        samples <- samples[keep]
        values <- values[keep]

        list(
          name = channel,
          x = samples,
          y = values
        )
      })

      traces <- drop_nulls(traces)

      list(
        traces = traces,
        sweep_id = sweep_id,
        sample_rate = cache$sample_rate,
        window = c(window_start, if (is.finite(window_end)) window_end else NA_real_)
      )
    }

    base_state <- reactive({
      list(
        cache = waveform_cache(),
        channels = selected_channels()
      )
    })

    output$plot <- plotly::renderPlotly({
      plot_ready(FALSE)
      on.exit(plot_ready(TRUE), add = TRUE)

      state <- base_state()
      cache <- state$cache
      channels <- state$channels
      req(!is.null(cache))
      req(length(channels) > 0)

      ctrl <- isolate(controls())
      view <- get_view_data(cache, channels, ctrl)
      traces <- view$traces
      req(length(traces) > 0)

      plots <- lapply(traces, function(trace) {
        plotly::plot_ly(
          type = "scattergl",
          mode = "lines",
          x = trace$x,
          y = trace$y,
          name = trace$name,
          line = list(color = "rgba(255,255,255,0.9)", width = 1.25),
          hoverinfo = "text",
          text = sprintf(
            "Channel: %s<br>Sample: %s<br>mV: %s",
            trace$name,
            format(trace$x, trim = TRUE, scientific = FALSE),
            format(round(trace$y, 4), trim = TRUE)
          )
        ) |>
          plotly::layout(
            yaxis = list(
              title = trace$name,
              titlefont = list(color = "#FFFFFF"),
              tickfont = list(color = "#D7D7D7"),
              color = "#D7D7D7",
              gridcolor = "rgba(255,255,255,0.08)",
              zerolinecolor = "rgba(255,255,255,0.15)"
            ),
            xaxis = list(
              color = "#D7D7D7",
              tickfont = list(color = "#D7D7D7"),
              gridcolor = "rgba(255,255,255,0.08)",
              zerolinecolor = "rgba(255,255,255,0.15)"
            )
          )
      })

      subplot <- plotly::subplot(
        plots,
        nrows = length(plots),
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
          font = list(color = "#F0F0F0"),
          plot_bgcolor = "#111111",
          paper_bgcolor = "#111111",
          margin = list(l = 80, r = 20, t = 20, b = 40)
        ) |>
        plotly::config(displaylogo = FALSE)
    })

    window_controls <- reactive({
      ctrl <- controls()
      ctrl %||% list()
    })

    observeEvent(window_controls(), {
      req(plot_ready())
      cache <- waveform_cache()
      channels <- selected_channels()
      req(!is.null(cache))
      req(length(channels) > 0)

      view <- get_view_data(cache, channels, window_controls())
      traces <- view$traces

      proxy <- plotly::plotlyProxy("plot", session, deferUntilFlush = FALSE)

      for (idx in seq_along(traces)) {
        trace <- traces[[idx]]
        hover_text <- sprintf(
          "Channel: %s<br>Sample: %s<br>mV: %s",
          trace$name,
          format(trace$x, trim = TRUE, scientific = FALSE),
          format(round(trace$y, 4), trim = TRUE)
        )

        plotly::plotlyProxyInvoke(
          proxy,
          "restyle",
          list(
            x = list(trace$x),
            y = list(trace$y),
            text = list(hover_text)
          ),
          list(idx - 1)
        )
      }

      window_range <- view$window
      if (length(window_range) == 2 && all(is.finite(window_range))) {
        plotly::plotlyProxyInvoke(
          proxy,
          "relayout",
          list(xaxis = list(range = window_range))
        )
      } else {
        plotly::plotlyProxyInvoke(
          proxy,
          "relayout",
          list(xaxis = list(autorange = TRUE))
        )
      }
    }, ignoreNULL = FALSE)
  })
}

## To be copied in the UI
# mod_wave_viewer_ui("wave_viewer_1")

## To be copied in the server
# mod_wave_viewer_server("wave_viewer_1")
