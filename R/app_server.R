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

  viewer_state <- reactiveValues(
    start_time = 0,
    window_length = 2000,
    sweep_speed = 1000,
    sample_max = 10000,
    sample_rate = NA_real_,
    playing = FALSE
  )

  get_max_start <- function() {
    max(0, viewer_state$sample_max - viewer_state$window_length)
  }

  set_start_time <- function(value, from_slider = FALSE, stop_playback = FALSE) {
    max_start <- get_max_start()
    clamped <- suppressWarnings(as.numeric(value))[1]
    if (is.null(clamped) || length(clamped) == 0 || is.na(clamped) || !is.finite(clamped)) {
      clamped <- 0
    }
    clamped <- max(0, min(clamped, max_start))
    viewer_state$start_time <- clamped
    if (stop_playback) {
      viewer_state$playing <- FALSE
    }
    if (!from_slider) {
      updateSliderInput(session, "start_time", value = clamped)
    }
  }

  observeEvent(input$start_time, {
    set_start_time(input$start_time, from_slider = TRUE, stop_playback = TRUE)
  }, ignoreNULL = FALSE)

  observeEvent(input$window_length, {
    value <- suppressWarnings(as.numeric(input$window_length))
    if (!is.finite(value) || is.na(value) || value <= 0) {
      value <- 1
    }
    max_window <- max(1, viewer_state$sample_max)
    value <- max(1, min(value, max_window))
    viewer_state$window_length <- value
    updateSliderInput(session, "window_length", value = value)
    set_start_time(viewer_state$start_time, stop_playback = TRUE)
  }, ignoreNULL = FALSE)

  observeEvent(input$sweep_speed, {
    value <- suppressWarnings(as.numeric(input$sweep_speed))
    if (!is.finite(value) || is.na(value) || value < 0) {
      value <- 0
    }
    viewer_state$sweep_speed <- value
  }, ignoreNULL = FALSE)

  observeEvent(input$play_window, {
    if (viewer_state$sample_max <= 0) {
      return()
    }
    viewer_state$playing <- TRUE
  })

  observeEvent(input$pause_window, {
    viewer_state$playing <- FALSE
  })

  observeEvent(input[["wave_viewer-toggle_play"]], {
    viewer_state$playing <- !isTRUE(viewer_state$playing)
  })

  observeEvent(input$sweep_index, {
    viewer_state$playing <- FALSE
    set_start_time(0, stop_playback = TRUE)
  })

  observeEvent(egm(), {
    viewer_state$playing <- FALSE

    max_sample <- 0
    sample_rate <- NA_real_

    waveform <- try(ggm(egm()), silent = TRUE)
    if (!inherits(waveform, "try-error") && !is.null(waveform$data)) {
      data <- waveform$data
      if (is.data.frame(data) && "sample" %in% names(data)) {
        sample_values <- suppressWarnings(as.numeric(data$sample))
        if (length(sample_values) > 0) {
          max_sample <- suppressWarnings(max(sample_values, na.rm = TRUE))
          if (!is.finite(max_sample) || is.na(max_sample)) {
            max_sample <- 0
          }
        }
      }

      sr_candidates <- c(
        waveform$metadata$sampling_rate %||% NA_real_,
        waveform$config$sampling_rate %||% NA_real_,
        attr(data, "sampling_rate") %||% NA_real_
      )
      sample_rate <- suppressWarnings(as.numeric(sr_candidates[!is.na(sr_candidates)][1]))
    }

    if (!is.finite(max_sample) || max_sample <= 0) {
      max_sample <- 1
    }

    viewer_state$sample_max <- max_sample
    viewer_state$sample_rate <- if (is.finite(sample_rate) && sample_rate > 0) sample_rate else NA_real_

    max_window <- max(1, round(max_sample))
    new_window <- min(max(1, round(viewer_state$window_length)), max_window)
    viewer_state$window_length <- new_window

    updateSliderInput(session, "start_time", min = 0, max = max_sample, value = 0)
    updateSliderInput(session, "window_length", min = 1, max = max_window, value = new_window)

    default_speed <- if (is.finite(sample_rate) && sample_rate > 0) sample_rate else viewer_state$sweep_speed
    if (!is.finite(default_speed) || is.na(default_speed) || default_speed <= 0) {
      default_speed <- 1
    }
    viewer_state$sweep_speed <- default_speed
    updateSliderInput(
      session,
      "sweep_speed",
      min = 1,
      max = max(1, if (is.finite(sample_rate) && sample_rate > 0) sample_rate * 4 else 5000),
      value = default_speed
    )

    set_start_time(0)
  }, ignoreNULL = FALSE)

  observeEvent(input[["wave_viewer-window_nudge"]], {
    evt <- input[["wave_viewer-window_nudge"]]
    if (is.null(evt) || is.null(evt$direction)) {
      return()
    }

    base_step <- max(1, round(viewer_state$window_length * 0.1))
    if (isTRUE(evt$shift)) {
      base_step <- base_step * 5
    } else if (isTRUE(evt$alt)) {
      base_step <- max(1, round(base_step * 0.2))
    }

    delta <- if (identical(evt$direction, "back")) {
      -base_step
    } else {
      base_step
    }

    set_start_time(viewer_state$start_time + delta, stop_playback = TRUE)
  })

  auto_timer <- reactiveTimer(100)

  observe({
    auto_timer()

    if (!isTRUE(viewer_state$playing)) {
      return()
    }

    if (!is.finite(viewer_state$sweep_speed) || viewer_state$sweep_speed <= 0) {
      return()
    }

    if (!is.finite(viewer_state$sample_max) || viewer_state$sample_max <= 0) {
      viewer_state$playing <- FALSE
      set_start_time(0)
      return()
    }

    max_start <- get_max_start()
    if (max_start <= 0) {
      viewer_state$playing <- FALSE
      set_start_time(0)
      return()
    }

    increment <- max(1, round(viewer_state$sweep_speed * 0.1))
    new_start <- viewer_state$start_time + increment

    if (new_start >= max_start) {
      if (isTRUE(input$loop_window)) {
        set_start_time(0)
      } else {
        set_start_time(max_start)
        viewer_state$playing <- FALSE
      }
      return()
    }

    set_start_time(new_start)
  })

  viewer_controls <- reactive({
    list(
      sweep_index = input$sweep_index,
      start_time = viewer_state$start_time,
      window_length = viewer_state$window_length,
      sweep_speed = viewer_state$sweep_speed
    )
  })

  mod_wave_viewer_server("wave_viewer", egm = egm, controls = viewer_controls)


}
