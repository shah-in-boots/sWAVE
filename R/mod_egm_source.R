#' egm_source UI Function
#'
#' @description A Shiny module that provides inputs for sourcing
#' electrophysiology data either via file upload or a programmatic setter.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_egm_source_ui <- function(id) {
  ns <- shiny::NS(id)
  readers <- egm_available_readers()
  choices <- stats::setNames(
    vapply(readers, function(reader) reader$label, character(1)),
    names(readers)
  )

  shiny::tagList(
    shiny::selectInput(ns("reader"), "Select File Type", choices = choices),
    shiny::uiOutput(ns("file_input"))
  )
}

#' egm_source Server Functions
#'
#' @param initial_egm An optional `EGM` object that should be loaded into the
#'   viewer when the module initializes.
#'
#' @noRd
mod_egm_source_server <- function(id, initial_egm = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    readers <- egm_available_readers()
    current_egm <- shiny::reactiveVal(NULL)
    current_source <- shiny::reactiveVal(NULL)

    if (!is.null(initial_egm)) {
      current_egm(initial_egm)
      current_source("programmatic")
    }

    output$file_input <- shiny::renderUI({
      reader <- input$reader
      shiny::req(reader)
      reader_def <- readers[[reader]]
      if (is.null(reader_def)) {
        return(NULL)
      }

      accept <- reader_def$accept
      if (is.null(accept)) {
        accept <- character(0)
      }

      shiny::fileInput(
        ns("file"),
        label = reader_def$file_label,
        accept = accept
      )
    })

    shiny::observeEvent(input$file, {
      shiny::req(input$reader)
      reader_def <- readers[[input$reader]]
      shiny::req(reader_def)
      shiny::req(is.function(reader_def$reader))
      shiny::req(input$file)
      shiny::req(input$file$datapath)

      egm <- reader_def$reader(input$file$datapath)
      current_egm(egm)
      current_source(reader_def$id)
    })

    set_egm <- function(egm) {
      current_egm(egm)
      current_source("programmatic")
      invisible(egm)
    }

    list(
      egm = shiny::reactive({
        shiny::req(current_egm())
        current_egm()
      }),
      set_egm = set_egm,
      source = shiny::reactive(current_source())
    )
  })
}

egm_available_readers <- function() {
  readers <- list(
    muse = list(
      id = "muse",
      label = "ECG (.xml)",
      file_label = "Upload ECG File",
      accept = ".xml",
      reader = EGM::read_muse
    ),
    lspro = list(
      id = "lspro",
      label = "LS Pro (.txt)",
      file_label = "Upload LS Pro File",
      accept = ".txt",
      reader = EGM::read_lspro
    )
  )

  exported <- getNamespaceExports("EGM")
  read_funs <- grep("^read_", exported, value = TRUE)
  known <- c("read_muse", "read_lspro")
  extra <- setdiff(read_funs, known)

  for (fun_name in extra) {
    reader_fun <- getFromNamespace(fun_name, "EGM")
    label <- format_reader_label(fun_name)
    readers[[fun_name]] <- list(
      id = fun_name,
      label = label,
      file_label = paste0("Upload ", label, " File"),
      accept = NULL,
      reader = reader_fun
    )
  }

  readers
}

format_reader_label <- function(fun_name) {
  label <- gsub("^read_", "", fun_name)
  parts <- strsplit(label, "[_\-]")[[1]]
  parts <- paste0(toupper(substring(parts, 1, 1)), substring(parts, 2))
  paste(parts, collapse = " ")
}
