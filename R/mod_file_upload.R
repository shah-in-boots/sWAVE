#' file_upload UI Function
#'
#' @description A shiny Module for file upload functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("file_type"),
      "Select File Type",
      choices = c("Bard / LS Pro" = "bard")
    ),
    fileInput(
      ns("file"),
      "Choose File",
      multiple = FALSE,
      accept = c(".txt")
    ),
    hr(),
    h4("File Information"),
    verbatimTextOutput(ns("file_info")),
    hr(),
    actionButton(
      ns("read_btn"),
      "Read File",
      class = "btn-primary",
      icon = icon("upload")
    ),
    hr(),
    h4("EGM Object"),
    verbatimTextOutput(ns("egm_summary"))
  )
}

#' file_upload Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom EGM read_bard
mod_file_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to store uploaded file data (the egm object)
    file_data <- reactiveVal(NULL)
    
    # Display file information on file upload
    output$file_info <- renderPrint({
      req(input$file)
      file_info <- input$file
      file_size_kb <- file_info$size / 1024
      
      if (file_size_kb < 1024) {
        size_display <- sprintf("%.2f KB", file_size_kb)
      } else if (file_size_kb < 1024 * 1024) {
        size_display <- sprintf("%.2f MB", file_size_kb / 1024)
      } else {
        size_display <- sprintf("%.2f GB", file_size_kb / (1024 * 1024))
      }
      
      cat("Name:", file_info$name, "\n")
      cat("Size:", size_display, "\n")
      cat("Type:", file_info$type, "\n")
      cat("Format: Bard / LS Pro", "\n")
    })
    
    # Display EGM summary after reading
    output$egm_summary <- renderPrint({
      req(file_data())
      print(file_data())
    })
    
    # Read file when button is clicked
    observeEvent(input$read_btn, {
      req(input$file)
      req(input$file_type)
      
      # Attempt to read the file
      tryCatch({
        file_path <- input$file$datapath
        file_ext <- tools::file_ext(input$file$name)
        
        # Validate file extension
        if (file_ext != "txt") {
          stop("File must be a .txt file for Bard / LS Pro format")
        }
        
        # Parse based on selected file type
        data <- switch(
          input$file_type,
          "bard" = EGM::read_bard(file_path),
          stop("Unsupported file format")
        )
        
        # Validate that data is an egm object
        if (!EGM::is_egm(data)) {
          stop("Failed to read file as egm object")
        }
        
        # Store the data
        file_data(data)
        
        showNotification(
          "File successfully read as egm object!",
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        file_data(NULL)
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Expose a reactive that returns the parsed egm object
    egm <- shiny::reactive({
      file_data()
    })

    # Expose the egm reactive to other modules
    list(
      egm = egm
    )
  })
}

## To be copied in the UI
# mod_file_upload_ui("file_upload_1")

## To be copied in the server
# mod_file_upload_server("file_upload_1")
