#' file_upload UI Function
#'
#' @description A shiny Module for file upload functionality.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyFiles shinyFileChoose
mod_file_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("file_type"),
      "Select File Type",
      choices = c("Bard / LS Pro" = "bard", "WFDB" = "wfdb")
    ),
    shinyFiles::shinyFilesButton(
      ns("file"),
      "Choose File",
      "Please select a file",
      multiple = FALSE
    ),
    uiOutput(ns("annotator_ui")),
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
#' @importFrom shiny moduleServer reactive req renderUI uiOutput reactiveVal observeEvent renderPrint showNotification
#' @importFrom shinyFiles shinyFileChoose parseFilePaths getVolumes
#' @importFrom EGM read_bard read_wfdb is_egm
#' @importFrom fs path_home
mod_file_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set up file chooser with root volumes
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    shinyFiles::shinyFileChoose(input, "file", roots = volumes, session = session)
    
    # Reactive to store selected file path
    selected_file <- reactive({
      req(input$file)
      shinyFiles::parseFilePaths(volumes, input$file)
    })
    
    # Reactive to store uploaded file data (the egm object)
    file_data <- reactiveVal(NULL)
    
    # Conditionally show annotator selection for WFDB files
    output$annotator_ui <- renderUI({
      req(input$file_type)
      if (input$file_type == "wfdb") {
        tagList(
          helpText("Select the .dat file. The .hea file must be in the same directory."),
          selectInput(
            ns("annotator_type"),
            "Select Annotator",
            choices = c(
              "None" = "none",
              "ecgpuwave" = "ecgpuwave",
              "sqrs" = "sqrs",
              "Other (specify below)" = "other"
            ),
            selected = "none"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'other'", ns("annotator_type")),
            textInput(
              ns("annotator_custom"),
              "Custom Annotator Name",
              placeholder = "Enter annotator extension"
            )
          )
        )
      }
    })
    
    # Display file information on file selection
    output$file_info <- renderPrint({
      file_path_df <- selected_file()
      req(nrow(file_path_df) > 0)
      
      file_path <- as.character(file_path_df$datapath[1])
      file_name <- basename(file_path)
      file_size_bytes <- file.info(file_path)$size
      file_size_kb <- file_size_bytes / 1024
      
      if (file_size_kb < 1024) {
        size_display <- sprintf("%.2f KB", file_size_kb)
      } else if (file_size_kb < 1024 * 1024) {
        size_display <- sprintf("%.2f MB", file_size_kb / 1024)
      } else {
        size_display <- sprintf("%.2f GB", file_size_kb / (1024 * 1024))
      }
      
      format_name <- switch(
        input$file_type,
        "bard" = "Bard / LS Pro",
        "wfdb" = "WFDB",
        "Unknown"
      )
      
      cat("Name:", file_name, "\n")
      cat("Path:", file_path, "\n")
      cat("Size:", size_display, "\n")
      cat("Format:", format_name, "\n")
    })
    
    # Display EGM summary after reading
    output$egm_summary <- renderPrint({
      req(file_data())
      print(file_data())
    })
    
    # Read file when button is clicked
    observeEvent(input$read_btn, {
      file_path_df <- selected_file()
      req(nrow(file_path_df) > 0)
      req(input$file_type)
      
      # Attempt to read the file
      tryCatch({
        file_path <- as.character(file_path_df$datapath[1])
        file_name <- basename(file_path)
        file_ext <- tools::file_ext(file_name)
        
        # Validate file extension based on file type
        if (input$file_type == "bard" && file_ext != "txt") {
          stop("Please select a .txt file for Bard / LS Pro format")
        } else if (input$file_type == "wfdb" && file_ext != "dat") {
          stop("Please select a .dat file for WFDB format")
        }
        
        # Parse based on selected file type
        data <- switch(
          input$file_type,
          "bard" = {
            # For Bard files, read directly from the selected path
            EGM::read_bard(file_path)
          },
          "wfdb" = {
            # For WFDB, extract record name and directory
            record_name <- tools::file_path_sans_ext(file_name)
            record_dir <- dirname(file_path)
            
            # Determine annotator
            annotator <- NULL
            if (!is.null(input$annotator_type) && input$annotator_type != "none") {
              if (input$annotator_type == "other") {
                # Use custom annotator if specified
                if (!is.null(input$annotator_custom) && nzchar(input$annotator_custom)) {
                  annotator <- input$annotator_custom
                }
              } else {
                annotator <- input$annotator_type
              }
            }
            
            # Read WFDB using directory, record name, and annotator
            # read_wfdb will automatically find the .hea file in the same directory
            EGM::read_wfdb(
              record = record_name, 
              record_dir = record_dir,
              annotator = annotator
            )
          },
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
