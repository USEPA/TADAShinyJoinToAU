#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_load_file_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    htmltools::h2("1. Load Data"),
    
    shiny::fluidRow(
      shiny::column(
        width = 4,
        
        htmltools::strong("Purpose"),
        
        htmltools::p("Upload a water quality dataset exported from TADAShiny:
          Module 1 Data Discovery and Cleaning. This dataset will be used
          throughout the TADAShinyJoinToAU workflow."),
        
        htmltools::strong("Instructions"),
        
        htmltools::p("Upload a long-format water quality dataset. The file will be
          validated and summarized before proceeding."),
        
        htmltools::p(paste0("Maximum upload size: "
                            , round(getOption("shiny.maxRequestSize") / 1024^2)
                            ," MB.")),
        
        shiny::fileInput(
          inputId = ns("input_file"),
          label = "Choose Module 1 output file",
          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
          width = "100%")
        
        ),
      
      shiny::column(width = 8
                    , htmltools::h3("Data Summary")
                    , shiny::verbatimTextOutput(outputId = ns("input_summary")
                                                , placeholder = TRUE)
                    , htmltools::h3("Data Preview")
                    , htmltools::p("Preview of uploaded dataset.")
                    , DT::dataTableOutput(ns("df_import_dt"))
      )
    )
  )
}


#' load_file Server Functions
#'
#' @noRd

mod_load_file_server <- function(id, tadat) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Read input file ####
    read_input_file <- function(file_path) {
      
      file_ext <- tolower(tools::file_ext(file_path))
      
      if (file_ext == "csv") {
        df <- readr::read_csv(file_path, show_col_types = FALSE)
        
      } else if (file_ext %in% c("tsv", "txt")) {
        df <- readr::read_tsv(file_path, show_col_types = FALSE)
        
      } else if (file_ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(file_path, guess_max = 100000)
        
      } else {
        stop("Unsupported file type.")
      }
      
      as.data.frame(df)
    } # END ~ read_input_file <- function(file_path)
    
    required_cols <- c(
      "MonitoringLocationIdentifier",
      "MonitoringLocationTypeName",
      "LatitudeMeasure",
      "LongitudeMeasure",
      "TADA.MonitoringLocationIdentifier",
      "TADA.LatitudeMeasure",
      "TADA.LongitudeMeasure"
    )
    
    ## Read in data ####
    df_import <- shiny::eventReactive(input$input_file, {
      
      shiny::req(input$input_file)
      
      df <- read_input_file(input$input_file$datapath)
      
      ## Data validation ####
      missing_cols <- setdiff(required_cols, names(df))
      
      missing_cols <- setdiff(required_cols, names(df))
      
      if (length(missing_cols) > 0) {
        
        shiny::showNotification(
          paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
        
        return(NULL)
      }
      
      ### Data Summary ####
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "\nLoaded file: ", input$input_file$name,
          "\nRows: ", nrow(df),
          "\nColumns: ", ncol(df), "\n"
        )
      )
      
      ### tadat save ####
      tadat$df_WQ_input_original <- df
      tadat$df_working <- df
      
      shiny::showNotification(
        paste("Successfully loaded", input$input_file$name),
        type = "message",
        duration = 5
      )
      
      df
    }) # END ~ df_import <- shiny::eventReactive
    
    # Input summary ####
    output$input_summary <- shiny::renderText({
      
      df <- df_import()
      
      org_count <- if ("OrganizationIdentifier" %in% names(df)) {
        dplyr::n_distinct(df$OrganizationIdentifier)
      } else {
        "Not Available"
      }
      
      coord_count <- sum(!is.na(df$LatitudeMeasure) 
                         & !is.na(df$LongitudeMeasure))
      
      paste0(
        "Rows: ", nrow(df), "\n",
        "Columns: ", ncol(df), "\n",
        "Unique Monitoring Locations: ",
        dplyr::n_distinct(df$MonitoringLocationIdentifier), "\n",
        "Organizations: ", org_count, "\n",
        "Records with Coordinates: ", coord_count
      )
    }) # END ~ output$input_summary <- shiny::renderText
    
    # Display table ####
    output$df_import_dt <- DT::renderDT({
      
      DT::datatable(
        df_import(),
        filter = "top",
        class = "compact",
        rownames = FALSE,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          autoWidth = TRUE,
          rownames = FALSE,
          searching = FALSE,
          lengthMenu = c(10, 25, 50, 100)
        )
      )
    }) # END ~ output$df_import_dt <- DT::renderDT
    
    # Enable Tab 2 ####
    shiny::observeEvent(df_import(), {
      shinyjs::enable(
        selector = '.nav li a[data-value="site_review"]'
      )
    }, ignoreInit = TRUE)
    
  })
}