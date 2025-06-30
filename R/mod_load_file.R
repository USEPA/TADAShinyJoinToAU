#' load_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' 
mod_load_file_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    # header
    htmltools::h2("1. Load File"),
    
    # start fluidrow
    shiny::fluidRow(
      
      # left column file load prompts
      column(
        width = 4,
        htmltools::h3("a. Load file"),
        htmltools::p("You can download a file with Monitoring location identifiers and associated chemistry data from the Water Quality Portal (WQP) or use the TADAShinyApp at (https://rconnect-public.epa.gov/TADAShiny/). Only comma-separated or tab-separated files can be loaded (i.e., .csv or .tsv)."),
        htmltools::p("The file loaded here will be used for all subsequent steps."),
        htmltools::p(paste0("Note: Loaded file sizes are limited to a maximum size of ", get_golem_config("MB_LIMIT"), " MB.")),
        htmltools::h3("b. Select file parameters"),
        shiny::radioButtons(
          inputId = ns("separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Tab = "\t"),
          selected = ","
        ),
        shiny::fileInput(
          inputId = ns("input_file"),
          label = "Choose file to load:",
          width = "90%",
          placeholder = "No file selected.",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv", ".tsv", ".txt"
          )
        )
      ),
      
      # right column table
      column(
        width = 8,
        htmltools::h3("c. Loaded file summary"),
        shiny::verbatimTextOutput(outputId = ns("input_summary"), placeholder = TRUE),
        htmltools::h3("d. Preview loaded file"),
        htmltools::p("Scroll, search, or sort the table below to explore previewed the loaded file."),
        # htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_import_dt"))
      )
    )
  )
}


# load input data if needed
# data_path1 <- app_sys("extdata/HUC8.RData")
# load(data_path1)

# source any other info
# see tadashiny app mod_query_data.R dev branch for ideas

# custom functions
# see tadashiny app mod_query_data.R dev branch for ideas
    
#' load_file Server Functions
#'
#' @noRd 
mod_load_file_server <- function(id, tadat){
  shiny::moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    # define event reactive object when input file is selected
    df_import <- shiny::eventReactive(input$input_file, {
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      # validate file is selected
      # shiny::req(input$input_file)
      shiny::validate(need(!is.null(input$input_file), "No file selected."))
      
      # log to command line
      message(
        paste0(
          paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S \n")),
          paste0("Import, separator: '", input$separator, "'\n"),
          paste0("Import, file name: ", input$input_file$name, "\n"),
          paste0("Import, file path: ", input$input_file$datapath, "\n")
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "Import, separator: '", input$separator, "'\n",
          "Import, file name: ", input$input_file$name, "\n" #,
          # "Import, file path: ", input$input_file$datapath
        ),
        type = "message",
        duration = 5
      )
      
      # Read user imported file
      df_ml_input <- utils::read.delim(input$input_file$datapath,
                                       header = TRUE,
                                       sep = input$separator,
                                       stringsAsFactors = FALSE,
                                       na.strings = c("", "NA"))
      
      # define required columns
      required_cols <- c("MonitoringLocationIdentifier",
                         "MonitoringLocationTypeName",
                         "LatitudeMeasure",
                         "LongitudeMeasure",
                         "TADA.MonitoringLocationIdentifier",
                         "TADA.LatitudeMeasure",
                         "TADA.LongitudeMeasure")
      
      # get missing columns
      missing_cols <- setdiff(required_cols, names(df_ml_input))
      if (length(missing_cols) > 0) {
        shiny::validate(
          need(
            FALSE,
            paste0("Error: Missing required columns in loaded dataset.\n",
                   "Required columns missing from loaded dataset:\n",
                   paste0("* ", missing_cols, collapse = "\n"))
          )
        )
      }
      
      # save to tadat
      tadat$df_ml_input <- df_ml_input
      
      # return
      df_ml_input
    })
    
    # enable second tab to be selected once input data is processed
    shiny::observeEvent(!is.null(df_import()), {
      shinyjs::enable(selector = '.nav li a[data-value="Join"]')
    }, ignoreNULL = FALSE)
    
    # render data in a table
    output$df_import_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_import())
      shiny::validate(need(!is.null(input$input_file), "No file selected."))
      
      # render table
      DT::datatable(df_import(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE))
    })
    
    # render summary
    output$input_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_import)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_ml_summary <- df_import()
        
        # print
        paste0(
          "Loaded dataset has ", nrow(df_ml_summary), " rows and ", ncol(df_ml_summary), " columns.\n",
          "There are ", length(unique(df_ml_summary$MonitoringLocationIdentifier)), " unique monitoring locations."
        )
      }
    })
  })
}

# TODO do we need the separator type > can get that from the file?
# TODO need to check that if they say csv that they upload csv
# TODO add shiny validate for file size
# TODO reorder columns in df_ml_input before DT stuff

    
## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
