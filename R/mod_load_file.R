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
  ns <- shiny::NS(id)
  
  # start taglist
  shiny::tagList(
    # header
    htmltools::h2("1. Load File"),
    
    # start fluidrow
    shiny::fluidRow(
      
      # left column file load prompts
      shiny::column(
        width = 4,
        htmltools::strong("Purpose"),
        htmltools::p("This app joins monitoring locations (MLs) to assessment
        units (AUs) which can be exported. It also outputs an AU to designated 
        use table. In this tab, load an external water quality file before
        proceeding (required). The user has the option to upload an external
        ML to AU crosswalk table as well as an AU to use crosswalk table. 
        If the user does not provide either table, defaults from ATTAINS will be used."),
        htmltools::strong("Instructions"),
        htmltools::p("The required input file for this app is a long-format data 
        file with MLs and water quality data (Step 1b). You can download a file 
        with MLs and associated chemistry data from the Water Quality Portal
        or use the", htmltools::a("TADAShinyApp."
                          , href = "https://rconnect-public.epa.gov/TADAShiny/"
                          , target = "_blank")),
        htmltools::p("The optional input files for this app is 1) a long-format data 
        file with MLs and AUs (Step 1c) and 2) a long-format data file with AUs and
        uses (Step 1d). Required fields are noted in each section."),
        htmltools::p(paste0("Note: Loaded file sizes are limited to a 
                            maximum size of ", get_golem_config("MB_LIMIT")
                            , " MB.")),
        htmltools::h3("Select file parameters"),
        selectizeInput(
          inputId = ns("org_selector"),
          label = "1a. Choose an Organization:",
          choices = NULL,  # choices are populated in server
          multiple = FALSE,
          width = "90%"
        ), 
        h3("1b. Choose water quality file to load (required):"),
        shiny::radioButtons(
          inputId = ns("input_file_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("input_file_ui")),
        
        htmltools::hr(),
        
        h3("1c. Choose ML to AU crosswalk file to load (optional):"),
        htmltools::p("Required fields include: MonitoringLocationIdentifier 
                     and AssessmentUnitIdentifier."), 
        shiny::radioButtons(
          inputId = ns("input_Xwalk_file_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("input_Xwalk_file_ui")),
        
        htmltools::hr(),
        
        h3("1d. Choose AU to Use crosswalk file to load (optional):"),
        htmltools::p("Required fields include: ATTAINS.OrganizationIdentifier
        , ATTAINS.AssessmentUnitIdentifier, ATTAINS.UseName, ATTAINS.WaterType          
        , TADA.AssessmentUnitStatus, and IncludeOrExclude."), 
        shiny::radioButtons(
          inputId = ns("input_UseXwalk_file_separator"),
          label = "Choose file separator:",
          choices = c(Comma = ",", Excel = "excel" , Tab = "\t"),
          selected = ","
        ),
        shiny::uiOutput(ns("input_UseXwalk_file_ui")),
      ),
      
      # right column table
      shiny::column(
        width = 8,
        htmltools::h3("Water Quality input file summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("input_summary"), placeholder = TRUE),
        htmltools::h3("Input data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_import_dt")), 
        
        htmltools::h3("ML to AU Crosswalk Table input file summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("input_Xwalk_summary")
                                  , placeholder = TRUE),
        htmltools::h3("Input data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_import_Xwalk_dt")),
        
        htmltools::h3("AU to Use Crosswalk Table input file summary"),
        htmltools::p("Summary of loaded file (blank until file is loaded)."),
        shiny::verbatimTextOutput(outputId = ns("input_UseXwalk_summary")
                                  , placeholder = TRUE),
        htmltools::h3("Input data Preview"),
        htmltools::p("Interactive table of input dataset (blank until file 
        is loaded). Scroll, search, or sort the table below to explore."),
        DT::dataTableOutput(outputId = ns("df_import_UseXwalk_dt"))
        
      ), # END ~  shiny::column
    ) # END ~ shiny::fluidRow
  ) # END ~ shiny::tagList
} # END ~ function


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
    
    # Render dynamic file input for input_file
    output$input_file_ui <- shiny::renderUI({
      accept_types <- switch(
        input$input_file_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls") # default
      )
      
      shiny::fileInput(
        inputId = ns("input_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
    # Render dynamic file input for input_Xwalk_file
    output$input_Xwalk_file_ui <- shiny::renderUI({
      accept_types <- switch(
        input$input_Xwalk_file_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls") # default
      )
      
      shiny::fileInput(
        inputId = ns("input_Xwalk_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
    # Render dynamic file input for input_UseXwalk_file
    output$input_UseXwalk_file_ui <- shiny::renderUI({
      accept_types <- switch(
        input$input_UseXwalk_file_separator,
        "," = c("text/csv", "text/comma-separated-values", ".csv"),
        "excel" = c(".xlsx", ".xls"),
        "\t" = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"),
        c(".csv", ".tsv", ".txt", ".xlsx", ".xls") # default
      )
      
      shiny::fileInput(
        inputId = ns("input_UseXwalk_file"),
        label = "Choose file to load:",
        width = "90%",
        placeholder = "No file selected.",
        multiple = FALSE,
        accept = accept_types
      )
    })
    
    # Org name ####
    observe({
      req(tadat$df_ATTAINS_orgs)
      updateSelectizeInput(
        session,
        inputId = "org_selector",
        choices = c("Select an organization" = ""
                    , sort(unique(tadat$df_ATTAINS_orgs$name))),
        server = TRUE
      )
    })
    
    observeEvent(input$org_selector, {
      tadat$org_name <- input$org_selector
    })
    
    # Import WQ ####
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
      
      # define file path and extension
      file_path_input <- input$input_file$datapath
      file_ext_input <- tools::file_ext(file_path_input)
      
      print(paste("Detected file extension:", file_ext_input))
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "Monitoring Location Import, separator: '", input$input_file_separator, "'\n",
          "Monitoring Location Import, file name: ", input$input_file$name, "\n",
          "Monitoring Location Import, file path: ", file_path_input, "\n",
          "Monitoring Location Import, file extension: ", file_ext_input, "\n"
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "Import, separator: '", input$input_file_separator, "'\n",
          "Import, file name: ", input$input_file$name, "\n" #,
          # "Import, file path: ", input$input_file$datapath
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file based on extension
      if (file_ext_input %in% c("csv", "tsv", "txt")) {
        df_ml_input <- utils::read.delim(file_path_input, header = TRUE
                                           , sep = input$input_file_separator
                                           , stringsAsFactors = FALSE
                                           , na.strings = c("", "NA"))
      } else if (file_ext_input %in% c("xlsx", "xls")) {
        df_ml_input <- readxl::read_excel(file_path_input, na = c("NA","")
                                            , trim_ws = TRUE, col_names = TRUE
                                            , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
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
    }) # END ~ shiny::eventReactive
    
    # Import XWalk ####
    df_import_Xwalk <- shiny::eventReactive(input$input_Xwalk_file, {
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      # validate file is selected
      # shiny::req(input$input_file)
      shiny::validate(need(!is.null(input$input_Xwalk_file), "No file selected."))
      
      # define file path and extension
      file_path_input_Xwalk <- input$input_Xwalk_file$datapath
      file_ext_input_Xwalk <- tools::file_ext(file_path_input_Xwalk)
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "ML to AU Import, separator: '", input$input_Xwalk_file_separator, "'\n",
          "ML to AU, file name: ", input$input_Xwalk_file$name, "\n",
          "ML to AU, file path: ", file_path_input_Xwalk, "\n",
          "ML to AU, file extension: ", file_ext_input_Xwalk, "\n"
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "Import, separator: '", input$input_Xwalk_file_separator, "'\n",
          "Import, file name: ", input$input_Xwalk_file$name, "\n" #,
          # "Import, file path: ", input$input_file$datapath
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file based on extension
      if (file_ext_input_Xwalk %in% c("csv", "tsv", "txt")) {
        df_xwalk_input <- utils::read.delim(file_path_input_Xwalk, header = TRUE
                                         , sep = input$input_Xwalk_file_separator
                                         , stringsAsFactors = FALSE
                                         , na.strings = c("", "NA"))
      } else if (file_ext_input_Xwalk %in% c("xlsx", "xls")) {
        df_xwalk_input <- readxl::read_excel(file_path_input_Xwalk, na = c("NA","")
                                          , trim_ws = TRUE, col_names = TRUE
                                          , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
      # define required columns
      required_cols <- c("MonitoringLocationIdentifier",
                         "AssessmentUnitIdentifier"#,
                         #"WaterType"
                         )
      
      # get missing columns
      missing_cols <- setdiff(required_cols, names(df_xwalk_input))
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
      tadat$df_xwalk_input <- df_xwalk_input
      
      # return
      df_xwalk_input
    }) # END ~ shiny::eventReactive
    
    # Import Use XWalk ####
    df_import_UseXwalk <- shiny::eventReactive(input$input_UseXwalk_file, {
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      # validate file is selected
      # shiny::req(input$input_file)
      shiny::validate(need(!is.null(input$input_UseXwalk_file), "No file selected."))
      
      # define file path and extension
      file_path_input_UseXwalk <- input$input_UseXwalk_file$datapath
      file_ext_input_UseXwalk <- tools::file_ext(file_path_input_UseXwalk)
      
      # log to command line
      message(
        paste0(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
          "AU to Use, separator: '", input$input_UseXwalk_file_separator, "'\n",
          "AU to Use, file name: ", input$input_UseXwalk_file$name, "\n",
          "AU to Use, file path: ", file_path_input_UseXwalk, "\n",
          "AU to Use, file extension: ", file_ext_input_UseXwalk, "\n"
        )
      )
      
      # user notification that file is loaded
      shiny::showNotification(
        paste0(
          "Import, separator: '", input$input_UseXwalk_file_separator, "'\n",
          "Import, file name: ", input$input_UseXwalk_file$name, "\n" #,
          # "Import, file path: ", input$input_file$datapath
        ),
        type = "message",
        duration = 5
      )
      
      # read user imported file based on extension
      if (file_ext_input_UseXwalk %in% c("csv", "tsv", "txt")) {
        df_UseXwalk_input <- utils::read.delim(file_path_input_UseXwalk, header = TRUE
                                            , sep = input$input_UseXwalk_file_separator
                                            , stringsAsFactors = FALSE
                                            , na.strings = c("", "NA"))
      } else if (file_ext_input_UseXwalk %in% c("xlsx", "xls")) {
        df_UseXwalk_input <- readxl::read_excel(file_path_input_UseXwalk, na = c("NA","")
                                             , trim_ws = TRUE, col_names = TRUE
                                             , guess_max = 100000)
      } else {
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      } # END ~ if/else
      
      # define required columns
      required_cols <- c( "ATTAINS.OrganizationIdentifier"
                          , "ATTAINS.AssessmentUnitIdentifier"
                          , "ATTAINS.UseName"
                          , "ATTAINS.WaterType"               
                          , "TADA.AssessmentUnitStatus"
                          , "IncludeOrExclude")
      
      # get missing columns
      missing_cols <- setdiff(required_cols, names(df_UseXwalk_input))
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
      tadat$df_UseXwalk_input <- df_UseXwalk_input
      
      # return
      df_UseXwalk_input
    }) # END ~ shiny::eventReactive
    
    # Enable second tab ####
    # enable second tab to be selected once input data is processed
    shiny::observeEvent(!is.null(df_import()), {
      shinyjs::enable(selector = '.nav li a[data-value="Join"]')
    }, ignoreNULL = FALSE)
    
    # Render dt WQ ####
    # render data in a table
    output$df_import_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_import())
      shiny::validate(need(!is.null(input$input_file)
                           , message = "File must be loaded."))
      
      # render table
      DT::datatable(df_import(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE,
                                   rownames = FALSE,
                                   searching = FALSE))
    }) # END ~ DT::renderDT
    
    # Render dt Xwalk ####
    # render data in a table
    output$df_import_Xwalk_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_import())
      shiny::validate(need(!is.null(input$input_Xwalk_file)
                           , message = "File must be loaded."))
      
      # render table
      DT::datatable(df_import_Xwalk(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE,
                                   rownames = FALSE,
                                   searching = FALSE))
    }) # END ~ DT::renderDT
    
    #Render dt Use Xwalk ####
    # render data in a table
    output$df_import_UseXwalk_dt <- DT::renderDT({
      # validate data is there
      # shiny::req(df_import())
      shiny::validate(need(!is.null(input$input_UseXwalk_file)
                           , message = "File must be loaded."))
      
      # render table
      DT::datatable(df_import_UseXwalk(),
                    filter = "top",
                    class = "compact",
                    options = list(scrollX = TRUE,
                                   pageLength = 5,
                                   lengthMenu = c(5, 10, 25, 50, 100),
                                   autoWidth = TRUE,
                                   rownames = FALSE,
                                   searching = FALSE))
    }) # END ~ DT::renderDT
    
    # Render WQ summary ####
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
        paste0("Loaded dataset has ", nrow(df_ml_summary), " rows and "
          , ncol(df_ml_summary), " columns.\n","There are "
          , length(unique(df_ml_summary$MonitoringLocationIdentifier))
          , " unique monitoring locations.")
      } # END ~ IF/ELSE
    }) # END ~ shiny::renderText
    
    # Render Xwalk summary ####
    output$input_Xwalk_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_import_Xwalk)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_Xwalk_summary <- df_import_Xwalk()
        
        # print
        paste0("Loaded dataset has ", nrow(df_Xwalk_summary), " rows and "
               , ncol(df_Xwalk_summary), " columns.\n","There are "
               , length(unique(df_Xwalk_summary$MonitoringLocationIdentifier))
               , " unique monitoring locations."," There are "
               , length(unique(df_Xwalk_summary$AssessmentUnitIdentifier))
               , " unique assessment units.")
      } # END ~ IF/ELSE
    }) # END ~ shiny::renderText
    
    # Render Use Xwalk summary ####
    output$input_UseXwalk_summary <- shiny::renderText({
      # if file was selected
      if (is.null(df_import_UseXwalk)) {
        
        # print
        "No file selected or file invalid."
      }
      
      # 
      else {
        # define data to summarize
        df_UseXwalk_summary <- df_import_UseXwalk()
        
        # print
        paste0("Loaded dataset has ", nrow(df_UseXwalk_summary), " rows and "
               , ncol(df_UseXwalk_summary), " columns.\n","There are "
               , length(unique(df_UseXwalk_summary$MonitoringLocationIdentifier))
               , " unique monitoring locations."," There are "
               , length(unique(df_UseXwalk_summary$AssessmentUnitIdentifier))
               , " unique assessment units.")
      } # END ~ IF/ELSE
    }) # END ~ shiny::renderText
  }) # END ~ shiny::moduleServer
} # END ~ function

# TODO do we need the separator type > can get that from the file?
# TODO need to check that if they say csv that they upload csv
# TODO add shiny validate for file size
# TODO reorder columns in df_ml_input before DT stuff

## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
