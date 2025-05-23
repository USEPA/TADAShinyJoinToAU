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
    htmltools::h2("Load File"),
    
    # start fluidrow
    shiny::fluidRow(
      
      # left column file load prompts
      column(
        width = 4,
        htmltools::h3("Load Monitoring Location File"),
        htmltools::p("Only comma-separated or tab-separated files can be loaded."),
        htmltools::h3("Select file parameters:"),
        shiny::radioButtons(
          inputId = ns("separator"),
          label = "Separator",
          choices = c(Comma = ",", Tab = "\t"),
          selected = ","
        ),
        shiny::fileInput(
          inputId = ns("input_file"),
          label = "Choose file to load:",
          width = "90%",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/tab-separated-values",
            "text/plain",
            ".csv", ".tsv", ".txt"
          )
        ),
        htmltools::hr(),
        htmltools::p("The 'separator' parameter allows the user to load a file with different formats (e.g., csv, tsv, or txt)."),
        htmltools::p("The file loaded here will be used for all subsequent steps."),
        htmltools::p(paste0("Loaded file sizes are limited to a maximum size of ", get_golem_config("MB_LIMIT"), " MB."))
      ),
      
      # right column table
      column(
        width = 8,
        htmltools::h3("Preview Loaded Data"),
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
    
    # display file name
    output$input_file_display <- shiny::renderText({
      
      # if not loaded
      if (is.null(input$input_file)) {
        return("..no file loaded yet...")
      }
      
      # else return name
      return(paste0("'", input$input_file$name, "'"))
    })
    
    # define file_watch() reactive object
    file_watch <- shiny::reactive({
      
      # trigger for df_import()
      input$input_file
    })
    
    # define df_import() event reactive object
    df_import <- shiny::eventReactive(file_watch(), {
      # input$df_import will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      # check if there's a file
      if (is.null(input$input_file)) {
        return(NULL)
      }
      
      # message
      message(paste0("Import, separator: '", input$separator, "'"))
      message(paste0("Import, file name: ", input$input_file$name))
      message(paste0("IMport, file path: ", input$input_file$datapath))
      
      # Read user imported file
      df_ml_input <- utils::read.delim(input$input_file$datapath,
                                    header = TRUE,
                                    sep = input$separator,
                                    stringsAsFactors = FALSE,
                                    na.strings = c("", "NA"))
      
      required_cols <- c("MonitoringLocationIdentifier",
                         "LatitudeMeasure",
                         "LongitudeMeasure")
      
      # qc column names against required columns list
      col_names <- colnames(df_ml_input)
      col_req_match <- required_cols %in% col_names
      col_missing <- required_cols[!col_req_match]
      
      # qc check validation message
      shiny::validate(
        need(all(required_cols %in% col_names),
             paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n",
                    paste("Required columns missing from the data:\n"),
                    paste("* ", col_missing, collapse = "\n")))
      )
      
      # commenting this out for now b/c not sure if we want it (sheila)
      # Add "results" folder if missing
      # boo_Results <- dir.exists(file.path(path_results))
      # if (boo_Results == FALSE) {
      #   dir.create(file.path(path_results))
      # }
      
      # Remove all files in "Results" folder
      # fn_results <- list.files(file.path(path_results), full.names = TRUE)
      # file.remove(fn_results)
      
      # Write to "Results" folder - Import as TSV
      # fn_input <- file.path(path_results, "data_import.tsv")
      
      # Copy to "Results" folder - Import "as is"
      # file.copy(input$fn_input$datapath
      #           , file.path(path_results, input$fn_input$name))
      
      # button, disable, download
      # shinyjs::disable("b_download")
      
      # enable second tab to be selected once input data is processed
      shinyjs::enable(selector = '.nav li a[data-value="Join"]')
      
      # return
      return(df_ml_input)
    })
    
    # import data
    output$df_import_dt <- DT::renderDT({
      
      # save event reactive object
      df_data <- df_import()
      
      # save to tadat
      tadat$df_ml_input <- df_data
    },
    filter = "top",
    options = list(scrollX = TRUE, pageLength = 5,
                   lengthMenu = c(5, 10, 25, 50, 100),
                   autoWidth = TRUE)
    )
  })
}
    
## To be copied in the UI
# mod_load_file_ui("load_file_1")
    
## To be copied in the server
# mod_load_file_server("load_file_1")
