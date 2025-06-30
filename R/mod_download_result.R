#' download_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_result_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # horizontal line
    # htmltools::hr(),
    
    # start fluidrow
    shiny::fluidRow(
      # download results
      htmltools::h3("e. Download results"),
      htmltools::p("Click on the button below to download the join to AU module results for review."),
      shinyjs::disabled(shiny::downloadButton(
        outputId = ns("download_results"),
        label = "Download Results (.zip)",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4") # download button
      ) # shinyjs
    ) # fluid row
  ) # taglist
}
    
#' download_result Server Functions
#'
#' @noRd 
mod_download_result_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    # add empty reactive object to save things too
    download_objects <- shiny::reactiveValues(temp_files = NULL)
    
    # note to self: dataframes to be zipped for download
    # tadat$df_ml_input
    # tadat$df_mltoau_for_review
    # tadat$df_autouse_for_review
    # tadat$default_outfile
    
    # function to save tadat values
    write_tadat_file <- function(tadat, filename) {
      default_outfile <- tadat$default_outfile
      job_id <- tadat$job_id
      df_ml_input <- tadat$df_ml_input
      df_mltoau_for_review <- tadat$df_mltoau_for_review
      df_autouse_for_review <- tadat$df_autouse_for_review
      temp_dir <- tadat$temp_dir
      
      save(default_outfile,
           job_id,
           df_ml_input,
           df_mltoau_for_review,
           df_autouse_for_review,
           temp_dir
      )
    }
    
    # When data is loaded, enable the download buttons
    shiny::observeEvent(tadat$dfdf_mltoau_for_review, {
      if (is.null(tadat$df_mltoau_for_review)) {
        
      } else {
        
        # define temporary directory
        temp_dir <- tempdir()
        # setwd(tempdir())
        
        # save temp_dir to tadat
        tadat$temp_dir <- temp_dir
        
        # save input file name
        ml_input_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_copy_input_file.csv"))
        
        # save result file name
        mltoaus_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_mltoaus_to_review.csv"))
        
        # save result file name
        autouse_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_autouse_to_review.csv"))
        
        # progres file name (i.e., tadat)
        progress_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_prog.RData"))
        
        # save file paths to reactive object
        download_objects$temp_files <- c(ml_input_file_path, mltoaus_file_path, autouse_file_path, progress_file_path)
        
        # write tadat RData file with session info
        write_tadat_file(tadat, progress_filename)
        
        # write other dataframes
        readr::write_csv(x = tadat$df_ml_input, file = ml_input_file_path)
        readr::write_csv(x = tadat$df_mltoau_for_review, file = mltoaus_file_path)
        readr::write_csv(x = tadat$df_autouse_review, file = autouse_file_path)
        
        # user notification that save is complete
        shiny::showNotification(
          paste0("Saved files to temporary folder..."),
          type = "message",
          duration = 2
        )
        
        # enable download
        shinyjs::enable("download_results")
        
        # zip up whatever is in the reactive object
        output$download_results <- downloadHandler(
          filename = function() {
            # define zip_file path
            paste0(tadat$default_outfile, ".zip")
          },
          content = function(zip_file) {
            # make sure files exist
            shiny::req(!is.null(download_objects$temp_files))
            
            # zip() by default preserves folder structure; "-j" drops paths
            utils::zip(
              zipfile   = zip_file,
              files     = download_objects$temp_files,
              flags     = "-j"
            )
          },
          contentType = "application/zip"
        )
        
        # user notification that save is complete
        shiny::showNotification(
          paste0("Zipped files."),
          type = "message",
          duration = 2
        )
      }
    }) # observe event
  }) # module server
} # server

## To be copied in the UI
# mod_download_result_ui("download_result_1")
    
## To be copied in the server
# mod_download_result_server("download_result_1")
