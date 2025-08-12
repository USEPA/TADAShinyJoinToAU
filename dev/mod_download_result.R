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
    
    # note to self: dataframes to be zipped for download
    # tadat$df_ml_input
    # tadat$df_mltoau_for_review
    # tadat$df_autouse_for_review
    # tadat$default_outfile
    
    # add empty reactive object to save things too
    download_objects <- shiny::reactiveValues()
    
    # function to save tadat values
    write_tadat_file <- function(tadat, filename) {
      
      # define file variables to be saved
      default_outfile <- tadat$default_outfile
      job_id <- tadat$job_id
      df_ml_input <- tadat$df_ml_input
      df_mltoau_for_review <- tadat$df_mltoau_for_review
      df_autouse_for_review <- tadat$df_autouse_for_review
      temp_dir <- tadat$temp_dir
      
      # save file
      save(default_outfile,
           job_id,
           df_ml_input,
           df_mltoau_for_review,
           df_autouse_for_review,
           temp_dir,
           file = filename)
    }
    
    # temporary diagnostic
    shiny::observe({
      shiny::req(tadat$df_mltoau_for_review, tadat$df_autouse_for_review)
      
      message("Enabling download button and zipping files...")
      shinyjs::enable("download_results")
    })
    
    # When data is loaded, enable the download buttons
    shiny::observe({
      
      # check for required values
      shiny::req(tadat$df_mltoau_for_review, tadat$df_autouse_for_review)
      
      # define temporary directory
      temp_dir <- tempdir()
      # setwd(tempdir())
      
      # save temp_dir to tadat
      tadat$temp_dir <- temp_dir
      
      # log to command line
      message("Set temp_dir...")
      message("Temporary directory location is: ", temp_dir)
      
      # save input file name
      ml_input_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_copy_input_file.csv"))
      
      # save result file name
      mltoaus_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_mltoaus_for_review.csv"))
      
      # save result file name
      autouse_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_autouse_for_review.csv"))
      
      # progres file name (i.e., tadat)
      progress_file_path <- file.path(temp_dir, paste0(tadat$default_outfile, "_prog.rda"))
      
      # save file paths to reactive object
      download_objects$temp_files <- c(ml_input_file_path, mltoaus_file_path, autouse_file_path, progress_file_path)
      
      # write tadat RData file with session info
      write_tadat_file(tadat, progress_file_path)
      
      # log to command line
      message("Saved progress file...")
      
      # write other dataframes
      readr::write_csv(x = as.data.frame(tadat$df_ml_input), file = ml_input_file_path)
      readr::write_csv(x = as.data.frame(tadat$df_mltoau_for_review), file = mltoaus_file_path)
      readr::write_csv(x = as.data.frame(tadat$df_autouse_for_review), file = autouse_file_path)
      
      # log to command line
      message("Saved other files...")
      
      # user notification that save is complete
      shiny::showNotification(
        paste0("Saved files to temporary folder..."),
        type = "message",
        duration = 2
      )
      
      # log to command line
      # message("Packing up files into zipped folder...")
      
      # zip up whatever is in the reactive object
      output$download_results <- shiny::downloadHandler(
        filename = function() {
          # define zip_file path
          paste0(tadat$default_outfile, ".zip")
        },
        content = function(zip_file) {
          # make sure files exist
          shiny::req(download_objects$temp_files)
          
          # make sure all files exist
          missing_files <- download_objects$temp_files[!file.exists(download_objects$temp_files)]
          if (length(missing_files)) {
            stop("Some expected files are missing: ", paste(missing, collapse = ", "))
          }
          
          # zip() by default preserves folder structure; "-j" drops paths
          utils::zip(
            zipfile   = zip_file,
            files     = download_objects$temp_files,
            flags     = "-j"
          )
        },
        contentType = "application/zip"
      )
      
      # log to command line
      message("Folder zipped up...")
      
      # user notification that save is complete
      shiny::showNotification(
        paste0("Zipped files."),
        type = "message",
        duration = 2
      )
    }) # observe
  }) # module server
} # server

## To be copied in the UI
# mod_download_result_ui("download_result_1")
    
## To be copied in the server
# mod_download_result_server("download_result_1")
