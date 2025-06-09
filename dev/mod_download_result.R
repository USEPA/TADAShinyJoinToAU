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
      htmltools::h3("d. Download results"),
      htmltools::p("Click on the button below to download the join to AU results for review."),
      # htmltools::p("<button here?>"),
      # disabling this for now (sheila)
      shinyjs::disabled(shiny::downloadButton(
        outputId = ns("download_results"),
        label = "Download Results (.zip)"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
    )
  )
}
    
#' download_result Server Functions
#'
#' @noRd 
mod_download_result_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    # add empty reactive object to save things too
    download_things <- shiny::reactiveValues()
    
    # note to self: dataframes to be zipped for download
    # tadat$df_ml_input
    # tadat$df_for_review
    # tadat$default_outfile
    
    # save input file name
    input_filename <- paste0(tadat$default_outfile, "_copy_input_file.csv")
    
    # save result file name
    results_filename <- paste0(tadat$default_outfile, "_mltoaus_to_review.csv")
    
    # progres file name
    progress_filename <- paste0(tadat$default_outfile, "_prog.RData")
    
    # save tempfiles to tadat
    download_things$temp_files <- c(input_filename, results_filename, progress_filename)
    
    # TODO add in observe event etc. here
    
    # from ben
    # results_dirname <- "results"
    # results_full_pathname <- file.path(results_dirname, results_filename)
    # write.csv(df_mltoau_review_v2, results_full_pathname, row.names = FALSE)
    # Create zip file of results
    # file_name_zip <- list.files(path = path_results, full.names = TRUE)
    # zip::zip(file.path(path_results, "results.zip"), file_name_zip)
    # enable download button
    # shinyjs::enable("download_results")

    #### 10. allow results download ####
    # output$download_results <- downloadHandler(
    #   filename = function() {
    #     # define path
    #     paste0(tadat$default_outfile, "_results_to_review.zip")
    #   },
    #   content = function(fname) {
    #     utils::zip(zipfile = fname, files = download_things$temp_files)
    #   },
    #   contentType = "application/zip"
    # ) # download handler
    
    # increment progress bar, and update the detail text
    # shiny::incProgress(amount = 1/n_inc, detail = "Saving results for download...")
    # Sys.sleep(0.25)
    
  }) # module server
} # server

## To be copied in the UI
# mod_download_result_ui("download_result_1")
    
## To be copied in the server
# mod_download_result_server("download_result_1")
