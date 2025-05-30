#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# set options
# below increases the max data upload size from the shiny default of 5mb per file to 30mb for file
options(shiny.maxRequestSize = 400 * 1024^2)
options(warn = 2)

# server
app_server <- function(input, output, session) {
  # Your application server logic
  
  # create list object to hold reactive values passed between modules
  tadat <- shiny::reactiveValues()
  
  # modules
  mod_load_file_server("load_file_1", tadat)
  mod_join_aus_server("join_aus_1", tadat)
  mod_download_result_server("download_result_1", tadat)
  
  # disable other tabs upon start
  shinyjs::disable(selector = '.nav li a[data-value="Join"]')
  
  # save session info to tadat
  job_id <- paste0("ts", format(Sys.time(), "%y%m%d%H%M%S"))
  tadat$default_outfile <- paste0("tada_output_", job_id)
  tadat$job_id <- job_id
}
