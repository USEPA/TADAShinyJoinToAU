#' join_aus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# TODO need to move shinyjs from ui down to here for download button
# TODO how to show messages() to the developer vs notifications to the ui?
# TODO TADA_GetATTAINS gives an empty df back if site is not in ATTAINS, add case to handle this
# TODO what to do if required columns aren't there... (statement about TADAShiny?)
# TODO check w/ epa: need to join based on TADA.MLID not just MLID?
# TODO check w/ epa: seeing that same 6 uses are coming up for all sites
# TODO check w/ epa: there are a lot of ML ID's with missing/unresovled AU ID info
# TODO functionalize code in server


mod_join_aus_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("2. Join Monitoring Locations to AUs"),
    
    # start fluidrow
    shiny::fluidRow(
      
      # use shinyjs
      shinyjs::useShinyjs(),
      
      # left column prompts
      column(
        width = 4,
        htmltools::strong("Purpose"),
        htmltools::p("This app joins monitoring locations (MLs) to assessment
        units (AUs) which can be exported. It also outputs an AU to designated 
        use table. In this tab, you first join the MLs to AUs, then review the 
                     results, and lastly, download the data for external review."),
        htmltools::strong("Instructions"),
        htmltools::p("Click on the button below to join MLs to AUs and their 
                     designated uses (Step 2a). Once the process is completed, 
                     summary tables and a map will be generated (see right). 
                     Lastly, the results will be downloaded for external review 
                     (Step 2b)."),
        htmltools::strong("Review"), 
        htmltools::p("After downloading, please review the tables for accuracy 
                     before proceeding to the analysis module. Pay particular 
                     attention to the 'Needs_Review', 'Source', and 'FLAG' 
                     fields. The user needs to confirm that the correct AU is 
                     assigned to each ML (JoinToAU.AssessmentUnitIdentifier 
                     field)."),
        htmltools::h3("2a. Join Monitoring Locations to AUs and Uses"),
        htmltools::p("Click on the button below."),
        shiny::actionButton(
          inputId = ns("join_calc"),
          label = "Join AUs and Uses",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        htmltools::h3("2b. Download results"),
        htmltools::p("Click on the button below."),
        htmltools::strong("The button will be disabled until the join is 
                          complete (Step 2a)."),
        shinyjs::disabled(shiny::downloadButton(
          outputId = ns("download_results"),
          label = "Download Results (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4") # download button
        ) # shinyjs
      ), # END ~ column
      
      # right column map and table
      column(
        width = 8,
        htmltools::h3("Join summary"),
        htmltools::p("Below is a numerical summary of the join process 
                     (blank until join is completed)."),
        shiny::verbatimTextOutput(outputId = ns("join_summary"), placeholder = TRUE),
        htmltools::h3("Explore results"),
        htmltools::p("Below are tabular and graphical results of the join
                     (blank until join is completed)."),
        htmltools::h3("Site Map"),
        htmltools::p("Map of sites in join organized by join results.
                     Click on the site pin to view the SiteID."),
        htmltools::br(),
        leaflet::leafletOutput(outputId = ns("join_map"), width = "90%"),
        htmltools::hr(),
        htmltools::h3("ML to AU Table"),
        htmltools::p("Summary of ML to AU join. Scroll, search, or sort the 
                     table below to explore."),
        htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_ml_results_dt")),
        htmltools::br(),
        htmltools::h3("AU to Use Table"),
        htmltools::p("Summary of AU to Use join. Scroll, search, or sort the 
                     table below to explore."),
        htmltools::br(),
        DT::dataTableOutput(outputId = ns("df_au_results_dt"))
      ) # END ~ column
    ) # END ~ shiny::fluidRow
  ) # END ~ tagList
} # END ~ function

# load crosswalk tables
# df_mltoau_cw <-TADAShinyJoinToAU::mltoau_crosswalk_simple
# df_autouse_cw <- TADAShinyJoinToAU::autouse_crosswalk_simple
# df_mltoau_cw <- mltoau_crosswalk_simple
# df_autouse_cw <- autouse_crosswalk_simple

# source any other info
# see tadashiny app mod_query_data.R dev branch for ideas

# custom functions
# see tadashiny app mod_query_data.R dev branch for ideas
    
#' join_aus Server Functions
#'
#' @noRd 
mod_join_aus_server <- function(id, tadat){
  moduleServer(id, function(input, output, session){
    # get module session id
    ns <- session$ns
    
    #### 1. join au button ####
    
    # join au button
    shiny::observeEvent(input$join_calc, {
      shiny::withProgress({
        
        # define number of increments
        n_inc <- 10
        
        # skipping output sink that ben added for now
        
        # log to command line
        message("Begin joining monitoring locations to AUs...")
        message(paste0("Current system time: ", Sys.time()))
        # message(paste0("Imported file name: ", input$input_file$name))
        
        # define data
        df_ml_data <- tadat$df_ml_input
        
        # qc check for empty data
        if(is.null(df_ml_data)){
          return(NULL)
        }
        
        #### 2. join crosswalk ####
    
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Join initialized...")
        Sys.sleep(0.25)
        
        # join ml id's to aus
        df_join_au <- df_ml_data |>
          dplyr::left_join(mltoau_crosswalk_simple, by = ("MonitoringLocationIdentifier"))
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Joined crosswalk...")
        Sys.sleep(0.25)
        
        # columns to keep
        cols_to_keep <- c("Needs_Review", # this module
                          "Source", # this module
                          "MonitoringLocationIdentifier", # file
                          "MonitoringLocationTypeName", # input file
                          "LatitudeMeasure", # input file
                          "LongitudeMeasure", # input file
                          "TADA.MonitoringLocationIdentifier", # input file
                          "TADA.LatitudeMeasure", # input file
                          "TADA.LongitudeMeasure", # input file
                          "ATTAINS.assessmentunitidentifier", # this module
                          "ATTAINS.waterTypeCode", # this module
                          "R8.AssessmentUnitIdentifier") # # this module
        
        #### 3. filter ml id's that match to an au ####
        
        # filter out ml id's that matched an au and tidy df
        df_ml_matched <- df_join_au |>
          dplyr::filter(!is.na(AssessmentUnitIdentifier)) |>
          dplyr::select(MonitoringLocationIdentifier,
                        MonitoringLocationTypeName,
                        LatitudeMeasure,
                        LongitudeMeasure,
                        TADA.MonitoringLocationIdentifier,
                        TADA.LatitudeMeasure,
                        TADA.LongitudeMeasure,
                        R8.AssessmentUnitIdentifier = AssessmentUnitIdentifier) |>
          dplyr::distinct() |>
          dplyr::mutate(Source = "Regional Crosswalk Table",
                        Needs_Review = "No",
                        ATTAINS.assessmentunitidentifier = NA,
                        ATTAINS.waterTypeCode = NA) |>
          dplyr::select(dplyr::all_of(cols_to_keep)) |>
          sf::st_drop_geometry()
        # this df can sometimes be empty (if all are unmatched)
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Checked matched AUs...")
        Sys.sleep(0.25)
        
        #### 4. filter ml id's that don't match an au ####
      
        # filter out ml id's that do not have a matched au
        df_ml_unmatched <- df_join_au |>
          dplyr::filter(is.na(AssessmentUnitIdentifier))
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Checked unmatched AUs...")
        Sys.sleep(0.25)
        
        #### 5. create ml to au table for review ####
        
        # if there is unmatched data pull spatial data from ATTAINS
        if(dim(df_ml_unmatched)[1] > 0) {
          # convert to spatial data
          df_ml_unmatched_spatial <- EPATADA::TADA_MakeSpatial(df_ml_unmatched) |>
            sf::st_transform(crs = sf::st_crs(4326))
          # force to wgs84 epsg = 4326 (unprojected)
          
          # define parameters
          site_ids <- unique(df_ml_unmatched_spatial$MonitoringLocationIdentifier)
          num_site_ids <- length(site_ids)
          chunk_size <- 1 # force this to be 1 b/c it runs faster
          # could use chunk size to loop through but skipping this for now
          results_list <- list()
          
          # loop through sites
          for (s in seq(1, num_site_ids, by = chunk_size)) {
            # get site
            df_temp_ml <- df_ml_unmatched_spatial |>
              dplyr::filter(MonitoringLocationIdentifier == site_ids[s]) |>
              sf::st_drop_geometry()
            # force this to be df b/c was getting "arguments have different crs" error (sheila) 
            
            # get data from attains
            # tictoc::tic()
            df_temp_ml_attains <- df_temp_ml |>
              EPATADA::TADA_GetATTAINS(return_sf = FALSE)
            # tictoc::toc()
            
            # store result
            results_list[[length(results_list) + 1]] <- df_temp_ml_attains
            
            # log to command line
            message(paste0(s, " of ", num_site_ids, " unique site ids complete"))
            
            # user notification that site ids complete
            shiny::showNotification(
              paste0(s, " of ", num_site_ids, " unique site ids complete"),
              type = "message",
              duration = 2
            )
          } # END ~ loop through sites
          
          # increment progress bar, and update the detail text
          shiny::incProgress(amount = 1/n_inc, detail = "Pulled ATTAINS spatial data for unmatched AUs...")
          Sys.sleep(0.25)
          
          # combine results
          df_ml_unmatched_attains <- dplyr::bind_rows(results_list)
          
          # reset row id numbers
          rownames(df_ml_unmatched_attains) <- NULL
          
          # log to command line
          message("Bound unmatched ATTAINS info together...")
          
          # pull joined aus
          df_ml_unmatched_attains_aus <- df_ml_unmatched_attains |>
            sf::st_drop_geometry() |>
            dplyr::select(MonitoringLocationIdentifier,
                          MonitoringLocationTypeName,
                          LatitudeMeasure,
                          LongitudeMeasure,
                          TADA.MonitoringLocationIdentifier,
                          TADA.LatitudeMeasure,
                          TADA.LongitudeMeasure,
                          R8.AssessmentUnitIdentifier = AssessmentUnitIdentifier,
                          ATTAINS.assessmentunitidentifier,
                          ATTAINS.waterTypeCode) |>
            dplyr::distinct() |>
            dplyr::mutate(Source = dplyr::case_when((!is.na(ATTAINS.assessmentunitidentifier)) ~ "TADA ATTAINS Geospatial",
                                                            TRUE ~ "No Match; Manual Match Needed"),
                          Needs_Review = "Yes") |>
            dplyr::select(dplyr::all_of(cols_to_keep))
          
          # merge matched and unmatched df's
          df_mltoau_review <- dplyr::bind_rows(df_ml_unmatched_attains_aus, df_ml_matched)
          # df_mltoau_review <- rbind(df_ml_unmatched_attains_aus, df_ml_matched)
          # has to use rbind if columns are slightly different order (sheila)
          
          # check that all from input are in output
          mlid_input_list <- unique(df_ml_data$MonitoringLocationIdentifier)
          
          # log to command line
          message("Checking for missing sites in output table for review...")
          
          # get missing ml id's
          mlid_missing_list <- setdiff(mlid_input_list, df_mltoau_review$MonitoringLocationIdentifier)
          
          # if values are missing need to add in missing values for review
          if (length(mlid_missing_list)[1] > 0) {
            # make empty dataset to hold values
            df_temp_missing_to_add <- df_mltoau_review |> 
              tidyr::as_tibble() |>
              dplyr::slice(0)
            
            for (m in seq(1, length(mlid_missing_list), by = 1)) {
              # get missing site info from input
              df_temp_missing_sel <- df_join_au |>
                dplyr::filter(MonitoringLocationIdentifier == mlid_missing_list[m]) |>
                dplyr::distinct() |>
                dplyr::slice(1)
              # take first row > what if this is multiple rows?
              
              # add row
              df_temp_missing_to_add <- dplyr::add_row(
                Needs_Review = "Yes",
                Source = "No Match; Manual Match Needed",
                MonitoringLocationIdentifier = df_temp_missing_sel$MonitoringLocationIdentifier,
                MonitoringLocationTypeName = df_temp_missing_sel$MonitoringLocationTypeName,
                LatitudeMeasure = df_temp_missing_sel$LatitudeMeasure,
                LongitudeMeasure = df_temp_missing_sel$LongitudeMeasure,
                TADA.MonitoringLocationIdentifier = df_temp_missing_sel$TADA.MonitoringLocationIdentifier,
                TADA.LatitudeMeasure = df_temp_missing_sel$TADA.LatitudeMeasure,
                TADA.LongitudeMeasure = df_temp_missing_sel$TADA.LongitudeMeasure,
                ATTAINS.assessmentunitidentifier = NA,
                ATTAINS.waterTypeCode = NA,
                R8.AssessmentUnitIdentifier = df_temp_missing_sel$AssessmentUnitIdentifier)
            }
            
            # append
            df_mltoau_review <- dplyr::bind_rows(df_mltoau_review, df_temp_missing_to_add)
            
            # log to command line
            message("Appending missing sites to output table for review...")
          }
          
          # else do nothing (keep df_mltoau_review as is)
          else{
            # log to command line
            message("No sites missing from output table for review...")
          }
          
          # increment progress bar, and update the detail text
          shiny::incProgress(amount = 1/n_inc, detail = "Initial merge of AUs for review...")
          Sys.sleep(0.25)
        }
        
        # unmatched data is empty
        else {
          # save data for review
          df_mltoau_review <- df_ml_matched
          
          # check that all from input are in output
          mlid_input_list <- unique(df_ml_data$MonitoringLocationIdentifier)
          
          # log to command line
          message("Checking for missing sites in output table for review...")
          
          # get missing ml id's
          mlid_missing_list <- setdiff(mlid_input_list, df_mltoau_review$MonitoringLocationIdentifier)
          
          # if values are missing need to add in missing values for review
          if (length(mlid_missing_list)[1] > 0) {
            # make empty dataset to hold values
            df_temp_missing_to_add <- df_mltoau_review |> 
              tidyr::as_tibble() |>
              dplyr::slice(0)
            
            for (m in seq(1, length(mlid_missing_list), by = 1)) {
              # get missing site info from input
              df_temp_missing_sel <- df_join_au |>
                dplyr::filter(MonitoringLocationIdentifier == mlid_missing_list[m]) |>
                dplyr::distinct() |>
                dplyr::slice(1)
              # take first row > what if this is multiple rows?
              
              # add row
              df_temp_missing_to_add <- dplyr::add_row(
                Needs_Review = "Yes",
                Source = "No Match; Manual Match Needed",
                MonitoringLocationIdentifier = df_temp_missing_sel$MonitoringLocationIdentifier,
                MonitoringLocationTypeName = df_temp_missing_sel$MonitoringLocationTypeName,
                LatitudeMeasure = df_temp_missing_sel$LatitudeMeasure,
                LongitudeMeasure = df_temp_missing_sel$LongitudeMeasure,
                TADA.MonitoringLocationIdentifier = df_temp_missing_sel$TADA.MonitoringLocationIdentifier,
                TADA.LatitudeMeasure = df_temp_missing_sel$TADA.LatitudeMeasure,
                TADA.LongitudeMeasure = df_temp_missing_sel$TADA.LongitudeMeasure,
                ATTAINS.assessmentunitidentifier = NA,
                ATTAINS.waterTypeCode = NA,
                R8.AssessmentUnitIdentifier = df_temp_missing_sel$AssessmentUnitIdentifier)
            }
            
            # append
            df_mltoau_review <- dplyr::bind_rows(df_mltoau_review, df_temp_missing_to_add)
            
            # log to command line
            message("Appending missing sites to output table for review...")
          }
          
          # else do nothing (keep df_mltoau_review as is)
          else{
            # log to command line
            message("No sites missing from output table for review...")
          }
          
          # increment progress bar, and update the detail text
          # shiny::incProgress(amount = 1/n_inc, detail = "fill in")
          # Sys.sleep(0.25)
        }
        
        
        #### 6. qc checks ####
        
        # log to command line
        message("Run QC checks...")
        
        # check all sites are there
        # length(unique(df_ml_data$MonitoringLocationIdentifier))
        
        # check duplicates
        df_dup_check <- df_mltoau_review |>
          dplyr::count(MonitoringLocationIdentifier) |>
          dplyr::filter(n > 1) |>
          dplyr::mutate(FLAG_Duplicate = "Duplicate from ATTAINS (Multiple AU IDs)") |>
          dplyr::select(-c(n))
        
        # revise df for review
        df_mltoau_review_v2 <- df_mltoau_review |>
          dplyr::left_join(df_dup_check, by = "MonitoringLocationIdentifier") |>
          dplyr::mutate(ML_Type = dplyr::case_when((MonitoringLocationTypeName == "Lake, Reservoir, Impoundment" | MonitoringLocationTypeName == "Lake") ~ "LAKE",
                                                   (MonitoringLocationTypeName == "Stream" | MonitoringLocationTypeName == "River/Stream") ~ "STREAM",
                                                   (is.na(MonitoringLocationTypeName)) ~ NA, TRUE ~ "OTHER"),
                        AU_Type = dplyr::case_when((ATTAINS.waterTypeCode == "LAKE, FRESHWATER") | (ATTAINS.waterTypeCode == "LAKE, NATURAL") ~ "LAKE",
                                                   (ATTAINS.waterTypeCode == "RIVER") ~ "STREAM",
                                                   (is.na(ATTAINS.waterTypeCode)) ~ NA,
                                                   TRUE ~ "OTHER"),
                        FLAG_MonitoringLocationTypeName = dplyr::case_when((ML_Type == AU_Type) ~ NA,
                                                                           (ML_Type != AU_Type) ~ "Type Mismatch",
                                                                           (is.na(R8.AssessmentUnitIdentifier) | is.na(ATTAINS.assessmentunitidentifier)) ~ NA,
                                                                           (is.na(ML_Type) | is.na(AU_Type)) ~ "Missing Info",
                                                                           TRUE ~ "ERROR")) |>
          dplyr::ungroup() |>
          dplyr::rowwise() |>
          dplyr::mutate(FLAG_Logic = sum(any(!is.na(FLAG_MonitoringLocationTypeName), !is.na(FLAG_Duplicate))) + sum(any(Source == "No Match; Manual Match Needed"))) |>
          dplyr::ungroup() |>
          dplyr::mutate(Needs_Review = dplyr::case_when(Needs_Review == "No" & FLAG_Logic == 0 ~ "No",
                                                        Needs_Review == "No" & FLAG_Logic >= 1 ~ "Yes",
                                                        Needs_Review == "Yes" & FLAG_Logic == 0 ~ "No",
                                                        Needs_Review == "Yes" & FLAG_Logic >= 1 ~ "Yes"),
                        interm.AssessmentUnitIdentifier = dplyr::coalesce(ATTAINS.assessmentunitidentifier, R8.AssessmentUnitIdentifier),
                        JoinToAU.AssessmentUnitIdentifier = dplyr::case_when(is.na(FLAG_MonitoringLocationTypeName) & is.na(FLAG_Duplicate) ~ interm.AssessmentUnitIdentifier,
                                                                             .default = NA)) |>
          dplyr::select(MonitoringLocationIdentifier,
                        TADA.MonitoringLocationIdentifier,
                        Needs_Review,
                        Source,
                        LatitudeMeasure,
                        LongitudeMeasure,
                        TADA.LatitudeMeasure,
                        TADA.LongitudeMeasure,
                        MonitoringLocationTypeName,
                        ATTAINS.waterTypeCode,
                        FLAG_MonitoringLocationTypeName,
                        R8.AssessmentUnitIdentifier,
                        ATTAINS.assessmentunitidentifier,
                        FLAG_Duplicate,
                        JoinToAU.AssessmentUnitIdentifier) |>
          dplyr::arrange(Needs_Review, MonitoringLocationIdentifier)
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Completed QC checks...")
        Sys.sleep(0.25)
        
        
        #### 7. create au to use table for review ####
        
        # select au's from step 4
        df_ml_sel_au <- df_mltoau_review_v2 |>
          dplyr::select(JoinToAU.AssessmentUnitIdentifier) |>
          na.omit()
        
        # join to crosswalk
        df_autouse_review <- df_ml_sel_au |>
          dplyr::left_join(autouse_crosswalk_simple, by = c("JoinToAU.AssessmentUnitIdentifier" = "AssessmentUnitIdentifier"),
                    relationship = "many-to-many") |>
          dplyr::arrange(JoinToAU.AssessmentUnitIdentifier, ATTAINS.UseName) |>
          dplyr::distinct()
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Finalize AU to use table...")
        Sys.sleep(0.25)
        
        
        #### 8. display summary ####
        
        # render summary
        output$join_summary <- shiny::renderText({
          # if file was selected
          if (is.null(df_mltoau_review_v2)) {
            
            # print
            "Results have not been joined."
          }
          
          # 
          else {
            # count regional crosswalk matched sites
            num_cross_match <- length(unique(df_mltoau_review_v2$MonitoringLocationIdentifier[df_mltoau_review_v2$Source == "Regional Crosswalk Table"]))
            
            # count attains sites
            num_attains_match <- length(unique(df_mltoau_review_v2$MonitoringLocationIdentifier[df_mltoau_review_v2$Source == "TADA ATTAINS Geospatial"]))
            
            # count unmatched sites
            num_unmatch <- length(unique(df_mltoau_review_v2$MonitoringLocationIdentifier[df_mltoau_review_v2$Source == "No Match; Manual Match Needed"]))
              
            # total sites
            num_total <- length(unique(df_mltoau_review_v2$MonitoringLocationIdentifier))
            
            # total unique au's
            num_unique_aus <- length(unique(df_autouse_review$JoinToAU.AssessmentUnitIdentifier))
            
            # total unique uses
            num_unique_uses <- length(unique(df_autouse_review$ATTAINS.UseName))
            
            # print
            paste0(
              "There are ", num_total, " unique monitoring locations.\n",
              "Regional crosswalk matched: ", num_cross_match, " sites\n",
              "ATTAINS matched: ", num_attains_match, " sites\n",
              "Unmatched (needs manual review): ", num_unmatch, " sites \n",
              "Number of unique AU's (manual review not included): ", num_unique_aus, " \n",
              "Number of unique associated uses (manual review not included): ", num_unique_uses, " \n"
            )
          }
        })
        
        #### 9. display data in tables ####
        
        # show ml to use data as simple table (no selection)
        output$df_ml_results_dt <- DT::renderDT({
          
          # save event reactive object
          df_data <- df_mltoau_review_v2
          
          },
          filter = "top",
          options = list(scrollX = TRUE, pageLength = 5,
                         lengthMenu = c(5, 10, 25, 50, 100),
                         autoWidth = TRUE)
          )
        
        # show au to use data as simple table (no selection)
        output$df_au_results_dt <- DT::renderDT({
          
          # save event reactive object
          df_data <- df_autouse_review
          
        },
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 5,
                       lengthMenu = c(5, 10, 25, 50, 100),
                       autoWidth = TRUE)
        )
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Showed tables...")
        Sys.sleep(0.25)
        
        
        #### 10. display data map ####
        
        # show map
        output$join_map <- leaflet::renderLeaflet({
          
          # check for required values
          shiny::req(df_mltoau_review_v2)
          
          # Subset data match type
          df_sub_match <- df_mltoau_review_v2 |>
            dplyr::filter(Source == "Regional Crosswalk Table")
          
          df_sub_attains <- df_mltoau_review_v2 |>
            dplyr::filter(Source == "TADA ATTAINS Geospatial") 
          
          df_sub_unmatch <- df_mltoau_review_v2 |>
            dplyr::filter(Source == "No Match; Manual Match Needed")
          
          leaflet::leaflet("join_map", options = leaflet::leafletOptions(attributionControl = FALSE)) |>
            leaflet::addTiles() |>
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap, group="Esri WSM") |>
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri Ortho") |>
            leaflet::addCircleMarkers(data = df_sub_match, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure,
                             group = "Matched Sites",
                             popup = paste("SiteID:", df_sub_match$MonitoringLocationIdentifier, "<br>"),
                             color = "black", fillColor = "#66c2a5", fillOpacity = 2, stroke = TRUE) |>
            leaflet::addCircleMarkers(data = df_sub_attains, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure,
                             group = "ATTAINS Sites",
                             popup = paste("SiteID:", df_sub_attains$MonitoringLocationIdentifier, "<br>"),
                             color = "black", fillColor = "#8da0cb", fillOpacity = 2, stroke = TRUE)|>
            leaflet::addCircleMarkers(data = df_sub_unmatch, lat = ~LatitudeMeasure, lng = ~LongitudeMeasure,
                             group = "No Match Sites",
                             popup = paste("SiteID:", df_sub_unmatch$MonitoringLocationIdentifier, "<br>"),
                             color = "black", fillColor = "#fc8d62", fillOpacity = 2, stroke = TRUE) |>
            leaflet::addLayersControl(overlayGroups = c("Matched Sites", "ATTAINS Sites", "No Match Sites"),
                             baseGroups = c("Esri WSM", "Esri Ortho"),
                             options = leaflet::layersControlOptions(collapsed = TRUE)) |>
            leaflet::addMiniMap(toggleDisplay = TRUE, tiles = leaflet::providers$Esri.WorldStreetMap,
                       position = "bottomright") |>
            leaflet::addLegend(position = "bottomleft", 
                      colors = c("#66c2a5", "#8da0cb", "#fc8d62"), 
                      labels = c("Matched Sites", "ATTAINS Sites", "No Match Sites"), 
                      title = "Site Types")
        })
        
        #### 11. zoomed map ####
        
        # map that filters to single location
        # shiny::observeEvent(input$mlid_choice, {
        #   
        #   # check for required value
        #   shiny::req(input$mlid_choice != "")
        #   
        #   df_mltoau_select <- df_mltoau_review_v2 |>
        #     dplyr::filter(MonitoringLocationIdentifier == input$mlid_choice)
        #   
        #   # get lat and lon
        #   site_long <- df_mltoau_select$Longitude # longitude
        #   site_lat <- df_mltoau_select$Latitude # latitude
        #   
        #   # modfiy map
        #   leaflet::leafletProxy("join_map") |>
        #     leaflet::flyTo(lng = site_long, lat = site_lat, zoom = 16)
        # })
        
        # increment progress bar, and update the detail text
        # shiny::incProgress(amount = 1/n_inc, detail = "Showed zoomed map...")
        # Sys.sleep(0.25)
        
        #### 12. save results ####

        # append to tadat
        tadat$df_mltoau_for_review <- df_mltoau_review_v2
        tadat$df_autouse_for_review <- df_autouse_review
        
        #### 13. download button ####
        
        # Save data frames to temporary files
        output$download_results <- downloadHandler(
          filename = function() {
            paste0("AU_Join_Results_"
                   , format(Sys.time(), "%Y%m%d_%H%M%S")
                   , ".zip")
          },
          content = function(file) {
            # Define file names for inside the ZIP
            file1 <- "MLtoAU_for_Review.csv"
            file2 <- "AUtoUse_for_Review.csv"
            
            # Create temp files
            tmpdir <- tempdir()
            tmpfile1 <- file.path(tmpdir, file1)
            tmpfile2 <- file.path(tmpdir, file2)
            zipfile <- file.path(tmpdir, "results.zip")
            
            # Write data frames to CSV
            utils::write.csv(df_mltoau_review_v2, tmpfile1, row.names = FALSE)
            utils::write.csv(df_autouse_review, tmpfile2, row.names = FALSE)
            
            # Zip them
            utils::zip(zipfile = zipfile, files = c(tmpfile1, tmpfile2)
                       , flags = "-j")
            
            # Copy zip to final destination
            file.copy(zipfile, file)
          }
        ) # END ~ downloadHandler
        
        shinyjs::enable("download_results")
        
      }) # with progress
    }) # observe event
  }) # module server
} # server
    
## To be copied in the UI
# mod_join_aus_ui("join_aus_1")
    
## To be copied in the server
# mod_join_aus_server("join_aus_1")
