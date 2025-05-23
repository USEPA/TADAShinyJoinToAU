#' join_aus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_join_aus_ui <- function(id) {
  # set module session id
  ns <- NS(id)
  
  # start taglist
  tagList(
    
    # header
    htmltools::h2("Join Monitoring Locations to AUs"),
    
    # start fluidrow
    shiny::fluidRow(
      
      # left column prompts
      column(
        width = 4,
        htmltools::h3("1. Join Monitoring Locations to AUs"),
        htmltools::p("fill in"),
        htmltools::h3("fill in"),
        shiny::actionButton(
          inputId = ns("join_calc"),
          label = "Join AUs"
        ),
        htmltools::h3("2. Download Results"),
        htmltools::p("fill in"),
        # disabling this for now (sheila)
        # shinyjs::disabled(shiny::downloadButton(
        #   outputId = ns("download_results"),
        #   label = "Download Results")
        # ),
        htmltools::h3("3. Select Monitoring Locations"),
        htmltools::p("fill in"),
        shiny::textInput(
          inputId = "mlid_choice",
          label = "MonitoringLocationIdentifier",
          value = ""
        )
      ),
      
      # right column map
      column(
        width = 8,
        htmltools::h3("Join Results Map"),
        leaflet::leafletOutput(outputId = ns("join_map"),
                               width = "90%")
      )
    ),
    
    # results summary table
    shiny::fluidRow(
      # left column prompts
      column(
        width = 6,
        htmltools::h3("Join Results Table"),
        DT::dataTableOutput(outputId = ns("df_results_dt")),
        htmltools::p("fill in"),
        # htmltools::h3("fill in"),
      ) #,
      
      # match/unmatch count summary table
      # leaving this out for now b/c ben's code was shinydashboard based
      # need to create a separte module for this
      # see tadashiny mod_summary.R
      # column(
      #   width = 6,
      #   htmltools::h3("Join Results Summary"),
      #   shinydashboard::valueBoxOutput(outputID = "match_count", width = 2),
      #   shinydashboard::valueBoxOutput(outputID = "unmatch_count_l50", width = 2),
      #   shinydashboard::valueBoxOutput(outputID = "unmatch_count_g50", width = 2),
      #   htmltools::p("fill in"),
      #   # htmltools::h3("fill in"),
      # ) #,
      
      # extra - delete?
      # htmltools::hr(),
      # htmltools::p("fill in")
    )
  )
}

# load crosswalk table
df_mltoau_cw <-TADAShinyJoinToAU::mltoau_crosswalk
# mltoau_crosswalk is in data/ and is active b/c 

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
    
    # TODO need to move shinyjs from ui down to here for download button
    # TODO add more messages?
    # TODO improve qc check for empty data?
    # TODO how to show messages() to the developer vs notifications to the ui?
    # TODO TADA_GetATTAINS gives an empty df back if site is not in ATTAINS, add case to handle this
    # TODO make chunk adjustable? or not really worth it?
    # TODO add qc check to make sure df_ml_unmatched_attains is the expected size
    # TODO add qc check before rbind (and change to bind_raws()?)
    
    
    # join au button
    shiny::observeEvent(input$join_calc, {
      shiny::withProgress({
        
        # define number of increments
        n_inc <- 10
        
        # skipping output sink that ben added for now
        
        # message
        message("Begin joining monitoring locations to AUs...")
        message(paste0("Current system time: ", Sys.time()))
        # message(paste0("Imported file name: ", input$input_file$name))
        
        # define data
        df_ml_data <- tadat$df_ml_input
        
        # qc check for empty data
        if(is.null(df_ml_data)){
          return(NULL)
        }
        
        #### 1. join crosswalk ####
    
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Join initialized...")
        Sys.sleep(0.25)
        
        # join ml id's to aus
        df_join_au <- df_ml_data |>
          dplyr::left_join(df_mltoau_cw, by = "MonitoringLocationIdentifier")
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Joined crosswalk...")
        Sys.sleep(0.25)
        
        #### 2. filter ml id's that match to an au ####
        
        # filter out ml id's that matched an au and tidy df
        df_ml_matched <- df_join_au |>
          dplyr::filter(!is.na(AU_ID)) |>
          dplyr::select(MonitoringLocationIdentifier, MonitoringLocationName,
                        MonitoringLocationTypeName, AU_ID, AU_NAME,
                        LatitudeMeasure, LongitudeMeasure) |>
          dplyr::distinct() |>
          dplyr::rename(AU_ID_CrosswalkMatch = AU_ID,
                        AU_NAME_CrosswalkMatch = AU_NAME) |>
          dplyr::mutate(ATTAINS.assessmentunitidentifier = NA,
                        ATTAINS.assessmentunitname = NA,
                        ATTAINS.waterTypeCode = NA,
                        AU_Info_Source = "Regional Crosswalk Table",
                        Need_Review = "No") |>
          dplyr::select(Need_Review, AU_Info_Source, dplyr::everything()) |>
          sf::st_drop_geometry()
        # this df can sometimes be empty (if all are unmatched)
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Checked matched AUs...")
        Sys.sleep(0.25)
        
        #### 3. filter ml id's that don't match an au ####
      
        # filter out ml id's that do not have a matched au
        df_ml_unmatched <- df_join_au |>
          dplyr::filter(is.na(AU_ID))
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Checked unmatched AUs...")
        Sys.sleep(0.25)
        
        #### 4. create ml to au table for review ####
        
        # if there is unmatched data pull spatial data from ATTAINS
        if(!is.null(dim(df_ml_unmatched)[1])) {
          
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
              
              # message
              message(paste0(s, " of ", num_site_ids, " unique site ids complete"))
            }
            
            # increment progress bar, and update the detail text
            shiny::incProgress(amount = 1/n_inc, detail = "Pulled ATTAINS spatial data for unmatched AUs...")
            Sys.sleep(0.25)
            
            # combine results
            df_ml_unmatched_attains <- dplyr::bind_rows(results_list)
            
            # message
            message("Bound unmatched ATTAINS info together...")
            
            # pull joined aus
            df_ml_unmatched_attains_aus <- df_ml_unmatched_attains |>
              sf::st_drop_geometry() |>
              dplyr::select(MonitoringLocationIdentifier, MonitoringLocationName,
                            MonitoringLocationTypeName, ATTAINS.assessmentunitidentifier,
                            ATTAINS.assessmentunitname, ATTAINS.waterTypeCode,
                            LatitudeMeasure, LongitudeMeasure) |>
              dplyr::distinct() |>
              dplyr::mutate(AU_ID_CrosswalkMatch = NA,
                            AU_NAME_CrosswalkMatch = NA,
                            AU_Info_Source = dplyr::case_when((!is.na(ATTAINS.assessmentunitidentifier)) ~ "TADA ATTAINS Geospatial",
                                                              TRUE ~ "No Match; Manual Match Needed"),
                            Need_Review = "Yes") |>
              dplyr::select(Need_Review, AU_Info_Source, dplyr::everything())
            
            # merge matched and unmatched df's
            df_mltoau_review <- rbind(df_ml_unmatched_attains_aus, df_ml_matched)
            # has to use rbind b/c columns are slightly different order (sheila)
            
            # increment progress bar, and update the detail text
            shiny::incProgress(amount = 1/n_inc, detail = "Initial merge of AUs for review...")
            Sys.sleep(0.25)
          }

          # no unmatched data (df is empty)
          else {
            # save data for review
            df_mltoau_review <- df_ml_matched
            
            # increment progress bar, and update the detail text
            # shiny::incProgress(amount = 1/n_inc, detail = "fill in")
            # Sys.sleep(0.25)
          }
        }
        
        # unmatched data is null or na
        else {
          # save data for review
          df_mltoau_review <- df_ml_matched
          
          # increment progress bar, and update the detail text
          # shiny::incProgress(amount = 1/n_inc, detail = "fill in")
          # Sys.sleep(0.25)
        }
        
        #### 5. qc checks ####
        
        # message
        message("Run QC checks...")
        
        # check duplicates
        df_dup_check <- df_mltoau_review |>
          dplyr::count(MonitoringLocationIdentifier) |>
          dplyr::filter(n > 1) |>
          dplyr::mutate(FLAG_Duplicate = "Duplicate from ATTAINS") |>
          dplyr::select(-c(n))
        
        # revise df for review
        df_mltoau_review_v2 <- df_mltoau_review |>
          dplyr::left_join(df_dup_check, by = "MonitoringLocationIdentifier") |>
          dplyr::mutate(ML_Type = dplyr::case_when((MonitoringLocationTypeName == "Lake, Reservoir, Impoundment" | MonitoringLocationTypeName == "Lake") ~ "LAKE",
                                                   (MonitoringLocationTypeName == "Stream" | MonitoringLocationTypeName == "River/Stream") ~ "STREAM",
                                                   (is.na(MonitoringLocationTypeName)) ~ NA, TRUE ~ "OTHER"),
                        AU_Type = dplyr::case_when((ATTAINS.waterTypeCode == "LAKE, FRESHWATER") ~ "LAKE",
                                                   (ATTAINS.waterTypeCode == "RIVER") ~ "STREAM",
                                                   (is.na(ATTAINS.waterTypeCode)) ~ NA,
                                                   TRUE ~ "OTHER"),
                        FLAG_WaterType = dplyr::case_when((ML_Type == AU_Type) ~ NA,
                                                          (ML_Type != AU_Type) ~ "Type Mismatch",
                                                          (is.na(AU_ID_CrosswalkMatch) | is.na(ATTAINS.assessmentunitidentifier)) ~ NA,
                                                          (is.na(ML_Type) | is.na(AU_Type)) ~ "Missing Info",
                                                          TRUE ~ "ERROR"),
                        Need_Review = dplyr::case_when((!is.na(FLAG_WaterType)) ~ "Yes",
                                                       (!is.na(FLAG_Duplicate)) ~ "Yes",
                                                       TRUE ~ Need_Review)) |>
          dplyr::relocate(FLAG_WaterType, .after = AU_Info_Source) |>
          dplyr::relocate(FLAG_Duplicate, .after = FLAG_WaterType) |>
          dplyr::select(-c(ML_Type, AU_Type))
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Completed QC checks...")
        Sys.sleep(0.25)
        
        #### 6. display data in table ####
        
        # show table
        output$df_results_dt <- DT::renderDT({
          
          # qc check for empty data
          if (is.null(df_mltoau_review_v2)) {
            return(NULL)
          }
          
          # initialize ml id choice filter
          df_mlid_choice <- df_mltoau_review_v2
          
          # if ml id is chosen then filter table to that value
          if (input$mlid_choice != "") {
            
            # revise ml id choice
            df_mlid_choice <- df_mltoau_review_v2 |>
              dplyr::filter(MonitoringLocationIdentifier == input$mlid_choice)
          }
          
          # Return the filtered dataframe
          return(df_mlid_choice)
          
        },
        selection = "single",
        filter = "top",
        options = list(scrollX = TRUE,
                       pageLength = 5,
                       lengthMenu = c(5, 10, 25, 50, 100),
                       autoWidth = TRUE)
        )
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Showed table...")
        Sys.sleep(0.25)
        
        #### 7. display data map ####
        
        # show map
        output$join_map <- leaflet::renderLeaflet({
          
          # check for required values
          shiny::req(df_mltoau_review_v2)
          
          # Subset data match type
          df_sub_match <- df_mltoau_review_v2 |>
            dplyr::filter(AU_Info_Source == "Regional Crosswalk Table")
          
          df_sub_attains <- df_mltoau_review_v2 |>
            dplyr::filter(AU_Info_Source == "TADA ATTAINS Geospatial") 
          
          df_sub_unmatch <- df_mltoau_review_v2 |>
            dplyr::filter(AU_Info_Source == "No Match; Manual Match Needed")
          
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
        
        #### 8. zoomed map ####
        
        # map that filters to single location
        shiny::observeEvent(input$mlid_choice, {
          
          # check for required value
          shiny::req(input$mlid_choice != "")
          
          df_mltoau_select <- df_mltoau_review_v2 |>
            dplyr::filter(MonitoringLocationIdentifier == input$mlid_choice)
          
          # get lat and lon
          site_long <- df_mltoau_select$Longitude # longitude
          site_lat <- df_mltoau_select$Latitude # latitude
          
          # modfiy map
          leaflet::leafletProxy("join_map") |>
            leaflet::flyTo(lng = site_long, lat = site_lat, zoom = 16)
        })
        
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1/n_inc, detail = "Showed zoomed map...")
        Sys.sleep(0.25)
        
        #### 9. download data ####
        # leaving this out for now
        
      }) # with progress
    }) # observe event
  }) # module server
} # server
    
## To be copied in the UI
# mod_join_aus_ui("join_aus_1")
    
## To be copied in the server
# mod_join_aus_server("join_aus_1")
