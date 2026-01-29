#' join_aus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# TODO how to show messages() to the developer vs notifications to the ui?
# TODO what to do if required columns aren't there... (statement about TADAShiny?)
# TODO check w/ epa: need to join based on TADA.MLID not just MLID?
# TODO check w/ epa: seeing that same 6 uses are coming up for all sites
# TODO check w/ epa: there are a lot of ML ID's with missing/unresovled AU ID info
# TODO functional-ize code in server


mod_join_aus_ui <- function(id) {
  # set module session id
  ns <- shiny::NS(id)

  # start taglist
  shiny::tagList(
    # header
    htmltools::h2("2. Join Monitoring Locations to AUs"),

    # start fluidrow
    shiny::fluidRow(
      # use shinyjs
      shinyjs::useShinyjs(),

      # left column prompts
      shiny::column(
        width = 4,
        htmltools::strong("Purpose"),
        htmltools::p("This app joins monitoring locations (MLs) to assessment
        units (AUs) which can be exported. It also outputs an AU to designated
        use table. In this tab, data can be reviewed via tables and an interactive map.
        Then download the data for external review."),
        htmltools::strong("Instructions"),
        htmltools::p("Click on the button below to join MLs to AUs and their
                     designated uses (Step 2a). Once the process is completed,
                     summary tables and a map will be generated (see right).
                     Lastly, the results can be downloaded for external review
                     (Step 2b)."),
        htmltools::strong("Review"),
        htmltools::p("After downloading, please review the tables for accuracy
                     before proceeding to the analysis module. The user needs to
                     confirm that the correct AU is assigned to each ML and
                     that the correct uses are assigned to each AU."),
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
        shinyjs::disabled(
          shiny::downloadButton(
            outputId = ns("download_results"),
            label = "Download Results (.zip)",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ) # download button
        ) # shinyjs
      ), # END ~ column

      # right column map and table
      shiny::column(
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
        shiny::uiOutput(outputId = ns("join_map"), width = "90%"),
        # leaflet::leafletOutput(outputId = ns("join_map"), width = "90%"),
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
mod_join_aus_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
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

        # define organization and get code
        selected_org_code <- reactive({
          req(tadat$org_name, tadat$df_ATTAINS_orgs)
          tadat$df_ATTAINS_orgs |>
            dplyr::filter(name == tadat$org_name) |>
            dplyr::pull(code)
        })

        myOrg <- selected_org_code()

        # define data
        df_ml_data <- tadat$df_ml_input
        df_xwalk_input <- tadat$df_xwalk_input
        df_UseXwalk_input <- tadat$df_UseXwalk_input

        # qc check for empty data
        if (is.null(df_ml_data)) {
          return(NULL)
        }

        # qc check for required fields
        #  Required columns
        required_cols <- c(
          "ResultIdentifier", "TADA.MonitoringLocationName",
          "TADA.LatitudeMeasure", "TADA.LongitudeMeasure",
          "HorizontalCoordinateReferenceSystemDatumName",
          "TADA.MonitoringLocationIdentifier",
          "TADA.CharacteristicName", "ActivityStartDate",
          "OrganizationIdentifier"
        )

        missing_cols <- setdiff(required_cols, colnames(df_ml_data))

        if (length(missing_cols) > 0) {
          stop(paste(
            "The following required columns are missing from the
             df_WQ_Input dataframe:",
            paste(missing_cols, collapse = ", ")
          ))
        } # End ~ if statement

        # qc ensure input format matches TADA expectations
        # otherwise coerce
        df_ml_data <- df_ml_data |>
          # use new internal TADA function for conversion
          EPATADA::TADA_CorrectColType()

        #### 2. Create ML/AU crosswalk ####
        # log to command line
        message("Create ML/AU crosswalk...")

        # increment progress bar, and update the detail text
        shiny::incProgress(
          amount = 1 / n_inc,
          detail = "Join initialized...will take time..."
        )
        Sys.sleep(0.25)

        # browser()

        if (exists("df_xwalk_input")) {
          suppressWarnings(AUMLRef_list <- EPATADA::TADA_CreateAUMLCrosswalk(
            df_ml_data,
            au_ref = df_xwalk_input,
            org_id = myOrg,
            batch_upload = FALSE
          ))
        } else {
          suppressWarnings(AUMLRef_list <- EPATADA::TADA_CreateAUMLCrosswalk(
            df_ml_data,
            au_ref = NULL,
            org_id = myOrg,
            batch_upload = FALSE
          ))
        } # end

        # browser()
        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1 / n_inc, detail = "Join completed...")
        Sys.sleep(0.25)

        # Pull out crosswalk
        df_AUMLRef <- AUMLRef_list$ATTAINS_crosswalk

        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1 / n_inc, detail = "Fetching uses...")
        Sys.sleep(0.25)

        #### 3. Create AU/Use crosswalk####
        # log to command line
        message("Create AU/Use crosswalk...")
        # get uses
        if (exists("df_UseXwalk_input")) {
          df_UseAURef <- EPATADA::TADA_AssignUsesToAU(
            AUMLRef = df_AUMLRef,
            AU_UsesRef = df_UseXwalk_input,
            org_id = myOrg
          )
        } else {
          df_UseAURef <- EPATADA::TADA_AssignUsesToAU(
            AUMLRef = df_AUMLRef,
            AU_UsesRef = NULL,
            org_id = myOrg
          )
        } # end

        #### 4. qc checks ####

        # log to command line
        message("Run QC checks...")

        # add extra fields to ML to AU ref
        df_AUMLRef_v2 <- df_AUMLRef |>
          dplyr::mutate(Needs_Review = dplyr::case_when(
            (TADA.AURefSource == "TADA_CreateATTAINSAUMLCrosswalk" |
              TADA.AURefSource == "User-supplied Ref") ~ "Yes",
            TRUE ~ "No"
          ))

        # check all sites are there
        df_ml_data_miss_sites <- df_ml_data |>
          dplyr::filter(!(TADA.MonitoringLocationIdentifier
          %in% df_AUMLRef$TADA.MonitoringLocationIdentifier)) |>
          dplyr::select(TADA.MonitoringLocationIdentifier) |>
          dplyr::distinct() |>
          dplyr::mutate(
            ATTAINS.AssessmentUnitIdentifier = NA_character_,
            ATTAINS.WaterType = NA_character_,
            TADA.AURefSource = "No AU source found",
            ATTAINS.OrganizationIdentifier = NA_character_,
            OrganizationIdentifier = NA_character_,
            Needs_Review = "Yes"
          )

        if (nrow(df_ml_data_miss_sites) > 0) {
          df_AUMLRef_v3 <- rbind(df_AUMLRef_v2, df_ml_data_miss_sites)
        } else {
          df_AUMLRef_v3 <- df_AUMLRef_v2
        } # end

        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1 / n_inc, detail = "Completed QC checks...")
        Sys.sleep(0.25)

        #### 5. display summary ####
        # log to command line
        message("Summary information...")

        # render summary
        output$join_summary <- shiny::renderText({
          # if file was selected
          if (is.null(df_AUMLRef_v3)) {
            # print
            "Results have not been joined."
          }

          #
          else {
            # count regional crosswalk matched sites
            num_cross_match <- length(unique(df_AUMLRef_v3$TADA.MonitoringLocationIdentifier[df_AUMLRef_v3$TADA.AURefSource == "User Supplied Crosswalk"]))

            # count attains sites
            num_attains_match <- length(unique(df_AUMLRef_v3$TADA.MonitoringLocationIdentifier[df_AUMLRef_v3$TADA.AURefSource == "ATTAINS Crosswalk"]))

            # count geospatial sites
            num_geospatial <- length(unique(df_AUMLRef_v3$TADA.MonitoringLocationIdentifier[df_AUMLRef_v3$TADA.AURefSource == "TADA_CreateATTAINSAUMLCrosswalk"]))

            # total sites
            num_total <- length(unique(df_AUMLRef_v3$TADA.MonitoringLocationIdentifier))

            # total unique au's
            num_unique_aus <- length(unique(df_UseAURef$ATTAINS.AssessmentUnitIdentifier))

            # total unique uses
            num_unique_uses <- length(unique(df_UseAURef$ATTAINS.UseName))

            # print
            paste0(
              "There are ", num_total, " unique monitoring locations.\n",
              "User-supplied crosswalk matched: ", num_cross_match, " sites\n",
              "ATTAINS matched: ", num_attains_match, " sites\n",
              "TADA geospatial matched: ", num_geospatial, " sites\n",
              # "Unmatched (needs manual review): ", num_unmatch, " sites \n",
              "Number of unique AU's (manual review not included): ", num_unique_aus, " \n",
              "Number of unique associated uses (manual review not included): ", num_unique_uses, " \n"
            )
          }
        })

        #### 6. display data in tables ####
        # log to command line
        message("Display data in tables...")
        # show ml to use data as simple table (no selection)
        output$df_ml_results_dt <- DT::renderDT(
          {
            # save event reactive object
            df_data <- df_AUMLRef_v3 |>
              dplyr::rename(
                Site = TADA.MonitoringLocationIdentifier,
                AU = ATTAINS.AssessmentUnitIdentifier,
                ATTAINS_Type = ATTAINS.WaterType,
                AU_Source = TADA.AURefSource
              )
          },
          filter = "top",
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            lengthMenu = c(5, 10, 25, 50, 100),
            autoWidth = TRUE,
            rownames = FALSE,
            searching = FALSE
          )
        )

        # show au to use data as simple table (no selection)
        output$df_au_results_dt <- DT::renderDT(
          {
            # save event reactive object
            df_data <- df_UseAURef
          },
          filter = "top",
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            lengthMenu = c(5, 10, 25, 50, 100),
            autoWidth = TRUE,
            rownames = FALSE,
            searching = FALSE
          )
        )

        # increment progress bar, and update the detail text
        shiny::incProgress(amount = 1 / n_inc, detail = "Show results...")
        Sys.sleep(0.25)


        #### 7. display data map ####
        # log to command line
        message("Display data map...")

        # show map
        output$join_map <- shiny::renderUI(EPATADA::TADA_ViewATTAINS(AUMLRef_list))

        #### 8. save results ####
        # log to command line
        message("Save results...")

        # append to tadat
        tadat$df_AUMLRef <- df_AUMLRef_v3
        tadat$df_UseAURef <- df_UseAURef

        #### 9. download button ####

        # Save data frames to temporary files
        output$download_results <- shiny::downloadHandler(
          filename = function() {
            paste0(tadat$default_outfile, ".zip")
            # paste0("AU_Join_Results_",
            #        format(Sys.time(), "%Y%m%d_%H%M%S"),
            #        ".zip")
          },
          content = function(file) {
            # define file paths
            temp_dir <- tempdir()
            ml_input_file_path <- file.path(temp_dir, paste0("TADAShinyJoinToAU_copy_input_file.csv"))
            mltoaus_file_path <- file.path(temp_dir, paste0("TADAShinyJoinToAU_MLtoAUs_for_review.csv"))
            autouse_file_path <- file.path(temp_dir, paste0("TADAShinyJoinToAU_AUtoUses_for_review.csv"))
            progress_file_path <- file.path(temp_dir, paste0("TADAShinyJoinToAU_prog.rda"))
            zipfile <- file.path(temp_dir, paste0(tadat$default_outfile, ".zip"))

            # function to save tadat values
            write_tadat_file <- function(tadat, filename) {
              # define file variables to be saved
              default_outfile <- tadat$default_outfile
              job_id <- tadat$job_id
              df_ml_input <- tadat$df_ml_input
              df_mltoau_for_review <- tadat$df_AUMLRef
              df_autouse_for_review <- tadat$df_UseAURef
              temp_dir <- tadat$temp_dir

              # save file
              save(default_outfile,
                job_id,
                df_ml_input,
                df_mltoau_for_review,
                df_autouse_for_review,
                temp_dir,
                file = filename
              )
            }

            # write tadat RData file with session info
            write_tadat_file(tadat, progress_file_path)

            # Write data frames to CSV
            # utils::write.csv(df_mltoau_review_v2, tmpfile1, row.names = FALSE)
            # utils::write.csv(df_autouse_review, tmpfile2, row.names = FALSE)
            readr::write_csv(
              x = as.data.frame(tadat$df_ml_input),
              file = ml_input_file_path
            )
            readr::write_csv(
              x = as.data.frame(tadat$df_AUMLRef),
              file = mltoaus_file_path
            )
            readr::write_csv(
              x = as.data.frame(tadat$df_UseAURef),
              file = autouse_file_path
            )

            # Zip them
            utils::zip(
              zipfile = zipfile,
              files = c(
                ml_input_file_path, mltoaus_file_path,
                autouse_file_path, progress_file_path
              ),
              # files = c(tmpfile1, tmpfile2),
              flags = "-j"
            )

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
