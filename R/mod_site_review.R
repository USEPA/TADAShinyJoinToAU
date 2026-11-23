#' site_review UI Function
#'
#' @noRd

mod_site_review_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    htmltools::h2("2. Flag and Review Sites"),
    
    shiny::fluidRow(
      shiny::column(
        width = 3,
        
        htmltools::strong("Purpose"),
        
        htmltools::p(
          "Review monitoring locations identified by TADA as having potential
           coordinate issues or nearby sites."
        ),
        
        htmltools::strong("Instructions"),
        
        htmltools::p(
          "Run the site review process ('Run Site Review' button). If coordinate 
          issues or nearby sites are flagged, maps and a review table will appear. 
          Otherwise, pass notifications will appear. If necessary, review the maps 
          and update TADA.MonitoringLocationIdentifier, TADA.Remove
          , and/or TADA.RemovalReason in the editable Site Review Table. Lastly, 
          click to 'Apply Site Decisions' button to save changes to working dataset."
        ),
        
        shiny::actionButton(inputId = ns("run_site_review"), label = "Run Site Review"),
        
        htmltools::br(),
        htmltools::br(),
        
        shiny::actionButton(inputId = ns("apply_decisions"), label = "Apply Site Decisions")
      ),
      
      shiny::column(
        width = 9,
        
        htmltools::h3("Coordinate Review"),
        
        shiny::verbatimTextOutput(ns("coordinate_summary")),
        
        shiny::uiOutput(ns("flagged_map")),
        
        htmltools::hr(),
        
        htmltools::h3("Nearby Site Review"),
        
        shiny::verbatimTextOutput(ns("nearby_summary")),
        
        shiny::uiOutput(ns("nearby_map")),
        
        htmltools::hr(),
        
        htmltools::h3("Site Review Table"),
        htmltools::p("The following sites are those that were flagged by one of 
        the two TADA functions."),
        
        DT::dataTableOutput(ns("review_table"))
      )
    )
  )
}


#' site_review Server Function
#'
#' @noRd

mod_site_review_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(df_review = NULL, review_table = NULL)
    
    
    # Run TADA functions ####
    
    observeEvent(input$run_site_review, {
      req(tadat$df_working)
      
      rv$df_review <- tadat$df_working |>
        EPATADA::TADA_FlagCoordinates() |>
        EPATADA::TADA_FindNearbySites(catchment = FALSE) |> 
        dplyr::mutate(TADA.Remove = FALSE
               , TADA.RemovalReason = NA_character_)
      
      rv$review_table <- rv$df_review |>
        dplyr::select(
          TADA.MonitoringLocationIdentifier,
          MonitoringLocationIdentifier,
          LatitudeMeasure,
          LongitudeMeasure,
          OrganizationIdentifier,
          TADA.SuspectCoordinates.Flag,
          TADA.NearbySiteGroup,
          TADA.NearbySites.Flag,
          TADA.Remove,
          TADA.RemovalReason
        ) |>
        dplyr::distinct(MonitoringLocationIdentifier,.keep_all = TRUE) |> 
        dplyr::filter(TADA.SuspectCoordinates.Flag != "Pass"
                      | TADA.NearbySites.Flag != "No nearby sites detected using input buffer distance.")

      
      shiny::showNotification("Site review complete.", type = "message")
      
    })
    
    
    # Coordinate review summary ####
    
    output$coordinate_summary <- shiny::renderText({
      req(rv$review_table)
      
      coord_summary <- rv$review_table |>
        dplyr::count(TADA.SuspectCoordinates.Flag, name = "n")
      
      paste(
        paste0(
          coord_summary$TADA.SuspectCoordinates.Flag,
          ": ",
          coord_summary$n
        ),
        collapse = "\n"
      )
      
    })
    
    
    # Nearby site summary ####
    
    output$nearby_summary <- shiny::renderText({
      req(rv$review_table)
      
      nearby_summary <- rv$review_table |>
        dplyr::count(TADA.NearbySites.Flag, name = "n")
      
      paste(
        paste0(
          nearby_summary$TADA.NearbySites.Flag,
          ": ",
          nearby_summary$n
        ),
        collapse = "\n"
      )
      
    })
    
    
    # Coordinate map ####
    
    output$flagged_map <- shiny::renderUI({
      req(rv$df_review)
      
      flagged_sites <- rv$df_review |>
        dplyr::filter(TADA.SuspectCoordinates.Flag != "Pass")
      
      if (nrow(flagged_sites) == 0) {
        return(htmltools::h4("No coordinate issues detected."))
      }
      
      EPATADA::TADA_FlaggedSitesMap(flagged_sites)
      
    })
    
    
    # Nearby site map ####
    
    output$nearby_map <- shiny::renderUI({
      req(rv$df_review)
      
      nearby_sites <- rv$df_review |>
        dplyr::filter(TADA.NearbySites.Flag !=
                        "No nearby sites detected.")
      
      if (nrow(nearby_sites) == 0) {
        return(htmltools::h4("No nearby sites detected."))
      }
      
      EPATADA::TADA_NearbySitesMap(nearby_sites)
      
    })
    
    
    # Review table ####
    
    output$review_table <- DT::renderDT({
      req(rv$review_table)
      
      DT::datatable(
        rv$review_table,
        filter = "top",
        rownames = FALSE,
        editable = TRUE,
        selection = "single",
        options = list(pageLength = 15, scrollX = TRUE)
      )
      
    })
    
    
    # Review table edits ####
    
    observeEvent(input$review_table_cell_edit, {
      req(rv$review_table)
      
      info <- input$review_table_cell_edit
      
      rv$review_table[info$row, info$col] <- info$value
      
    })
    
    
    # Apply decisions ####
    
    observeEvent(input$apply_decisions, {
      req(rv$df_review)
      req(rv$review_table)
      
      rv$df_review <- rv$df_review |>
        dplyr::left_join(
          rv$review_table |>
            dplyr::select(
              MonitoringLocationIdentifier,
              TADA.MonitoringLocationIdentifier,
              TADA.Remove,
              TADA.RemovalReason
            ),
          by = "MonitoringLocationIdentifier",
          suffix = c("", "_review")
        ) |>
        dplyr::mutate(
          TADA.MonitoringLocationIdentifier =
            dplyr::coalesce(
              TADA.MonitoringLocationIdentifier_review,
              TADA.MonitoringLocationIdentifier
            ),
          TADA.Remove =
            dplyr::coalesce(
              TADA.Remove_review,
              TADA.Remove
            ),
          TADA.RemovalReason =
            dplyr::coalesce(
              TADA.RemovalReason_review,
              TADA.RemovalReason
            )
        ) |>
        dplyr::select(
          -dplyr::ends_with("_review")
        )
      
      tadat$df_working <- rv$df_review
      
      tadat$df_final <- tadat$df_working |>
        dplyr::filter(!TADA.Remove)
      
      shiny::showNotification("Site review decisions applied.", type = "message")
      
    })
    
  })
  
}