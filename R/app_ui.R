#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# css below addresses https://github.com/USEPA/TADAShiny/issues/198?
css <- "
.nav li a.disabled {
  background-color: #F5F5F5 !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #F5F5F5 !important;
}

.row {
    margin-right: 0px;
    margin-left: 0px;
}
"

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    # add scroll back to the top feature
    shiny::fluidPage(
      tags$html(class = "no-js", lang = "en"),

      # standardized Go to Top button appears on lower-right corner when window is scrolled down 100 pixels
      gotop::use_gotop( # add it inside the ui
        src = "fas fa-chevron-circle-up", # css class from Font Awesome
        opacity = 0.8, # transparency
        width = 60, # size
        appear = 100 # number of pixels before appearance
      ),

      # add development banner
      # HTML("<div id='eq-disclaimer-banner' class='padding-1 text-center text-white bg-secondary-dark'><strong>EPA development environment:</strong> The
      # content on this page is not production ready. This site is being used
      # for <strong>development</strong> and/or <strong>testing</strong> purposes
      # only.</div>"),

      # add epa header html from here: https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
      shiny::includeHTML(app_sys("app/www/header.html")),
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(css),
      htmltools::br(),
      shiny::headerPanel(title = "Tools for Automated Data Analysis (TADA) Module 2: Join Monitoring Locations to AUs"),
      htmltools::br(),

      # create a navbar page with tabs at the top
      shiny::tabsetPanel(
        id = "tabbar",
        shiny::tabPanel("1. Load File",
          value = "Load", # each tabPanel represents a tab page at the top of the navbar
          htmltools::br(),
          mod_load_file_ui("load_file_1")
        ),
        shiny::tabPanel("2. Join AUs",
          value = "Join",
          htmltools::br(),
          mod_join_aus_ui("join_aus_1") # ,
          # htmltools::hr(),
          # mod_download_result_ui("download_result_1")
        ),
      ),

      # add horizontal line
      # htmltools::hr(),

      # add download button above footer
      # mod_download_result_ui("download_result_1"),

      # add break
      # htmltools::br(),

      # add epa footer html
      shiny::includeHTML(app_sys("app/www/footer.html"))
    ) # ,

    # fluidPage(
    #   golem::golem_welcome_page() # Remove this line to start building your UI
    # )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TADAShinyJoinToAU"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
