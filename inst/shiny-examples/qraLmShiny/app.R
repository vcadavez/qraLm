# Unified R Script for Shiny App

# Function to load necessary libraries and source required files
source_all <- function() {
  libraries <- c(
    "shiny", "plotly", "DT", "shinyjs", "shinyauthr", "dplyr", "doseresponsemodels",
    "qraLm", "ggplot2", "hrbrthemes", "shinythemes", "shinyWidgets", "shinymanager",
    "shinyalert", "Hmisc", "mc2d", "fontawesome", "shinydashboard", "matrixStats"
  )
  
  log_error <- function(message) {
    write(message, file = "error_log.txt", append = TRUE)
  }
  
  # Install and load libraries
  lapply(libraries, function(lib) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      if (!require(lib, character.only = TRUE)) {
        log_error(paste("Package", lib, "failed to load or install"))
        stop(paste("Package", lib, "failed to load or install"))
      }
    }
  })
  
  # Source module and page files
  module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
  page_files <- list.files(path = "pages", pattern = "\\.R$", full.names = TRUE)
  lapply(c(module_files, page_files), source)
}

# Call the function to load libraries and source files
source_all()

# Function to create the navigation bar
create_navbar <- function() {
  navbarPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel="icon", src="www/img/favicon.ico", type="image/icon"),
      tags$script(src = "custom.js"),
      tags$style(HTML("        
        body > nav > div {
          display: flex;
          flex-direction: row;
          align-items: center;
          flex-wrap: nowrap;
        }
      "))
    ),
    title = tags$div(
      tags$img(src = 'img/logo.svg', height = '30px', alt = "qraLmShiny", style="margin-right: 10px")
    ),
    windowTitle = "qraLmShiny",
    
    tabPanel("About", about_ui("about")),
    tabPanel("Frozen Vegetables", frozenvegetables_ui("frozenvegetables")),
    tabPanel("Smoked Fish", smokedfish_ui("smokedfish")),
    tabPanel("RTE Cantaloupe", rtecantaloupe_ui("rtecantaloupe")),
    
    id = "navbarPageId",
    collapsible = TRUE,
    position = "static-top",
    
    inverse = TRUE,
    fluid = TRUE,
    theme = "custom-bootstrap.css",
    selected = "About"
  )
}

# Function to create the footer
create_footer <- function() {
  tags$div(
    id = "footer-content",
    style = "position: fixed; bottom: 0; left: 0; right: 0; z-index: 1; background-color: #005393; color: #ffffff; display: flex; justify-content: space-between; align-items: center;",
    tags$div(
      style = "padding: 10px;",
      "Copyright © 2023, WHO"
    ),
    tags$div(
      style = "padding: 10px;",
      tags$a(href = "https://www.who.int//terms", target = "_blank", "Terms and Conditions"),
      " | ",
      tags$a(href = "https://www.who.int//privacy", target = "_blank", "Privacy Policy")
    ),
    tags$div(
      style = "padding: 10px;",
      "qraLmShiny website:",
      tags$a(href = "https://pif.esa.ipb.pt/shiny/qraLmShiny/", target = "_blank", "qraLmShiny")
    )
  )
}

# Function to create the UI
create_ui <- function(id) {
  fluidPage(
    create_navbar(),
    create_footer()
  )
}

# Function to create the server logic
create_server <- function(input, output, session) {
  about_server(input, output, session, "about")
  frozenvegetables_server(input, output, session, "frozenvegetables")
  smokedfish_server(input, output, session, "smokedfish")
  rtecantaloupe_server(input, output, session, "rtecantaloupe")
}

# Run the Shiny app
shinyApp(ui = create_ui, server = create_server)
