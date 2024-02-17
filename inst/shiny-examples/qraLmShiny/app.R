source_all <- function() {
  libraries <- c("shiny", "plotly", "DT", "shinyjs", "shinyauthr", "dplyr","doseresponsemodels",
                 "qraLm", "analyze.stuff", "ggplot2", "hrbrthemes", "shinythemes", 
                 "shinyWidgets", "shinymanager", "shinyalert", "Hmisc", "mc2d",
                 "fontawesome", "shinydashboard", "matrixStats")
  lapply(libraries, library, character.only = TRUE)

  files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
  
  files <- list.files(path = "pages", pattern = "\\.R$", full.names = TRUE)
  lapply(files, source)
  }

source_all()

ui <- function(id) {
  about_tab_ui            <- tabPanel("qraLmShiny", about_ui("about"))

  frozenvegetables_tab_ui <- tabPanel("Frozen Vegetables", frozenvegetables_ui("frozenvegetables"))

  smokedfish_tab_ui       <- tabPanel("Smoked Fish", smokedfish_ui("smokedfish"))
  
  rtecantaloupe_tab_ui    <- tabPanel("RTE Cantaloupe", rtecantaloupe_ui("rtecantaloupe"))

  navbarPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel="icon", src="www/img/favicon.ico", type="image/icon"),  # Favicon
      tags$script(src = "custom.js"),  # custom JavaScript
      tags$style(HTML("      
        body > nav > div{
                         display: flex;
                         flex-direction: row;
                         align-items: center;
                         flex-wrap: nowrap;
                         }
      "))
    ),
    title = tags$div(
      tags$img(src = 'img/logo.svg', height = '30px', 
               alt = "World Health Organization", style="margin-right: 10px")),
    windowTitle = "qraLmShiny",
    about_tab_ui,
    
    tabPanel("Frozen Vegetables", frozenvegetables_ui("frozenvegetables")),
    
    tabPanel("Smoked Fish", smokedfish_ui("smokedfish")),
    
    tabPanel("RTE Cantaloupe", rtecantaloupe_ui("rtecantaloupe")),

    id = "navbarPageId",
    collapsible = TRUE,
    position = "static-top",
    footer = tagList(
      tags$div(
        id = "footer-content",
        style = "position: fixed; bottom: 0; left: 0; right: 0;
        z-index: 1; background-color: #005393; color: #ffffff;
        display: flex; justify-content: space-between; align-items: center;",
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
          "Visit us on:",
          tags$a(href = "https://www.who.int/", target = "_blank", "who.int")
        )
      )
    ),
    inverse = FALSE,
    fluid = FALSE,
    theme = "readable",
    selected = "qraLmShiny"
  )
}

server <- function(input, output, session) {
  
  about_server(input, output, session, "about")
    
  frozenvegetables_server(input, output, session, "frozenvegetables")
  
  smokedfish_server(input, output, session, "smokedfish")

  rtecantaloupe_server(input, output, session, "rtecantaloupe")
  }

shinyApp(ui = ui, server = server)