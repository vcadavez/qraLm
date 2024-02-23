about_ui <- function(id) {
  tabItem(
    tabName = id,
    dashboardBody(
      fluidRow(
        column(12,
          tags$div(style = "text-align:center;", img(src = "img/hex-qraLm.png", width = "10%")),
          h3("qraLmShiny", style = "color: #005393; padding-top:20px;"),
          aboutProjectContentUI("about_project_content")
        )
      )
    )
  )
}

about_server <- function(input, output, session, id) {
  
  output$about_project_content <- renderUI(aboutProjectContent())
  
  output$key_features_content <- renderUI(keyFeaturesContent())
  
}

aboutProjectContent <- function() {
  tags$ul(
    tags$p(
      "The qraLmShiny application is designed to offer ",
      tags$b("comprehensive risk assessment for Listeria monocytogenes in foods"),
      ". Developed in collaboration WHO, the tool aims to empower industries, 
      researchers, and consumers with critical insights for food safety."
    ),
    iconList(
      "check-circle", "Interactive Models: Comprehensive risk assessment models tailored for precision.",
      "desktop",      "Fluid UI: Intuitive and interactive interface for an optimized user experience.",
      "chart-bar",    "Data Visualization: Advanced graphics and charts for in-depth analysis."
    ),
    h3("Quick Links", style = "color: #337ab7; padding-top:20px;"),
    quickLinksUI()
  )
}

keyFeaturesContent <- function() {
  iconList(
    "check-circle", " Interactive Models",
    "desktop", " Fluid UI",
    "chart-bar", " Data Visualization"
  )
}

iconList <- function(...) {
  args <- list(...)
  tags$ul(lapply(seq(1, length(args), 2), function(i) {
    tags$li(icon(args[[i]], lib = "font-awesome"), args[[i + 1]])
  }))
}

quickLinksUI <- function() {
  tags$div(
    style = "margin-top:20px;",
    icon("file-download"), " ",
    a(href = "https://www.fao.org/3/cc6993en/cc6993en.pdf", "Summary Report", style = "text-decoration:none; color:#333;"),
    br(),
    icon("github"), " ",
    a(href = "https://github.com/vcadavez/qraLmShiny", "Visit our GitHub!", style = "text-decoration:none; color:#333;")
  )
}

aboutProjectContentUI <- function(id) uiOutput(id)
keyFeaturesContentUI  <- function(id) uiOutput(id)
