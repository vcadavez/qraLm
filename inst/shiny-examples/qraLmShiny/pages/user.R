user_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "User",
    icon("user"),
    column(
      width = 12,
      tags$h2("User Information"),      
      verbatimTextOutput("user_data")
    )
  )
}

user_server <- function(input, output, session, id, credentials) {
  ns <- session$ns
  
  output$user_data <- renderPrint(dplyr::glimpse(credentials()$info))
}
