about_ui <- function(id) {
  tabItem(
    tabName = id,
    dashboardBody(
      fluidRow(
        column(12,
          tags$div(style = "text-align:center;", img(src = "img/logo.svg", width = "25%")),
          h1("qraLmShiny", style = "color: #005393; padding-top:20px;"),
          aboutProjectContentUI("about_project_content")
        )
      )
    )
  )
}

about_server <- function(input, output, session, id) {
  
  output$about_project_content <- renderUI(aboutProjectContent())
   }

aboutProjectContent <- function() {
  fluidRow(
    column(
      width = 12,
      withTags({
        ul(
          p("In response to a request by the Codex Committee on Food Hygiene (CCFH) at its fifty-second session,
          formal risk assessment models were developed by the Joint FAO/WHO Expert meeting on microbiological risk 
          assessment of ", em("Listeria monocytogenes"), " in foods; Part 1 (FAO HQ, Rome, Italy: 24 – 28 October 2022), taking 
          into account the effects of agrifood practices, climate change and the latent possibility of cross-contamination along 
          the production chain for produce and seafood commodities.",
            style = "font-size: 15px;"
          ),
          p(),
          p("Quantitative risk assessment (QRA) models were commissioned by WHO to a team of risk modellers,
          who programmed them in open-source software, according 
            to the designed formal models and based upon an extensive literature review for data retrieval.",
            style = "font-size: 15px;"
          ),
          p(),
          p("The qraLmShiny application was designed as an easy-to-use tool to carry out risk assessments of 
          ", em("Listeria monocytogenes"), " in frozen blanched vegetables, RTE smoked fish, RTE gravad fish and RTE diced 
          cantaloupe. Using qraLmShiny it is possible to assess the effect of many processing stages; the effect of 
          preventing cross-contamination and recontamination events along the production chain; the impact of
          different within-lot testing schemes; the effectiveness of improved consumers' practices related to
          handling and storage; and the effectiveness of intervention strategies. The qraLmShiny has been
          developed as a tool for supporting decision-making by food safety authorities.",
            style = "font-size: 15px;"
          ),
          p(),
          p("Full definition of model parameters and explanation of functions can be found on the ", 
            a("Function reference manual", href = "https://vcadavez.github.io/qraLm/reference/", style = "color: blue;"),
            " of the qraLM package.",
            style = "font-size: 15px;"
          ),
          p(),
          p("V. Cadavez, V., R. Pouillot, L. Guillier, M. Sanaa, U. Gonzales-Barron (2024). qraLm: An R package
            for quantitative risk assessment of ", em("Listeria monocytogenes"), " in foods. ", 
            a("https://github.com/vcadavez/qraLm/", href = "https://github.com/vcadavez/qraLm/", style = "color: blue;"),
            style = "font-size: 15px;"
          )
        )
      })
    )#,
    # column(
    #   width = 4,
    #   img(src = "img/logo.svg", height = "100px", style = "display: block; margin-bottom: 20px;")
    # )
  )
}

aboutProjectContentUI <- function(id) {
  uiOutput(id)
}