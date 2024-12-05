sf_stageSelector_ui <- function(id, sf_stages) {
    ns = NS(id)
    print(ns("selection"))
    
    pickerInput(
        inputId = ns("selection"),
        label = h3("Select Stage"),
        choices = sf_stages,
        options = list(
            style = "btn-primary"
        ),
        selected = names(sf_stages)[1]
    )
}

sf_stageSelector_server <- function(input, output, session, suffix, sf_stages) {
    ns = NS(suffix)
    id = ns("selector")

    output[[id]] <- renderUI({
        sf_stageSelector_ui(id, sf_stages)
    })
}
