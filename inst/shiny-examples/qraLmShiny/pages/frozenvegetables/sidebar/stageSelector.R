fv_stageSelector_ui <- function(id, fv_stages) {
    ns = NS(id)
    print(ns("selection"))
    
    pickerInput(
        inputId = ns("selection"),
        label = h3("Select Stage"),
        choices = fv_stages,
        options = list(
            style = "btn-primary"
        ),
        selected = names(fv_stages)[1]
    )
}

fv_stageSelector_server <- function(input, output, session, suffix, fv_stages) {
    ns = NS(suffix)
    id = ns("selector")

    output[[id]] <- renderUI({
        fv_stageSelector_ui(id, fv_stages)
    })
}