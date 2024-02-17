sf_stageInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = id,  
    shinyjs::useShinyjs(),
    tagList(  
      shinyjs::hidden(  
        div(
          id = ns("Production"), 
          sf_ProductionInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Prefilleting"), 
          sf_PrefilletingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Filleting"), 
          sf_FilletingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Holding"), 
          sf_HoldingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("BriningOrSalting"), 
          sf_BriningOrSaltingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Smoking"), 
          sf_SmokingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Slicing"), 
          sf_SlicingInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Packaging"), 
          sf_PackagingInputs_ui(id)
        )
      ),
      
      shinyjs::hidden(  
        div(
          id = ns("Characteristics"), 
          sf_CharacteristicsInputs_ui(id)
        )
      ),  
      
      shinyjs::hidden(  
        div(
          id = ns("ColdChain"), 
          sf_ColdChainInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Home"), 
          sf_HomeInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Portioning"), 
          sf_PortioningInputs_ui(id)
        )
      ),  
      shinyjs::hidden(  
        div(
          id = ns("Risk"), 
          sf_RiskInputs_ui(id)
        )
      ) 
    )  
  )
}  
  
sf_stageInputs_server <- function(input, output, session, suffix, sf_stages) {  
  ns <- NS(suffix)  
  id <- ns("inputs")  
  
  output[[id]] <- renderUI({  
    sf_stageInputs_ui(id)  
  })  
  
  sf_stageSelectedId <- "smokedfish-sidebar-selector-selection"  
  ns1 <- NS(id)  # smokedfish-sidebar-inputs-Production 
  
  observe({  
    req(input[[sf_stageSelectedId]])  
    cat("Selected stage:", input[[sf_stageSelectedId]], "\n")  # Debugging line
    for (stage in sf_stages) {  
      cat("Checking stage:", stage, "\n")  # Debugging line
      if (input[[sf_stageSelectedId]] == stage) { 
        cat("Showing stage: ", stage, "with ns1 ==", ns1(stage), "\n") 
        shinyjs::show(ns1(stage))  
      } else {  
        shinyjs::hide(ns1(stage))  
      }  
    }  
  })  
}
