ca_stageInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
    id = id,  
    shinyjs::useShinyjs(),
    tagList(  
      shinyjs::hidden(  
        div(
          id = ns("Production"), 
          ca_ProductionInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Harvest"), 
          ca_HarvestInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Brushing"), 
          ca_BrushingInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Storing"), 
          ca_StoringInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Washing"), 
          ca_WashingInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Dicing"), 
          ca_DicingInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Partitioning"), 
          ca_PartitioningInputs_ui(id)
        )
      ),      
      shinyjs::hidden(  
        div(
          id = ns("Testing"), 
          ca_TestingInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("Transport"), 
          ca_TransportInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("RTEStorage"), 
          ca_RTEStorageInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("ConsumersTransport"), 
          ca_ConsumersTransportInputs_ui(id)
        )
      ),
      shinyjs::hidden(  
        div(
          id = ns("StoredDices"), 
          ca_StoredDicesInputs_ui(id)
        )
      ),
      
      shinyjs::hidden(  
        div(
          id = ns("Risk"), 
          ca_RiskInputs_ui(id)
        )
      ) 
    )  
  )
}  
  
ca_stageInputs_server <- function(input, output, session, suffix, ca_stages) {  
  ns <- NS(suffix)  
  id <- ns("inputs")  
  
  output[[id]] <- renderUI({  
    ca_stageInputs_ui(id)  
  })  
  
  ca_stageSelectedId <- "rtecantaloupe-sidebar-selector-selection"  
  ns1 <- NS(id)  # rtecantaloupe-sidebar-inputs-Production 
  
  observe({  
    req(input[[ca_stageSelectedId]])  
    cat("Selected stage:", input[[ca_stageSelectedId]], "\n")  # Debugging line
    for (stage in ca_stages) {  
      cat("Checking stage:", stage, "\n")  # Debugging line
      if (input[[ca_stageSelectedId]] == stage) { 
        cat("Showing stage: ", stage, "with ns1 ==", ns1(stage), "\n") 
        shinyjs::show(ns1(stage))  
      } else {  
        shinyjs::hide(ns1(stage))  
      }  
    }  
  })  
}