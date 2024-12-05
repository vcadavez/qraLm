# Ui
dataUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("fishCharacteristics"))
}  

# server
dataServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
  output$fishCharacteristics <-  DT::renderDataTable({
    
    df <- as.data.frame(do.call(cbind, data()))
    names(df) <- c("NaCl", "pH", "P", "CO2equi", "NIT", "aaWph", 
                   "baWph", "caWph", "daWph", "laWph", "saWph")
    dt <- DT::datatable(df,
                        # caption = "Summary statistics: between lots LM counts", 
                        class = "cell-border stripe", 
                        extensions= 'Buttons',
                        rownames = FALSE,
                        options = list(dom = 'Blrt')) %>%
       DT::formatSignif(1:11,digits = 4)
    return(dt)
    })
  
  })
}
