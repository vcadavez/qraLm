sf_Packaging_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
    column(6,
           h4("Prevalence of contaminated lots"), prevLotsUI("sf_prev_lots_pack"),
           h4("Between lots LM counts"), mcstatsLotsUI("sf_mcstats_lots_pack")
           #           h4("Between lots LM Counts distribution"), countsLotsDistUI("sf_counts_lots_dist_pack")
           ),
     column(6,
            h4("Prevalence of contaminated units"), prevUnitsUI("sf_prev_units_pack"),
            h4("Within lots/Between units LM counts"), mcstatsUnitsUI("sf_mcstats_units_pack")
            #            h4("Within lots/Between units distribution"), countsUnitsDistUI("sf_counts_units_dist_pack")
            ),
     column(12, h4("Empirical Cumulative Distribution Function (ECDF)"), ecdfLotsUI("sf_ecdf_prob_pack")
            )
    )
    )
}

sf_Packaging_server <- function(input, output, session, suffix, datSlice) {
  ns <- NS(suffix)
  id <- ns("Packaging")

  output[[id]] <- renderUI({ sf_Packaging_ui(id) })

  prefix <- "smokedfish-sidebar-inputs-"
  datPack = reactive({ generate_datPack(input, prefix, datSlice) })

  prevLotsServer("sf_prev_lots_pack", data = datPack)
  prevUnitsServer("sf_prev_units_pack", data = datPack)
  mcstatsLotsServer("sf_mcstats_lots_pack", data = datPack)
  mcstatsUnitsServer("sf_mcstats_units_pack", data = datPack)
  countsLotsDistServer("sf_counts_lots_dist_pack", data = datPack)
  countsUnitsDistServer("sf_counts_units_dist_pack", data = datPack)
  ecdfLotsServer("sf_ecdf_prob_pack", data = datPack)

  return(datPack)
}

generate_datPack <- function(input, prefix, datSlice) {
  df <- sfPackaging(
                    datSlice(),
                    slicesPerPack = get_input_value(input, prefix, "slices_per_pack"))

  return(df)
}

sf_PackagingInputs_ui <- function(id) {  
  ns <- NS(id)  
  div(  
  id = ns("Packaging"),   
#    tagList(  
      sliderInput(ns("slices_per_pack"), 
                  "slicesPerPack: Number of slices per pack",
                  value = 8, min = 4, max = 20, step = 2)
#    )  
  )
}
