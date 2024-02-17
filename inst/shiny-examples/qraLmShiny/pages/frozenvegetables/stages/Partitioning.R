fv_Partitioning_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(
  fluidRow(
           column(6,
                  h4("Prevalence of contaminated lots"),prevLotsUI("fv_prev_lots_part"),
                  h4("Between lots LM counts"), mcstatsLotsUI("fv_lots_mcstats_part")
                  #                  h4("Between lots LM Counts distribution"), countsLotsDistUI("fv_counts_lots_dist_part")
                  ),
           column(6,
                  h4("Prevalence of contaminated units"), prevUnitsUI("fv_prev_units_part"),
                  h4("Within lots/Between units LM counts"), mcstatsUnitsUI("fv_units_mcstats_part")
#                  h4("Within lots/Between units LM counts distribution"), countsUnitsDistUI("fv_counts_units_dist_part")
                  ),
           column(12, 
                  h4("ECDF plot"), ecdfLotsUI("fv_ecdf_prob_part")
                  )
           )
  )
}

fv_Partitioning_server <- function(input, output, session, suffix, datBlanch) {
  ns <- NS(suffix)
  id <- ns("Partitioning")

  output[[id]] <- renderUI({ fv_Partitioning_ui(id) })
  
  prefix <- "frozenvegetables-sidebar-inputs-"
  datPart <- reactive({ generate_datPart(input, prefix, datBlanch) })
  
  prevLotsServer("fv_prev_lots_part",                data=datPart)
  prevUnitsServer("fv_prev_units_part",              data=datPart)
  mcstatsLotsServer("fv_lots_mcstats_part",          data=datPart)
  mcstatsUnitsServer("fv_units_mcstats_part",        data=datPart)
  countsLotsDistServer("fv_counts_lots_dist_part",   data=datPart)
  countsUnitsDistServer("fv_counts_units_dist_part", data=datPart)
  ecdfLotsServer("fv_ecdf_prob_part",                data=datPart)
  return(datPart)
}

generate_datPart <- function(input, prefix, datBlanch) {
  set.seed(get_input_value(input, prefix, "seed"))
  df <- fvPartitioningCC(
                     datBlanch(),
                     probCC = get_input_value(input, prefix, "pcc"),
                      nEquip = get_input_value(input, prefix, "n_equip"),
                     bCCFV  = get_input_value(input, prefix, "bccfv")
  )
  return(df)
}

fv_PartitioningInputs_ui <- function(id) {
  ns <- NS(id)
  div(
  id = ns("Partitioning"),
#  tagList(
    sliderInput(ns("pcc"), 
                "probCC: Prob. cross-contamination", 
                value=0.25, min=0.0, max=1.0, step=0.01),
    sliderInput(ns("n_equip"), 
                "nEquip: Numbers of LM cells on the surface (CFU)", 
                value=45000, min=0, max=100000, step=10000),
    sliderInput(ns("bccfv"), 
                "bCCFV: Dispersion factorof LM cells", 
                value=1.0, min=0, max=2, step=0.10)
 #   )
 )
}

probCC <- 0.25
trMean <- -0.44
trSd <- 0.40
nEquip <- 9*5000 # not running with 9 or 25
bCCFV <- 1
